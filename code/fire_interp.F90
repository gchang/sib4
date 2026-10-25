!--------------------------------------------------------------
subroutine fire_interp(indx, lon, lat, &
           sibg)

!--------------------------------------------------------------
!
! This subroutine interpolates the sibdrv fire emissions
!     between their read times
!
!--------------------------------------------------------------

use kinds
use module_io, only: &
   fire_step, fire_seccur, fire_secnext
use module_oparams, only: &
    fire_leaff, fire_stwdf, &
    fire_cdbf, fire_metlf, fire_strlf
use module_param, only: poolcon
use module_pparams, only: &
     mwc, mol_to_umol, month_names
use module_pftinfo, only: pft_num
use module_poolinfo
use module_sib, only: gridcell_type
use module_sibconst, only: &
   npoolpft, npoollu, nsoil, &
   fireb_print, fireb_stop, fireb_thresh
use module_time, only: &
   month, day, year, &
   dtisib, dtsib, sec_tot, wt_daily

implicit none

!...input variables
integer(i4), intent(in) :: indx
real(r4), intent(in) :: lon, lat
type (gridcell_type), intent(inout) :: sibg

!...parameters
integer(i4), parameter :: isave=3
real(r8) :: dnzero=1.E-10

!...interpolation variables
real(r8) :: facsibdrv  ! scaling factor between driver data points
real(r8) :: totemis, curemis, pcemis

!...distribution variables
integer(i4) :: ntpft
integer(i4), dimension(:), allocatable :: tpref, tpnum
real(r4), dimension(:), allocatable :: tparea
real(r8), dimension(:), allocatable :: tpagb, tpagbtemp
real(r8), dimension(:), allocatable :: tpbiomass

real(r8) :: tfarea
integer(i4), dimension(:), allocatable :: tsortref
real(r8), dimension(:), allocatable :: flossb
real(r8), dimension(:,:), allocatable :: flosspft, flosslu

!...net balance variables
logical :: fb_err

!...misc variables
integer :: ido, idoc, l, p, s, myl
real(r8) :: myemis, tempemis
integer(i4), dimension(1) :: tempstore

!-----------------------------------------------
! reset values
do l=1, sibg%g_nlu
   sibg%l(l)%poollt%resp_fire = dzero
   sibg%l(l)%poollt%rmmd_fire = dzero
   sibg%l(l)%poollt%loss_fire_lay(:,:) = dzero
   sibg%l(l)%pooldt%loss_fire_lay(:,:) = dzero
enddo
   
! only continue if fire emissions are being used
IF (fire_step .le. izero) RETURN

! get scaling factors
facsibdrv = dble(fire_seccur-sec_tot) / dble(fire_step)

! only continue if fire emissions are valid at this time
IF ((facsibdrv .GT. 1) .OR. (facsibdrv .LT. 0)) THEN
   sibg%gprogt%firec = dzero
   sibg%gprogt%fireco2 = dzero
   RETURN
ENDIF

! interpolate fire C
sibg%gprogt%firec = facsibdrv*sibg%gprogt%firec1 + &
                    (1.-facsibdrv) * sibg%gprogt%firec2

! interpolate fire CO2
sibg%gprogt%fireco2 = facsibdrv*sibg%gprogt%fireco21 &
         + (1.-facsibdrv) * sibg%gprogt%fireco22

! distribute fire emissions per PFT/land unit
if (sibg%gprogt%firec .gt. dzero) then
   totemis = sibg%gprogt%firec * dtsib
   ntpft = sibg%g_nlu
   allocate(tpref(ntpft),tpnum(ntpft),tparea(ntpft))
   allocate(tpagb(ntpft),tpbiomass(ntpft))
   tpref(:) = sibg%l(1:ntpft)%ipft   
   tpnum(:) = pft_num(tpref)
   tparea(:) = sibg%l(1:ntpft)%larea
   tpagb(:) = dzero
   tpbiomass(:) = dzero

   do l=1, ntpft
     !...calculate total above-ground biomass (m-2)
      do p=1, npoolpft
         if (pool_indx_lay(p) .eq. 1) then
            tpagb(l) = tpagb(l) + tparea(l) &
                 * (sibg%l(l)%poollt%poolpft(p) &
                 - sum(sibg%l(l)%poollt%poolpft_dloss(p,:)) &
                 - poolcon(tpnum(l))%poolpft_min(p))
         endif
      enddo
      do p=1, npoollu
         if (pool_indx_lay(p+npoolpft) .eq. 1) then
            tpagb(l) = tpagb(l) + tparea(l) &
                 * (sibg%l(l)%pooldt%poollu(p) &
                    - sum(sibg%l(l)%pooldt%poollu_dloss(p,:)))
         endif
      enddo
      
      !...calculate total biomass (m-2)
      tpbiomass(l) = tpbiomass(l) + sum(sibg%l(l)%poollt%poolpft) &
           - sum(sibg%l(l)%poollt%poolpft_dloss) &
           - sum(poolcon(tpnum(l))%poolpft_min)
      tpbiomass(l) = tpbiomass(l) + sum(sibg%l(l)%pooldt%poollu) &
           - sum(sibg%l(l)%pooldt%poollu_dloss)
   enddo


   !...rank PFTs by above ground biomass
   allocate(tpagbtemp(ntpft),tsortref(ntpft))
   tpagbtemp = tpagb
   do l=1,ntpft
       tempstore = maxloc(tpagbtemp)
       tsortref(l) = tempstore(1)
       tpagbtemp(tsortref(l)) = -1
   enddo
   
   !...check area of top PFTs
   ido=MIN(isave,ntpft)
   idoc=ido
   do l=1,ido
      myl = tsortref(l)
      if (tpagb(myl) .lt. totemis) then
         idoc=MAX(ione, idoc-1)
      endif
   enddo
   ido=idoc
   tfarea = SUM(tparea(tsortref(1:ido)))

   !...remove carbon from top PFTs
   allocate(flosspft(ntpft,npoolpft))
   allocate(flosslu(ntpft,npoollu))
   flosspft(:,:) = dzero
   flosslu(:,:) = dzero
   allocate(flossb(ntpft))
   flossb(:) = dzero
   
   do l=ido,1,-1
      myl = tsortref(l)
      sibg%l(myl)%poollt%nd_fire = sibg%l(myl)%poollt%nd_fire + wt_daily
      
      tempemis = totemis*(tparea(myl)/tfarea)
      curemis = tempemis

      !....remove C from leaf pool
      myemis = MIN(fire_leaff*tempemis, MAX(dzero, &
           sibg%l(myl)%poollt%poolpft(pool_indx_leaf) &
             - sibg%l(myl)%poollt%poolpft_dloss(pool_indx_leaf,1) &
             - poolcon(tpnum(myl))%poolpft_min(pool_indx_leaf)))
      flosspft(myl,pool_indx_leaf) = myemis
      sibg%l(myl)%poollt%loss_fire_lay(pool_indx_leaf,1) = myemis * dtisib
      sibg%l(myl)%poollt%poolpft_dloss(pool_indx_leaf,1) = &
           sibg%l(myl)%poollt%poolpft_dloss(pool_indx_leaf,1) &
           + myemis
      curemis = curemis - myemis

      !....remove C from wood pool
      myemis = MIN(fire_stwdf*tempemis, MAX(dzero, &
           sibg%l(myl)%poollt%poolpft(pool_indx_stwd) &
             - sibg%l(myl)%poollt%poolpft_dloss(pool_indx_stwd,1) &
             - poolcon(tpnum(myl))%poolpft_min(pool_indx_stwd)))
      sibg%l(myl)%poollt%loss_fire_lay(pool_indx_stwd,1) = myemis * dtisib
      sibg%l(myl)%poollt%poolpft_dloss(pool_indx_stwd,1) = &
           sibg%l(myl)%poollt%poolpft_dloss(pool_indx_stwd,1) &
           + myemis
      flosspft(myl,pool_indx_stwd) = myemis
      curemis = curemis - myemis

      !....remove C from metabolic litter
      myemis = MIN(fire_metlf*tempemis, MAX(dzero, &
           sibg%l(myl)%pooldt%poollu(pool_indx_metl-npoolpft) &
           - sibg%l(myl)%pooldt%poollu_dloss(pool_indx_metl-npoolpft,1)))
      sibg%l(myl)%pooldt%loss_fire_lay(pool_indx_metl-npoolpft,1) = myemis * dtisib
      sibg%l(myl)%pooldt%poollu_dloss(pool_indx_metl-npoolpft,1) = &
           sibg%l(myl)%pooldt%poollu_dloss(pool_indx_metl-npoolpft,1) &
           + myemis
      flosslu(myl,pool_indx_metl-npoolpft) = myemis
      curemis = curemis - myemis

      !....remove C from structural litter
      myemis = MIN(fire_strlf*tempemis, MAX(dzero, &
           sibg%l(myl)%pooldt%poollu(pool_indx_strl-npoolpft) &
           - sibg%l(myl)%pooldt%poollu_dloss(pool_indx_strl-npoolpft,1)))
      sibg%l(myl)%pooldt%loss_fire_lay(pool_indx_strl-npoolpft,1) = myemis * dtisib
      sibg%l(myl)%pooldt%poollu_dloss(pool_indx_strl-npoolpft,1) = &
           sibg%l(myl)%pooldt%poollu_dloss(pool_indx_strl-npoolpft,1) &
           + myemis
      flosslu(myl,pool_indx_strl-npoolpft) = myemis
      curemis = curemis - myemis

      !....remove C from coarse dead biomass
      myemis = MIN(fire_cdbf*tempemis, MAX(dzero, &
           sibg%l(myl)%pooldt%poollu(pool_indx_cdb-npoolpft) &
           - sibg%l(myl)%pooldt%poollu_dloss(pool_indx_cdb-npoolpft,1)))
      sibg%l(myl)%pooldt%loss_fire_lay(pool_indx_cdb-npoolpft,1) = myemis * dtisib
      sibg%l(myl)%pooldt%poollu_dloss(pool_indx_cdb-npoolpft,1) = &
           sibg%l(myl)%pooldt%poollu_dloss(pool_indx_cdb-npoolpft,1) &
           + myemis
      flosslu(myl,pool_indx_cdb-npoolpft) = myemis
      curemis = curemis - myemis

      !...remove C from product
      IF (curemis .gt. dnzero) THEN
         myemis = MIN(curemis, MAX(dzero, &
              sibg%l(myl)%poollt%poolpft(pool_indx_prod) &
              - sibg%l(myl)%poollt%poolpft_dloss(pool_indx_prod,1) &
              - poolcon(tpnum(myl))%poolpft_min(pool_indx_prod)))
         sibg%l(myl)%poollt%loss_fire_lay(pool_indx_prod,1) = myemis * dtisib
         sibg%l(myl)%poollt%poolpft_dloss(pool_indx_prod,1) = &
              sibg%l(myl)%poollt%poolpft_dloss(pool_indx_prod,1) &
              + myemis
         flosspft(myl,pool_indx_prod) = myemis
         curemis = curemis - myemis
      ENDIF
      
      !...remove C from roots
      IF (curemis .gt. dnzero) THEN
          myemis = MIN(curemis, MAX(dzero, &
               sibg%l(myl)%poollt%poolpft(pool_indx_froot) &
               - sum(sibg%l(myl)%poollt%poolpft_dloss(pool_indx_froot,:)) &
               - poolcon(tpnum(myl))%poolpft_min(pool_indx_froot)))
          DO s=1,nsoil 
             sibg%l(myl)%poollt%loss_fire_lay(pool_indx_froot,s) = &
                  myemis * dtisib * sibg%l(myl)%poollt%poolpft_flay(pool_indx_froot,s)
             sibg%l(myl)%poollt%poolpft_dloss(pool_indx_froot,s) = &
                  sibg%l(myl)%poollt%poolpft_dloss(pool_indx_froot,s) &
                  + myemis * sibg%l(myl)%poollt%poolpft_flay(pool_indx_froot,s)
          ENDDO
          flosspft(myl,pool_indx_froot) = myemis
          curemis = curemis - myemis
       ENDIF

       IF (curemis .gt. dnzero) THEN        
           myemis = MIN(curemis, MAX(dzero, &
               sibg%l(myl)%poollt%poolpft(pool_indx_croot) &
               - sum(sibg%l(myl)%poollt%poolpft_dloss(pool_indx_croot,:)) & 
               - poolcon(tpnum(myl))%poolpft_min(pool_indx_croot)))
          DO s=1,nsoil 
             sibg%l(myl)%poollt%loss_fire_lay(pool_indx_croot,s) = &
                  myemis * dtisib * sibg%l(myl)%poollt%poolpft_flay(pool_indx_croot,s)
             sibg%l(myl)%poollt%poolpft_dloss(pool_indx_croot,s) = &
                  sibg%l(myl)%poollt%poolpft_dloss(pool_indx_croot,s) &
                  + myemis * sibg%l(myl)%poollt%poolpft_flay(pool_indx_croot,s)
          ENDDO
          flosspft(myl,pool_indx_croot) = myemis
          curemis = curemis - myemis
       ENDIF

       !...remove C from soil litter
       IF (curemis .gt. dnzero) THEN
           myemis = MIN(curemis, MAX(dzero, &
                sibg%l(myl)%pooldt%poollu(pool_indx_slit-npoolpft) &
                - sum(sibg%l(myl)%pooldt%poollu_dloss(pool_indx_slit-npoolpft,:))))
           DO s=1,nsoil 
             sibg%l(myl)%pooldt%loss_fire_lay(pool_indx_slit-npoolpft,s) = &
                  myemis * dtisib * sibg%l(myl)%pooldt%poollu_flay(pool_indx_slit-npoolpft,s)
             sibg%l(myl)%pooldt%poollu_dloss(pool_indx_slit-npoolpft,s) = &
                  sibg%l(myl)%pooldt%poollu_dloss(pool_indx_slit-npoolpft,s) &
                  + myemis * sibg%l(myl)%pooldt%poollu_flay(pool_indx_slit-npoolpft,s)
          ENDDO
          flosslu(myl,pool_indx_slit-npoolpft) = myemis
         curemis = curemis - myemis
      ENDIF
      
      !...remove C from soil slow
      IF (curemis .gt. dnzero) THEN
          myemis = MIN(curemis, MAX(dzero, &
               sibg%l(myl)%pooldt%poollu(pool_indx_slow-npoolpft) &
               - sum(sibg%l(myl)%pooldt%poollu_dloss(pool_indx_slow-npoolpft,:))))
          DO s=1,nsoil 
             sibg%l(myl)%pooldt%loss_fire_lay(pool_indx_slow-npoolpft,s) = &
                  myemis * dtisib * sibg%l(myl)%pooldt%poollu_flay(pool_indx_slow-npoolpft,s)
             sibg%l(myl)%pooldt%poollu_dloss(pool_indx_slow-npoolpft,s) = &
                  sibg%l(myl)%pooldt%poollu_dloss(pool_indx_slow-npoolpft,s) &
                  + myemis * sibg%l(myl)%pooldt%poollu_flay(pool_indx_slow-npoolpft,s)
          ENDDO
          flosslu(myl,pool_indx_slow-npoolpft) = myemis
          curemis = curemis - myemis
      ENDIF

      !...remove C from soil passive
      IF (curemis .gt. dnzero) THEN
          myemis = MIN(curemis, MAX(dzero, &
               sibg%l(myl)%pooldt%poollu(pool_indx_arm-npoolpft) &
               - sum(sibg%l(myl)%pooldt%poollu_dloss(pool_indx_arm-npoolpft,:))))
         DO s=1,nsoil 
             sibg%l(myl)%pooldt%loss_fire_lay(pool_indx_arm-npoolpft,s) = &
                  myemis * dtisib * sibg%l(myl)%pooldt%poollu_flay(pool_indx_arm-npoolpft,s)
             sibg%l(myl)%pooldt%poollu_dloss(pool_indx_arm-npoolpft,s) = &
                  sibg%l(myl)%pooldt%poollu_dloss(pool_indx_arm-npoolpft,s) &
                  + myemis * sibg%l(myl)%pooldt%poollu_flay(pool_indx_arm-npoolpft,s)
          ENDDO
          flosslu(myl,pool_indx_arm-npoolpft) = myemis
          curemis = curemis - myemis
      ENDIF

      !...save what C was emitted but not removed
      flossb(myl) = curemis
      sibg%l(myl)%poollt%rmmd_fire = flossb(myl)*dtisib
      sibg%l(myl)%poollt%resp_fire = tempemis * dtisib
   enddo

   !...check to make sure all emissions have been taken from somewhere
   curemis = totemis - sum(flosspft) - sum(flosslu) - sum(flossb)

   IF (curemis .gt. fireb_thresh) THEN
      fb_err = .true.
   else
      fb_err = .false.
   endif
   
   !...Print Results
   IF ((fb_err) .OR. (fireb_print)) THEN
      print*,''
      print*,'---FIRE CARBON---'
      IF (fb_err) THEN
         print('(a)'),'!!Fire Carbon Imbalance!!'
         print*,'Fire Emissions Mismatch (mol C/m2): ', curemis
      ENDIF

      print('(a,a,i3,a,i4)'), &
          '      Date: ', trim(month_names(month)), day, ', ', year
      print('(a,i6,2f8.2)'),  '      Point/Lon/Lat: ', indx, lon, lat
      print('(a,i14)'),       '      Current Model Second: ', sec_tot
      print('(a,2i12)'),      '      Current/Next Fire Second: ', &
              fire_seccur, fire_secnext

      print*,''
      print('(a,f18.8)'),     '      Fire C Emissions (umol/m2/s): ', &
           sibg%gprogt%firec*mol_to_umol
      print('(a,f18.8)'),     '      Time-Step C Losses (mol/m2):', totemis

      tempemis = sum(flosspft) + sum(flosslu)
      print('(a,f18.8)'),     '      SiB4 C Removal (mol/m2):    ', tempemis
      print('(a)'),           '         PFT    Loss          %-BioBurned   Fraction'
         do l=1,ido
            myl = tsortref(l)
            curemis = sum(flosslu(myl,:)) + sum(flosspft(myl,:))
            if (tpbiomass(myl) .gt. dzero) then
               pcemis = curemis/tpbiomass(myl)*100.
            else
               pcemis = dzero
            endif
            print('(a,i2,2f14.8,a,f6.2)'), '          ', tpref(myl),  &
               curemis, pcemis, '  ',tparea(myl)/tfarea
         enddo
      print('(a,f12.8)'),     '      Non-Matched C Respired: ', sum(flossb)

      IF (fb_err .AND. (fireb_stop)) STOP
   ENDIF  !print

   deallocate(tpref,tpnum,tparea)
   deallocate(tpagb,tpagbtemp)
   deallocate(flosspft,flosslu,flossb)

endif  !firec > 0

end subroutine fire_interp

