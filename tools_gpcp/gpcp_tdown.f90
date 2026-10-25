!===================================
! Subroutine to get areal information
!    for regridding to a smaller grid.
!    (Downscaling...)
!
! kdhaynes, 06/16
 
subroutine regrid_downinfo( &
     nlonin, nlatin,   &
     nlonout, nlatout, maxng, &
     lonin, latin, lonout, latout, &
     ngmin, refiin, refjin, &
     arlonin, arlatin, &
     arlonout, arlatout, &
     contribin, areagin, areagout)

implicit none

!...parameters
logical, parameter :: print_area=.false.
real, parameter :: radius=6371000
real :: pi

!...input variables
integer, intent(in) :: nlonin, nlatin
integer, intent(in) :: nlonout, nlatout
integer, intent(in) :: maxng
real, dimension(nlonin), intent(in) :: lonin
real, dimension(nlatin), intent(in) :: latin
real, dimension(nlonout), intent(in) :: lonout
real, dimension(nlatout), intent(in) :: latout
integer, dimension(nlonout,nlatout), intent(inout) :: ngmin
integer, dimension(nlonout,nlatout,maxng), intent(inout) :: refiin, refjin
real*8, dimension(nlonin), intent(inout) :: arlonin
real*8, dimension(nlatin), intent(inout) :: arlatin
real*8, dimension(nlonout), intent(inout) :: arlonout
real*8, dimension(nlatout), intent(inout) :: arlatout
real*8, dimension(nlonout,nlatout,maxng), intent(inout) :: contribin
real*8, dimension(nlonin,nlatin), intent(inout) :: areagin
real*8, dimension(nlonout,nlatout), intent(inout) :: areagout

!...local variables
real :: deltalonin, deltalatin
real*8 :: areatotin
real :: loninWt, loninEt
real, dimension(nlonin) :: loninloc, loninE, loninW
real, dimension(nlatin) :: latinN, latinS

real :: deltalonout, deltalatout
real, dimension(nlonout) :: lonoutE, lonoutW
real, dimension(nlatout) :: latoutN, latoutS

real*8 :: latsr, latnr, lonwr, loner
real*8 :: londiff,sinlatdiff

!...misc variables
integer :: i,j,ii,jj
integer :: irefin, jrefin
real*8  :: temparea

!----------------------------------------
pi = acos(-1.0)
ngmin(:,:) = 0
refiin(:,:,:) = 0
refjin(:,:,:) = 0

!Calculate areal variables for original grid
deltalonin=(lonin(3)-lonin(2))/2.
deltalatin=(latin(3)-latin(2))/2.

do i=1,nlonin
   if (lonin(i) .gt. 180.) then
       loninloc(i) = lonin(i) - 360.
   else
       loninloc(i) = lonin(i)
   endif
   loninE(i)=loninloc(i)+deltalonin
   loninW(i)=loninloc(i)-deltalonin
   arlonin(i)=(loninE(i)-loninW(i))/360.
enddo
arlonin = arlonin*2*pi*radius*radius

do j=1,nlatin
   if (j .eq. 1) then
      latinS(j) = -90.
      latinN(j) = (latin(j+1) + latin(j))*0.5
   elseif (j .eq. 2) then
      latinS(j) = (latin(j-1) + latin(j))*0.5
      latinN(j) = latin(j) + deltalatin
   elseif (j .eq. nlatin-1) then
      latinS(j) = latin(j) - deltalatin
      latinN(j) = (latin(j) + latin(j+1))*0.5
   elseif (j .eq. nlatin) then
      latinS(j) = (latin(j-1) + latin(j))*0.5
      latinN(j) = 90.
   else
      latinS(j) = latin(j) - deltalatin
      latinN(j) = latin(j) + deltalatin
   endif

   arlatin(j) = -sin(pi*latinS(j)/180.) + &
                 sin(pi*latinN(j)/180.)
enddo

areatotin=0.
do ii=1,nlonin
   do jj=1,nlatin
      latsr=latinS(jj)*pi/180.
      latnr=latinN(jj)*pi/180.
      lonwr=loninW(ii)*pi/180.
      loner=loninE(ii)*pi/180.
      londiff=loner-lonwr
      sinlatdiff=sin(latnr)-sin(latsr)
      areagin(ii,jj) = radius*radius*londiff*sinlatdiff
      areatotin = areatotin + areagin(ii,jj)
   enddo !jj
enddo !ii

!Calculate areal variables for new grid
deltalonout=(lonout(3)-lonout(2))/2.
deltalatout=(latout(3)-latout(2))/2.

do i=1,nlonout
   lonoutE(i)=lonout(i)+deltalonout
   lonoutW(i)=lonout(i)-deltalonout
   arlonout(i)=(lonoutE(i)-lonoutW(i))/360.
enddo
arlonout = arlonout*2*pi*radius*radius

do j=1,nlatout
   latoutS(j) = latout(j) - deltalatout
   latoutN(j) = latout(j) + deltalatout

   IF (latoutS(j) .lt. -90.) THEN
      latoutS(j) = -90.
   ENDIF

   IF (latoutN(j) .gt. 90.) THEN
      latoutN(j) = 90.
   ENDIF

   arlatout(j) = -sin(pi*latoutS(j)/180.) + &
                  sin(pi*latoutN(j)/180.)
enddo

do ii=1,nlonout
   do jj=1,nlatout
      latsr=latoutS(jj)*pi/180.
      latnr=latoutN(jj)*pi/180.
      lonwr=lonoutW(ii)*pi/180.
      loner=lonoutE(ii)*pi/180.
      londiff=loner-lonwr
      sinlatdiff=sin(latnr)-sin(latsr)

      areagout(ii,jj) = radius*radius*londiff*sinlatdiff
   enddo !jj
enddo !ii

!Area calculations
ngmin(:,:)=0
do ii=1,nlonout
   do jj=1,nlatout

      do i=1,nlonin
         loninWt = loninW(i)
         loninEt = loninE(i)
         if ((loninW(i) .le. 180.) .and. (loninE(i) .ge. 180.)) then
              if (lonout(ii) .lt. 0.) then
                  loninWt = loninW(i) - 360.
                  loninEt = loninE(i) - 360.
              endif
         endif
         do j=1,nlatin
            IF ((lonoutE(ii) .GT. loninWt) .AND. &
                 (lonoutW(ii) .LT. loninEt) .AND. &
                 (latoutN(jj) .GT. latinS(j))  .AND. &
                 (latoutS(jj) .LT. latinN(j))) THEN
                  latsr=MAX(latoutS(jj)*pi/180.,latinS(j)*pi/180.)
                  latnr=MIN(latoutN(jj)*pi/180.,latinN(j)*pi/180.)
                  lonwr=MAX(lonoutW(ii)*pi/180.,loninWt*pi/180.)
                  loner=MIN(lonoutE(ii)*pi/180.,loninEt*pi/180.)
                  londiff=loner-lonwr
                  sinlatdiff=sin(latnr)-sin(latsr)
                  temparea = radius*radius*londiff*sinlatdiff
                  IF (temparea .LT. 0.) THEN
                     print*,'Negative Area: ',temparea
                     print*,' (loninE,lonoutE,loninW,lonoutW): ', &
                         loninEt,lonoutE(ii),loninWt,lonoutW(ii),londiff
                     print*,' (latinS,latoutS,latinN,latoutN): ', &
                         latinS(j),latoutS(jj),latinN(j),latoutN(jj),sinlatdiff
                  ENDIF

                  ngmin(ii,jj) = ngmin(ii,jj) + 1
                  !print*,'Lon In: ',loninWt,loninEt
                  !print*,'Lon Out: ',lonoutW(ii),lonoutE(ii)
                  !print*,'Lat In: ',latinS(j),latinN(j)
                  !print*,'Lat Out: ',latoutS(jj),latoutN(jj)
                  !print*,'Grid Contributions: ',ngmin(ii,jj)

                  IF (ngmin(ii,jj) .GT. maxng) THEN
                      print('(a,3i6)'),'Too many grid cell contributors: ', &
                              ngmin(ii,jj),ii,jj
                      print*,'Longitudes: ',lonout(ii),lonoutW(ii),lonoutE(ii)
                      print*,'Latitudes: ',latout(jj),latoutS(jj),latoutN(jj)
                      stop
                  ENDIF
                  irefin = i
                  jrefin = j

                  refiin(ii,jj,ngmin(ii,jj)) = irefin
                  refjin(ii,jj,ngmin(ii,jj)) = jrefin
                  contribin(ii,jj,ngmin(ii,jj)) = temparea/areagout(ii,jj)
             ENDIF
            ENDDO !jj
        ENDDO !ii

   ENDDO !j
ENDDO !i      

!ratio=areatotin/sum(areagout)
!areagout=areagout*ratio
!ratio=areatotin/sum(contribin)
!contribin=contribin*ratio

!Check areas
if (print_area) then
   print('(a)'), '    Area should be: 0.51E+15 m2'
   print('(a)'), '    Area used for calculation:'
   print('(2(a,E15.8))'), '       Orig   = ', &
               sum(areagin),'  ',sum(arlonin)*sum(arlatin)
   print('(2(a,E15.8))'), '       New    = ', &
               sum(areagout),'  ',sum(arlonout)*sum(arlatout)
endif

!Check to make sure all new grid points 
!  have a contributor
do ii=1,nlonout
   do jj=1,nlatout
      if ((sum(contribin(ii,jj,:)) .le. 0.99) .or. &
          (sum(contribin(ii,jj,:)) .gt. 1.01)) then
          print*,'Bad Original Contribution: ',ii,jj,sum(contribin(ii,jj,:))
          print*,'   Lon/Lat: ',lonout(ii),latout(jj)
          print*,'   Contributions: ',contribin(ii,jj,:)
          STOP
      endif
   enddo
enddo

end subroutine regrid_downinfo



!===================================
! Subroutine to output regrid a variable.
!
! kdhaynes, 06/16
 
subroutine regrid_vardown( flag_mmpmon, &
     nlonin, nlatin, ntime, &
     nlonout, nlatout, maxng, &
     ngmin, refiin, refjin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin, varin, varout)

implicit none

!parameters
real, parameter :: earth_sfc_area = 5.1E14
logical, parameter :: print_sums=.true.
character(len=3), dimension(12), parameter :: &
   monname = ['Jan','Feb','Mar','Apr','May','Jun', &
    'Jul','Aug','Sep','Oct','Nov','Dec']

!input variables
logical, intent(in) :: flag_mmpmon
integer, intent(in) :: nlonin, nlatin, ntime
integer, intent(in) :: nlonout, nlatout, maxng

integer, dimension(nlonout,nlatout), intent(in) :: ngmin
integer, dimension(nlonout,nlatout,maxng), intent(in) :: refiin, refjin

real*8, dimension(nlonin), intent(in) :: arlonin
real*8, dimension(nlatin), intent(in) :: arlatin
real*8, dimension(nlonout), intent(in) :: arlonout
real*8, dimension(nlatout), intent(in) :: arlatout
real*8, dimension(nlonout,nlatout,maxng), intent(in) :: contribin

real, dimension(nlonin,nlatin,ntime), intent(in) :: varin
real, dimension(nlonout,nlatout,ntime), intent(inout) :: varout

!data variables
real, dimension(nlonin,nlatin,ntime) :: varinloc

!processing area variables
real*8 :: ratio
real*8 :: sumvarin, sumvarout1, sumvarout2

!local variables
integer :: i,j,k,l,t
integer :: irefin, jrefin
integer :: monref, dayspermon(12)
real :: myconvert

!------------------
!set local variables
data dayspermon/31,28,31,30,31,30,31,31,30,31,30,31/

!-------------------------
!regrid
monref = 1
varout = 0.0

do t=1,ntime
   do i=1,nlonout
      do j=1,nlatout
         do l=1,ngmin(i,j)
            irefin=refiin(i,j,l)
            jrefin=refjin(i,j,l)

             varout(i,j,t) = varout(i,j,t) + &
                varin(irefin,jrefin,t) * &
                real(contribin(i,j,l))
          enddo !l=1,ngmin
      enddo !j=1,nlatin
   enddo !i=1,nlonin

   !calculate global totals
   sumvarin=0.0
   do i=1,nlonin
      do j=1,nlatin
         sumvarin = sumvarin + &
            varin(i,j,t)*arlonin(i)*arlatin(j)
      enddo  !j=1,nlatin
   enddo !i=1,nlonin

   sumvarout1=0.0
   do i=1,nlonout
      do j=1,nlatout
         sumvarout1 = sumvarout1 + &
              varout(i,j,t)*arlonout(i)*arlatout(j)
      enddo !j=1,nlatout
   enddo !i=1,nlonout

   !rescale
   if (sumvarout1 .ne. 0.) then
      ratio=sumvarin/sumvarout1
   else
      ratio=1.
   endif
   varout(:,:,t)=varout(:,:,t)*real(ratio)

   !recheck
   sumvarout2=0.0
   do i=1,nlonout
      do j=1,nlatout
         sumvarout2 = sumvarout2 + &
                 (varout(i,j,t))*arlonout(i)*arlatout(j)
      enddo  !j=1,nlatout
   enddo !i=1,nlonout

   if (print_sums) then
      print*,''
      print('(a,a,i5)'), &
         'Precipitation for ',monname(monref)
      
      if (flag_mmpmon) then
          myconvert = 1.
      else
          myconvert = real(dayspermon(t))
      endif

      print*,'  Orig Max/Min Precip (mm/mon)  : ', &
            maxval(varin(:,:,t))*myconvert, &
            minval(varin(:,:,t))*myconvert
      print*,'  Regrid Max/Min Precip (mm/mon): ', &
            maxval(varout(:,:,t))*myconvert, &
            minval(varout(:,:,t))*myconvert
      print*,'  Totals (global mean in mm/mon)'
      print('(a,f15.8)'),'     In : ',sumvarin/earth_sfc_area*myconvert
      print('(a,f15.8)'),'     Out: ',sumvarout2/earth_sfc_area*myconvert
   endif

   monref = monref + 1
enddo !t=1,ntime

end subroutine regrid_vardown
