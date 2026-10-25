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
logical, parameter :: print_area=.true.
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
arlonin = arlonin*2*pi*radius*radius/1.e15

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
arlonout = arlonout*2*pi*radius*radius/1.e15

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
   print('(a)'), '    Area should be: 0.50990436E+15 m2'
   print('(a)'), '    Area used for calculation:'
   print('(a,E15.8)'), '       Orig   = ',sum(areagin)
   print('(a,E15.8)'), '       New    = ',sum(areagout)
endif

!Check to make sure all new grid points 
!  have a contributor
do ii=1,nlonout
   do jj=1,nlatout
      if ((sum(contribin(ii,jj,:)) .le. 0.99) .or. &
          (sum(contribin(ii,jj,:)) .gt. 1.01)) then
          print*,'Bad Original Contribution: ',ii,jj,sum(contribin(ii,jj,:))
          STOP
      endif
   enddo
enddo

end subroutine regrid_downinfo


!=========================================
!Subroutine to regrid PFTs (area and type)
!
subroutine regrid_downinfo_pft( &
     nlonin, nlatin, nlev, &
     nlonout, nlatout, maxng, &
     ngmin, refiin, refjin, &
     contribin, pft_refin, pft_areain, &
     lonout, latout, refkin, &
     pft_refout, pft_areaout)

implicit none

!input variables
integer, intent(in) :: nlonin, nlatin, nlev
integer, intent(in) :: nlonout, nlatout, maxng
integer, dimension(nlonout,nlatout), intent(in) :: ngmin
integer, dimension(nlonout,nlatout,maxng), intent(in) :: refiin, refjin
real*8, dimension(nlonout,nlatout,maxng), intent(in) :: contribin
integer, dimension(nlonin,nlatin,nlev), intent(in) :: pft_refin
real, dimension(nlonin,nlatin,nlev),intent(in) :: pft_areain

real, dimension(nlonout), intent(in) :: lonout
real, dimension(nlatout), intent(in) :: latout

integer, dimension(nlonout,nlatout,nlev,maxng), intent(inout) :: refkin
integer, dimension(nlonout,nlatout,nlev), intent(inout) :: pft_refout
real, dimension(nlonout,nlatout,nlev), intent(inout) :: pft_areaout

!parameters
logical, parameter :: print_pft=.true.

!data variables
integer, dimension(nlonout,nlatout) :: numpftout

integer :: pftrefmax, pftrefmin
real    :: pftareamax, pftareamin

integer :: countland, countcoast, countocean
real    :: newsumpft

!misc variables
integer :: i,j,l,lr,ng,npfto
integer :: irefin, jrefin
integer :: krefadd, pftref

!-----------------------------------
pft_refout = 0
pft_areaout = 0.0
numpftout = 0

DO i=1,nlonout
   DO j=1,nlatout
      DO ng=1,ngmin(i,j)
         irefin = refiin(i,j,ng)
         jrefin = refjin(i,j,ng)

         DO l=1,nlev
            IF (pft_areain(irefin,jrefin,l) .gt. 0.01) THEN
               pftref = pft_refin(irefin,jrefin,l)
               krefadd = 1

               !check for existing PFT
               npfto = numpftout(i,j)
               DO lr=1,npfto
                  IF (pftref .eq. pft_refout(i,j,lr)) THEN
                     pft_areaout(i,j,lr) = &
                         pft_areaout(i,j,lr) &
                         + pft_areain(irefin,jrefin,l) &
                         * real(contribin(i,j,ng))
                     refkin(i,j,l,ng) = lr
                     krefadd = 0
                  ENDIF
               ENDDO

               !PFT not included yet, so add or combine
               IF (krefadd .eq. 1) THEN

                  !add new PFT
                  IF (npfto+1 .le. nlev) THEN
                     npfto = npfto + 1
                     pft_refout(i,j,npfto) = pftref
                     pft_areaout(i,j,npfto) = &
                         pft_areaout(i,j,npfto) &
                         + pft_areain(irefin,jrefin,l) &
                         * real(contribin(i,j,ng))
                     refkin(i,j,l,ng) = npfto
                     numpftout(i,j) = npfto
                
                 !replace the smallest current PFT,
                 !  adding the area to the largest PFT
                 ELSE
                    pftareamin=999.
                    pftareamax=-999.
                    pftrefmin=0
                    pftrefmax=0
                    DO lr=1,npfto
                       if (pft_areaout(i,j,lr) .lt. &
                           pftareamin) THEN
                           pftareamin=pft_areaout(i,j,lr)
                           pftrefmin = lr
                       endif
                       if (pft_areaout(i,j,lr) .gt. &
                           pftareamax) then
                           pftareamax = pft_areaout(i,j,lr)
                           pftrefmax = lr
                       endif
                    ENDDO

                    !current PFT bigger than smallest PFT, 
                    !  so replace
                    IF (pft_areain(irefin,jrefin,l) .gt. pftareamax) THEN
                        pft_areaout(i,j,pftrefmax) = &
                           pft_areaout(i,j,pftrefmax) + pftareamin

                        pft_refout(i,j,pftrefmin) = pftref
                        pft_areaout(i,j,pftrefmin) = &
                             pft_areain(irefin,jrefin,l) &
                           * real(contribin(i,j,ng))
                        refkin(i,j,l,ng) = pftrefmin
 
                    !current PFT is the smallest, 
                    !  so add to largest
                    else
                        pft_areaout(i,j,pftrefmax) = &
                            pft_areaout(i,j,pftrefmax)  &
                            + pft_areain(irefin,jrefin,l) &
                            * real(contribin(i,j,ng))
                        refkin(i,j,l,ng) = pftrefmax
                    endif
                 ENDIF !add new PFT
               endif !krefadd
            endif !PFT area > 0.01
         enddo !l=1,nlev
       enddo !ng=1,ngmin
   enddo !j=1,nlatin
enddo !i=1,nlonin


!Check new PFT area
countland=0
countcoast=0
countocean=0
do i=1,nlonout
   do j=1,nlatout
      newsumpft=sum(pft_areaout(i,j,:))
       if (newsumpft .gt. 0.01) then
            if (newsumpft .le. 0.99) then
               countcoast=countcoast+1
            elseif (newsumpft .le. 1.01) then
               countland=countland + 1
            else
               print*,'!Regridded PFT Area Too Big!'
               print*,'Lon/Lat/IRef/JRef',lonout(i),latout(j),i,j
               print*,'Area Total: ',newsumpft
               print*,'PFT Refs : ',pft_refout(i,j,1:numpftout(i,j))
               print*,'PFT Areas: ',pft_areaout(i,j,1:numpftout(i,j))

               irefin = refiin(i,j,1)
               jrefin = refjin(i,j,1)
               print*,'PFT Original Refs:',pft_refin(irefin,jrefin,:)
               print*,'PFT Original Areas: ',pft_areain(irefin,jrefin,:)
               stop
            endif
         elseif (newsumpft .gt. -0.0001) then
            countocean=countocean+1
         else
            print*,'Negative PFT Areas!'
            print*,'Lon/Lat/IRef/JRef',lonout(i),latout(j),i,j
            print*,'Area Total: ',newsumpft
            print*,'PFT Refs : ',pft_refout(i,j,1:numpftout(i,j))
            print*,'PFT Areas: ',pft_areaout(i,j,1:numpftout(i,j))

            irefin = refiin(i,j,1)
            jrefin = refjin(i,j,1)
            print*,'PFT Original Refs:',pft_refin(irefin,jrefin,:)
            print*,'PFT Original Areas: ',pft_areain(irefin,jrefin,:)
            stop
       endif
   enddo
enddo

if (print_pft) then
    print('(a,i12)'), '       Land PFT Regrids:  ',countland
    print('(a,i12)'), '       Coast PFT Regrids: ',countcoast
    print('(a,i12)'), '       Ocean PFT Regrids: ',countocean
endif



end subroutine regrid_downinfo_pft


!===================================
! Subroutine to output regridded data.
!
! kdhaynes, 06/16
subroutine regrid_down( &
     filein, fileout, &
     nlonin, nlatin, nlev, nlu, &
     nlonout, nlatout, maxng, &
     lonout, latout, &
     pft_areain,     &
     pft_refout, pft_areaout, &
     ngmin, refiin, refjin, refkin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin)

use netcdf
use sibinfo_module, only: use_deflate, deflev
implicit none

!input variables
character(len=256), intent(in) :: filein, fileout
integer, intent(in) :: nlonin, nlatin, nlev, nlu
integer, intent(in) :: nlonout, nlatout
integer, intent(in) :: maxng
real, dimension(nlonout), intent(in) :: lonout
real, dimension(nlatout), intent(in) :: latout
real, dimension(nlonin,nlatin,nlev), intent(in) :: pft_areain
integer, dimension(nlonout,nlatout,nlev), intent(in) :: pft_refout
real, dimension(nlonout,nlatout,nlev), intent(in) :: pft_areaout

integer, dimension(nlonout,nlatout), intent(in) :: ngmin
integer, dimension(nlonout,nlatout,maxng), intent(in) :: refiin, refjin
integer, dimension(nlonout,nlatout,nlev,maxng), intent(in) :: refkin
real*8, dimension(nlonin), intent(in) :: arlonin
real*8, dimension(nlatin), intent(in) :: arlatin
real*8, dimension(nlonout), intent(in) :: arlonout
real*8, dimension(nlatout), intent(in) :: arlatout
real*8, dimension(nlonout,nlatout,maxng), intent(in) :: contribin

!netcdf variables
integer :: status, inid, outid
integer :: varid

integer :: xtype
integer :: numatts, numvars, numdims, numvdims
integer, dimension(:), allocatable :: dimlens, dimids
integer, dimension(:), allocatable :: vardims
character(len=20) :: varname, attname
character(len=20), dimension(:), allocatable :: dimnames

!data variables
real, dimension(:), allocatable :: fvalues
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4in, fvalues4out
real*8, dimension(:), allocatable :: dvalues
real*8, dimension(:,:), allocatable :: dvalues2
integer*1, dimension(:), allocatable :: bvalues
integer*1, dimension(:,:,:,:), allocatable :: bvalues4in, bvalues4out
integer, dimension(:), allocatable :: ivalues
integer, dimension(:,:,:), allocatable :: ivalues3
character(len=3), dimension(:), allocatable :: cvalues3
character(len=7), dimension(:), allocatable :: cvalues7
character(len=10), dimension(:), allocatable :: cvalues10

!misc variables
integer :: n
integer :: ntemp3, ntemp4
integer :: att, dim, var

!format parameters
logical, parameter :: printoutfile = .false.
character(len=30), parameter :: &
   err_message='regrid_down_sib_error', &
   a1   = '(a)',     &
   a1i  = '(a,i6)',  &
   a2   = '(2a)',    &
   a3i  = '(3a,i6)'

!-------------------
!Open the files
status = nf90_open(trim(filein),nf90_nowrite,inid)
IF (status .ne. nf90_noerr) THEN
   print*,'Error Opening Input File: '
   print*,'  ',trim(filein)
   print*,'Stopping.'
   STOP
ENDIF

if (use_deflate) then
   status = nf90_create(trim(fileout),nf90_netcdf4,outid)
   !status = nf90_create(trim(fileout),nf90_hdf5,outid)
else
   status = nf90_create(trim(fileout),nf90_64bit_offset,outid)
endif

IF (status .ne. nf90_noerr) THEN
   print*,'Error Opening Output File: '
   print*,'  ',trim(filein)
   print*,'Stopping.'
   STOP
ELSE
   if (printoutfile) then
       print('(a)'),'    Writing File: '
       print('(2a)'),'     ',trim(fileout)
   endif
ENDIF

!Get dimensions and attributes
status = nf90_inquire(inid, nDimensions=numdims, nVariables=numvars, &
           nAttributes=numatts)
if (status .ne. nf90_noerr) then
    print*,'Error Inquiring Dimensions'
    print*,'Stopping.'
    STOP
endif

!Copy global attributes over to output file
do att=1,numatts
   status = nf90_inq_attname(inid,nf90_global,att,attname)
   if (status .ne. nf90_noerr) then
       print*,'Error Inquiring Attributes'
       print*,'Stopping.'
       STOP
   endif

   status = nf90_copy_att(inid,nf90_global,trim(attname),outid,nf90_global)
   if (status .ne. nf90_noerr) then
       print*,'Error Copying Global Attributes'
       print*,'  ',trim(attname)
       print*,'Stopping.'
       STOP
   endif
enddo

!Define output file dimensions
allocate(dimlens(numdims))
allocate(dimnames(numdims))
allocate(dimids(numdims))

do dim=1,numdims
   status = nf90_inquire_dimension(inid,dim,name=dimnames(dim),len=dimlens(dim))

   if ((dimnames(dim) .eq. 'lon') .or. &
       (dimnames(dim) .eq. 'nlon') .or. &
       (dimnames(dim) .eq. 'longitude')) then
       dimlens(dim)=nlonout
       dimnames(dim)='lon'
   endif

   if ((dimnames(dim) .eq. 'lat') .or. &
       (dimnames(dim) .eq. 'nlat') .or. &
       (dimnames(dim) .eq. 'longitude')) then
        dimlens(dim)=nlatout
        dimnames(dim)='lat'
   endif

   status = nf90_def_dim(outid,trim(dimnames(dim)),dimlens(dim),dimids(dim))
   if (status .ne. nf90_noerr) then
      print*,'Error Defining Dimension: ',trim(dimnames(dim))
      print*,'Stopping.'
      STOP
   endif
enddo

!Process the variables
allocate(vardims(numdims))
do var=1,numvars
   status = nf90_inquire_variable(inid,var,name=varname, &
            xtype=xtype, ndims=numvdims, dimids=vardims, &
            natts=numatts)

   !define the variables
   status = nf90_redef(outid)
   status = nf90_def_var(outid,trim(varname),xtype, &
            vardims(1:numvdims),varid)
   if (use_deflate) then
      status = nf90_def_var_deflate(outid,varid,1,1,2)
   endif
   if (status .ne. nf90_noerr) then
       print*,'Error Defining Variable: ',trim(varname)
       print('(a,2i4)'),'   Type/Ndims: ',xtype,numvdims
       print('(a)'),'Stopping.'
       STOP
   ENDIF

   !copy attributes
   do att=1,numatts
      status = nf90_inq_attname(inid,var,att,name=attname)
      status = nf90_copy_att(inid,var,trim(attname),outid,varid)
      if (status .ne. nf90_noerr) then
          print*,'Error Copying Variable Attribute: ', &
                 trim(varname),' ',trim(attname)
          print*,'Stopping.'
          STOP
      endif
    enddo

    !end define mode
    status = nf90_enddef(outid)

    !regrid the variable and write to output
    if ((xtype .eq. 6) .and. (numvdims .eq. 1)) then
       allocate(dvalues(dimlens(vardims(1))))
       status=nf90_get_var(inid,var,dvalues)
       status=nf90_put_var(outid,var,dvalues)
       deallocate(dvalues)

    elseif ((xtype .eq. 6) .and. (numvdims .eq. 2)) then
       allocate(dvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
       status = nf90_get_var(inid,var,dvalues2)
       status = nf90_put_var(outid,var,dvalues2)
       deallocate(dvalues2)

    elseif ((xtype .eq. 5) .and. (numvdims .eq. 1)) then
       if (dimnames(vardims(1)) .eq. 'lon') then
          if ((trim(varname) .eq. 'lon') .or. &
              (trim(varname) .eq. 'longitude')) then
              status=nf90_put_var(outid,var,lonout)
          else
              print(a1),'Unable To Handle 1D-Float Lon Output'
              stop err_message
          endif
       elseif (dimnames(vardims(1)) .eq. 'lat') then
          if ((trim(varname) .eq. 'lat') .or. &
              (trim(varname) .eq. 'latitude')) then
              status=nf90_put_var(outid,var,latout)
          else
              print(a1),'Unable To Handle 1D-Float Lat Output'
              stop err_message
          endif
       else
          allocate(fvalues(dimlens(vardims(1))))
          status=nf90_get_var(inid,var,fvalues)
          status=nf90_put_var(outid,var,fvalues)
          deallocate(fvalues)
       endif

    elseif ((xtype .eq. 5) .and. (numvdims .eq. 3)) then 
       if ((varname .eq. 'lu_area') .or. &
           (varname .eq. 'pft_area') .or. &
           (varname .eq. 'area')) then
             allocate(fvalues3(nlonout,nlatout,nlev))
             fvalues3=pft_areaout
             call regrid_sortf3(nlonout,nlatout,nlev, &
                  pft_areaout, fvalues3)
             status = nf90_put_var(outid,var,fvalues3)
             deallocate(fvalues3)
       elseif ((dimnames(vardims(1)) .eq. 'lon') .and. &
               (dimnames(vardims(2)) .eq. 'lat')) then
             ntemp4=dimlens(vardims(3))
             allocate(fvalues4in(nlonin,nlatin,1,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,1,ntemp4))

             status=nf90_get_var(inid,var,fvalues4in(:,:,1,:), &
                   start=(/1,1,1/),count=(/nlonin,nlatin,ntemp4/))
             IF (status .ne. nf90_noerr) THEN
                print*,'Error Getting Variable: ',trim(varname)
                print*,'Stopping.'
                STOP
             ENDIF

             call regrid_vardown(varname, &
                  nlonin,nlatin,1,ntemp4, &
                  nlonout,nlatout,nlu,maxng, &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, fvalues4in, fvalues4out)       

             status = nf90_put_var(outid,var,fvalues4out, &
                  start=(/1,1,1/),count=(/nlonout,nlatout,ntemp4/))
             deallocate(fvalues4in,fvalues4out)
       else
           print(a1),'Expecting 3-D Float Array To Have Lon/Lat'
           stop err_message
       endif

    elseif ((xtype .eq. 5) .and. (numvdims .eq. 4)) then
         if ((dimnames(vardims(1)) .eq. 'lon') .and. &
             (dimnames(vardims(2)) .eq. 'lat') .and. &
             (dimlens(vardims(3)) .eq. nlev)) then
             ntemp4=dimlens(vardims(4))
             allocate(fvalues4in(nlonin,nlatin,nlev,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,nlev,ntemp4))

             status=nf90_get_var(inid,var,fvalues4in)

             call regrid_vardown(varname, &
                  nlonin,nlatin,nlev,ntemp4, &
                  nlonout,nlatout,nlu,maxng, &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, fvalues4in, fvalues4out)

             call regrid_sortf4(nlonout,nlatout,nlev,ntemp4, &
                  pft_areaout, fvalues4out)

             status=nf90_put_var(outid,var,fvalues4out)
             deallocate(fvalues4in,fvalues4out)
         else
             print(a1),'Expecting 4-D Float Array To Have Lon/Lat/Lev'
             stop err_message
         endif

    elseif ((xtype .eq. 4) .and. (numvdims .eq. 1)) then
          allocate(ivalues(dimlens(vardims(1))))
          status=nf90_get_var(inid,var,ivalues)
          status=nf90_put_var(outid,var,ivalues)
          deallocate(ivalues)
    elseif ((xtype .eq. 4) .and. (numvdims .eq. 3)) then
         if ((varname .eq. 'lu_pref') .or. &
             (varname .eq. 'pft_ref')) then
             allocate(ivalues3(nlonout,nlatout,nlev))
             ivalues3=pft_refout
             call regrid_sorti3(nlonout, nlatout, nlev, &
                   pft_areaout, ivalues3)
             status = nf90_put_var(outid,var,ivalues3)
             deallocate(ivalues3)
         else
             print(a1),'Unable To Process 3D Integer'
             stop err_message
         endif     
    elseif ((xtype .eq. 2) .and. (numvdims .eq. 2)) then
         if ((dimnames(vardims(2)) .ne. 'lon') .and. &
             (dimnames(vardims(2)) .ne. 'lat')) then
             if (dimlens(vardims(1)) .eq. 10) then
                 allocate(cvalues10(dimlens(vardims(2))))
                 status=nf90_get_var(inid,var,cvalues10)
                 status=nf90_put_var(outid,var,cvalues10)
                 deallocate(cvalues10)
             elseif (dimlens(vardims(1)) .eq. 7) then
                 allocate(cvalues7(dimlens(vardims(2))))
                 status=nf90_get_var(inid,var,cvalues7)
                 status=nf90_put_var(outid,var,cvalues7)
                 deallocate(cvalues7)
             elseif (dimlens(vardims(1)) .eq. 3) then
                 allocate(cvalues3(dimlens(vardims(2))))
                 status=nf90_get_var(inid,var,cvalues3)
                 status=nf90_put_var(outid,var,cvalues3)
                 deallocate(cvalues3)
             else
                 print*,'Unexpected Character Length Array.'
                 print*,'  Variable: ',trim(varname)
                 print*,'  Number of Dimensions: ',numvdims
                 print*,'  Character Length: ',dimlens(vardims(1))
                 stop err_message
             endif

        else
             print*,'Unable To Handle Character Array Dimension: ', &
                     trim(dimnames(vardims(2)))
             stop err_message
        endif

   elseif ((xtype .eq. 1) .and. (numvdims .eq. 1)) then
           allocate(bvalues(vardims(1)))
           status=nf90_get_var(inid,var,bvalues)
           status=nf90_put_var(inid,var,bvalues)
           deallocate(bvalues)

   elseif ((xtype .eq. 1) .and. (numvdims .eq. 2)) then
        if ((dimnames(vardims(1)) .eq. 'lon') .and. &
            (dimnames(vardims(2)) .eq. 'lat')) then
            ntemp3=1
            ntemp4=1
            allocate(bvalues4in(nlonin,nlatin,ntemp3,ntemp4))
            allocate(bvalues4out(nlonout,nlatout,ntemp3,ntemp4))

            status=nf90_get_var(inid,var,bvalues4in(:,:,1,1), &
                   start=(/1,1/),count=(/nlonin,nlatin/))

             allocate(fvalues4in(nlonin,nlatin,ntemp3,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,ntemp3,ntemp4))
             fvalues4in(:,:,:,:) = real(bvalues4in(:,:,:,:))
             call regrid_vardown(varname, &
                  nlonin,nlatin,ntemp3,ntemp4, &
                  nlonout,nlatout,nlu,maxng,   &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, fvalues4in, fvalues4out)

             bvalues4out = nint(fvalues4out,kind=1)
             status=nf90_put_var(outid,var,bvalues4out, &
                     start=(/1,1/),count=(/nlonout,nlatout/))
             deallocate(bvalues4in,bvalues4out)
             deallocate(fvalues4in,fvalues4out)
        else
             print*,'Unable To Handle Byte Array Dimension: ', &
                    trim(dimnames(vardims(2)))
             stop err_message
        endif
    else
       print(a1),'Regrid_SiB4: Unexpected Output Type and/or Dimension.'
       print(a3i),'  Var name and type: ',trim(varname),' ',xtype
       print(a1i),'  Number of dimensions: ',numvdims
       print(a1), '  Dimension Names:' 
       do n=1,numvdims
           print(a2),'      ',trim(dimnames(vardims(n)))
       enddo
    endif
enddo

!Close the files
status = nf90_close(inid)
status = nf90_close(outid)

end subroutine regrid_down


!===================================
! Subroutine to output regrid a variable.
!
! kdhaynes, 06/16
 
subroutine regrid_vardown( varname, &
     nlonin, nlatin, nlev, ntime, &
     nlonout, nlatout, nlu, maxng, &
     pft_areain, pft_areaout, &
     ngmin, refiin, refjin, refkin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin, varin, varout)

implicit none

!parameters
logical, parameter :: print_sums=.false.

!input variables
character(len=*), intent(in) :: varname
integer, intent(in) :: nlonin, nlatin, nlev, ntime
integer, intent(in) :: nlonout, nlatout, nlu, maxng

real, dimension(nlonin,nlatin,nlu), intent(in) :: pft_areain
real, dimension(nlonout,nlatout,nlu), intent(in) :: pft_areaout

integer, dimension(nlonout,nlatout), intent(in) :: ngmin
integer, dimension(nlonout,nlatout,maxng), intent(in) :: refiin, refjin
integer, dimension(nlonout,nlatout,nlev,maxng), intent(in) :: refkin

real*8, dimension(nlonin), intent(in) :: arlonin
real*8, dimension(nlatin), intent(in) :: arlatin
real*8, dimension(nlonout), intent(in) :: arlonout
real*8, dimension(nlatout), intent(in) :: arlatout
real*8, dimension(nlonout,nlatout,maxng), intent(in) :: contribin

real, dimension(nlonin,nlatin,nlev,ntime), intent(in) :: varin
real, dimension(nlonout,nlatout,nlev,ntime), intent(inout) :: varout

!data variables
real, dimension(nlonin,nlatin,nlev,ntime) :: varinloc

!processing area variables
real*8 :: ratio
real*8, dimension(:), allocatable :: sumvarin, sumvarout1, sumvarout2

!local variables
integer :: i,j,k,l,t
integer :: irefin, jrefin, krefin
real    :: parea

!-------------------------
!regrid
varout = 0.0
varinloc=varin

do i=1,nlonout
   do j=1,nlatout
      do l=1,ngmin(i,j)
         irefin=refiin(i,j,l)
         jrefin=refjin(i,j,l)
          if (nlu .gt. 1) then
              k=1
              parea=pft_areain(irefin,jrefin,k)
              do while (parea .gt. 0.001)
                 krefin=refkin(i,j,k,l)

                 if ((irefin .lt. 1) .or. (jrefin .lt. 1) .or. &
                     (krefin .lt. 1)) then
                     !print*,'Point Lon/Lat: ',lon(i),lat(j)
                     print*,'Bad Origincal References!', &
                            irefin,jrefin,krefin
                     print*,'New References: ',i,j,l
                     print*,'Stopping.'
                     STOP
                 else
                     varout(i,j,k,:) = varout(i,j,k,:) + &
                          varinloc(irefin,jrefin,krefin,:) * &
                          real(contribin(i,j,l))
                 endif

                 k=k+1
                 if (k .gt. nlev) then
                    parea=-1
                 else
                    parea=pft_areain(irefin,jrefin,k)
                 endif
              enddo !while pft_area > 0.01

           else  !nlu == 1
              varout(i,j,:,:) = varout(i,j,:,:) + &
                   varinloc(irefin,jrefin,:,:) * &
                   real(contribin(i,j,l))
           endif !nlev
       enddo !l=1,ngmin
   enddo !j=1,nlatin
enddo !i=1,nlonin

!calculate global totals
allocate(sumvarin(ntime))
sumvarin(:)=0.0
do i=1,nlonin
   do j=1,nlatin
      do k=1,nlev
         if (nlu .gt. 1) then
             if (pft_areain(i,j,k) .gt. 0.01) then
                 do t=1,ntime
                    sumvarin(t) = sumvarin(t) + &
                        varin(i,j,k,t)*arlonin(i)* &
                        arlatin(j)*pft_areain(i,j,k)
                 enddo
              endif
           else
              sumvarin(:) = sumvarin(:) + &
                   varinloc(i,j,k,:)*arlonin(i)*arlatin(j)
           endif
           
       enddo !k=1,nlev
   enddo  !j=1,nlatin
enddo !i=1,nlonin

allocate(sumvarout1(ntime))
sumvarout1(:)=0.0
do i=1,nlonout
   do j=1,nlatout
      do k=1,nlev
         if (nlu .gt. 1) then
             if (pft_areaout(i,j,k) .gt. 0.01) then
                 do t=1,ntime
                    sumvarout1(t) = sumvarout1(t) + &
                        (varout(i,j,k,t))*arlonout(i)* &
                        arlatout(j)*pft_areaout(i,j,k)
                 enddo
              endif
           else
               sumvarout1(:) = sumvarout1(:) + &
                  varout(i,j,k,:)*arlonout(i)*arlatout(j)
           endif
       enddo !k=1,nlev
   enddo !j=1,nlatout
enddo !i=1,nlonout

!rescale
if (sum(sumvarout1) .ne. 0.) then
    ratio=sum(sumvarin)/sum(sumvarout1)
else
    ratio=1.
endif
varout=varout*real(ratio)

!recheck
allocate(sumvarout2(ntime))
sumvarout2(:)=0.0
do i=1,nlonout
   do j=1,nlatout
      do k=1,nlev
         if (nlu .gt. 1) then
             if (pft_areaout(i,j,k) .gt. 0.01) then
                 do t=1,ntime
                     sumvarout2(t) = sumvarout2(t) + &
                        varout(i,j,k,t)*arlonout(i)* &
                        arlatout(j)*pft_areaout(i,j,k)
                 enddo
              endif
         else
             sumvarout2(:) = sumvarout2(:) + &
                 (varout(i,j,k,:))*arlonout(i)*arlatout(j)
         endif !nlu
      enddo !k=1,nlev
   enddo  !j=1,nlatout
enddo !i=1,nlonout

if (print_sums) then
   print*,''
   print*,'   ',trim(varname)
   print*,'     Init Max/Min: ',maxval(varin),minval(varin)
   print*,'     End  Max/Min: ',maxval(varout),minval(varout)
   print*,'     Totals'
   print*,'          In: ',sum(sumvarin)
   print*,'         Out: ',sum(sumvarout1)
   print*,'       Final: ',sum(sumvarout2)
endif

!deallocate
deallocate(sumvarin)
deallocate(sumvarout1,sumvarout2)

end subroutine regrid_vardown
