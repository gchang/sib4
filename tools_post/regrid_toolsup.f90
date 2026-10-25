!===================================
! Subroutine to get areal information
!    for regridding to a coarser grid.
!    (Upscaling...)
!
! kdhaynes, 10/17
 
subroutine regrid_upinfo( &
     nlonin, nlatin, &
     nlonout, nlatout, maxng, &
     lonin, latin, lonout, latout, &
     ngmin, refiin, refjin, &
     arlonin, arlatin, arlonout, arlatout, &
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

integer, dimension(nlonin,nlatin), intent(inout) :: ngmin
integer, dimension(nlonin,nlatin,maxng), intent(inout) :: refiin, refjin
real*8, dimension(nlonin), intent(inout) :: arlonin
real*8, dimension(nlatin), intent(inout) :: arlatin
real*8, dimension(nlonout), intent(inout) :: arlonout
real*8, dimension(nlatout), intent(inout) :: arlatout
real*8, dimension(nlonin,nlatin,maxng), intent(inout) :: contribin
real*8, dimension(nlonin,nlatin), intent(inout) :: areagin
real*8, dimension(nlonout,nlatout), intent(inout) :: areagout

!...local variables
real :: deltalonin, deltalatin
real*8 :: areatotin
real, dimension(nlonin) :: loninE, loninW
real, dimension(nlatin) :: latinN, latinS

real :: deltalonout, deltalatout
real, dimension(nlonout+1) :: lonoutEe, lonoutWe
real, dimension(nlatout) :: latoutN, latoutS

real*8 :: latsr, latnr, lonwr, loner
real*8 :: londiff,sinlatdiff

!...misc variables
integer :: i,j,ii,jj
integer :: irefout, jrefout
real*8  :: ratio, temparea

!----------------------------------------
pi = acos(-1.0)
ngmin(:,:) = 0
refiin(:,:,:) = 0
refjin(:,:,:) = 0

!Calculate areal variables for original grid
deltalonin=(lonin(3)-lonin(2))/2.
deltalatin=(latin(3)-latin(2))/2.

do i=1,nlonin
   loninE(i)=lonin(i)+deltalonin
   loninW(i)=lonin(i)-deltalonin
   arlonin(i)=(loninE(i)-loninW(i))/360.
enddo
arlonin = arlonin*2*pi*radius*radius/1.e15

do j=1,nlatin
   latinS(j) = latin(j) - deltalatin
   latinN(j) = latin(j) + deltalatin

   IF (latinS(j) .lt. -90.) THEN
      latinS(j) = -90.
   ENDIF

   IF (latinN(j) .gt. 90.) THEN
      latinN(j) = 90.
   ENDIF

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
   lonoutEe(i)=lonout(i)+deltalonout
   lonoutWe(i)=lonout(i)-deltalonout
   arlonout(i)=(lonoutEe(i)-lonoutWe(i))/360.
enddo
lonoutEe(nlonout+1)=lonout(nlonout)+3*deltalonout
lonoutWe(nlonout+1)=lonout(nlonout)+2*deltalonout
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

do ii=1,nlonout+1
   do jj=1,nlatout
      latsr=latoutS(jj)*pi/180.
      latnr=latoutN(jj)*pi/180.
      lonwr=lonoutWe(ii)*pi/180.
      loner=lonoutEe(ii)*pi/180.
      londiff=loner-lonwr
      sinlatdiff=sin(latnr)-sin(latsr)

      IF (ii .LT. nlonout+1) THEN
         areagout(ii,jj) = radius*radius*londiff*sinlatdiff
      ENDIF
   enddo !jj
enddo !ii

!Area calculations
ngmin(:,:)=0
do i=1,nlonin
   do j=1,nlatin
       
      do ii=1,nlonout+1
         do jj=1,nlatout
             IF ((loninE(i) .GT. lonoutWe(ii)) .AND. &
                 (loninW(i) .LT. lonoutEe(ii)) .AND. &
                 (latinN(j) .GT. latoutS(jj))  .AND. &
                 (latinS(j) .LT. latoutN(jj))) THEN
                  latsr=MAX(latoutS(jj)*pi/180.,latinS(j)*pi/180.)
                  latnr=MIN(latoutN(jj)*pi/180.,latinN(j)*pi/180.)
                  lonwr=MAX(lonoutWe(ii)*pi/180.,loninW(i)*pi/180.)
                  loner=MIN(lonoutEe(ii)*pi/180.,loninE(i)*pi/180.)
                  londiff=loner-lonwr
                  sinlatdiff=sin(latnr)-sin(latsr)
                  temparea = radius*radius*londiff*sinlatdiff
                  IF (temparea .LT. 0.) THEN
                     print*,'Negative Area: ',temparea
                     print*,' (loninE,lonoutE,loninW,lonoutW): ', &
                         loninE(i),lonoutEe(ii),loninW(i),lonoutWe(ii),londiff
                     print*,' (latinS,latoutS,latinN,latoutN): ', &
                         latinS(j),latoutS(jj),latinN(j),latoutN(jj),sinlatdiff
                  ENDIF

                  ngmin(i,j) = ngmin(i,j) + 1
                  IF (ngmin(i,j) .GT. maxng) THEN
                      print*,'Too many original grid cell contributors: ', &
                              ngmin(i,j),i,j,lonin(i),latin(i)
                      stop
                  ENDIF
                  IF (ii .EQ. nlonout+1) THEN
                      irefout = 1
                  ELSE
                      irefout = ii
                  ENDIF
                  jrefout = jj

                  refiin(i,j,ngmin(i,j)) = irefout
                  refjin(i,j,ngmin(i,j)) = jrefout
                  contribin(i,j,ngmin(i,j)) = temparea

             ENDIF
            ENDDO !jj
        ENDDO !ii

   ENDDO !j
ENDDO !i      

!Rescale areas
ratio=areatotin/sum(areagout)
areagout=areagout*ratio
ratio=areatotin/sum(contribin)
contribin=contribin*ratio

!Check areas
if (print_area) then
   print('(a)'), '    Area should be: 0.50990436E+15 m2'
   print('(a)'), '    Area used for calculation:'
   print('(a,E15.8)'), '       Orig   = ',sum(areagin)
   print('(a,E15.8)'), '       New    = ',sum(areagout)
endif


end subroutine regrid_upinfo


!=========================================
!Subroutine to regrid PFTs (area and type)
!
subroutine regrid_upinfo_pft( &
     nlonin, nlatin, nlev, &
     nlonout, nlatout, maxng, &
     ngmin, refiin, refjin, &
     contribin, pft_refin, pft_areain, &
     lonout, latout, areagout, refkin, &
     pft_refout, pft_areaout)

implicit none

!input variables
integer, intent(in) :: nlonin, nlatin, nlev
integer, intent(in) :: nlonout, nlatout, maxng
integer, dimension(nlonin,nlatin), intent(in) :: ngmin
integer, dimension(nlonin,nlatin,maxng), intent(in) :: refiin, refjin
real*8, dimension(nlonin,nlatin,maxng), intent(in) :: contribin
integer, dimension(nlonin,nlatin,nlev), intent(in) :: pft_refin
real, dimension(nlonin,nlatin,nlev), intent(in) :: pft_areain

real, dimension(nlonout), intent(in) :: lonout
real, dimension(nlatout), intent(in) :: latout
real*8, dimension(nlonout,nlatout), intent(in) :: areagout
integer, dimension(nlonin,nlatin,nlev,maxng), intent(inout) :: refkin
integer, dimension(nlonout,nlatout,nlev), intent(inout) :: pft_refout
real, dimension(nlonout,nlatout,nlev), intent(inout) :: pft_areaout

!parameters
logical, parameter :: print_pft=.false.

!data variables
integer, dimension(nlonout,nlatout) :: numpftout

integer :: pftrefmax, pftrefmin
real    :: pftareamax, pftareamin

integer :: countland, countcoast, countocean
real :: newsumpft

!misc variables
integer :: i,j,l,lr,ng,npfto
integer :: irefout,jrefout
integer :: krefadd, pftref

!-----------------------------------
pft_refout = 0
pft_areaout = 0.0
numpftout = 0

do i=1, nlonin
   do j=1,nlatin
      do ng=1,ngmin(i,j)
         irefout = refiin(i,j,ng)
         jrefout = refjin(i,j,ng)

         do l=1,nlev
             if (pft_areain(i,j,l) .gt. 0.01) then
                 pftref = pft_refin(i,j,l)
                 krefadd = 1

                 !check for existing PFT
                 npfto = numpftout(irefout,jrefout)
                 do lr=1,npfto
                    if (pftref .eq. pft_refout(irefout,jrefout,lr)) then
                       pft_areaout(irefout,jrefout,lr) = &
                             pft_areaout(irefout,jrefout,lr) &
                             + pft_areain(i,j,l) &
                             * real(contribin(i,j,ng) &
                                /  areagout(irefout,jrefout))
                       refkin(i,j,l,ng) = lr
                       krefadd = 0
                    endif
                 enddo

                 !PFT not included yet, so add or combine
                 if (krefadd .eq. 1) then

                     !add new PFT
                     if (npfto+1 .le. nlev) then
                         npfto = npfto + 1
                         pft_refout(irefout,jrefout,npfto) = pftref
                         pft_areaout(irefout,jrefout,npfto) = &
                              pft_areaout(irefout,jrefout,npfto) &
                              + pft_areain(i,j,l) &
                              * real(contribin(i,j,ng) &
                                 / areagout(irefout,jrefout))
                         refkin(i,j,l,ng) = npfto
                         numpftout(irefout,jrefout) = npfto

                     !replace the smallest current PFT,
                     !  adding the area to the largest PFT
                     else
                         pftareamin=999.
                         pftareamax=-999.
                         pftrefmin=0
                         pftrefmax=0
                         do lr=1,npfto
                             if (pft_areaout(irefout,jrefout,lr) .lt. &
                                 pftareamin) then
                                 pftareamin=pft_areaout(irefout,jrefout,lr)
                                 pftrefmin = lr
                             endif
                             if (pft_areaout(irefout,jrefout,lr) .gt. &
                                 pftareamax) then
                                 pftareamax=pft_areaout(irefout,jrefout,lr)
                                 pftrefmax = lr
                             endif
                         enddo

                        !current PFT bigger than smallest PFT,
                        !  so replace
                        if (pft_areain(i,j,l) .gt. pftareamax) then
                           pft_areaout(irefout,jrefout,pftrefmax) = &
                                 pft_areaout(irefout,jrefout,pftrefmax) + &
                                 pftareamin

                           pft_refout(irefout,jrefout,pftrefmin) = pftref
                           pft_areaout(irefout,jrefout,pftrefmin) = &
                                 pft_areain(i,j,l) * real(  &
                                 contribin(i,j,ng) / areagout(irefout,jrefout))
                           refkin(i,j,l,ng) = pftrefmin

                        !current PFT is the smallest,
                        !   so add to largest
                        else
                           pft_areaout(irefout,jrefout,pftrefmax) = &
                                 pft_areaout(irefout,jrefout,pftrefmax) + &
                                 pft_areain(i,j,l) * real( &
                                 contribin(i,j,ng) / areagout(irefout,jrefout))
                           refkin(i,j,l,ng) = pftrefmax
                        endif
                     endif !add new PFT
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
               !print*,'!Regridded PFT Area Too Big!'
               !print*,'Lon/Lat/IRef/JRef',lonout(i),latout(j),i,j
               !print*,'Area Total: ',newsumpft
               !print*,'PFT Refs : ',pft_refout(i,j,1:numpftout(i,j))
               !print*,'PFT Areas: ',pft_areaout(i,j,1:numpftout(i,j))
               !stop
            endif
         elseif (newsumpft .gt. -0.0001) then
            countocean=countocean+1
         else
            !print*,'Negative PFT Areas!'
            !print*,'Lon/Lat/IRef/JRef',lonout(i),latout(j),i,j
            !print*,'Area Total: ',newsumpft
            !print*,'PFT Refs : ',pft_refout(i,j,1:numpftout(i,j))
            !print*,'PFT Areas: ',pft_areaout(i,j,1:numpftout(i,j))
            !stop
       endif
   enddo
enddo

if (print_pft) then
    print('(a,i12)'), '      Land PFT Regrids:  ',countland
    print('(a,i12)'), '      Coast PFT Regrids: ',countcoast
    print('(a,i12)'), '      Ocean PFT Regrids: ',countocean
endif


end subroutine regrid_upinfo_pft


!===================================
! Subroutine to output regridded data.
!
! kdhaynes, 06/16
 
subroutine regrid_up( &
     filein, fileout, &
     nlonin, nlatin, nlev, nlu, &
     nlonout, nlatout, maxng,   &
     lonout, latout, &
     pft_areain,     &
     pft_refout, pft_areaout, &
     ngmin, refiin, refjin, refkin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin, areagout)

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
real, dimension(nlonin,nlatin,nlu), intent(in) :: pft_areain
integer, dimension(nlonout,nlatout,nlu), intent(in) :: pft_refout
real, dimension(nlonout,nlatout,nlu), intent(in) :: pft_areaout

integer, dimension(nlonin,nlatin), intent(in) :: ngmin
integer, dimension(nlonin,nlatin,maxng), intent(in) :: refiin, refjin
integer, dimension(nlonin,nlatin,nlev,maxng), intent(in) :: refkin
real*8, dimension(nlonin), intent(in) :: arlonin
real*8, dimension(nlatin), intent(in) :: arlatin
real*8, dimension(nlonout), intent(in) :: arlonout
real*8, dimension(nlatout), intent(in) :: arlatout
real*8, dimension(nlonin,nlatin,maxng), intent(in) :: contribin
real*8, dimension(nlonout,nlatout), intent(in) :: areagout

!netcdf variables
integer :: status
integer :: inid, outid, varid

integer :: xtype
integer :: numdims, numvars, numatts, numvdims
integer, dimension(:), allocatable :: dimlens, dimids
integer, dimension(:), allocatable :: vardims
character(len=20) :: varname, attname
character(len=20), dimension(:), allocatable :: dimnames

!data variables
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4in, fvalues4out
real*8, dimension(:), allocatable :: dvalues
real*8, dimension(:,:), allocatable :: dvalues2
real*8, dimension(:,:,:), allocatable :: dvalues3
real*8, dimension(:,:,:,:), allocatable :: dvalues4in, dvalues4out
integer*1, dimension(:), allocatable :: bvalues
integer*1, dimension(:,:,:,:), allocatable :: bvalues4in, bvalues4out
integer*2, dimension(:,:), allocatable :: iivalues2
integer*2, dimension(:,:,:,:), allocatable :: iivalues4in, iivalues4out
integer, dimension(:), allocatable :: ivalues
integer, dimension(:,:,:), allocatable :: ivalues3
character(len=3), dimension(:), allocatable :: cvalues3
character(len=7), dimension(:), allocatable :: cvalues7
character(len=10), dimension(:), allocatable :: cvalues10

!misc variables
integer :: n
integer :: ntemp3, ntemp4
integer :: att, dim, var
logical :: sort_pfts

!format parameters
logical, parameter :: printoutfile = .false.
character(len=30), parameter :: &
   err_message='regrid_up_sib_error', &
   a1   = '(a)',     &
   a1i  = '(a,i6)',  &
   a2   = '(2a)',    &
   a3i  = '(3a,i6)'

!-------------------------------------
!Set local variables
sort_pfts = .false.
IF (nlu .eq. nlev) sort_pfts=.true.

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
   print*,'  ',trim(fileout)
   print*,'Stopping.'
   STOP
ELSE
   if (printoutfile) then
       print*,'  Writing File: '
       print*,'   ',trim(fileout)
   endif
ENDIF

!Get dimensions and attributes
status = nf90_inquire(inid, nDimensions=numdims, nVariables=numvars, &
          nAttributes=numatts)

!Copy global attributes over to ouptut file
do att=1,numatts
   status = nf90_inq_attname(inid,nf90_global,att,attname)
   status = nf90_copy_att(inid,nf90_global,trim(attname),outid,nf90_global)
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
       (dimnames(dim) .eq. 'latitude')) then
        dimlens(dim)=nlatout
        dimnames(dim)='lat'
   endif

   status = nf90_def_dim(outid,trim(dimnames(dim)),dimlens(dim),dimids(dim))
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Defining Dimensions'
       print*,'  Dim Name: ',trim(dimnames(dim))
       print*,'Stopping.'
       STOP
   ENDIF
enddo

!Process the variables
allocate(vardims(numdims))
do var=1,numvars
   status = nf90_inquire_variable(inid,var,name=varname, &
        xtype=xtype, ndims=numvdims, dimids=vardims, &
        natts=numatts)

   status = nf90_redef(outid)
   status = nf90_def_var(outid,trim(varname),xtype, &
            vardims(1:numvdims),varid)
   if (use_deflate) then
       status = nf90_def_var_deflate(outid,varid,1,1,2)
   endif
   IF (status .ne. nf90_noerr) THEN
      print('(a)'),'Error Defining Variables.'
      print('(2a)'),'   Variable Name: ',trim(varname)
      print('(a,2i4)'),'   Variable Type/Ndims: ',xtype,numvdims
      print('(a)'),'Stopping.'
      STOP
   ENDIF

   !copy attributes
   do att=1,numatts
      status=nf90_inq_attname(inid,var,att,name=attname)
      status=nf90_copy_att(inid,var,trim(attname),outid,varid)
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
      if ((dimnames(vardims(1)) .eq. 'lon') .and. &
          (dimnames(vardims(2)) .eq. 'lat')) then
          ntemp3=1
          ntemp4=1
          allocate(dvalues4in(nlonin,nlatin,ntemp3,ntemp4))
          allocate(dvalues4out(nlonout,nlatout,ntemp3,ntemp4))
          dvalues4out = 0
          status=nf90_get_var(inid,var,dvalues4in(:,:,1,1), &
                   start=(/1,1/),count=(/nlonin,nlatin/))

           allocate(fvalues4in(nlonin,nlatin,ntemp3,ntemp4))
           allocate(fvalues4out(nlonout,nlatout,ntemp3,ntemp4))
           fvalues4in = real(dvalues4in)
           fvalues4out = 0
           call regrid_varup(varname, &
                nlonin,nlatin,ntemp3,ntemp4, &
                nlonout,nlatout,nlu,maxng,   &
                pft_areain, pft_areaout, &
                ngmin, refiin, refjin, refkin, &
                arlonin, arlatin, arlonout, arlatout, &
                contribin, areagout, &
                fvalues4in, fvalues4out)

           dvalues4out = dble(fvalues4out)
           status=nf90_put_var(outid,var,fvalues4out, &
                   start=(/1,1/),count=(/nlonout,nlatout/))
           deallocate(dvalues4in,dvalues4out)
           deallocate(fvalues4in,fvalues4out)
      else
         allocate(dvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
         status = nf90_get_var(inid,var,dvalues2)
         status = nf90_put_var(outid,var,dvalues2)
         deallocate(dvalues2)
      endif
    elseif ((xtype .eq. 6) .and. (numvdims .eq. 3)) then
        if ((dimnames(vardims(1)) .eq. 'lon') .and. &
            (dimnames(vardims(2)) .eq. 'lat')) then
            ntemp3=1
            ntemp4=dimlens(vardims(3))
            allocate(dvalues4in(nlonin,nlatin,ntemp3,ntemp4))
            allocate(dvalues4out(nlonout,nlatout,ntemp3,ntemp4))
            dvalues4out = 0
            status=nf90_get_var(inid,var,dvalues4in(:,:,1,:), &
                   start=(/1,1,1/),count=(/nlonin,nlatin,ntemp4/))

             allocate(fvalues4in(nlonin,nlatin,ntemp3,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,ntemp3,ntemp4))
             fvalues4in=real(dvalues4in)
             fvalues4out=0.
             call regrid_varup(varname, &
                  nlonin,nlatin,ntemp3,ntemp4, &
                  nlonout,nlatout,nlu,maxng,   &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, areagout, &
                  fvalues4in, fvalues4out)

             dvalues4out = dble(fvalues4out)
             status=nf90_put_var(outid,var,dvalues4out, &
                     start=(/1,1,1/),count=(/nlonout,nlatout,ntemp4/))
             deallocate(dvalues4in,dvalues4out)
             deallocate(fvalues4in,fvalues4out)
        else
             allocate(dvalues3(dimlens(vardims(1)), &
                        dimlens(vardims(2)),dimlens(vardims(3))))
             status=nf90_get_var(inid,var,dvalues3)
             status=nf90_put_var(outid,var,dvalues3)
             deallocate(dvalues3) 
        endif

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

    elseif ((xtype .eq. 5) .and. (numvdims .eq. 2)) then
        if ((dimnames(vardims(1)) .eq. 'lon') .and. &
            (dimnames(vardims(2)) .eq. 'lat')) then
            ntemp3=1
            ntemp4=1

            allocate(fvalues4in(nlonin,nlatin,ntemp3,ntemp4))
            allocate(fvalues4out(nlonout,nlatout,ntemp3,ntemp4))
            fvalues4out = 0
            status=nf90_get_var(inid,var,fvalues4in(:,:,1,1), &
                   start=(/1,1/),count=(/nlonin,nlatin/))

             call regrid_varup(varname, &
                  nlonin,nlatin,ntemp3,ntemp4, &
                  nlonout,nlatout,nlu,maxng,   &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, areagout, &
                  fvalues4in, fvalues4out)

             status=nf90_put_var(outid,var,fvalues4out, &
                     start=(/1,1/),count=(/nlonout,nlatout/))
             deallocate(fvalues4in,fvalues4out)
        else
             allocate(fvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
             status=nf90_get_var(inid,var,fvalues2)
             status=nf90_put_var(outid,var,fvalues2)
             deallocate(fvalues2) 
        endif
    elseif ((xtype .eq. 5) .and. (numvdims .eq. 3)) then
       if ((sort_pfts) .AND. &
           ((varname .eq. 'lu_area') .or. &
            (varname .eq. 'pft_area') .or. &
            (varname .eq. 'area'))) then
             allocate(fvalues3(nlonout,nlatout,nlev))
             fvalues3=pft_areaout
             call regrid_sortf3(nlonout,nlatout,nlev, &
                  pft_areaout, fvalues3)
             status = nf90_put_var(outid,var,fvalues3)
             deallocate(fvalues3)
       else 
          if ((dimnames(vardims(1)) .eq. 'lon') .and. &
               (dimnames(vardims(2)) .eq. 'lat')) then
             ntemp4=dimlens(vardims(3))
             IF (ntemp4 .EQ. nlev) THEN
                allocate(fvalues4in(nlonin,nlatin,ntemp4,1))
                allocate(fvalues4out(nlonout,nlatout,ntemp4,1))
                fvalues4out = 0.
                status=nf90_get_var(inid,var,fvalues4in(:,:,:,1), &
                     start=(/1,1,1/),count=(/nlonin,nlatin,ntemp4/))
                
                call regrid_varup(varname, &
                     nlonin,nlatin,ntemp4,1, &
                     nlonout,nlatout,nlu,maxng, &
                     pft_areain, pft_areaout, &
                     ngmin, refiin, refjin, refkin, &
                     arlonin, arlatin, arlonout, arlatout, &
                     contribin, areagout, &
                     fvalues4in, fvalues4out)

                status = nf90_put_var(outid,var,fvalues4out, &
                     start=(/1,1,1/), count=(/nlonout,nlatout,ntemp4/))
                deallocate(fvalues4in,fvalues4out)
             ELSE
                ntemp4=dimlens(vardims(3))
                allocate(fvalues4in(nlonin,nlatin,1,ntemp4))
                allocate(fvalues4out(nlonout,nlatout,1,ntemp4))
                fvalues4out = 0.
                status=nf90_get_var(inid,var,fvalues4in(:,:,1,:), &
                   start=(/1,1,1/),count=(/nlonin,nlatin,ntemp4/))

                call regrid_varup(varname, &
                     nlonin,nlatin,1,ntemp4, &
                     nlonout,nlatout,nlu,maxng, &
                     pft_areain, pft_areaout, &
                     ngmin, refiin, refjin, refkin, &
                     arlonin, arlatin, arlonout, arlatout, &
                     contribin, areagout, &
                     fvalues4in, fvalues4out)

                status=nf90_put_var(outid,var,fvalues4out, &
                        start=(/1,1,1/),count=(/nlonout,nlatout,ntemp4/))
                deallocate(fvalues4in,fvalues4out)
             ENDIF
          else
             print(a1),'Expecting 3-D Float Array To Have Lon/Lat'
             stop err_message
          endif         
       endif
    elseif ((xtype .eq. 5) .and. (numvdims .eq. 4)) then
         if ((dimnames(vardims(1)) .eq. 'lon') .and. &
             (dimnames(vardims(2)) .eq. 'lat') .and. &
             (dimlens(vardims(3)) .eq. nlev)) then
             ntemp4=dimlens(vardims(4))
             allocate(fvalues4in(nlonin,nlatin,nlev,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,nlev,ntemp4))
             fvalues4out = 0.
             status=nf90_get_var(inid,var,fvalues4in)

             call regrid_varup(varname, &
                  nlonin,nlatin,nlev,ntemp4, &
                  nlonout,nlatout,nlu,maxng, &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, areagout, &
                  fvalues4in, fvalues4out)

             IF (sort_pfts) THEN
                call regrid_sortf4(nlonout,nlatout,nlev,ntemp4, &
                     pft_areaout, fvalues4out)
             ENDIF
                
             status=nf90_put_var(outid,var,fvalues4out)
             deallocate(fvalues4in,fvalues4out)
         else
             print(a1),'Expecting 4-D Float Array To Have Lon/Lat/Lev'
             stop err_message
         endif

    elseif ((xtype .eq. 4) .and. (numvdims .eq. 1)) then
       allocate(ivalues(dimlens(vardims(1))))
       status = nf90_get_var(inid,var,ivalues)
       if (status .ne. nf90_noerr) then
          print*,'Error Getting Variable: ',trim(varname)
       endif
       status = nf90_put_var(outid,var,ivalues)
       if (status .ne. nf90_noerr) then
          print*,'Error Writing Variable: ',trim(varname)
       endif
       deallocate(ivalues)
       
    elseif ((xtype .eq. 4) .and. (numvdims .eq. 3)) then
       if ((sort_pfts) .AND. &
           ((varname .eq. 'lu_pref') .or. &
            (varname .eq. 'pft_ref'))) then
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
    elseif ((xtype .eq. 3) .and. (numvdims .eq. 2)) then
        if ((dimnames(vardims(1)) .eq. 'lon') .and. &
            (dimnames(vardims(2)) .eq. 'lat')) then
            ntemp3=1
            ntemp4=1
            allocate(iivalues4in(nlonin,nlatin,ntemp3,ntemp4))
            allocate(iivalues4out(nlonout,nlatout,ntemp3,ntemp4))
            iivalues4out = 0
            status=nf90_get_var(inid,var,iivalues4in(:,:,1,1), &
                   start=(/1,1/),count=(/nlonin,nlatin/))

             allocate(fvalues4in(nlonin,nlatin,ntemp3,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,ntemp3,ntemp4))
             fvalues4in = real(iivalues4in)
             fvalues4out = 0.
             call regrid_varup(varname, &
                  nlonin,nlatin,ntemp3,ntemp4, &
                  nlonout,nlatout,nlu,maxng,   &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, areagout, &
                  fvalues4in, fvalues4out)

             iivalues4out = nint(fvalues4out,kind=1)
             status=nf90_put_var(outid,var,iivalues4out, &
                     start=(/1,1/),count=(/nlonout,nlatout/))
             deallocate(iivalues4in,iivalues4out)
             deallocate(fvalues4in,fvalues4out)
         else
             allocate(iivalues2(dimlens(vardims(1)),dimlens(vardims(2))))
             status=nf90_get_var(inid,var,iivalues2)
             status=nf90_put_var(outid,var,iivalues2)
             deallocate(iivalues2) 
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
                 print*,'  Dimensions: ', &
                         dimlens(vardims(1)),dimlens(vardims(2))
                 print*,'Stopping.'
                 stop err_message
             endif
        else
             print*,'Unable To Handle Character Array Dimension: ', &
                     trim(dimnames(vardims(2)))
             stop err_message
        endif

   elseif ((xtype .eq. 1) .and. (numvdims .eq. 1)) then
           allocate(bvalues(dimlens(vardims(1))))
           status = nf90_get_var(inid,var,bvalues)
           status = nf90_put_var(outid,var,bvalues)
           deallocate(bvalues)

   elseif ((xtype .eq. 1) .and. (numvdims .eq. 2)) then
        if ((dimnames(vardims(1)) .eq. 'lon') .and. &
            (dimnames(vardims(2)) .eq. 'lat')) then
            ntemp3=1
            ntemp4=1
            allocate(bvalues4in(nlonin,nlatin,ntemp3,ntemp4))
            allocate(bvalues4out(nlonout,nlatout,ntemp3,ntemp4))
            bvalues4out = 0
            status=nf90_get_var(inid,var,bvalues4in(:,:,1,1), &
                   start=(/1,1/),count=(/nlonin,nlatin/))

             allocate(fvalues4in(nlonin,nlatin,ntemp3,ntemp4))
             allocate(fvalues4out(nlonout,nlatout,ntemp3,ntemp4))
             fvalues4in = real(bvalues4in)
             fvalues4out = 0.
             call regrid_varup(varname, &
                  nlonin,nlatin,ntemp3,ntemp4, &
                  nlonout,nlatout,nlu,maxng,   &
                  pft_areain, pft_areaout, &
                  ngmin, refiin, refjin, refkin, &
                  arlonin, arlatin, arlonout, arlatout, &
                  contribin, areagout, &
                  fvalues4in, fvalues4out)

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

end subroutine regrid_up


!===================================
! Subroutine to sort the PFTs.
!    Expects the array to change
!    being a float.
!
! kdhaynes, 06/16
 
subroutine regrid_sortf3( &
     nlon, nlat, nlev, &
     sort_array, sort_fvar)

implicit none

!input variables
integer, intent(in) :: nlon, nlat, nlev
real, dimension(nlon,nlat,nlev), intent(in) :: sort_array
real, dimension(nlon,nlat,nlev), intent(inout) :: sort_fvar

!local variables
integer :: i,j
real, dimension(nlev) :: mysort
integer, dimension(nlev) :: myref

!-------------
do i=1,nlon
   do j=1,nlat
      mysort = sort_array(i,j,:)
      call hpsort(nlev, mysort, myref)
      call hprev(nlev, mysort, myref)

      sort_fvar(i,j,:) = sort_fvar(i,j,myref(:))
   enddo
enddo

end subroutine regrid_sortf3


!===================================
! Subroutine to sort the PFTs.
!    Expects the array to change
!    being a float.
!
! kdhaynes, 06/16
 
subroutine regrid_sortf4( &
     nlon, nlat, nlev, ntime, &
     sort_array, sort_fvar)

implicit none

!input variables
integer, intent(in) :: nlon, nlat, nlev, ntime
real, dimension(nlon,nlat,nlev), intent(in) :: sort_array
real, dimension(nlon,nlat,nlev,ntime), intent(inout) :: sort_fvar

!local variables
integer :: i,j
real, dimension(nlev) :: mysort
integer, dimension(nlev) :: myref

!-------------
do i=1,nlon
   do j=1,nlat
      mysort = sort_array(i,j,:)
      call hpsort(nlev,mysort, myref)
      call hprev(nlev,mysort,myref)

      sort_fvar(i,j,:,:) = sort_fvar(i,j,myref(:),:)
   enddo
enddo

end subroutine regrid_sortf4


!===================================
! Subroutine to sort the PFTs.
!    Expects the array to change
!    being an integer.
!
! kdhaynes, 06/16
 
subroutine regrid_sorti3( &
     nlon, nlat, nlev, &
     sort_array, sort_ivar)

implicit none

!input variables
integer, intent(in) :: nlon, nlat, nlev
real, dimension(nlon,nlat,nlev), intent(in) :: sort_array
integer, dimension(nlon,nlat,nlev), intent(inout) :: sort_ivar

!local variables
integer :: i,j
real, dimension(nlev) :: mysort
integer, dimension(nlev) :: myref

!-------------
do i=1,nlon
   do j=1,nlat
      mysort = sort_array(i,j,:)
      call hpsort(nlev,mysort, myref)
      call hprev(nlev,mysort,myref)

      sort_ivar(i,j,:) = sort_ivar(i,j,myref(:))
   enddo
enddo

end subroutine regrid_sorti3


!===================================
! Subroutine to output regrid a variable.
!
! kdhaynes, 06/16
 
subroutine regrid_varup( varname, &
     nlonin, nlatin, nlev, ntime, &
     nlonout, nlatout, nlu, maxng, &
     pft_areain, pft_areaout, &
     ngmin, refiin, refjin, refkin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin, areagout, varin, varout)

implicit none

!parameters
logical, parameter :: print_sums=.true.

!input variables
character(len=*), intent(in) :: varname
integer, intent(in) :: nlonin, nlatin, nlev, ntime
integer, intent(in) :: nlonout, nlatout, nlu, maxng

integer, dimension(nlonin,nlatin), intent(in) :: ngmin
integer, dimension(nlonin,nlatin,maxng), intent(in) :: refiin, refjin
integer, dimension(nlonin,nlatin,nlev,maxng), intent(in) :: refkin

real, dimension(nlonin,nlatin,nlu), intent(in) :: pft_areain
real, dimension(nlonout,nlatout,nlu), intent(in) :: pft_areaout

real*8, dimension(nlonin), intent(in) :: arlonin
real*8, dimension(nlatin), intent(in) :: arlatin
real*8, dimension(nlonout), intent(in) :: arlonout
real*8, dimension(nlatout), intent(in) :: arlatout
real*8, dimension(nlonin,nlatin,maxng), intent(in) :: contribin
real*8, dimension(nlonout,nlatout), intent(in) :: areagout

real, dimension(nlonin,nlatin,nlev,ntime), intent(in) :: varin
real, dimension(nlonout,nlatout,nlev,ntime), intent(inout) :: varout

!data variables
real*8 :: ratio
real*8, dimension(:), allocatable :: sumvarin, sumvarout1, sumvarout2

!local variables
integer :: i,j,k,l,t
integer :: irefout, jrefout, krefout
real    :: parea

!-------------------------

!regrid
varout(:,:,:,:) = 0.0

do i=1,nlonin
   do j=1,nlatin
      do l=1,ngmin(i,j)
         irefout=refiin(i,j,l)
         jrefout=refjin(i,j,l)

         if (nlu .gt. 1) then
             k=1
             parea=pft_areain(i,j,k)
             do while(parea .gt. 0.01)
                krefout=refkin(i,j,k,l)

                if ((irefout .lt. 1) .or. (jrefout .lt. 1) .or. &
                    (krefout .lt. 1)) then
                     print*,'Bad References!',irefout,jrefout,krefout
                     print*,'Original: ',i,j,k
                     print*,'Original PFT Areas: ',pft_areain(i,j,:)
                     print*,'New PFT Areas:  ',pft_areaout(irefout,jrefout,:)
                     stop
                 else
                     varout(irefout,jrefout,krefout,:) = &
                         varout(irefout,jrefout,krefout,:) + &
                         varin(i,j,k,:)* &
                         real(contribin(i,j,l)/areagout(irefout,jrefout))
                 endif

                 k=k+1
                 if (k .gt. nlev) then
                    parea=-1
                 else
                    parea=pft_areain(i,j,k)
                 endif
              enddo !while pft_area > 0.01
          else !nlu == 1
             varout(irefout,jrefout,:,:) = &
                       varout(irefout,jrefout,:,:) + &
                       varin(i,j,:,:) * &
                       real(contribin(i,j,l)/areagout(irefout,jrefout))
          endif !nlev >1
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
                 varin(i,j,k,:)*arlonin(i)*arlatin(j)
         endif !nlu > 1
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
         endif !nlu > 1
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
          endif
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

end subroutine regrid_varup


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine hprev(npts,rarray,refarray)

implicit none

integer :: npts
real, dimension(npts) :: rarray
integer, dimension(npts) :: refarray

real,dimension(npts) :: copyrarray
integer, dimension(npts) :: copyrefarray
integer :: i,count

copyrarray=rarray
copyrefarray=refarray
count=npts
DO i=1,npts
   rarray(i) = copyrarray(count)
   refarray(i) = copyrefarray(count)
   count=count-1
ENDDO

end subroutine hprev



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*  Sorts an array RA of length N in ascending order *
!*                by the Heapsort method             *
!* ------------------------------------------------- *
!* INPUTS:                                           *
!*	    N	  size of table RA                   *
!*          RA	  table to be sorted                 *
!* OUTPUT:                                           *
!*	    RA    table sorted in ascending order    *
!*                                                   *
!* NOTE: The Heapsort method is a N Log2 N routine,  *
!*       and can be used for very large arrays.      *
!*****************************************************       
subroutine hpsort(npts,rarray,refarray)

implicit none

integer :: npts
real, dimension(npts) :: rarray
integer, dimension(npts) :: refarray

real :: RRA
integer :: RRAREF

integer :: I,J,L,IR

DO I=1,npts
   refarray(I) = I
ENDDO
L=npts/2+1
IR=npts

10 continue
  if (L > 1) then
     L=L-1
     RRA=rarray(L)
     RRAREF=refarray(L)
  else
     RRA=rarray(IR)
     RRAREF=refarray(IR)
     rarray(IR) = rarray(1)
     refarray(IR) = refarray(1)
     IR=IR-1
     if (IR .eq. 1) then
        rarray(1)=RRA
        refarray(1)=RRAREF
        return
      endif
   endif
   I=L
   J=L+L
20 if (j.le.IR) then
       if (J < IR) then
          if (rarray(J) < rarray(J+1)) J=J+1
       endif
       if (RRA < rarray(J)) then
           rarray(I)=rarray(J)
           refarray(I)=refarray(J)
           I=J; J=J+J
        else
           J=IR+1
        endif
        goto 20
     endif

     rarray(I)=RRA
     refarray(I)=RRAREF
     goto 10

end subroutine hpsort

