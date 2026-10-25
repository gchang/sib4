!========================================
subroutine regrid_output(nfiles, &
    filesin, filesout, regridfile)
!========================================

use sibinfo_module, only: &
    lareaname, lprefname
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filesin, filesout
character(len=*), intent(in) :: regridfile

!parameters
integer, parameter :: maxng=4

!file variables
integer :: stringloc, stringend
character(len=180) :: shortname

!netcdf variables
integer :: inid
integer :: status
integer :: dimid, varid

!shared variables
integer :: nlu, nlev

!original variables
integer :: nlonin, nlatin
real, dimension(:), allocatable :: &
   lonin, latin
integer, dimension(:,:,:), allocatable :: &
   lu_prefin
real, dimension(:,:,:), allocatable :: &
   lu_areain
real*8, dimension(:,:), allocatable :: areagin

!new variables
integer :: nlonout, nlatout
real, dimension(:), allocatable :: &
   lonout, latout
integer, dimension(:,:,:), allocatable :: &
   lu_prefout
real, dimension(:,:,:), allocatable :: &
   lu_areaout
real*8, dimension(:,:), allocatable :: areagout

!areal information
integer, dimension(:,:), allocatable :: ngmin
integer, dimension(:,:,:), allocatable :: refiin,refjin
integer, dimension(:,:,:,:), allocatable :: refkin
real*8, dimension(:), allocatable :: arlonin, arlatin
real*8, dimension(:), allocatable :: arlonout, arlatout
real*8, dimension(:,:,:), allocatable :: contribin

!misc variables
integer :: f
logical :: regridup

!----------------------------------------
DO f=1, nfiles

   !Print file short names
   stringloc = index(filesin(f),'/',back=.true.)
   stringend = len_trim(filesin(f))
   shortname = (filesin(f)(stringloc+1:stringend))
   print('(2a)'),'   Regridding: ',trim(shortname)

   stringloc = index(filesout(f),'/',back=.true.)
   stringend = len_trim(filesout(f))
   shortname = (filesout(f)(stringloc+1:stringend))
   print('(2a)'),'   Writing   : ',trim(shortname)
   

   IF (f .EQ. 1) THEN  !only get grid info once
      !Get the new grid
      status = nf90_open(trim(regridfile),nf90_nowrite,inid)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening Regrid File: '
         print*,'  ',trim(regridfile)
         print*,'Stopping.'
         STOP
      ENDIF

      status = nf90_inq_dimid(inid,'lon',dimid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(inid,'nlon',dimid)
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Finding Regrid Lon Dimension.'
              print*,'Stopping.'
              STOP
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(inid,dimid,len=nlonout)
      allocate(lonout(nlonout))
      status = nf90_inq_varid(inid,'lon',varid)
      status = nf90_get_var(inid,varid,lonout)

      status = nf90_inq_dimid(inid,'lat',dimid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(inid,'nlat',dimid)
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Finding Regrid Lat Dimension.'
              print*,'Stopping.'
              STOP
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(inid,dimid,len=nlatout)
      allocate(latout(nlatout))
      status = nf90_inq_varid(inid,'lat',varid)
      status = nf90_get_var(inid,varid,latout)
      status = nf90_close(inid)


      !Get the original grid
      status = nf90_open(trim(filesin(f)),nf90_nowrite,inid)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening File: '
         print*,'  ',trim(filesin(f))
         print*,'Stopping.'
         STOP
      ENDIF

      status = nf90_inq_dimid(inid,'lon',dimid)
      IF (status .NE. nf90_noerr) THEN
          status = nf90_inq_dimid(inid,'nlon',dimid)
          IF (status .NE. nf90_noerr) THEN
              status = nf90_inq_dimid(inid,'longitude',dimid)
              IF (status .NE. nf90_noerr) THEN
                 print*,'Error Finding Original Lon Dimension.'
                 print*,'Stopping.'
                 STOP
              ENDIF
           ENDIF
      ENDIF
      status = nf90_inquire_dimension(inid,dimid,len=nlonin)
      allocate(lonin(nlonin))
      status = nf90_inq_varid(inid,'lon',varid)
      IF (status .NE. nf90_noerr) THEN
          status = nf90_inq_varid(inid,'longitude',varid)
          IF (status .NE. nf90_noerr) THEN
              print*,'Error Finding Original Longitude Array.'
              print*,'Stopping.'
              STOP
          ENDIF
      ENDIF
      status = nf90_get_var(inid,varid,lonin)
      IF (status .ne. nf90_noerr) THEN
        print*,'Error Getting Original Longitude Array.'
        print*,'Stopping.'
        STOP
      ENDIF

      status = nf90_inq_dimid(inid,'lat',dimid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(inid,'nlat',dimid)
          IF (status .ne. nf90_noerr) THEN
              status = nf90_inq_dimid(inid,'latitude',dimid)
              IF (status .ne. nf90_noerr) THEN
                  print*,'Error Finding Regrid Lat Dimension.'
                  print*,'Stopping.'
                  STOP
              ENDIF
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(inid,dimid,len=nlatin)
      allocate(latin(nlatin))
      status = nf90_inq_varid(inid,'lat',varid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_varid(inid,'latitude',varid)
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Finding Latitude Array.'
             print*,'Stopping.'
             STOP
          ENDIF
      ENDIF
      status = nf90_get_var(inid,varid,latin)
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Getting Latitude Array.'
          print*,'Stopping.'
          STOP
       ENDIF

      status = nf90_inq_dimid(inid,'nlu',dimid)
      IF (status .EQ. nf90_noerr) THEN !PFT stacked
         status = nf90_inquire_dimension(inid,dimid,len=nlu)
          allocate(lu_prefin(nlonin,nlatin,nlu))
          status = nf90_inq_varid(inid,trim(lprefname),varid)
          status = nf90_get_var(inid,varid,lu_prefin)

          allocate(lu_areain(nlonin,nlatin,nlu))
          status = nf90_inq_varid(inid,trim(lareaname),varid)
          status = nf90_get_var(inid,varid,lu_areain)

          nlev = nlu
      ELSE  !no stacked PFTs
          nlu = 1
          allocate(lu_prefin(nlonin,nlatin,nlu))
          lu_prefin = 1

          allocate(lu_areain(nlonin,nlatin,nlu))
          lu_areain = 1.

          nlev = 1
          status = nf90_inq_dimid(inid,'npft',dimid)
          IF (status .eq. nf90_noerr) THEN
             status = nf90_inquire_dimension(inid,dimid,len=nlev)
          ELSE
             status = nf90_inq_dimid(inid,'ncrop',dimid)
             IF (status .eq. nf90_noerr) THEN
                status = nf90_inquire_dimension(inid,dimid,len=nlev)
             ENDIF
             
             status = nf90_inq_dimid(inid,'level',dimid)
             IF (status .eq. nf90_noerr) THEN
                status = nf90_inquire_dimension(inid,dimid,len=nlev)
             ENDIF
          ENDIF
          
      ENDIF
      status = nf90_close(inid)

      !-----------------------
      !Compare the dimensions
      print('(a,3i4)'),'     Original Dimensions (nlon/nlat/nlev): ', &
           nlonin, nlatin, nlev
      print('(a,3i4)'),'     New Dimensions (nlon/nlat/nlev):      ', &
           nlonout, nlatout, nlev
      regridup = .true.
      IF (nlonout .GT. nlonin) regridup = .false.

      !Get areal information
      allocate(arlonin(nlonin),arlatin(nlatin))
      allocate(arlonout(nlonout),arlatout(nlatout))
      allocate(areagin(nlonin,nlatin))
      allocate(areagout(nlonout,nlatout))
      allocate(lu_prefout(nlonout,nlatout,nlu)) 
      allocate(lu_areaout(nlonout,nlatout,nlu))

      IF (regridup) THEN
          allocate(ngmin(nlonin,nlatin))
          allocate(refiin(nlonin,nlatin,maxng), &
                refjin(nlonin,nlatin,maxng), &
                refkin(nlonin,nlatin,nlu,maxng))
          allocate(contribin(nlonin,nlatin,maxng))

          call regrid_upinfo(  &
               nlonin, nlatin, &
               nlonout, nlatout, maxng, &
               lonin, latin, lonout, latout,  &
               ngmin, refiin, refjin,  &
               arlonin, arlatin, arlonout, arlatout, &
               contribin, areagin, areagout)

          if (nlu .gt. 1) then
              call regrid_upinfo_pft(  &
                   nlonin, nlatin, nlu, &
                   nlonout, nlatout, maxng, &
                   ngmin, refiin, refjin, &
                   contribin, lu_prefin, lu_areain, &
                   lonout, latout, areagout, refkin, &
                   lu_prefout, lu_areaout)
          endif
      ELSE
          allocate(ngmin(nlonout,nlatout))
          allocate(refiin(nlonout,nlatout,maxng), &
                refjin(nlonout,nlatout,maxng), &
                refkin(nlonout,nlatout,nlu,maxng))
          allocate(contribin(nlonout,nlatout,maxng))

          call regrid_downinfo(  &
               nlonin, nlatin, &
               nlonout, nlatout, maxng, &
               lonin, latin, lonout, latout, &
               ngmin, refiin, refjin, &
               arlonin, arlatin, arlonout, arlatout, &
               contribin, areagin, areagout)

          if (nlu .gt. 1) then
             call regrid_downinfo_pft(  &
                  nlonin, nlatin, nlu, &
                  nlonout, nlatout, maxng, &
                  ngmin, refiin, refjin, &
                  contribin, lu_prefin, lu_areain, &
                  lonout, latout, refkin, &
                  lu_prefout, lu_areaout)
          endif
      ENDIF
   ENDIF !f==1

   IF (regridup) THEN
       call regrid_up( &
            filesin(f), filesout(f), &
            nlonin, nlatin, nlev, nlu, &
            nlonout, nlatout, maxng, &
            lonout, latout, &
            lu_areain,     &
            lu_prefout, lu_areaout, &
            ngmin, refiin, refjin, refkin, &
            arlonin, arlatin, arlonout, arlatout, &
            contribin, areagout)
   ELSE
      call regrid_down( &
            filesin(f), filesout(f), &
            nlonin, nlatin, nlev, nlu, &
            nlonout, nlatout, maxng, &
            lonout, latout, &
            lu_areain,     &
            lu_prefout, lu_areaout, &
            ngmin, refiin, refjin, refkin, &
            arlonin, arlatin, arlonout, arlatout, &
            contribin)
   ENDIF
ENDDO !f=1,nfiles


end subroutine regrid_output


