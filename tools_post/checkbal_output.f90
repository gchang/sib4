subroutine checkbal_output(nfiles, filenames, &
     filername, filecname)

  use netcdf
  use sibinfo_module

  implicit none

  !input variables
  integer, intent(in) :: nfiles
  character(len=256), dimension(nfiles), intent(in) :: &
      filenames
  character(len=256), intent(in) :: filername, filecname

  !netcdf variables
  integer :: ncid, status, dimid

  !open the first file
  status = nf90_open(trim(filenames(1)), nf90_nowrite, ncid)

  !get dimensions
  status = nf90_inq_dimid(ncid, nsibnamed, dimid)

  if (status .eq. nf90_noerr) then
     call checkbal_outsib(nfiles, filenames, &
          filername, filecname)
  else
     call checkbal_outgrid(nfiles, filenames, &
          filername, filecname)
  endif

  status = nf90_close(ncid)
  
end subroutine checkbal_output
  

!=========================================
subroutine checkbal_outsib(nfiles, filenames, &
           filername, filecname)
!=========================================
!Subroutine controlling the sequence of 
!   calls to check the flux balance.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filenames
character(len=256), intent(in) :: filername, filecname

!netcdf variables
integer :: ncid, rfid
integer :: status, dimid
integer :: gppid, respid, rcid, varid

!area variables
real, dimension(:), allocatable :: lonsib, latsib
real*8, dimension(:), allocatable :: areasib

!data variables
integer :: ndims
integer :: nsib, nlu, ntime, nttot
real*8 :: tconvert

integer, dimension(:,:), allocatable :: lu_pref
real, dimension(:,:), allocatable :: lu_area
real, dimension(:,:), allocatable :: ratio, tempc
real, dimension(:,:,:), allocatable :: tempta, temptr
real*8, dimension(:,:), allocatable :: tempa, tempr

real*8, dimension(:), allocatable :: assima, assimac, assimanc
real*8, dimension(:), allocatable :: respa, respac, respanc
real*8, dimension(:), allocatable :: respc
real*8 :: assimtota, assimtotac, assimtotanc
real*8 :: resptota, resptotac, resptotanc
real*8 :: respctot

!local variables
integer :: n,l
integer :: fref
logical :: use_crop, use_respb
logical :: file_exist

!-----------------------------------------------------------------
!process the files
nttot = 0
DO fref=1,nfiles
   !open the file
   status = nf90_open(trim(filenames(fref)), nf90_nowrite, ncid)

   IF (fref .EQ. 1) THEN
      !get dimensions
      status = nf90_inq_dimid(ncid, nsibnamed, dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nsib)
   
      status = nf90_inq_dimid(ncid, nluname, dimid)
      IF (status .NE. nf90_noerr) THEN
         nlu=1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlu)
      ENDIF

      !get time and time conversion factor
      status = nf90_inq_dimid(ncid, 'time', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=ntime)
      call calc_tconvert(ntime, tconvert)

      print('(a,2i10)'), '  Dimensions (nsib/nlu): ', nsib, nlu
      allocate(ratio(nsib,nlu))
      ratio(:,:) = 1.

      allocate(tempa(nsib,nlu),tempr(nsib,nlu))
      tempa(:,:) = 0.
      tempr(:,:) = 0.

      use_crop = .false.
      IF (nlu .EQ. 1) THEN
         ndims=2
         allocate(lu_pref(nsib,nlu))
         lu_pref(:,:) = 0
         allocate(lu_area(nsib,nlu))
         lu_area(:,:) = 1.

      ELSE
         IF (trim(filecname) .NE. '') THEN
            inquire(file=trim(filecname), exist=file_exist)
            IF (file_exist) THEN
                use_crop = .true.
                print*,'   Using Crop File To Calculate Crop Respiration.'
            ENDIF
         ENDIF
            
         ndims=3
         
        !get PFT refs and area
        allocate(lu_pref(nsib,nlu))
        status = nf90_inq_varid(ncid, trim(lprefname), varid)
        status = nf90_get_var(ncid, varid, lu_pref) 
        IF (status .NE. nf90_noerr) THEN
           print*,'Error getting PFT references.'
           print*,'Stoping.'
           STOP
        ENDIF

        allocate(lu_area(nsib,nlu))
        status = nf90_inq_varid(ncid, trim(lareaname), varid)
        status = nf90_get_var(ncid, varid, lu_area)
        IF (status .NE. nf90_noerr) THEN
            print*,'Error getting PFT areal contributions.'
            print*,'Stopping.'
            STOP
        ENDIF

      ENDIF

      !get lon, lat, and area
      allocate(lonsib(nsib),latsib(nsib))
      status = nf90_inq_varid(ncid, trim(lonname), varid)
      IF (status .eq. nf90_noerr) THEN
         status = nf90_get_var(ncid, varid, lonsib)
      ELSE
         print*,'Missing Longitude. Stopping.'
         STOP
      ENDIF

      status = nf90_inq_varid(ncid, trim(latname), varid)
      IF (status .eq. nf90_noerr) THEN
         status = nf90_get_var(ncid, varid, latsib)
      ELSE
         print*,'Missing Latitude. Stopping.'
         STOP
      ENDIF

      allocate(areasib(nsib))
      call calc_sibarea(nsib,lonsib,latsib,.false.,areasib)

      !get balance ratio
      status = nf90_open(trim(filername), nf90_nowrite, rfid)
      IF (status .EQ. nf90_noerr) THEN
         print*,'   Using Ratio To Calculate Balanced Respiration.'
         status = nf90_inq_varid(rfid,'ratio',varid)
         status = nf90_get_var(rfid,varid,ratio)
         print*,'       Ratio Min: ',minval(ratio)
         print*,'       Ratio Max: ',maxval(ratio)
         status = nf90_close(rfid)
         use_respb = .false.
      ELSE
         status = nf90_inq_varid(ncid, respbname, respid)
         IF (status .EQ. nf90_noerr) THEN
             use_respb = .true.  
             print*,'   Using File-Saved Balanced Respiration.'
         ELSE
             use_respb = .false.
             print*,'   Using Native Respiration.'
         ENDIF
      ENDIF

   ENDIF

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)
   nttot = nttot + ntime

   !get the GPP and RE
   status = nf90_inq_varid(ncid, gppname, gppid)
   IF (status .NE. nf90_noerr) THEN
       print*,''
       print*,'GPP Not Included In Output.'
       print*,'Stopping.'
       print*,''
       stop
   ENDIF

   IF (use_respb) THEN
       status = nf90_inq_varid(ncid, respbname, respid)
   ELSE
       status = nf90_inq_varid(ncid, respname, respid)
   ENDIF
   IF (status .NE. nf90_noerr) THEN
       print*,''
       print*,'Respiration Not Included In Output.'
       print*,'Stopping.'
       print*,''
       stop
   ENDIF

   allocate(tempta(nsib,nlu,ntime))
   status = nf90_get_var(ncid, gppid, tempta)
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Getting GPP.'
       print*,'Stopping.'
       STOP
   ENDIF

    allocate(temptr(nsib,nlu,ntime))
    status = nf90_get_var(ncid, respid, temptr)
    IF (status .ne. nf90_noerr) THEN
        print*,'Error Getting Respiration.'
        print*,'Stopping.'
        STOP
    ENDIF

    DO l=1,nlu
       DO n=1,nsib
          tempa(n,l) = tempa(n,l) + SUM(dble(tempta(n,l,:)))
          tempr(n,l) = tempr(n,l) + SUM(dble(temptr(n,l,:)))*dble(ratio(n,l))
       ENDDO
    ENDDO

   status = nf90_close(ncid)
   deallocate(tempta,temptr)

ENDDO !fref=1,nfiles

!get the Crop Respiration (if necessary)
allocate(respc(nsib))
respc(:) = 0.
IF (use_crop) THEN
    status = nf90_open(trim(filecname), nf90_nowrite, ncid)
    IF (status .NE. nf90_noerr) THEN
        print*,'Error Opening File Containing Crop Resp.'
        print*,'   ',trim(filecname)
        print*,'Stopping.'
        STOP
    ENDIF

    status = nf90_inq_varid(ncid,trim(respcname),rcid)
    IF (status .NE. nf90_noerr) THEN
       print*,'Error Getting Crop Resp Variable ID.'
       print*,'Stopping.'
       STOP
    ENDIF      

    allocate(tempc(nsib,ncrop))
    status = nf90_get_var(ncid, rcid, tempc)
    IF (status .ne. nf90_noerr) THEN
        print*,'Error Retrieving Crop Respiration.'
        print*,'   File ID: ',ncid
        print*,'   Var ID: ',rcid
        print*,'Stopping.'
        STOP
    ENDIF
   status = nf90_close(ncid)

   DO n=1,nsib
      respc(n) = SUM(tempc(n,:))*nttot 
   ENDDO
ENDIF

!calculate total fluxes
allocate(assima(nsib),assimanc(nsib),assimac(nsib))
assima(:) = 0.
assimanc(:) = 0.
assimac(:) = 0.

allocate(respa(nsib),respanc(nsib),respac(nsib))
respa(:) = 0.
respanc(:) = 0.
respac(:) = 0.

DO n=1,nsib
   DO l=1,nlu
      IF (lu_pref(n,l) .GE. npftstart) THEN
          assima(n) = assima(n) + tempa(n,l)*dble(lu_area(n,l))
          respa(n) = respa(n) + tempr(n,l)*dble(lu_area(n,l))

          IF (lu_pref(n,l) .GE. npftcropstart) THEN
             assimac(n) = assimac(n) + tempa(n,l)*dble(lu_area(n,l))
             respac(n) = respac(n) + tempr(n,l)*dble(lu_area(n,l))
          ELSE
             assimanc(n) = assimanc(n) + tempa(n,l)*dble(lu_area(n,l))
             respanc(n) = respanc(n) + tempr(n,l)*dble(lu_area(n,l))
          ENDIF
      ENDIF
   ENDDO
ENDDO

assimtota = SUM(assima(:)*areasib(:)*tconvert)
assimtotac = SUM(assimac(:)*areasib(:)*tconvert)
assimtotanc = SUM(assimanc(:)*areasib(:)*tconvert)

resptota = SUM(respa(:)*areasib(:)*tconvert)
resptotac = SUM(respac(:)*areasib(:)*tconvert)
resptotanc = SUM(respanc(:)*areasib(:)*tconvert)

respctot = SUM(respc(:)*areasib(:)*tconvert)

!print the results
call checkbal_print(assimtota, assimtotac, assimtotanc, &
     resptota, resptotac, resptotanc, &
     respctot)


end subroutine checkbal_outsib


!=========================================
subroutine checkbal_print(&
    gpptotall, gpptotac, gpptotanc, &
    resptotall, resptotac, resptotanc, &
    respctot)
!=========================================
!Subroutine to print the flux totals and
!   resulting balance.

!kdhaynes, 10/17
use sibinfo_module, only: &
   fa, fa3f, fa5f
implicit none

!input variables
real*8, intent(in) :: gpptotall, gpptotac, gpptotanc
real*8, intent(in) :: resptotall, resptotac, resptotanc
real*8, intent(in) :: respctot

!local variables
real*8 :: pdiff

!parameters
real*8 :: aconvert = 12./1.E21  !convert umol C to GtC

IF (gpptotanc .gt. 0.) THEN
   pdiff = abs(gpptotanc-resptotanc)/gpptotanc*100.
ENDIF
print(fa),''
print(fa),' Non-Crop Fluxes (Gt C)'
print(fa),'      GPP           RESP          %DIFF'
print(fa3f),' ', gpptotanc*aconvert, resptotanc*aconvert, pdiff

IF (gpptotac .gt. 0.) THEN
   pdiff = abs(gpptotac - (resptotac+respctot))/gpptotac*100.
ELSE
   pdiff = 0.
ENDIF
print(fa),''
print(fa),' Crop Fluxes (Gt C)'
print(fa),'      GPP           RESP          RESP_CROP     RESP_TOT       %DIFF'
print(fa5f),' ', gpptotac*aconvert, resptotac*aconvert, &
                 respctot*aconvert, (resptotac+respctot)*aconvert, &
                 pdiff

IF (gpptotall .gt. 0.) THEN
   pdiff = abs(gpptotall-resptotall-respctot)/gpptotall*100.
ENDIF
print(fa),''
print(fa),'  Total Fluxes (Gt C)'
print(fa),'      GPP           RESP          RESP_CROP     RESP_TOT       %DIFF'
print(fa5f), ' ', gpptotall*aconvert, resptotall*aconvert, &
             respctot*aconvert, (resptotall+respctot)*aconvert, &
             pdiff
print(fa),''

end subroutine checkbal_print



!=========================================
subroutine checkbal_outgrid(nfiles, filenames, &
           filername, filecname)
!=========================================
!Subroutine controlling the sequence of 
!   calls to check the flux balance.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filenames
character(len=256), intent(in) :: filername, filecname

!netcdf variables
integer :: ncid, rfid
integer :: status, dimid
integer :: gppid, respid, rcid, varid

!area variables
real, dimension(:), allocatable :: lon, lat
real*8, dimension(:,:), allocatable :: area

!data variables
integer :: nlon, nlat, nlu, mynpft, myncrop
integer :: ntime, nttot
real*8 :: tconvert

integer, dimension(:,:,:), allocatable :: lu_pref
real, dimension(:,:,:), allocatable :: lu_area
real, dimension(:,:,:), allocatable :: ratio
real, dimension(:,:,:,:), allocatable :: tempta, temptr, temptc
real*8, dimension(:,:,:), allocatable :: tempa, tempr, tempc
real*8, dimension(:,:), allocatable :: assima, assimac, assimanc
real*8, dimension(:,:), allocatable :: respa, respac, respanc
real*8, dimension(:,:), allocatable :: respc
real*8 :: assimtota, assimtotac, assimtotanc
real*8 :: resptota, resptotac, resptotanc
real*8 :: respctot

!local variables
integer :: i,j,l
integer :: cstart
integer :: fref, pref
logical :: use_crop, use_respb

!-----------------------------------------------------------------
use_crop = .false.

!process the files
nttot = 0
DO fref=1,nfiles
   !open the file
   status = nf90_open(trim(filenames(fref)), nf90_nowrite, ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Opening File: '
      print*,'  ',trim(filenames(fref))
      print*,'Stopping.'
      STOP
   ENDIF
   !print*,'Opening File: '
   !print*,'  ',trim(filenames(fref))
   
   IF (fref .EQ. 1) THEN
      !get dimensions
      status = nf90_inq_dimid(ncid, 'lon', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nlon)

      status = nf90_inq_dimid(ncid, 'lat', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nlat)
      
      status = nf90_inq_dimid(ncid, nluname, dimid)
      IF (status .NE. nf90_noerr) THEN
         nlu=1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlu)
      ENDIF

      status = nf90_inq_dimid(ncid, 'npft', dimid)
      IF (status .NE. nf90_noerr) THEN
         mynpft = 1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=mynpft)
      ENDIF
         
      !get time and time conversion factor
      status = nf90_inq_dimid(ncid, 'time', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=ntime)
      call calc_tconvert(ntime, tconvert)

      print('(a,4i6)'), '  Dimensions (nlon/nlat/nlu/npft): ', &
            nlon, nlat, nlu, mynpft

       if (nlu .gt. 1) mynpft = nlu
       allocate(ratio(nlon,nlat,mynpft))
       ratio(:,:,:) = 1.

      allocate(tempa(nlon,nlat,mynpft),tempr(nlon,nlat,mynpft))
      tempa(:,:,:) = 0.
      tempr(:,:,:) = 0.

      !get lon, lat, and area
      allocate(lon(nlon),lat(nlat))
      status = nf90_inq_varid(ncid, 'lon', varid)
      IF (status .eq. nf90_noerr) THEN
         status = nf90_get_var(ncid, varid, lon)
      ELSE
         print*,'Missing Longitude. Stopping.'
         STOP
      ENDIF

      status = nf90_inq_varid(ncid, 'lat', varid)
      IF (status .eq. nf90_noerr) THEN
         status = nf90_get_var(ncid, varid, lat)
      ELSE
         print*,'Missing Latitude. Stopping.'
         STOP
      ENDIF

      allocate(area(nlon,nlat))
      call calc_gridarea(nlon, nlat, lon, lat, area)

      !get lu_area and lu_pref
      allocate(lu_area(nlon,nlat,mynpft),lu_pref(nlon,nlat,mynpft))
      lu_area(:,:,:) = 1.
      lu_pref(:,:,:) = 0
      
      IF (nlu .eq. 10) THEN
         status = nf90_inq_varid(ncid, 'lu_area', varid)
         IF (status .eq. nf90_noerr) THEN
             status = nf90_get_var(ncid, varid, lu_area)
         ELSE
            print*,'Missing LU Area.  Stopping.'
            STOP
         ENDIF

         status = nf90_inq_varid(ncid, 'lu_pref', varid)
         IF (status .eq. nf90_noerr) THEN
            status = nf90_get_var(ncid, varid, lu_pref)
         ELSE
            print*,'Missing LU PREF.  Stopping.'
            STOP
         ENDIF
         print('(a,2i6)'),'  Min/Max LU_PREF: ',minval(lu_pref),maxval(lu_pref)
      ENDIF
         
      !get balance ratio
      status = nf90_open(trim(filername), nf90_nowrite, rfid)
      IF (status .EQ. nf90_noerr) THEN
         print*,'   Using Ratio To Calculate Balanced Respiration.'
         status = nf90_inq_varid(rfid,'ratio',varid)
         status = nf90_get_var(rfid,varid,ratio)
         print*,'       Ratio Min: ',minval(ratio)
         print*,'       Ratio Max: ',maxval(ratio)
         status = nf90_close(rfid)
         use_respb = .false.
      ELSE
         status = nf90_inq_varid(ncid, respbname, respid)
         IF (status .EQ. nf90_noerr) THEN
             use_respb = .true.  
             print*,'   Using File-Saved Balanced Respiration.'
         ELSE
             use_respb = .false.
             print*,'   Using Native Respiration.'
         ENDIF
      ENDIF

      !get crop fluxes
      status = nf90_inq_varid(ncid, 'resp_crop', rcid)
      IF (status .EQ. nf90_noerr) THEN
         print*,'   Using File-Saved Crop Fluxes.'
         use_crop = .true.
         allocate(tempc(nlon,nlat,mynpft))
         tempc(:,:,:) = 0. 
      ELSE
         status = nf90_open(trim(filecname), nf90_nowrite, rfid)
         IF (status .EQ. nf90_noerr) THEN
             print*,'   Using Crop Flux File To Calculate Balanced Respiration.'
             status = nf90_inq_dimid(rfid,'ncrop',dimid)
             status = nf90_inquire_dimension(rfid, dimid, len=myncrop)

             allocate(tempc(nlon,nlat,myncrop))
             status = nf90_inq_varid(rfid,'resp_crop',varid)
             status = nf90_get_var(rfid,varid,tempc)
             IF (status .ne. nf90_noerr) THEN
                print*,'Error Getting Crop Respiration.'
                STOP
             ENDIF
             status = nf90_close(rfid)
         ELSE
             allocate(tempc(nlon,nlat,mynpft))
             tempc(:,:,:) = 0.
         ENDIF
      ENDIF
   ENDIF !fref == 1

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)
   nttot = nttot + ntime

   !get the GPP and RE
   status = nf90_inq_varid(ncid, 'gpp', gppid)
   IF (status .NE. nf90_noerr) THEN
      status = nf90_inq_varid(ncid, 'assim', gppid)
   ENDIF
   IF (status .NE. nf90_noerr) THEN
       print*,''
       print*,'GPP Not Included In Output.'
       print*,'Stopping.'
       print*,''
       stop
   ENDIF

   IF (use_respb) THEN
       status = nf90_inq_varid(ncid, respbname, respid)
   ELSE
       status = nf90_inq_varid(ncid, respname, respid)
   ENDIF
   IF (status .NE. nf90_noerr) THEN
       print*,''
       print*,'Respiration Not Included In Output.'
       print*,'Stopping.'
       print*,''
       stop
   ENDIF

   if (use_crop) THEN
      status = nf90_inq_varid(ncid, 'resp_crop', rcid)
   ENDIF

   allocate(tempta(nlon,nlat,mynpft,ntime))
   allocate(temptr(nlon,nlat,mynpft,ntime))
   allocate(temptc(nlon,nlat,mynpft,ntime))

   status = nf90_get_var(ncid, gppid, tempta)
   IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting GPP.'
         print*,'Stopping.'
         STOP
   ENDIF

   status = nf90_get_var(ncid, respid, temptr)
   IF (status .ne. nf90_noerr) THEN
        print*,'Error Getting Respiration.'
        print*,'Stopping.'
        STOP
   ENDIF

   IF (use_crop) THEN
       status = nf90_get_var(ncid, rcid, temptc)
       IF (status .ne. nf90_noerr) THEN
           print*,'Error Getting Crop Respiration.'
           print*,'Stopping.'
           STOP
       ENDIF
    ENDIF

    DO l=1,mynpft
       DO j=1,nlat
          DO i=1,nlon
             tempa(i,j,l) = tempa(i,j,l) + SUM(dble(tempta(i,j,l,:)))
             tempr(i,j,l) = tempr(i,j,l) + SUM(dble(temptr(i,j,l,:)))*dble(ratio(i,j,l))
             IF (use_crop) THEN
                tempc(i,j,l) = tempc(i,j,l) + SUM(dble(temptc(i,j,l,:)))
             ENDIF
          ENDDO
       ENDDO
    ENDDO

   status = nf90_close(ncid)
   deallocate(tempta,temptr,temptc)

ENDDO !fref=1,nfiles

!calculate total fluxes
allocate(assima(nlon,nlat),assimanc(nlon,nlat),assimac(nlon,nlat))
assima(:,:) = 0.
assimanc(:,:) = 0.
assimac(:,:) = 0.

allocate(respa(nlon,nlat),respanc(nlon,nlat),respac(nlon,nlat))
respa(:,:) = 0.
respanc(:,:) = 0.
respac(:,:) = 0.

allocate(respc(nlon,nlat))
respc(:,:) = 0.

DO i=1,nlon
   DO j=1,nlat

      IF (use_crop) THEN
          respc(i,j) = sum(tempc(i,j,:))
      ELSE
          respc(i,j) = sum(tempc(i,j,:))*nttot
      ENDIF
          
      DO l=1,mynpft
         assima(i,j) = assima(i,j) + tempa(i,j,l)*lu_area(i,j,l)
         respa(i,j) = respa(i,j) + tempr(i,j,l)*lu_area(i,j,l)

         IF (mynpft .EQ. 10) THEN
            pref = lu_pref(i,j,l)
            cstart = npftcropstart 
         ELSE
            pref = l
            cstart = 11
         ENDIF
         
         IF (pref .GE. cstart) THEN
            assimac(i,j) = assimac(i,j) + tempa(i,j,l)*lu_area(i,j,l)
            respac(i,j) = respac(i,j) + tempr(i,j,l)*lu_area(i,j,l)
         ELSE
            assimanc(i,j) = assimanc(i,j) + tempa(i,j,l)*lu_area(i,j,l)
            respanc(i,j) = respanc(i,j) + tempr(i,j,l)*lu_area(i,j,l)
         ENDIF
       ENDDO
    ENDDO
ENDDO

assimtota = SUM(assima(:,:)*area(:,:)*tconvert)
assimtotac = SUM(assimac(:,:)*area(:,:)*tconvert)
assimtotanc = SUM(assimanc(:,:)*area(:,:)*tconvert)

resptota = SUM(respa(:,:)*area(:,:)*tconvert)
resptotac = SUM(respac(:,:)*area(:,:)*tconvert)
resptotanc = SUM(respanc(:,:)*area(:,:)*tconvert)

respctot = SUM(respc(:,:)*area(:,:)*tconvert)

!print the results
call checkbal_print(assimtota, assimtotac, assimtotanc, &
     resptota, resptotac, resptotanc, &
     respctot)


end subroutine checkbal_outgrid
