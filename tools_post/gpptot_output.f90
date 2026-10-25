!=========================================
subroutine gpptot_output(nfiles, filenames, &
           isglobal, printbiomes, printpfts)
!=========================================
!Subroutine controlling the sequence of 
!   calls to calculate global annual GPP.
!
!kdhaynes, 10/17

use sibinfo_module, only: &
    nsibnamed
use netcdf

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames
logical, intent(in) :: isglobal, printbiomes, printpfts

!netcdf variables
integer :: status, ncid, dimid

!------------------------------------------
!Check the first file to determine if gridded or vector.
status = nf90_open(trim(filenames(1)),nf90_nowrite,ncid)
IF (status .ne. nf90_noerr) THEN
   print*,'Error Opening File: '
   print*,'  ',trim(filenames(1))
   print*,'Stoping.'
   STOP
ENDIF
  
!get nsib or nlon/nlat
status = nf90_inq_dimid(ncid, trim(nsibnamed), dimid)
IF (status .EQ. nf90_noerr) THEN
   call gpptot_sib(nfiles, filenames, &
        isglobal, printbiomes, printpfts)
ELSE
   status = nf90_inq_dimid(ncid, 'lon', dimid)
   IF (status .EQ. nf90_noerr) THEN
      call gpptot_grid(nfiles, filenames, &
           printbiomes, printpfts)
   ELSE
      print*,'Unknown Type Of File: '
      print*,'  Missing vector dimension: ',trim(nsibnamed)
      print*,'  Missing lon dimension: ','lon'
      print*,'Stopping.'
      STOP
   ENDIF
ENDIF

end subroutine gpptot_output


!=========================================
subroutine gpptot_sib(nfiles, filenames, &
           isglobal, printbiomes, printpfts)
!=========================================
!Subroutine to calculate total GPP
!  from a SiB4 or vector file.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames
logical, intent(in) :: isglobal, printbiomes, printpfts

!netcdf variables
integer :: ncid
integer :: status, dimid, gppid, varid
integer, dimension(:), allocatable :: &
   mycount, mystart

!area variables
real, dimension(:), allocatable :: lonsib, latsib
real*8, dimension(:), allocatable :: areasib

!data variables
integer :: ndims
integer :: nsib, nlu, nlev, ntime
real*8 :: tconvert

integer :: mynpft, mynbiome
integer, dimension(:), allocatable :: &
   mypftnum, mybiomenum
character(len=3), dimension(:), allocatable :: &
   mypftnames
character(len=20), dimension(:), allocatable :: &
   mybiomenames

integer :: npftper
integer, dimension(:), allocatable :: locpref
integer, dimension(:,:), allocatable :: lu_pref
real, dimension(:,:), allocatable :: lu_area
real, dimension(:,:), allocatable :: data
real*8, dimension(:), allocatable :: assim
real*8, dimension(:,:), allocatable :: assimall

real*8 :: assimtot
real*8, dimension(:), allocatable :: &
    assimpft, assimbiome

!local variables
integer :: f, n, l
integer :: tref
integer :: bnum, pnum, pref
logical :: ustack_pft

!-------------------------------------------------
!process the files
DO f=1,nfiles
   !open the file
   status = nf90_open(trim(filenames(f)),nf90_nowrite,ncid)
  
   IF (f .EQ. 1) THEN
      !get dimensions
      status = nf90_inq_dimid(ncid, nsibnamed, dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nsib)
   
      status = nf90_inq_dimid(ncid, nluname, dimid)
      IF (status .NE. nf90_noerr) THEN
         nlu=1
         ustack_pft = .false.
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlu)
         ustack_pft = .true.
      ENDIF

      status = nf90_inq_dimid(ncid, 'npft', dimid)
      IF (status .NE. nf90_noerr) THEN
         nlev=1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlev)
      ENDIF

      !get time and time conversion factor
      status = nf90_inq_dimid(ncid, 'time', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=ntime)
      call calc_tconvert(ntime, tconvert)

      !print('(a,2i10)'), '  Dimensions (nsib/nlu): ', nsib, nlu
      allocate(data(nsib,nlu))
      allocate(assim(nsib),assimall(nsib,nlu))
      assim(:) = 0.
      assimall(:,:) = 0.

      IF ((nlu .EQ. 1) .and. (nlev .EQ. 1)) THEN
         ndims=2
         allocate(mycount(ndims),mystart(ndims))
         mycount(1) = nsib
         mycount(2) = 1
         mystart(:) = 1

         allocate(lu_pref(nsib,nlu))
         lu_pref(:,:) = 1

         allocate(lu_area(nsib,nlu))
         lu_area(:,:) = 1.

         mynpft = 1
         allocate(assimpft(mynpft))
         assimpft(:) = 0.

         allocate(mypftnum(mynpft))
         mypftnum = 1

         allocate(mypftnames(mynpft))
         mypftnames = 'All'

         mynbiome = 1
         allocate(assimbiome(mynbiome))
         assimbiome(:) = 0.

         allocate(mybiomenum(mynpft))
         mybiomenum = 1

         allocate(mybiomenames(mynbiome))
         mybiomenames = 'All'

      ELSEIF (nlu .eq. 1) THEN !nlev /= 1
         ndims=3
         allocate(mycount(ndims),mystart(ndims))
         mycount(1) = nsib
         mycount(2) = nlev
         mycount(3) = 1
         mystart(:) = 1

         allocate(lu_pref(nsib,nlu))
         lu_pref(:,:) = 1

         !...assuming output already scaled by lu_area!!
         !...needs work later!!
         allocate(lu_area(nsib,nlu))
         lu_area(:,:) = 1.

         mynpft = nlev
         allocate(assimpft(mynpft))
         assimpft(:) = 0.

         allocate(mypftnames(mynpft))
         IF (nlev .eq. npft) THEN
            mypftnames = pftnames
         ELSE
            print*,'Need To Read PFT Names!'
         ENDIF

         allocate(mypftnum(mynpft))
         mypftnum(:) = 0

         allocate(mybiomenum(mynpft))
         allocate(mybiomenames(mynpft))
         IF (nlev .eq. npft) THEN
            mybiomenum(:) = biomeref(:)
            mybiomenames(:) = biomenames(:)
         ELSE
            print*,'Need To Define Biomes!'
         ENDIF
         
     ELSE !nlu /= 1
         ndims=3
         allocate(mycount(ndims),mystart(ndims))
         mycount(1) = nsib
         mycount(2) = nlu
         mycount(3) = 1
         mystart(:) = 1

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

        mynpft = npft
        allocate(assimpft(mynpft))
        assimpft(:) = 0.

        allocate(mypftnames(mynpft))
        mypftnames = pftnames

        allocate(mypftnum(npftrefmax))
        mypftnum(:) = pftnum(:) 
 

        mynbiome = nbiome
        allocate(assimbiome(mynbiome))
        assimbiome(:) = 0.

        allocate(mybiomenum(mynpft))
        mybiomenum(:) = biomeref(:)

        allocate(mybiomenames(mynbiome))
        mybiomenames(:) = biomenames
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
      call calc_sibarea(nsib,lonsib,latsib, &
                        isglobal,areasib)

   ENDIF

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)

   !get the GPP
   status = nf90_inq_varid(ncid, gppname, gppid)
   IF (status .NE. nf90_noerr) THEN
       print*,''
       print*,'GPP Not Included In Output.'
       print*,'Stopping.'
       print*,''
       stop
   ENDIF

   DO tref=1, ntime
      mystart(ndims) = tref
      status = nf90_get_var(ncid, gppid, data, &
               count=mycount, start=mystart)

      assimall(:,:) = assimall(:,:) + data(:,:)
      DO l=1, nlu
         assim(:) = assim(:) + data(:,l)*lu_area(:,l)
      ENDDO
       
   ENDDO

   status = nf90_close(ncid)
ENDDO !f=1,12

!calculate total fluxes
assimtot = SUM(assim(:)*areasib(:)*tconvert)

allocate(locpref(nlu))
DO n=1,nsib
   locpref = lu_pref(n,:)
   where(lu_pref(n,:) .GE. 2) locpref=1
   npftper = SUM(locpref)

  DO l=1,nlu
     pref = lu_pref(n,l)
     IF (pref .GT. 0) THEN
        pnum = mypftnum(pref)
        assimpft(pnum) = assimpft(pnum) &
               + assimall(n,l)*lu_area(n,l) &
               * areasib(n)*tconvert
        bnum = mybiomenum(pnum)

        !..special rules for biomes 
        !..such as lat dependencies

        !.....tropical vs temperate vs boreal forests
        IF (bnum .EQ. 2) THEN
           IF (abs(latsib(n)) .LT. 12.) THEN
               bnum = 1
           ELSEIF (abs(latsib(n)) .GT. 46.) THEN
               bnum = 3
           ENDIF
        ENDIF

        !.....tropical vs temperate grasslands (C3)
        IF ((bnum .EQ. 4) .AND. &
            (abs(latsib(n)) .GT. 28.)) THEN
            bnum = 5
        ENDIF

        !.....tropical vs temperate grasslands (C3)
        IF ((bnum .EQ. 5) .AND. &
            (abs(latsib(n)) .LT. 28.)) THEN
            bnum = 4
        ENDIF

        !.....shrubs to savannahs
        IF ((bnum .EQ. 6) .AND. (npftper .GT. 4)) THEN
            IF (abs(latsib(n)) .LT. 12.) THEN
                bnum = 4
            ELSEIF (abs(latsib(n)) .GT. 28) THEN
                bnum = 5
            ENDIF
        ENDIF

        assimbiome(bnum) = assimbiome(bnum) &
                + assimall(n,l)*lu_area(n,l) &
                * areasib(n)*tconvert
     ENDIF
   ENDDO
ENDDO

!print the results
call gpptot_print(assimtot, &
     mynpft, mypftnames, assimpft, printpfts, &
     mynbiome, mybiomenames, assimbiome, &
     printbiomes)


end subroutine gpptot_sib



!=========================================
subroutine gpptot_grid(nfiles, filenames, &
           printbiomes, printpfts)
!=========================================
!Subroutine to calculate the total GPP
!  from a gridded file.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames
logical, intent(in) :: printbiomes, printpfts

!netcdf variables
integer :: ncid
integer :: status, dimid, gppid, varid
integer, dimension(:), allocatable :: &
   mycount, mystart

!area variables
integer :: nlon, nlat
real, dimension(:), allocatable :: lon, lat
real*8, dimension(:,:), allocatable :: areagrid

!data variables
integer :: ndims
integer :: nlev, nlu, ntime
real*8 :: tconvert

integer :: mynpft, mynbiome
integer, dimension(:), allocatable :: &
   mypftnum, mybiomenum
character(len=3), dimension(:), allocatable :: &
   mypftnames
character(len=20), dimension(:), allocatable :: &
   mybiomenames

integer :: npftper
integer, dimension(:), allocatable :: locpref
integer, dimension(:,:,:), allocatable :: lu_pref
real, dimension(:,:,:), allocatable :: lu_area
real, dimension(:,:,:), allocatable :: pft_area
real, dimension(:,:,:), allocatable :: data
real*8, dimension(:,:), allocatable :: assim
real*8, dimension(:,:,:), allocatable :: assimall

real*8 :: assimtot
real*8, dimension(:), allocatable :: &
    assimpft, assimbiome

!local variables
integer :: f, i, j, l
integer :: bnum, pnum, pref, tref
integer :: myelu
logical :: use_pft

!-------------------------------------------------
!process the files
DO f=1,nfiles
   !open the file
   status = nf90_open(trim(filenames(f)),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Opening File: '
      print*,'  ',trim(filenames(f))
      STOP
   ENDIF  

   IF (f .EQ. 1) THEN
      !get dimensions
      status = nf90_inq_dimid(ncid, 'lon', dimid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(ncid,'nlon', dimid)
          IF (status .ne. nf90_noerr) THEN
             print*,'Unknown Longitude Dimension Name.'
             print*,'Stopping.'
             STOP
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(ncid, dimid, len=nlon)
   
      status = nf90_inq_dimid(ncid, 'lat', dimid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(ncid,'nlat', dimid)
          IF (status .ne. nf90_noerr) THEN
             print*,'Unknown Latitude Dimension Name.'
             print*,'Stopping.'
             STOP
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(ncid, dimid, len=nlat)

      status = nf90_inq_dimid(ncid, nluname, dimid)
      IF (status .NE. nf90_noerr) THEN
         nlu=1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlu)
      ENDIF

      status = nf90_inq_dimid(ncid, 'npft', dimid)
      IF (status .NE. nf90_noerr) THEN
         nlev = 1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlev)
      ENDIF
      print('(a,3i6)'), '  Dimensions (nlat/nlon/nlu/nlev): ', &
           nlat,nlon,nlu,nlev

      !get time and time conversion factor
      status = nf90_inq_dimid(ncid, 'time', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=ntime)
      call calc_tconvert(ntime, tconvert)

      !get latitude and longitude
      allocate(lon(nlon))
      status = nf90_inq_varid(ncid, 'lon', varid)
      IF (status .ne. nf90_noerr) THEN
         status = nf90_inq_varid(ncid,'longitude',varid)
         IF (status .ne. nf90_noerr) THEN
            print*,'Unknown Longitude Name.'
            print*,'Stopping.'
            STOP
         ENDIF
      ENDIF
      status = nf90_get_var(ncid, varid, lon)

      allocate(lat(nlat))
      status = nf90_inq_varid(ncid, 'lat', varid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_varid(ncid,'latitude',varid)
          IF (status .ne. nf90_noerr) THEN
              print*,'Unknown Latitude Name.'
              print*,'Stoping.'
              STOP
          ENDIF
      ENDIF
      status = nf90_get_var(ncid, varid, lat)

      !setup arrays to get data
      IF ((nlu .EQ. 1) .and. (nlev .EQ. 1)) THEN !no PFTs
         ndims=3
         use_pft=.false.
         allocate(mycount(ndims),mystart(ndims))
         allocate(data(nlon,nlat,nlu))
         allocate(assim(nlon,nlat),assimall(nlon,nlat,nlev))
         assim(:,:) = 0.
         assimall(:,:,:) = 0.
         
         mycount(1) = nlon
         mycount(2) = nlat
         mycount(3) = 1
         mystart(:) = 1

         allocate(lu_pref(nlon,nlat,nlu))
         lu_pref = 1

         allocate(lu_area(nlon,nlat,nlu))
         lu_area = 1.

         mynpft = 1
         allocate(assimpft(mynpft))
         assimpft(:) = 0.

         allocate(mypftnum(mynpft))
         mypftnum = 1

         allocate(mypftnames(mynpft))
         mypftnames = 'All'

         mynbiome = 1
         allocate(assimbiome(mynbiome))
         assimbiome(:) = 0.

         allocate(mybiomenum(mynpft))
         mybiomenum = 1

         allocate(mybiomenames(mynbiome))
         mybiomenames = 'All'

      ELSEIF (nlu .EQ. 1) THEN !unstacked PFTs
         ndims=4
         use_pft=.true.
         allocate(data(nlon,nlat,nlev))
         allocate(assim(nlon,nlat),assimall(nlon,nlat,nlev))
         assim(:,:) = 0.
         assimall(:,:,:) = 0.
         
         mynpft = nlev
         allocate(mycount(ndims),mystart(ndims))
         mycount(1) = nlon
         mycount(2) = nlat
         mycount(3) = nlev
         mycount(4) = 1
         mystart(:) = 1

         allocate(lu_pref(nlon,nlat,nlev))
         DO l=1,nlev
            lu_pref(:,:,l) = l
         ENDDO

         allocate(pft_area(nlon,nlat,nlev))
         status = nf90_inq_varid(ncid, 'pft_area', varid)
         status = nf90_get_var(ncid, varid, pft_area)
         IF (status .NE. nf90_noerr) THEN
            print*,'Error getting PFT areal contributions.'
            print*,'Stopping.'
            STOP
         ENDIF
         pft_area = 1.
         print*,'Setting pft_area to 1 right now!'
         print*,'Need to add link to switch'
         
         mynpft = npft
         allocate(assimpft(mynpft))
         assimpft(:) = 0.

         allocate(mypftnames(mynpft))
         IF (mynpft .EQ. npft) THEN
             mypftnames = pftnames
         ELSE
            print*,'Need To Get PFT Names!'
         ENDIF
             
        mynbiome = nbiome
        allocate(assimbiome(mynbiome))
        assimbiome(:) = 0.

        allocate(mybiomenum(mynpft))
        allocate(mybiomenames(nbiome))
        IF (mynpft .EQ. npft) THEN
           mybiomenum(:) = biomeref(:)
        ELSE
           print*,'Need to Specify Biome Refs and Names'
        ENDIF
        mybiomenames = biomenames
        
     ELSE  !stacked PFTs
         ndims=4
         use_pft=.false.
         allocate(data(nlon,nlat,nlu))
         allocate(assim(nlon,nlat),assimall(nlon,nlat,nlu))
         assim(:,:) = 0.
         assimall(:,:,:) = 0.
         
         allocate(mycount(ndims),mystart(ndims))
         mycount(1) = nlon
         mycount(2) = nlat
         mycount(3) = nlu
         mycount(4) = 1
         mystart(:) = 1

        !get PFT refs and area
        allocate(lu_pref(nlon,nlat,nlu))
        status = nf90_inq_varid(ncid, trim(lprefname), varid)
        status = nf90_get_var(ncid, varid, lu_pref) 
        IF (status .NE. nf90_noerr) THEN
           print*,'Error getting PFT references.'
           print*,'Stoping.'
           STOP
        ENDIF

        allocate(lu_area(nlon,nlat,nlu))
        status = nf90_inq_varid(ncid, trim(lareaname), varid)
        status = nf90_get_var(ncid, varid, lu_area)
        IF (status .NE. nf90_noerr) THEN
            print*,'Error getting PFT areal contributions.'
            print*,'Stopping.'
            STOP
        ENDIF

        mynpft = npft
        allocate(assimpft(mynpft))
        assimpft(:) = 0.

        allocate(mypftnames(mynpft))
        mypftnames = pftnames

        allocate(mypftnum(npftrefmax))
        mypftnum(:) = pftnum(:) 
 

        mynbiome = nbiome
        allocate(assimbiome(mynbiome))
        assimbiome(:) = 0.

        allocate(mybiomenum(mynpft))
        mybiomenum(:) = biomeref(:)

        allocate(mybiomenames(mynbiome))
        mybiomenames(:) = biomenames
      ENDIF

      allocate(areagrid(nlon,nlat))
      call calc_gridarea(nlon, nlat, &
           lon, lat, areagrid)

   ENDIF

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)

   !get the GPP
   status = nf90_inq_varid(ncid, 'assim', gppid)
   IF (status .ne. nf90_noerr) THEN
      status = nf90_inq_varid(ncid,'gpp',gppid)
      IF (status .ne. nf90_noerr) THEN
          print*,'Missing GPP Or Unknown GPP Name.'
          print*,'Stopping.'
          STOP
      ENDIF
   ENDIF

   DO tref=1, ntime
      mystart(ndims) = tref
      status = nf90_get_var(ncid, gppid, data, &
               count=mycount, start=mystart)

      assimall(:,:,:) = assimall(:,:,:) + data(:,:,:)
      IF (use_pft) THEN
         DO l=1, npft
            assim(:,:) = assim(:,:) + data(:,:,l)*pft_area(:,:,l)
         ENDDO
      ELSE
         DO l=1, nlu
            assim(:,:) = assim(:,:) + data(:,:,l)*lu_area(:,:,l)
         ENDDO
      ENDIF
   ENDDO

   status = nf90_close(ncid)
ENDDO !f=1,nfiles

!calculate total fluxes
assimtot = SUM(assim*areagrid*tconvert)

IF (use_pft) THEN
   myelu = mynpft
ELSE
   myelu = nlu
ENDIF

allocate(locpref(nlu))
DO i=1,nlon
   DO j=1,nlat
       locpref = lu_pref(i,j,:)
       where(lu_pref(i,j,:) .GE. 2) locpref=1
       npftper = SUM(locpref)

       DO l=1,myelu
          IF (use_pft) THEN
             pref = l
          ELSE
             pref = lu_pref(i,j,l)
          ENDIF
          IF (pref .GT. 0) THEN
             IF (use_pft) THEN
                pnum = l
                assimpft(pnum) = assimpft(pnum) &
                     + assimall(i,j,l)*pft_area(i,j,l) &
                     * areagrid(i,j)*tconvert
             ELSE
                pnum = mypftnum(pref)
                assimpft(pnum) = assimpft(pnum) &
                       + assimall(i,j,l)*lu_area(i,j,l) &
                       * areagrid(i,j)*tconvert
             ENDIF
             
             bnum = mybiomenum(pnum)
 
             !..special rules for biomes 
             !..such as lat dependencies

             !.....tropical vs temperate vs boreal forests
             IF (bnum .EQ. 2) THEN
                IF (abs(lat(j)) .LT. 12.) THEN
                   bnum = 1
                ELSEIF (abs(lat(j)) .GT. 46.) THEN
                    bnum = 3
                ENDIF
             ENDIF

             !.....tropical vs temperate grasslands (C3)
             IF ((bnum .EQ. 4) .AND. &
                 (abs(lat(j)) .GT. 28.)) THEN
                  bnum = 5
             ENDIF

             !.....tropical vs temperate grasslands (C3)
             IF ((bnum .EQ. 5) .AND. &
                 (abs(lat(j)) .LT. 28.)) THEN
                  bnum = 4
             ENDIF

             !.....shrubs to savannahs
             IF ((bnum .EQ. 6) .AND. (npftper .GT. 4)) THEN
                 IF (abs(lat(j)) .LT. 12.) THEN
                     bnum = 4
                 ELSEIF (abs(lat(j)) .GT. 28) THEN
                     bnum = 5
                 ENDIF
             ENDIF

             IF (use_pft) THEN
                assimbiome(bnum) = assimbiome(bnum) &
                     + assimall(i,j,l)*pft_area(i,j,l) &
                     * areagrid(i,j)*tconvert
             ELSE
                assimbiome(bnum) = assimbiome(bnum) &
                    + assimall(i,j,l)*lu_area(i,j,l) &
                    * areagrid(i,j)*tconvert
             ENDIF
         ENDIF !PFTs defined
      ENDDO !l=1,nlu
   ENDDO !j=1,nlat
ENDDO !i=1,nlon

!print the results
call gpptot_print(assimtot, &
     mynpft, mypftnames, assimpft, printpfts, &
     mynbiome, mybiomenames, assimbiome, &
     printbiomes)


end subroutine gpptot_grid



!=========================================
subroutine gpptot_print(gpptot, &
    npft, pftnames, gpppft, printpfts, &
    nbiome, biomenames, gppbiome, printbiomes)
!=========================================
!Subroutine to print the GPP.

!kdhaynes, 10/17
implicit none

!input variables
real*8, intent(in) :: gpptot
integer, intent(in) :: npft, nbiome
character(len=3), dimension(npft), intent(in) :: &
    pftnames
character(len=20), dimension(nbiome), intent(in) :: &
    biomenames
real*8, dimension(npft), intent(in) :: gpppft
real*8, dimension(nbiome), intent(in) :: gppbiome
logical, intent(in) :: printpfts, printbiomes

!parameters
real*8, parameter :: &
   aconvert = 12./1.E21  !convert umol C to GtC
character(len=30), parameter :: &
   af1 = '(a,f12.6)'

!local variables
integer :: i


print(af1),      '    Total GPP (Gt C): ',gpptot*aconvert
print*,''
IF (printbiomes) THEN
    print('(a,a,a,a)'), &
                 '    ','Biome','                ','GPP'
    DO i=1,nbiome
        print('(a,a16,f12.4)'), &
                 '    ',biomenames(i),gppbiome(i)*aconvert
    ENDDO
    print('(a,a,a,f12.4)'), &
                 '    ','Total','           ',SUM(gppbiome)*aconvert
    print('(a)'),''
ENDIF

IF (printpfts) THEN
    print('(a)'),'    PFT        GPP'
    DO i=1,npft
       print('(a,a,f12.4)'), &
                 '    ',pftnames(i),gpppft(i)*aconvert
    ENDDO
    print('(a,a,f12.4)'), &
                 '    ','TOT',SUM(gpppft)*aconvert
    print('(a)'),''
ENDIF

end subroutine gpptot_print
