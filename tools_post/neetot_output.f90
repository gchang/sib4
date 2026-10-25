!=========================================
subroutine neetot_output(nfiles, filenames, &
           isglobal, printbiomes, printpfts)
!=========================================
!Subroutine controlling the sequence of 
!   calls to calculate global annual NEE.
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
   call neetot_sib(nfiles, filenames, &
        isglobal, printbiomes, printpfts)
ELSE
   status = nf90_inq_dimid(ncid, 'lon', dimid)
   IF (status .EQ. nf90_noerr) THEN
      call neetot_grid(nfiles, filenames, &
           printbiomes, printpfts)
   ELSE
      print*,'Unknown Type Of File: '
      print*,'  Missing vector dimension: ',trim(nsibnamed)
      print*,'  Missing lon dimension: ','lon'
      print*,'Stopping.'
      STOP
   ENDIF
ENDIF

end subroutine neetot_output


!=========================================
subroutine neetot_sib(nfiles, filenames, &
           isglobal, printbiomes, printpfts)
!=========================================
!Subroutine to calculate total NEE
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
integer :: status, dimid, gppid, respid, varid
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
real, dimension(:,:), allocatable :: dataa, datar
real*8, dimension(:), allocatable :: assim, resp, nee
real*8, dimension(:,:), allocatable :: assimall, respall, neeall

real*8 :: neetot
real*8, dimension(:), allocatable :: &
    neepft, neebiome

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
      allocate(dataa(nsib,nlu), datar(nsib,nlu))
      allocate(assim(nsib),assimall(nsib,nlu))
      allocate(resp(nsib),respall(nsib,nlu))
      allocate(nee(nsib),neeall(nsib,nlu))

      assim(:) = 0.
      assimall(:,:) = 0.
      resp(:) = 0.
      respall(:,:) = 0.
      nee(:) = 0.
      neeall(:,:) = 0.

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
         allocate(neepft(mynpft))
         neepft(:) = 0.

         allocate(mypftnum(mynpft))
         mypftnum = 1

         allocate(mypftnames(mynpft))
         mypftnames = 'All'

         mynbiome = 1
         allocate(neebiome(mynbiome))
         neebiome(:) = 0.

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
         allocate(neepft(mynpft))
         neepft(:) = 0.

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
        allocate(neepft(mynpft))
        neepft(:) = 0.

        allocate(mypftnames(mynpft))
        mypftnames = pftnames

        allocate(mypftnum(npftrefmax))
        mypftnum(:) = pftnum(:) 
 

        mynbiome = nbiome
        allocate(neebiome(mynbiome))
        neebiome(:) = 0.

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

   !get the RE
   status = nf90_inq_varid(ncid, respbname, respid)
   IF (status .NE. nf90_noerr) THEN
      print*,''
      print*,'RESP Not Included In Output.'
      print*,'Stopping.'
      print*,''
      stop
    ENDIF

   DO tref=1, ntime
      mystart(ndims) = tref
      status = nf90_get_var(ncid, gppid, dataa, &
               count=mycount, start=mystart)
      status = nf90_get_var(ncid, respid, datar, &
               count=mycount, start=mystart)

      assimall(:,:) = assimall(:,:) + dataa(:,:)
      respall(:,:) = respall(:,:) + datar(:,:)

      DO l=1, nlu
         assim(:) = assim(:) + dataa(:,l)*lu_area(:,l)
         resp(:) = resp(:) + datar(:,l)*lu_area(:,l)
      ENDDO
   ENDDO

   status = nf90_close(ncid)
ENDDO !f=1,12

!calculate total fluxes
neeall(:,:) = respall(:,:) - assimall(:,:)
nee(:) = resp(:) - assim(:)
neetot = SUM(nee(:)*areasib(:)*tconvert)

allocate(locpref(nlu))
DO n=1,nsib
   locpref = lu_pref(n,:)
   where(lu_pref(n,:) .GE. 2) locpref=1
   npftper = SUM(locpref)

  DO l=1,nlu
     pref = lu_pref(n,l)
     IF (pref .GT. 0) THEN
        pnum = mypftnum(pref)
        neepft(pnum) = neepft(pnum) &
               + neeall(n,l)*lu_area(n,l) &
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

        neebiome(bnum) = neebiome(bnum) &
                + neeall(n,l)*lu_area(n,l) &
                * areasib(n)*tconvert
     ENDIF
   ENDDO
ENDDO

!print the results
call neetot_print(neetot, &
     mynpft, mypftnames, neepft, printpfts, &
     mynbiome, mybiomenames, neebiome, &
     printbiomes)


end subroutine neetot_sib



!=========================================
subroutine neetot_grid(nfiles, filenames, &
           printbiomes, printpfts)
!=========================================
!Subroutine to calculate the total NEE
!  from a gridded file.
!
!kdhaynes, 2018/11

use netcdf
use sibinfo_module
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames
logical, intent(in) :: printbiomes, printpfts

!netcdf variables
integer :: ncid
integer :: status, dimid, gppid, respid, varid
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
real, dimension(:,:,:), allocatable :: dataa, datar
real*8, dimension(:,:), allocatable :: assim, resp, nee
real*8, dimension(:,:,:), allocatable :: assimall, respall, neeall

real*8 :: neetot
real*8, dimension(:), allocatable :: &
    neepft, neebiome

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
         allocate(dataa(nlon,nlat,nlu),datar(nlon,nlat,nlu))
         allocate(assim(nlon,nlat),assimall(nlon,nlat,nlev))
         allocate(resp(nlon,nlat),respall(nlon,nlat,nlev))
         allocate(nee(nlon,nlat),neeall(nlon,nlat,nlev))
         assim(:,:) = 0.
         assimall(:,:,:) = 0.
         resp(:,:) = 0.
         respall(:,:,:) = 0.
         nee(:,:) = 0.
         neeall(:,:,:) = 0.         

         mycount(1) = nlon
         mycount(2) = nlat
         mycount(3) = 1
         mystart(:) = 1

         allocate(lu_pref(nlon,nlat,nlu))
         lu_pref = 1

         allocate(lu_area(nlon,nlat,nlu))
         lu_area = 1.

         mynpft = 1
         allocate(neepft(mynpft))
         neepft(:) = 0.

         allocate(mypftnum(mynpft))
         mypftnum = 1

         allocate(mypftnames(mynpft))
         mypftnames = 'All'

         mynbiome = 1
         allocate(neebiome(mynbiome))
         neebiome(:) = 0.

         allocate(mybiomenum(mynpft))
         mybiomenum = 1

         allocate(mybiomenames(mynbiome))
         mybiomenames = 'All'

      ELSEIF (nlu .EQ. 1) THEN !unstacked PFTs
         ndims=4
         use_pft=.true.
         allocate(dataa(nlon,nlat,nlev),datar(nlon,nlat,nlev))
         allocate(assim(nlon,nlat),assimall(nlon,nlat,nlev))
         allocate(resp(nlon,nlat),respall(nlon,nlat,nlev))
         allocate(nee(nlon,nlat),neeall(nlon,nlat,nlev))
         assim(:,:) = 0.
         assimall(:,:,:) = 0.
         resp(:,:) = 0.
         respall(:,:,:) = 0.
         nee(:,:) = 0.
         neeall(:,:,:) = 0.

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
         allocate(neepft(mynpft))
         neepft(:) = 0.

         allocate(mypftnames(mynpft))
         IF (mynpft .EQ. npft) THEN
             mypftnames = pftnames
         ELSE
            print*,'Need To Get PFT Names!'
         ENDIF
             
        mynbiome = nbiome
        allocate(neebiome(mynbiome))
        neebiome(:) = 0.

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
         allocate(dataa(nlon,nlat,nlu),datar(nlon,nlat,nlu))
         allocate(assim(nlon,nlat),assimall(nlon,nlat,nlu))
         allocate(resp(nlon,nlat),respall(nlon,nlat,nlu))
         allocate(nee(nlon,nlat),neeall(nlon,nlat,nlu))
         assim(:,:) = 0.
         assimall(:,:,:) = 0.
         resp(:,:) = 0.
         respall(:,:,:) = 0.
         nee(:,:) = 0.
         neeall(:,:,:) = 0.
         
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
        allocate(neepft(mynpft))
        neepft(:) = 0.

        allocate(mypftnames(mynpft))
        mypftnames = pftnames

        allocate(mypftnum(npftrefmax))
        mypftnum(:) = pftnum(:) 
 
        mynbiome = nbiome
        allocate(neebiome(mynbiome))
        neebiome(:) = 0.

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

   !get the RESP
   status = nf90_inq_varid(ncid,'resp_bal', respid)
   IF (status .ne. nf90_noerr) THEN
      status = nf90_inq_varid(ncid,'resp_tot',respid)
      IF (status .ne. nf90_noerr) THEN
          print*,'Missing RESP or Unknown RESP Name.'
          print*,'Stopping.'
          STOP
      ENDIF
   ENDIF

   DO tref=1, ntime
      mystart(ndims) = tref
      status = nf90_get_var(ncid, gppid, dataa, &
               count=mycount, start=mystart)
      status = nf90_get_var(ncid, respid, datar, &
               count=mycount, start=mystart)

      assimall(:,:,:) = assimall(:,:,:) + dataa(:,:,:)
      respall(:,:,:) = respall(:,:,:) + datar(:,:,:)
      IF (use_pft) THEN
         DO l=1, npft
            assim(:,:) = assim(:,:) + dataa(:,:,l)*pft_area(:,:,l)
            resp(:,:) = resp(:,:) + datar(:,:,l)*pft_area(:,:,l)
         ENDDO
      ELSE
         DO l=1, nlu
            assim(:,:) = assim(:,:) + dataa(:,:,l)*lu_area(:,:,l)
            resp(:,:) = resp(:,:) + datar(:,:,l)*lu_area(:,:,l)
         ENDDO
      ENDIF
   ENDDO

   status = nf90_close(ncid)
ENDDO !f=1,nfiles

!calculate total fluxes
nee(:,:) = resp(:,:) - assim(:,:)
neeall(:,:,:) = respall(:,:,:) - assimall(:,:,:)
neetot = SUM(nee*areagrid*tconvert)

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
                neepft(pnum) = neepft(pnum) &
                     + neeall(i,j,l)*pft_area(i,j,l) &
                     * areagrid(i,j)*tconvert
             ELSE
                pnum = mypftnum(pref)
                neepft(pnum) = neepft(pnum) &
                       + neeall(i,j,l)*lu_area(i,j,l) &
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
                neebiome(bnum) = neebiome(bnum) &
                     + neeall(i,j,l)*pft_area(i,j,l) &
                     * areagrid(i,j)*tconvert
             ELSE
                neebiome(bnum) = neebiome(bnum) &
                    + neeall(i,j,l)*lu_area(i,j,l) &
                    * areagrid(i,j)*tconvert
             ENDIF
         ENDIF !PFTs defined
      ENDDO !l=1,nlu
   ENDDO !j=1,nlat
ENDDO !i=1,nlon

!print the results
call neetot_print(neetot, &
     mynpft, mypftnames, neepft, printpfts, &
     mynbiome, mybiomenames, neebiome, &
     printbiomes)


end subroutine neetot_grid



!=========================================
subroutine neetot_print(neetot, &
    npft, pftnames, neepft, printpfts, &
    nbiome, biomenames, neebiome, printbiomes)
!=========================================
!Subroutine to print the NEE.

!kdhaynes, 10/17
implicit none

!input variables
real*8, intent(in) :: neetot
integer, intent(in) :: npft, nbiome
character(len=3), dimension(npft), intent(in) :: &
    pftnames
character(len=20), dimension(nbiome), intent(in) :: &
    biomenames
real*8, dimension(npft), intent(in) :: neepft
real*8, dimension(nbiome), intent(in) :: neebiome
logical, intent(in) :: printpfts, printbiomes

!parameters
real*8, parameter :: &
   aconvert = 12./1.E21  !convert umol C to GtC
character(len=30), parameter :: &
   af1 = '(a,f12.6)'

!local variables
integer :: i


print(af1),      '    Total NEE (Gt C): ',neetot*aconvert
print*,''
IF (printbiomes) THEN
    print('(a,a,a,a)'), &
                 '    ','Biome','                ','NEE'
    DO i=1,nbiome
        print('(a,a16,f12.4)'), &
                 '    ',biomenames(i),neebiome(i)*aconvert
    ENDDO
    print('(a,a,a,f12.4)'), &
                 '    ','Total','           ',SUM(neebiome)*aconvert
    print('(a)'),''
ENDIF

IF (printpfts) THEN
    print('(a)'),'    PFT        NEE'
    DO i=1,npft
       print('(a,a,f12.4)'), &
                 '    ',pftnames(i),neepft(i)*aconvert
    ENDDO
    print('(a,a,f12.4)'), &
                 '    ','TOT',SUM(neepft)*aconvert
    print('(a)'),''
ENDIF

end subroutine neetot_print
