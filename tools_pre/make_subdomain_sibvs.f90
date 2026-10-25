!Program to pull out a subdomain vegetation structure (sib_vs file)
!   from global sib pft/vegetation file.
!
!Compile with:
!>gfortran make_subdomain_sibvs.f90
!   -L/usr/local/netcdf3-gcc/lib -lnetcdf
!   -I/usr/local/netcdf3-gcc/include
!
!kdhaynes, 09/17
program make_subdomain_sibvs

use netcdf
implicit none

character(len=100), parameter :: &  !specified options
   specfile='make_subdomain.txt'

!Parameters
integer, parameter :: clen=3, slen=6
integer, parameter :: c3gref=14
integer, parameter :: c4gref=15
character(len=10), parameter :: ncsuffix='vs.nc'
character(len=10), parameter :: txtsuffix='vs.txt'
character(len=2), parameter :: blank2='  '
character(len=6), parameter :: blank='      '

!Specific Options
integer :: subtype
character(len=120) :: vsfile, restartfile
character(len=120) :: driverdir
character(len=120) :: outdir
character(len=120) :: trash
integer :: mypft
integer :: startyr, stopyr
integer :: outtype
real :: startlon, stoplon
real :: startlat, stoplat
integer :: numpts
real, dimension(:), allocatable :: lonpts, latpts
character(len=slen), dimension(:), allocatable :: namepts

!SiB input variables
integer(kind=1) :: nloc
integer :: nsibin,npft,nlu,clenin
character(len=clen), dimension(:), allocatable :: pftnames
integer(kind=1), dimension(:), allocatable :: pftrefs
real, dimension(:), allocatable :: lonsibin,latsibin
integer(kind=1), dimension(:), allocatable :: gnlu
real, dimension(:,:), allocatable :: lgarea
integer(kind=1), dimension(:,:), allocatable :: ppftref
real, dimension(:,:), allocatable :: sand, clay, vis, nir
character(len=120) :: infile

!SiB output variables
integer :: nsibout, srefloc
integer, dimension(:), allocatable :: sref
real, dimension(:), allocatable :: lonsibout,latsibout
integer(kind=1), dimension(:), allocatable :: gnluout
real, dimension(:,:), allocatable :: lgareaout
integer(kind=1), dimension(:,:), allocatable :: ppftrefout
real, dimension(:,:), allocatable :: sandout, clayout, visout, nirout
character(len=120) :: outfile

!Netcdf variables
integer :: ncid,dimid,varid,status
integer :: nsibid, npftid, nluid, clenid, slenid
integer :: pnameid, prefid, snameid
integer :: lonsibid, latsibid
integer :: gnluid, lgareaid, ppftrefid
integer :: sandid, clayid, visid, nirid

!Local variables
integer :: i,j,pt
integer :: strlen
character(len=3) :: suffix
logical :: file_exist

real :: diff, mindiff
real, dimension(:), allocatable :: lattemp,lontemp

integer :: c3ref, c4ref, myref
real :: c3area, c4area, carea

!-------------------------
!Read in user specifications
open(unit=17,file=trim(specfile),form='formatted')
read(17,*) subtype
read(17,*) trash
read(17,*) vsfile
read(17,*) trash
read(17,*) restartfile
read(17,*) trash
read(17,*) driverdir
read(17,*) trash
read(17,*) outdir
read(17,*) trash

read(17,*) mypft
read(17,*) startyr
read(17,*) stopyr
read(17,*) outtype
read(17,*) trash

read(17,*) startlat
read(17,*) stoplat
read(17,*) startlon
read(17,*) stoplon
read(17,*) trash

if (subtype .eq. 1) then
   read(17,*) numpts
   allocate(lonpts(numpts),latpts(numpts))
   allocate(namepts(numpts))
   do i=1,numpts
      read(17,*) latpts(i), lonpts(i), namepts(i)
   enddo
endif
close(17)

!------------------------
!Check if using global file or site files
strlen=len_trim(vsfile)
suffix=vsfile(strlen-2:strlen)
if (suffix .eq. '.nc') then
   file_exist = .true.
else
   file_exist = .false.
endif

!Read in global file
if (file_exist) then
   status = nf90_open(trim(vsfile),nf90_nowrite,ncid)
   if (status .ne. nf90_noerr) then
      print*,'Error Opening VS File: '
      print*,'  ',trim(vsfile)
      print*,'Stopping'
      STOP
   else  
      print*,''
      print*,'Opening file: '
      print*,'  ',trim(vsfile)
   endif
      
   call check ( nf90_inq_dimid( ncid, 'nsib', dimid ) )
   call check ( nf90_inquire_dimension( ncid, dimid, len=nsibin ) )
   call check ( nf90_inq_dimid( ncid, 'npft', dimid ) )
   call check ( nf90_inquire_dimension( ncid, dimid, len=npft ) )
   call check ( nf90_inq_dimid( ncid, 'nlu', dimid ) )
   call check ( nf90_inquire_dimension( ncid, dimid, len=nlu ) )
   call check ( nf90_inq_dimid( ncid, 'clen', dimid ) )
   call check ( nf90_inquire_dimension( ncid, dimid, len=clenin ) )
   if (clenin .ne. clen) then
      print*,'Mistmatching PFT Name Length.'
      print*,'File/Program: ',clenin,clen
      print*,'Stopping.'
      stop
   endif
   print('(a,3i8)'),'   Input File Dims (nsib/npft/nlu): ',nsibin,npft,nlu

   allocate(pftnames(npft),pftrefs(npft))
   allocate(lonsibin(nsibin),latsibin(nsibin))
   allocate(gnlu(nsibin))
   allocate(lgarea(nsibin,nlu))
   allocate(ppftref(nsibin,nlu))
   allocate(sand(nsibin,nlu),clay(nsibin,nlu))
   allocate(vis(nsibin,nlu),nir(nsibin,nlu))

   call check ( nf90_inq_varid( ncid, 'lonsib', varid ) )
   call check ( nf90_get_var( ncid, varid, lonsibin ) )
   call check ( nf90_inq_varid( ncid, 'latsib', varid ) )
   call check ( nf90_get_var( ncid, varid, latsibin ) )
   call check ( nf90_inq_varid( ncid, 'pft_names',varid ) )
   call check ( nf90_get_var( ncid, varid, pftnames ) )
   call check ( nf90_inq_varid( ncid, 'pft_refs',varid ) )
   call check ( nf90_get_var( ncid, varid, pftrefs ) )
   call check ( nf90_inq_varid( ncid, 'g_nlu', varid ) )
   call check ( nf90_get_var( ncid, varid, gnlu ) )
   call check ( nf90_inq_varid( ncid, 'larea', varid ) )
   call check ( nf90_get_var( ncid, varid, lgarea ) )
   call check ( nf90_inq_varid( ncid, 'pftref', varid ) )
   call check ( nf90_get_var( ncid, varid, ppftref ) ) 
   call check ( nf90_inq_varid( ncid, 'sand_frac', varid ) )
   call check ( nf90_get_var( ncid, varid, sand ) )
   call check ( nf90_inq_varid( ncid, 'clay_frac', varid ) )
   call check ( nf90_get_var( ncid, varid, clay ) )
   call check ( nf90_inq_varid( ncid, 'soref_vis', varid ) )
   call check ( nf90_get_var( ncid, varid, vis ) )
   call check ( nf90_inq_varid( ncid, 'soref_nir', varid ) )
   call check ( nf90_get_var( ncid, varid, nir ) )
   call check ( nf90_close( ncid ) )
   print*,'Closed Input File.'
   
   !Find the gridcells included in the specified subdomain
   if (subtype .eq. 1) then  !site or group
      nsibout=numpts
      allocate(sref(nsibout))

      DO pt=1,nsibout
         mindiff = 999.
         DO i=1,nsibin
             diff = abs(lonsibin(i)-lonpts(pt))+abs(latsibin(i)-latpts(pt))
             IF (diff .lt. mindiff) THEN
                mindiff = diff
                sref(pt) = i
             ENDIF
         ENDDO
       ENDDO

   else !region
      nsibout=0
      allocate(sref(nsibin))
      DO i=1,nsibin
         IF ((lonsibin(i) .GE. startlon) .AND. & 
             (lonsibin(i) .LE. stoplon)  .AND. &
             (latsibin(i) .GE. startlat) .AND. &
             (latsibin(i) .LE. stoplat)) THEN
             nsibout = nsibout + 1
             sref(nsibout) = i
         ENDIF
      ENDDO
   endif

   !Set the new values
   allocate(lonsibout(nsibout),latsibout(nsibout))
   allocate(gnluout(nsibout))
   allocate(lgareaout(nsibout,nlu))
   allocate(ppftrefout(nsibout,nlu))
   allocate(sandout(nsibout,nlu),clayout(nsibout,nlu))
   allocate(visout(nsibout,nlu),nirout(nsibout,nlu))

   DO i=1,nsibout
      srefloc=sref(i)
      if (subtype .eq. 1) then
         lonsibout(i) = lonpts(i)
         latsibout(i) = latpts(i)
      else
         lonsibout(i) = lonsibin(srefloc)
         latsibout(i) = latsibin(srefloc)
      endif

      if (mypft .eq. -1) then
         nloc = gnlu(srefloc)
         gnluout(i) = nloc
         lgareaout(i,:) = lgarea(srefloc,:)
         ppftrefout(i,:) = ppftref(srefloc,:)

         sandout(i,1:nloc) = sand(srefloc,1:nloc)
         clayout(i,1:nloc) = clay(srefloc,1:nloc)
         visout(i,1:nloc) = vis(srefloc,1:nloc)
         nirout(i,1:nloc) = nir(srefloc,1:nloc)
      elseif (mypft .eq. 101) then
         lgareaout(i,:) = 0.
         ppftrefout(i,:) = 0.
         sandout(i,:) = 0.
         clayout(i,:) = 0.
         visout(i,:) = 0.
         nirout(i,:) = 0.

         c3ref=0
         c3area=0.0
         c4ref=0
         c4area=0.0
         do j=1,gnlu(srefloc)
            if (ppftref(srefloc,j) .eq. c3gref) then
               c3ref=j
               c3area=lgarea(srefloc,j)
            elseif (ppftref(srefloc,j) .eq. c4gref) then
               c4ref=j
               c4area=lgarea(srefloc,j)
            endif
         enddo
         c3ref = MAX(1, c3ref)
         c4ref = MAX(1, c4ref)
         c3area = MAX(0.0,c3area)
         c4area = MAX(0.0,c4area)
         carea = c3area + c4area
         if (c3area .eq. 0.) then
            gnluout(i) = 1
            lgareaout(i,1) = 1.0
            ppftrefout(i,1) = c4gref
            sandout(i,1) = sand(srefloc,c4ref)
            clayout(i,1) = clay(srefloc,c4ref)
            visout(i,1) = vis(srefloc,c4ref)
            nirout(i,1) = nir(srefloc,c4ref)
         elseif (c4area .eq. 0.) then
            gnluout(i) = 1
            lgareaout(i,1) = 1.0
            ppftrefout(i,1) = c3gref
            sandout(i,1) = sand(srefloc,c3ref)
            clayout(i,1) = clay(srefloc,c3ref)
            visout(i,1) = vis(srefloc,c3ref)
            nirout(i,1) = nir(srefloc,c3ref)
         elseif (c3area .gt. c4area) then
            gnluout(i) = 2
            lgareaout(i,1) = c3area/carea
            lgareaout(i,2) = c4area/carea
            ppftrefout(i,1) = c3gref
            ppftrefout(i,2) = c4gref

            sandout(i,1) = sand(srefloc,c3ref)
            clayout(i,1) = clay(srefloc,c3ref)
            visout(i,1) = vis(srefloc,c3ref)
            nirout(i,1) = nir(srefloc,c3ref)
            sandout(i,2) = sand(srefloc,c4ref)
            clayout(i,2) = clay(srefloc,c4ref)
            visout(i,2) = vis(srefloc,c4ref)
            nirout(i,2) = nir(srefloc,c4ref)
         else
            gnluout(i) = 2
            lgareaout(i,1) = c4area/carea
            lgareaout(i,2) = c3area/carea
            ppftrefout(i,1) = c4gref
            ppftrefout(i,2) = c3gref

            sandout(i,1) = sand(srefloc,c4ref)
            clayout(i,1) = clay(srefloc,c4ref)
            visout(i,1) = vis(srefloc,c4ref)
            nirout(i,1) = nir(srefloc,c4ref)
            sandout(i,2) = sand(srefloc,c3ref)
            clayout(i,2) = clay(srefloc,c3ref)
            visout(i,2) = vis(srefloc,c3ref)
            nirout(i,2) = nir(srefloc,c3ref)
         endif

      else
         lgareaout(i,:) = 0.
         ppftrefout(i,:) = 0.
         sandout(i,:) = 0.
         clayout(i,:) = 0.
         visout(i,:) = 0.
         nirout(i,:) = 0.

         myref=1
         do j=1,gnlu(srefloc)
            if (ppftref(srefloc,j) .eq. mypft) then
               myref=j
            endif
         enddo
         gnluout(i) = 1
         lgareaout(i,1) = 1.0
         ppftrefout(i,1) = mypft
         sandout(i,1) = sand(srefloc,myref)
         clayout(i,1) = clay(srefloc,myref)
         visout(i,1) = vis(srefloc,myref)
         nirout(i,1) = nir(srefloc,myref)
      endif

   ENDDO  !nsibout
ELSE  !Site VS Files

   print*,''
   print*,'Using Site VS Files.'
   if (numpts .le. 0) then
      print*,'Expecting List of Points.'
      print*,'Stopping.'
      print*,''
      STOP
   else
      nsibout = numpts
   endif

   DO i=1,nsibout
      infile = trim(vsfile) // trim(namepts(i)) // &
               '/sib_vs.txt'
      inquire(file=infile,exist=file_exist)
      if (.not. file_exist) then
         print*,'Missing Input File: '
         print*,'  ',trim(infile)
         print*,'Stopping.'
         print*,''
         STOP
      else
         print*,'  Processing Site: ',trim(namepts(i))

         open(unit=32,file=trim(infile),form='formatted')
         read(32,*) nsibin
         if (nsibin /= 1) then
            print*,'Expecting Single Site Files.'
            print*,'Stopping.'
            STOP
         endif

         read(32,*) npft
         read(32,*) nlu

         if (i .eq. 1) then
             allocate(lonsibout(nsibout),latsibout(nsibout))
             allocate(gnluout(nsibout))
             allocate(lgareaout(nsibout,nlu))
             allocate(ppftrefout(nsibout,nlu))
             allocate(sandout(nsibout,nlu),clayout(nsibout,nlu))
             allocate(visout(nsibout,nlu),nirout(nsibout,nlu))
             lgareaout(:,:) = 0.0
             ppftrefout(:,:) = 0
             sandout(:,:) = 0.0
             clayout(:,:) = 0.0
             visout(:,:) = 0.0
             nirout(:,:) = 0.0

          endif
          
          read(32,*) latsibout(i)
          read(32,*) lonsibout(i)
          read(32,*) trash
          read(32,*) trash
          read(32,*) gnluout(i)

          do j=1,gnluout(i)
             read(32,*) trash
             read(32,*) ppftrefout(i,j)       
             read(32,*) lgareaout(i,j)
             read(32,*) sandout(i,j)
             read(32,*) clayout(i,j)
             read(32,*) visout(i,j)
             read(32,*) nirout(i,j)
          enddo !j=1,gnluout

          close(32)
      endif
   ENDDO !i=1,nsibout
   
ENDIF !Global vs Site VS File


!Write out the file
IF ((outtype .eq. 0) .or. (outtype .eq. 2)) THEN
   outfile = trim(outdir) // 'sib_' // trim(ncsuffix)
   print*,''
   print*, 'Writing file: ',trim(outfile)
   print*,''
   
   call check (nf90_create(trim(outfile), nf90_clobber, ncid))
   call check (nf90_def_dim( ncid,'nsib', nsibout, nsibid ) )
   call check (nf90_def_dim( ncid,'npft',npft, npftid ) )
   call check (nf90_def_dim( ncid,'nlu', nlu, nluid ) )
   call check (nf90_def_dim( ncid,'clen',clen,clenid ) )
   call check (nf90_def_dim( ncid,'slen',slen,slenid ) )

   if (allocated(pftnames)) &
      call check (nf90_def_var( ncid,'pft_names',nf90_char, (/clenid,npftid/), pnameid ) )
   if (allocated(pftrefs))  &
      call check (nf90_def_var( ncid,'pft_refs',nf90_byte, (/npftid/), prefid ) )
   call check (nf90_def_var( ncid,'lonsib',nf90_float, nsibid, lonsibid ) )
   call check (nf90_def_var( ncid,'latsib',nf90_float, nsibid, latsibid ) )
   IF (subtype .EQ. 1) THEN
      call check (nf90_def_var( ncid,'site_names',nf90_char, (/slenid,nsibid/), snameid ) )
   ENDIF
   call check (nf90_def_var( ncid,'g_nlu',nf90_byte, nsibid, gnluid ) )
   call check (nf90_def_var( ncid,'larea',nf90_float, (/nsibid,nluid/), lgareaid ) )
   call check (nf90_def_var( ncid,'pftref',nf90_byte, (/nsibid,nluid/), ppftrefid ) )
   call check (nf90_def_var( ncid,'sand_frac',nf90_float,(/nsibid,nluid/),sandid ) )
   call check (nf90_def_var( ncid,'clay_frac',nf90_float,(/nsibid,nluid/),clayid ) )
   call check (nf90_def_var( ncid,'soref_vis',nf90_float,(/nsibid,nluid/),visid ) )
   call check (nf90_def_var( ncid,'soref_nir',nf90_float,(/nsibid,nluid/),nirid ) )
   call check (nf90_enddef( ncid ) )

   call check ( nf90_put_var( ncid, lonsibid, lonsibout ) )
   call check ( nf90_put_var( ncid, latsibid, latsibout ) )
   IF (subtype .EQ. 1) THEN
      call check ( nf90_put_var( ncid, snameid, namepts ) )
   ENDIF

   if (allocated(pftnames)) &
      call check ( nf90_put_var( ncid, pnameid, pftnames ) )
   if (allocated(pftrefs))  &
      call check ( nf90_put_var( ncid, prefid, pftrefs ) )
   call check ( nf90_put_var( ncid, gnluid, gnluout ) )
   call check ( nf90_put_var( ncid, lgareaid, lgareaout ) )
   call check ( nf90_put_var( ncid, ppftrefid, ppftrefout ) )
   call check ( nf90_put_var( ncid, sandid, sandout ) )
   call check ( nf90_put_var( ncid, clayid, clayout ) )
   call check ( nf90_put_var( ncid, visid, visout ) )
   call check ( nf90_put_var( ncid, nirid, nirout ) )

   call check (nf90_close(ncid))

ELSE !single site text files

   print*,''
   print*,'Writing Files: '
   do i=1,numpts
      outfile = trim(outdir) // namepts(i) // '/sib_' // trim(txtsuffix)
      print*, trim(outfile)
   
      open(unit=1,file=trim(outfile),form='formatted')
      write(1,'(i8,a,a)') 1,blank,'nsib'
      write(1,'(i8,a,a)') npft,blank,'npft'
      write(1,'(i8,a,a)') nlu,blank,'nlu'
      write(1,'(f8.2,a,a)') latsibout(i),blank,'Latitude'
      write(1,'(f8.2,a,a)') lonsibout(i),blank,'Longitude'
      write(1,'(i8,a,a)') sref(i),blank,'SiB Point'
      write(1,'(a,a6,a,a)') blank2,namepts(i),blank,'Site Name'
      write(1,'(i8,a,a)') gnluout(i),blank,'Number of landunits per gridcell'

      do j=1,gnluout(i)
          write(1,'(a,i3,a)') ' ---Info for Land Unit: ',j,' ---'
          write(1,'(i8,2a)') ppftrefout(i,j),blank,'PFT Reference'
          write(1,'(f8.2,2a)') lgareaout(i,j),blank,'Areal Coverage'
          write(1,'(f8.2,2a)') sandout(i,j),blank,'Sand Fraction'
          write(1,'(f8.2,2a)') clayout(i,j),blank,'Clay Fraction'
          write(1,'(f8.2,2a)') visout(i,j),blank,'Visible Soil Reflectance'
          write(1,'(f8.2,2a)') nirout(i,j),blank,'NIR Soil Reflectance'
      enddo

      close(1)
   enddo !n=1,nsibout
ENDIF


end program

!===================================
! Subroutine to check netcdf routines
! kdhaynes, 02/11
 
subroutine check(status)

use netcdf
use typeSizes
implicit none

integer, intent ( in ) :: status
 
     if (status /= nf90_noerr) then
        !print *, trim(nf90_strerror(status))
        stop "Error with netcdf.  Stopped."
      end if
end subroutine check   


