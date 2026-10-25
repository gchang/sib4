!...program to pull a subdomain consisting of either:
!...- lon/lat range
!...- a list of sites
!...- a single site

!...creates the fire data from a global dataset
!
!...compile with...
!>gfortran make_subdomain_sibfire.f90 -L/usr/local/lib -lnetcdf -I/usr/local/include
!
!...kdhaynes, 2018/10

program make_subdomain_sibfire

use netcdf
use typesizes

implicit none

!Spec Information
character(len=100), parameter :: &
    specfile='make_subdomain.txt'
logical, parameter :: printinfo=.false.

!Parameters
integer, parameter :: startmon=1, stopmon=12
integer, parameter :: nvars=2, tvars=10 
integer, parameter :: numtperday=8

character(len=5), parameter ::  subdir='gfed4'
character(len=10), dimension(nvars), parameter :: &
   varnames = ['emis_C    ', &
               'emis_CO2  ']

character(len=3), dimension(12) :: monname
data monname/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/

!Specific Options
integer :: subtype
character(len=120) :: vsfile
character(len=120) :: firedir
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
character(len=6), dimension(:), allocatable :: namepts
integer, dimension(:), allocatable :: sibrefpts

!input information
integer :: nsibin, ntime
integer, allocatable, dimension(:) :: year, month, doy, day
real, allocatable, dimension(:) :: latsibin, lonsibin
real*8, allocatable, dimension(:) :: time
real, allocatable, dimension(:) :: hour

!output information
integer :: nsibout,srefloc
real, allocatable, dimension(:) :: latsibout, lonsibout

!netcdf variables
integer :: ncid,ncidout
integer :: status
integer :: nsibdid,ntimedid
integer :: dimid,varid,varoutid
integer :: timeid, lonsibid, latsibid
integer :: yearid, monid, doyid, dayid, hourid
integer, dimension(nvars) :: dataid

integer :: numatts
character(len=20) :: attname,varname

!data variables
real, allocatable, dimension(:,:,:) :: datain
real, allocatable, dimension(:,:,:) :: dataout

!misc variables
integer :: a, i, pt, t, var
integer :: yr, mon
integer :: numvars
real :: diff, mindiff
character(len=3) :: nameshort
character(len=120) :: filenamein, filenameout

!--------------------------------------------------
!Read in user specifications
open(unit=17,file=trim(specfile),form='formatted')
read(17,*) subtype
read(17,*) trash
read(17,*) vsfile
read(17,*) trash
read(17,*) firedir
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
   nsibout = numpts

   allocate(lonpts(numpts),latpts(numpts))
   allocate(namepts(numpts))
   do i=1,numpts
      read(17,*) latpts(i), lonpts(i), namepts(i)
   enddo
endif
close(17)

!...get the lat/lon information
write(filenamein,'(a,i4.4,i2.2,a)') trim(firedir), startyr, startmon, '.nc'
!print('(a)'),'Getting lat/lon information: '
!print*,' ',trim(filenamein)

status = nf90_open(trim(filenamein),nf90_nowrite,ncid)
IF (status .ne. nf90_noerr) THEN
   print*,'Error Opening File: '
   print*,' ',trim(filenamein)
   RETURN
ENDIF

status = nf90_inq_dimid( ncid, 'nsib', nsibdid )
status = nf90_inquire_dimension( ncid, nsibdid, len=nsibin )
IF (status .ne. nf90_noerr) THEN
   print*,'Error Getting Number of Points!'
   RETURN
ENDIF

allocate(latsibin(nsibin),lonsibin(nsibin))
status = nf90_inq_varid( ncid, 'latsib', varid )
status = nf90_get_var( ncid, varid, latsibin )
IF (status .ne. nf90_noerr) THEN
   print*,'Error Getting Latitude!'
   RETURN
ENDIF

status = nf90_inq_varid( ncid, 'lonsib', varid )
status = nf90_get_var( ncid, varid, lonsibin )
IF (status .ne. nf90_noerr) THEN
   print*,'Error Getting Longitude!'
   RETURN
ENDIF

status = nf90_close ( ncid )

!.......find gridcells included in subdomain
if (subtype .eq. 1) then
   allocate(sibrefpts(nsibout))

   DO pt=1,numpts
      mindiff = 999.
      DO i=1,nsibin
          diff = abs(lonsibin(i)-lonpts(pt))+abs(latsibin(i)-latpts(pt))
          IF (diff .lt. mindiff) THEN
             mindiff = diff
             sibrefpts(pt) = i
          ENDIF
      ENDDO
   ENDDO
else
   nsibout=0
   allocate(sibrefpts(nsibin))
   DO i=1,nsibin
      IF ((lonsibin(i) .GE. startlon) .AND. & 
          (lonsibin(i) .LE. stoplon)  .AND. &
          (latsibin(i) .GE. startlat) .AND. &
          (latsibin(i) .LE. stoplat)) THEN
          nsibout = nsibout + 1
          sibrefpts(nsibout) = i
      ENDIF
   ENDDO
endif

allocate(lonsibout(nsibout),latsibout(nsibout))
DO i=1,nsibout
   srefloc = sibrefpts(i)
   lonsibout(i) = lonsibin(srefloc)
   latsibout(i) = latsibin(srefloc)
ENDDO
print*,''
print('(a,i4)'),'# Requested Points: ',nsibout

!...loop over the specified time
!print*,''
DO yr=startyr, stopyr
   DO mon=startmon,stopmon
      write(filenamein,'(a,i4.4,i2.2,a3)') trim(firedir),yr,mon,'.nc'
      print('(a,a,a,i4.4)'),'Processing ',monname(mon),' ',yr

      !.....open the files
      if (printinfo) print*,'Opening file: ',trim(filenamein)
      status = nf90_open(trim(filenamein), nf90_nowrite, ncid )
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening File!'
         RETURN
      ENDIF

      !.....get dimensions
      status = nf90_inq_dimid(ncid,'nsib',dimid)
      status = nf90_inquire_dimension(ncid,dimid,len=nsibin)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Number of Points!'
         RETURN
      ENDIF

      status = nf90_inq_dimid(ncid,'time',dimid)
      status = nf90_inquire_dimension(ncid,dimid,len=ntime)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Number of Times!'
         RETURN
      ENDIF

      !.....get time variables
      allocate(time(ntime))
      status = nf90_inq_varid(ncid,'time',varid)
      status = nf90_get_var(ncid,varid,time)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Time!'
         RETURN
      ENDIF

      allocate(year(ntime))
      status = nf90_inq_varid(ncid,'year',varid)
      status = nf90_get_var(ncid,varid,year)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Year!'
         RETURN
      ENDIF

      allocate(month(ntime))
      status = nf90_inq_varid(ncid,'month',varid)
      status = nf90_get_var(ncid,varid,month)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Month!'
         RETURN
      ENDIF

      allocate(doy(ntime))
      status = nf90_inq_varid(ncid,'doy',varid)
      status = nf90_get_var(ncid,varid,doy)
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Getting DOY!'
          RETURN
      ENDIF

      allocate(day(ntime))
      status = nf90_inq_varid(ncid,'day',varid)
      status = nf90_get_var(ncid,varid,day)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Day!'
         RETURN
      ENDIF
 
      allocate(hour(ntime))
      status = nf90_inq_varid(ncid,'hour',varid)
      status = nf90_get_var(ncid,varid,hour)

      !.....get variables
      status = nf90_inquire(ncid,nVariables=numvars)
      IF (numvars .ne. tvars) THEN
          print*,'Unexpected Number of Variables!'
          print*,'  Expected/Output File: ',tvars,numvars
          RETURN
      ENDIF
      allocate(datain(nvars,nsibin,ntime))
      do var=1,nvars
         status = nf90_inq_varid(ncid,trim(varnames(var)),varid)
         status = nf90_get_var(ncid,varid,datain(var,:,:))
         IF (status .ne. nf90_noerr) THEN
             print*,'Error Getting Variable: ',trim(varnames(var))
             RETURN
         ENDIF
      enddo

     !.....set output
     allocate(dataout(nvars,nsibout,ntime))
     do pt=1,nsibout
        srefloc=sibrefpts(pt)
        dataout(:,pt,:) = datain(:,srefloc,:)
     enddo
     
     !.....write file
     if (outtype .eq. 0) then
          write(filenameout,'(3a,i4.4,i2.2,a3)') trim(outdir), &
                trim(subdir),'_',yr,mon,'.nc'
          status = nf90_create(trim(filenameout), nf90_clobber, ncidout )
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Creating File: '
              print*,'  ',trim(filenameout)
              RETURN
          ENDIF
          status = nf90_def_dim(ncidout,'nsib',nsibout,nsibdid)
          if (status .ne. nf90_noerr) THEN
              print*,'Error Creating Number of Points!'
              RETURN
          ENDIF
          status = nf90_def_dim(ncidout,'time',ntime,ntimedid)
          if (status .ne. nf90_noerr) THEN
             print*,'Error Creating Number of Times!'
             RETURN
          ENDIF
          
          !......define variables
          status = nf90_def_var(ncidout,'lonsib',nf90_float,nsibdid,lonsibid)
          status = nf90_def_var(ncidout,'latsib',nf90_float,nsibdid,latsibid)
          status = nf90_def_var(ncidout,'time',nf90_double,ntimedid,timeid)
          status = nf90_def_var(ncidout,'year',nf90_int,ntimedid,yearid)
          status = nf90_def_var(ncidout,'month',nf90_int,ntimedid,monid)
          status = nf90_def_var(ncidout,'doy',nf90_int,ntimedid,doyid)
          status = nf90_def_var(ncidout,'day',nf90_int,ntimedid,dayid)
          status = nf90_def_var(ncidout,'hour',nf90_float,ntimedid,hourid)

          do var=1,nvars
             status = nf90_def_var(ncidout,trim(varnames(var)),nf90_double, &
                       (/nsibdid,ntimedid/),dataid(var))
          enddo

          do var=1,numvars
             status = nf90_inquire_variable(ncid,var,name=varname,natts=numatts)
             !print*,'Variable Name and Num Atts: ',trim(varname),numatts
             DO a=1,numatts
                 status = nf90_inq_attname(ncid,var,a,name=attname)
                 status = nf90_copy_att(ncid,var,trim(attname),ncidout,var)
             ENDDO
          enddo

          status = nf90_enddef(ncidout)
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Defining Variables!'
              RETURN
          ENDIF

          !....put variables
          varoutid=1
          status = nf90_put_var(ncidout,varoutid,lonsibout)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,latsibout)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,time)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,year)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,month)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,doy)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,day)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,hour)
          varoutid=varoutid+1

          DO var=1,nvars
             status = nf90_put_var(ncidout,dataid(var),dataout(var,:,:))
             IF (status .ne. nf90_noerr) THEN
                print*,'Error Writing Variable: ',trim(varnames(var))
                RETURN
             ENDIF
           ENDDO

          !.....close the new output file
          status = nf90_close(ncidout)

      ELSEIF (outtype .eq. 1) THEN  !write out text files
          !.....write out the text file
          do pt=1,nsibout
             a=index(namepts(pt),'-')
             write(nameshort,('(a)')) namepts(pt)(a+1:a+3)
             call to_lower(nameshort)
             write(filenameout,'(a,a,a1,a,a1,a,a1,a,a1,i4.4,i2.2,a4)') trim(outdir), &
                  trim(namepts(pt)), '/', &
                  trim(subdir), '/', trim(subdir), '_', &
                  trim(nameshort), '_', yr, mon, '.dat'
             if (printinfo) THEN
                  print*,'Writing File: '
                  print*,'   ',trim(filenameout)
             endif
             open(unit=53,file=trim(filenameout),form='formatted')

             !...print the header info...
             write(53,'(a13,i5,a11,2F8.2)') '# SiB point: ',sibrefpts(pt), &
                      '  Lon/Lat: ',lonpts(pt),latpts(pt)
             write(53,'(3a)')'# Data are from the ',subdir, &
                      ' 0.5 x 0.5 degree files and is 3-hourly.'
             write(53,'(a)')'#  Yr   DOY   Hour      Emis_C            Emis_CO2'
             write(53,'(a)')'#                       (umol C/m2/s)     (umol C/m2/s)'

             do t=1,ntime
                 write(53,'(i7,i4,f7.2,2f18.10)') &
                       year(t), doy(t), hour(t),   &
                       dataout(1,pt,t), dataout(2,pt,t)
             enddo

             close(53)
          enddo !pt
          print*,'   Maximum Fire Emissions: ',maxval(dataout)
          
      ELSE
           print*,'Unexpected Output Type!'
           RETURN
      ENDIF !

      status = nf90_close(ncid)
      deallocate(time)
      deallocate(year)
      deallocate(month)
      deallocate(doy)
      deallocate(day)
      deallocate(hour)
      deallocate(datain,dataout)
   ENDDO
ENDDO

print*,''
print('(a)'),'Finished Creating Subdomain Driver Data.'
print*,''

end program make_subdomain_sibfire


!===================================
! Subroutine to check netcdf routines
! kdcorbin, 02/11
 
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


!===============================================
! Subroutine to change strings to all lower case
! kdhaynes, 07/15

subroutine to_lower(string)

implicit none

character(*), intent(inout) :: string

integer :: ic, i

character(len=26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

!...Capitalize each letter if it is lowercase
do i=1, len_trim(string)
   ic = index(cap, string(i:i))
   if (ic > 0) string(i:i) = low(ic:ic)
enddo

end subroutine to_lower
