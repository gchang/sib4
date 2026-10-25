!...program to pull a subdomain consisting of either:
!...- lon/lat range
!...- a list of sites
!...- a single site

!...creates the driver data from a global dataset
!
!...typically use MERRA data saved on archive
!...setting driver data directory to:
!'/archive/MERRA_0.5x0.5/merra_0.5dx0.5dmon_'
!'/archive/MERRA2_0.5deg/merra_0.5deg_'
!
!...compile with...
!>gfortran make_subdomain_sibdr_new.f90 -L/usr/local/netcdf3-gcc/lib -lnetcdf -I/usr/local/netcdf3-gcc/include
!
!...kdhaynes, 03/16


program make_subdomain_sibdr_new

use netcdf
use typesizes

implicit none

!Spec Information
character(len=100), parameter :: &
    specfile='make_subdomain.txt'

!Parameters
integer, parameter :: startmon=1, stopmon=12
character(len=10), parameter :: drvr_type='merra2'  !'merra','ncep'
integer, parameter :: varstart=9                   !8

character(len=3), dimension(12) :: monname
data monname/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/

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
character(len=6), dimension(:), allocatable :: namepts
integer, dimension(:), allocatable :: sibrefpts

!input information
integer :: nsibin, ntime
integer, allocatable, dimension(:) :: year, month, doy, day
real, allocatable, dimension(:) :: latsibin, lonsibin
real*8, allocatable, dimension(:) :: time
real, allocatable, dimension(:) :: hour
real, allocatable, dimension(:,:) :: varin

!output information
integer :: nsibout,srefloc
real, allocatable, dimension(:) :: latsibout, lonsibout
real, allocatable, dimension(:,:) :: varout

!netcdf variables
integer :: ncid,ncidout
integer :: status
integer :: nsibdid,ntimedid
integer :: dimid,varid,varoutid
integer :: timeid, lonsibid, latsibid
integer :: yearid, monid, doyid, dayid, hourid

integer :: numatts
character(len=20) :: attname,varname

!text file variables
integer, dimension(2) :: fstart, fcount
real, allocatable, dimension(:) :: &
    t2m, sh, wind, &
    pres, lw, sw, lsp, cup

!misc variables
integer :: a, i, pt, t
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
   nsibout = numpts

   allocate(lonpts(numpts),latpts(numpts))
   allocate(namepts(numpts))
   do i=1,numpts
      read(17,*) latpts(i), lonpts(i), namepts(i)
   enddo
endif
close(17)

!...get the lat/lon information
write(filenamein,'(a,i4.4,i2.2,a)') trim(driverdir), startyr, startmon, '.nc'
print('(a)'),'Getting lat/lon information: '
print*,' ',trim(filenamein)

call check ( nf90_open(trim(filenamein),nf90_nowrite,ncid) )
call check ( nf90_inq_dimid( ncid, 'nsib', nsibdid ) )
call check ( nf90_inquire_dimension( ncid, nsibdid, len=nsibin ) )

allocate(latsibin(nsibin),lonsibin(nsibin))
call check ( nf90_inq_varid( ncid, 'latsib', varid ) )
call check ( nf90_get_var( ncid, varid, latsibin ) )
call check ( nf90_inq_varid( ncid, 'lonsib', varid ) )
call check ( nf90_get_var( ncid, varid, lonsibin ) )

call check ( nf90_close ( ncid ) )

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
print('(a,i8)'),'  Number of points in subdomain: ',nsibout


!...loop over the specified time
print*,''
DO yr=startyr, stopyr
   DO mon=startmon,stopmon
      write(filenamein,'(a,i4.4,i2.2,a3)') trim(driverdir),yr,mon,'.nc'
      print('(a,a,a,i4.4)'),'Processing ',monname(mon),' ',yr

      !.....open the files
      print*,'Opening file: '
      print*,' ',trim(filenamein)
      status = nf90_open(trim(filenamein), nf90_nowrite, ncid )
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening File!'
         RETURN
      ENDIF

      !.....get dimensions
      status = nf90_inq_dimid(ncid,'time',dimid)
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Getting Time Dimension!'
          RETURN
      ENDIF
      call check ( nf90_inquire_dimension(ncid,dimid,len=ntime))

      status = nf90_inq_dimid(ncid,'nsib',dimid)
      IF (status .ne. nf90_noerr) THEN
         status = nf90_inq_dimid(ncid,'landpoints',dimid)
         IF (status .ne. nf90_noerr) THEN
             print*,'Error Getting Number of Points!'
             RETURN
         ENDIF
      ENDIF
      status = nf90_inquire_dimension(ncid,dimid,len=nsibin)

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
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Hour!'
         RETURN
      ENDIF

      if (outtype .eq. 0) then
          write(filenameout,'(a,i4.4,i2.2,a3)') trim(outdir),yr,mon,'.nc'
          status = nf90_create(trim(filenameout), nf90_clobber, ncidout )
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Creating Output File!'
              print*,'  ',trim(filenameout)
              RETURN
          ELSE
              print*,'Creating file: '
              print*,' ',trim(filenameout)
          ENDIF

          !......define dimensions
          status = nf90_def_dim(ncidout,'nsib',nsibout,nsibdid)
          status = nf90_def_dim(ncidout,'time',ntime,ntimedid)

          !......define variables
          status = nf90_def_var(ncidout,'time',nf90_double,ntimedid,timeid)
          status = nf90_def_var(ncidout,'lonsib',nf90_float,nsibdid,lonsibid)
          status = nf90_def_var(ncidout,'latsib',nf90_float,nsibdid,latsibid)
          status = nf90_def_var(ncidout,'year',nf90_int,ntimedid,yearid)
          status = nf90_def_var(ncidout,'month',nf90_int,ntimedid,monid)
          status = nf90_def_var(ncidout,'doy',nf90_int,ntimedid,doyid)
          status = nf90_def_var(ncidout,'day',nf90_int,ntimedid,dayid)
          status = nf90_def_var(ncidout,'hour',nf90_float,ntimedid,hourid)

          status = nf90_inquire(ncid,nVariables=numvars)
          !print('(a,i3)'),'  Number of variables: ',numvars-varstart+1
          DO i=varstart,numvars
             status = nf90_inquire_variable(ncid,i,name=varname,natts=numatts)
             status = nf90_def_var(ncidout,trim(varname),nf90_float,(/nsibdid,ntimedid/),varid)
             IF (status .ne. nf90_noerr) THEN
                print*,'Error Defining Variable: ',trim(varname)
                RETURN
             ENDIF
             DO a=1,numatts
                status = nf90_inq_attname(ncid,i,a,name=attname)
                status = nf90_copy_att(ncid,i,trim(attname),ncidout,varid)
             ENDDO
          ENDDO

          status = nf90_enddef(ncidout)
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Defining Variables in Output File!' 
              RETURN
          ENDIF
          !print*,' Finished defining new file.'

          !....get/copy variables
          varoutid=1
          status = nf90_put_var(ncidout,varoutid,time)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,lonsibout)
          varoutid=varoutid+1

          status = nf90_put_var(ncidout,varoutid,latsibout)
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

          allocate(varin(nsibin,ntime))
          allocate(varout(nsibout,ntime))
          DO i=varstart,numvars
             status = nf90_get_var(ncid,i,varin)
             DO a=1,nsibout
                srefloc=sibrefpts(a)
                varout(a,:) = varin(srefloc,:)
             ENDDO
             status = nf90_put_var(ncidout,i,varout)
             IF (status .ne. nf90_noerr) THEN
                 status = nf90_inquire_variable(ncid,i,name=varname)
                 print*,'Error Writing Variable: ',trim(varname)
                 RETURN
             ENDIF
          ENDDO
          deallocate(varin)
          deallocate(varout)

          !.....close the new output file
          status = nf90_close(ncidout)
      ELSE  !write out text files
          !.....get data
          allocate(t2m(ntime), sh(ntime), wind(ntime))
          allocate(pres(ntime), lw(ntime), sw(ntime))
          allocate(lsp(ntime),cup(ntime))

          fstart(2) = 1
          fcount(1) = 1
          fcount(2) = ntime

          do pt=1,numpts
             fstart(1) = sibrefpts(pt)
             status = nf90_inq_varid(ncid,'tm',varid)  ! Temperature at 2 m (K)
             status = nf90_get_var(ncid,varid,t2m,fstart,fcount)
             status = nf90_inq_varid(ncid,'sh',varid)   ! Specific Humidity (kg/kg)
             status = nf90_get_var(ncid,varid,sh,fstart,fcount)
             status = nf90_inq_varid(ncid,'spdm',varid) ! Wind speed (m/s)
             status = nf90_get_var(ncid,varid,wind,fstart,fcount)
             status = nf90_inq_varid(ncid,'ps',varid)   ! Surface Pressure (mb)
             status = nf90_get_var(ncid,varid,pres,fstart,fcount)
             status = nf90_inq_varid(ncid,'lwd',varid)  ! Surface thermal rad down (W/m2)
             status = nf90_get_var(ncid,varid,lw,fstart,fcount)
             status = nf90_inq_varid(ncid,'swd',varid)  ! Surface solar rad down (W/m2)
             status = nf90_get_var(ncid,varid,sw,fstart,fcount)
    
             status = nf90_inq_varid(ncid,'lspr_scaled',varid)
             if (status .ne. 0) then
                status = nf90_inq_varid(ncid,'lspr',varid)  ! Large Scale Precip (kg/m2/s)
                IF (status .eq. 0) then
                    print*,'Error Getting Large-Scale Precip!'
                    RETURN
                ENDIF
             endif
             status = nf90_get_var(ncid,varid,lsp,fstart,fcount)

             status = nf90_inq_varid(ncid,'cupr_scaled',varid)  ! Convective Precip (kg/m2/s)
             if (status .ne. nf90_noerr) then
                status = nf90_inq_varid(ncid,'cupr',varid)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Getting Convective Precip!'
                   RETURN
                ENDIF
             endif
            status = nf90_get_var(ncid,varid,cup,fstart,fcount)


             !.....write out the text file
             a=index(namepts(pt),'-')
             write(nameshort,('(a)')) namepts(pt)(a+1:a+3)
             call to_lower(nameshort)
             write(filenameout,'(a,a,a1,a,a1,a,a1,a,a1,i4.4,i2.2,a4)') trim(outdir), &
                  trim(namepts(pt)), '/', &
                  trim(drvr_type), '/', trim(drvr_type), '_', trim(nameshort), '_', &               
                  yr,mon,'.dat'

             print*,'Writing File: '
             print*,'   ',trim(filenameout)
             open(unit=53,file=trim(filenameout),form='formatted')

             !...print the header info...
             write(53,'(a13,i5,a11,2F8.2)') '# SiB point: ',sibrefpts(pt),'  Lon/Lat: ',lonpts(pt),latpts(pt)
             write(53,'(3a)')'# Data are from the ',drvr_type,' 0.5 x 0.5 degree files and is hourly.'
             write(53,'(a)')'#  Yr DOY Hour  T2m    SH     wspeed  press  lwave    swave    lspr    cupr'
             write(53,'(a)')'#               (K)  (kg/kg)  (m/s)   (mb)   (W/m2)   (W/m2)  (mm/s)  (mm/s)'

             do t=1,ntime
                 write(53,'(3i7, 6f18.10, 2f22.14)') &
                     year(t), doy(t), int(hour(t)),   &
                     t2m(t), sh(t), wind(t), pres(t), &
                     lw(t), sw(t), lsp(t), cup(t)
             enddo

             close(53)
          enddo !pt

           deallocate(t2m,sh,wind)
           deallocate(pres,lw,sw)
           deallocate(lsp,cup)
      ENDIF !

      status = nf90_close(ncid)
      deallocate(time)
      deallocate(year)
      deallocate(month)
      deallocate(doy)
      deallocate(day)
      deallocate(hour)

   ENDDO
ENDDO

print*,''
print('(a)'),'Finished Creating Subdomain Driver Data.'
print*,''

end program make_subdomain_sibdr_new


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
