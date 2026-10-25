!...program to create a restart file for
!...a list of sites from single site data
!
!...reads the user specifications from the 
!...make_subdomain.txt file
!
!...assumes creating a single site or a
!...group consisting of a list of sites
!...(subdomain type=1)
!
!...compile with:
!>gfortran make_subdomain_sibdr_copy.f90 -L/usr/local/netcdf3-gcc/lib -lnetcdf -I/usr/local/netcdf3-gcc/include
!...
!
!...kdhaynes, 04/16


program make_subdomain_sibdr_copy

use netcdf
use typesizes

implicit none

!Spec Information
character(len=100), parameter :: &
    specfile='make_subdomain.txt'
character(len=10), parameter :: mymerra='merra2'

!Parameters
integer, parameter :: startmon=1, stopmon=12
integer, parameter :: numv=8, drid=11
integer, parameter :: clen=6
character(len=4), parameter :: suffix='.dat'
character(len=12), dimension(numv) :: varname
data varname/'tm','sh','spdm','ps','lwd','swd','lspr','cupr'/
character(len=10), dimension(numv) :: varunit
data varunit/'K','kg/kg','m/s','mb','W/m2','W/m2','mm/s','mm/s'/
character(len=3), dimension(12) :: monname
data monname/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/
integer, dimension(12) :: dayspermon
data dayspermon/31,28,31,30,31,30,31,31,30,31,30,31/

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
real, dimension(:), allocatable :: sitelon, sitelat
character(len=6), dimension(:), allocatable :: sitename

!input information
integer :: spos, status
integer :: numt, numday, yrlen
real*8 :: dayfrac
real*8, allocatable, dimension(:) :: time
real, allocatable, dimension(:) :: year
real, allocatable, dimension(:) :: month, hour
real, allocatable, dimension(:) :: doy, day
character(len=3) :: sitetemp
character(len=3), allocatable, dimension(:) :: siteshort

character(len=256) :: record
integer :: yrin, doyin, hrin
real*8 :: psin,tmin,shin,spdmin,lsprin,cuprin,dlwbotin,swdnin

!output information
real, allocatable, dimension(:,:,:) :: varout

!netcdf variables
integer :: ncid
integer :: nsibdid,ntimedid,clendid
integer :: dimid
integer :: timeid, lonsibid, latsibid
integer :: snameid, yearid, monid, doyid, dayid, hrid
integer, dimension(numv) :: varid

!misc variables
integer :: v, site
integer :: yr, mon, dy, hr
integer :: count
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
   allocate(sitelon(numpts),sitelat(numpts))
   allocate(sitename(numpts),siteshort(numpts))
   do site=1,numpts
      read(17,*) sitelat(site), sitelon(site), sitename(site)

      spos=index(trim(sitename(site)),'-')
      write(sitetemp,('(a)')) sitename(site)(spos+1:spos+3) 
      call to_lower(sitetemp)
      siteshort(site) = sitetemp
   enddo
  
endif
close(17)

!...only combine driver data for a single site list
if (subtype .ne. 1) then
   print*,'Expecting Single Site List.'
   RETURN
endif

!...loop over the specified time
DO yr=startyr, stopyr
    DO mon=startmon,stopmon
       numday=dayspermon(mon)
       yrlen=365
       if ((mod(yr,4) .eq. 0) .and. &
           (mon .eq. 2)) then
           numday=numday+1
           yrlen=yrlen+1
       endif
       numt=numday*24
            
       !...loop over sites
       DO site=1,numpts
          !print*,'Processing site: ',trim(sitename(site))
          write(filenamein,'(a,a,a1,a,a1,a,a1,a,a1,i4.4,i2.2,a4)') &
               trim(driverdir), &
               trim(sitename(site)),'/',trim(mymerra),'/',trim(mymerra),'_', &
               trim(siteshort(site)),'_',yr,mon,trim(suffix)
          write(filenameout,'(a,a,a1,a,a1,i4.4,i2.2,a3)') &
               trim(outdir),trim(mymerra),'/',trim(mymerra),'_',yr,mon,'.nc'
          print*,'Input File: '
          print*,'  ',trim(filenamein)
          print*,'Output File: '
          print*,'  ',trim(filenameout)

          !.....open the files
          open( unit=drid, file=trim(filenamein), status='old', &
                 form='formatted', iostat=status)

          if (site .eq. 1) then
              call check ( nf90_create(trim(filenameout), nf90_clobber, ncid ) )

              call check ( nf90_def_dim(ncid,'nsib',numpts,nsibdid))
              call check ( nf90_def_dim(ncid,'time',numt,ntimedid))
              call check ( nf90_def_dim(ncid,'clen',clen,clendid))

              !......define variables
              call check ( nf90_def_var(ncid,'lonsib',nf90_float,nsibdid,lonsibid))
              call check ( nf90_def_var(ncid,'latsib',nf90_float,nsibdid,latsibid))
              call check ( nf90_def_var(ncid,'site_names',nf90_char,(/clendid,nsibdid/),snameid))
              call check ( nf90_def_var(ncid,'time',nf90_double,ntimedid,timeid))
              call check ( nf90_def_var(ncid,'year',nf90_int,ntimedid,yearid))
              call check ( nf90_def_var(ncid,'month',nf90_int,ntimedid,monid))
              call check ( nf90_def_var(ncid,'doy',nf90_int,ntimedid,doyid))
              call check ( nf90_def_var(ncid,'day',nf90_int,ntimedid,dayid))
              call check ( nf90_def_var(ncid,'hour',nf90_float,ntimedid,hrid))

              DO v=1,numv
                 call check ( nf90_def_var(ncid,trim(varname(v)),nf90_float,(/nsibdid,ntimedid/),varid(v)))
                 call check ( nf90_put_att(ncid,varid(v),'units',varunit(v)) )
              ENDDO

             call check ( nf90_enddef(ncid))

             call check ( nf90_put_var(ncid,lonsibid,sitelon) )
             call check ( nf90_put_var(ncid,latsibid,sitelat ) )
             call check ( nf90_put_var(ncid,snameid,sitename) )

             allocate(time(numt))
             allocate(year(numt))
             allocate(month(numt))
             allocate(doy(numt))
             allocate(day(numt))
             allocate(hour(numt))
             allocate(varout(numpts,numv,numt))

         endif !site == 1

         count=1
         DO dy=1,numday
            DO hr=1,24
               DO  !Read until not a comment
                   read(drid,'(a)', iostat=status) record
                   IF ( status /= 0) THEN
                      print*,'Stopping due to error in driver data.'
                      print*,'Processing site: ',trim(sitename(site))
                      stop
                   ENDIF
                   IF (record(1:1) .ne. '#') THEN
                      exit
                   ENDIF
                ENDDO

                read(unit=record,fmt=*) yrin,doyin,hrin,tmin,shin, &
                      spdmin,psin,dlwbotin,swdnin,lsprin,cuprin                

                dayfrac = doyin + dble(hrin+0.5)/dble(24.)
                time(count) = yrin + dayfrac/dble(yrlen+1)
                year(count) = yrin
                month(count) = mon
                doy(count) = doyin
                day(count) = dy
                hour(count) = hrin+0.5
                varout(site,1,count) = tmin
                varout(site,2,count) = shin
                varout(site,3,count) = spdmin
                varout(site,4,count) = psin
                varout(site,5,count) = dlwbotin
                varout(site,6,count) = swdnin
                varout(site,7,count) = lsprin
                varout(site,8,count) = cuprin

                count=count+1
            ENDDO !hr
         ENDDO !day

         !...close the site data
         close(drid)

       ENDDO !site=1,numpts

      !.....put data in new output file
      call check ( nf90_put_var(ncid,timeid,time) )
      call check ( nf90_put_var(ncid,yearid,year) )
      call check ( nf90_put_var(ncid,monid,month) )
      call check ( nf90_put_var(ncid,doyid,doy) )
      call check ( nf90_put_var(ncid,dayid,day) )
      call check ( nf90_put_var(ncid,hrid,hour) )

      DO v=1,numv
         call check ( nf90_put_var(ncid,varid(v),varout(:,v,:) ) )
      ENDDO

      !.....close the new output file
      call check ( nf90_close(ncid))

      deallocate(time,year,month)
      deallocate(doy,day,hour)
      deallocate(varout)

   ENDDO  !mon=1,12
ENDDO  !yr=startyr,stopyr

print*,''
print('(a)'),'Finished Creating Subdomain Driver Data.'
print*,''

end program make_subdomain_sibdr_copy


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
