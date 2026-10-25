!Program to change the time in a file
!
!kdhaynes, 04/16

program fluxnet_fixtime

use netcdf
implicit none

integer, parameter :: startyr=2002
integer, parameter :: stopyr=2004

character*120, parameter :: &
   filename='/Users/kdhaynes/Dropbox/datas/fluxnet_data/CN-QHB/qhb_0204_h.nc'

!...time variables
integer :: ntime
real*8 :: hrfrac, dayfrac
real*8, dimension(:), allocatable :: time

!...netcdf variables
integer :: ncid,dimid,varid
integer :: status

!...misc variables
integer :: yr, day, hr
integer :: count, numdays

!---------
!Open the file
print*,'Opening file: '
print*,'   ',trim(filename)
status = ( nf90_open( trim(filename), nf90_write, ncid) )

status = nf90_inq_dimid(ncid,'numt',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=ntime)
print*,'   Number of timesteps: ',ntime
allocate(time(ntime))

count=1
do yr=startyr, stopyr
   if (mod(yr,4) .eq. 0) then
        numdays=366.
   else 
        numdays=365.
   endif

   do day=1,numdays
      do hr=1,24
         hrfrac=(hr-0.5)/dble(24.)
         dayfrac = (day+hrfrac)/dble(numdays+1)
         time(count) = yr + dayfrac
         count = count + 1
      enddo !hr
    enddo !day
enddo !yr

!print*,'New Time Test: ',time

status = ( nf90_inq_varid(ncid,'time',varid))
status = ( nf90_put_var(ncid,varid,time) )
print*,'   Put new time.'

call check ( nf90_close(ncid) )
 
deallocate(time)

print*,''
print*,'Finished processing.'
end


!=========================
subroutine check(status)

use netcdf
use typeSizes

implicit none

integer, intent(in) :: status

    if (status /= nf90_noerr) then
       stop "Error with netcdf.  Stopping."
    endif
end subroutine check
