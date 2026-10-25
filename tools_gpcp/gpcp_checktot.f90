!Program that prints the total precipitation from GPCP.
!Expecting monthly totals in mm/mon per gridcell.
!
!To compile, use:
!>make gpcp_checktot
!
!kdhaynes, 2019/03
!
program gpcp_checktot

use netcdf
implicit none

!User specifications
character*120, parameter :: &
    filename='/Users/kdhaynes/gpcp_v2.3/gpcp_v2.3_2018.nc'
logical, parameter :: flag_mmpmon = .false.

!Global constants
real*8, parameter :: areaEarth = 510072000000000.

!dimension variables
integer :: nlat,nlon,numt

!data variables
integer, dimension(:), allocatable :: month
real*4, dimension(:), allocatable :: lon, lat
real*4, dimension(:,:,:), allocatable :: data

!netcdf variables
integer :: ncid, varid
integer :: status

!grid variables
real*8 :: deltalon, deltalat
real*8 :: lone, lonw, loner, lonwr
real*8 :: londiff
real*8 :: latn, lats, latnr, latsr
real*8 :: sinlatnr, sinlatsr, sinlatdiff
real*8 :: area,areatot

real*8 :: pi
real*8, parameter :: radius = 6370000.

!local variables
integer :: i,j,t
integer :: monref, dayspermon(12)
real :: myconvert

character*3, dimension(12) :: monname
real*8 :: preciptotall
real*8, dimension(:), allocatable :: preciptot

!-----------------
!Set variables
pi = acos(-1.0)
areatot = 0.0
data monname/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/
data dayspermon/31,28,31,30,31,30,31,31,30,31,30,31/

!------------------------
!Open the file
print*,''
print('(a)'),'Checking file: '
print('(2a)'),'  ',trim(filename)
call check ( nf90_open( trim(filename), nf90_nowrite, ncid ) )

!...get the dimensions
status = nf90_inq_dimid(ncid,'nlon',varid)
IF (status .NE. 0) THEN
   status = nf90_inq_dimid(ncid,'lon',varid)
ENDIF
call check (nf90_inquire_dimension(ncid,varid,len=nlon))

status = nf90_inq_dimid(ncid,'nlat',varid)
IF (status .NE. 0) THEN
   status = nf90_inq_dimid(ncid,'lat',varid)
ENDIF
call check ( nf90_inquire_dimension(ncid,varid,len=nlat) )

call check ( nf90_inq_dimid(ncid,'time',varid) )
call check ( nf90_inquire_dimension(ncid,varid,len=numt) )
print('(a,3I8)'),'  Dimensions (numlon/numlat/numt):',nlon,nlat,numt

!...get the variables
allocate(month(numt))
allocate(lon(nlon),lat(nlat))
allocate(data(nlon,nlat,numt))
allocate(preciptot(numt))
call check ( nf90_inq_varid(ncid,'longitude',varid) )
call check ( nf90_get_var( ncid,varid,lon ) )
call check ( nf90_inq_varid(ncid,'latitude',varid) )
call check ( nf90_get_var( ncid,varid,lat) )
call check ( nf90_inq_varid(ncid,'month',varid))
call check ( nf90_get_var( ncid, varid, month) )

call check ( nf90_inq_varid(ncid,'precip',varid) )
call check ( nf90_get_var( ncid,varid,data ) )

!...close the file
call check ( nf90_close(ncid) )
!print*,' Closed File.'


!Calculate the areal variables for total
deltalon = (lon(4)-lon(3))/2.
deltalat = (lat(4)-lat(3))/2.

preciptot=0.
preciptotall=0.
do i=1,nlon
   lone=lon(i)+deltalon
   lonw=lon(i)-deltalon
   loner=lone*pi/180.
   lonwr=lonw*pi/180.
   londiff=loner-lonwr

   do j=1,nlat
      lats=lat(j)-deltalat
      latn=lat(j)+deltalat
      latsr=lats*pi/180.
      latnr=latn*pi/180.
      sinlatsr=sin(latsr)
      sinlatnr=sin(latnr)
      sinlatdiff=sinlatnr-sinlatsr

      area=radius*radius*londiff*sinlatdiff
      areatot=areatot+area

      do t=1,numt
         preciptot(t)=preciptot(t) + data(i,j,t)*area
      enddo

   enddo
enddo

print('(a,E14.5)'),'  Surface area calculated and surface area of Earth (m2):'
print('(a,E14.5,a,E14.5)'), '    ',areatot,'   ',areaEarth
print*,''
print('(a)'),'Monthly Precipitation Totals (mm)'
DO t=1,numt
   monref = month(t)
   if (flag_mmpmon) then
       myconvert = 1.
   else
       myconvert = dayspermon(t)
   endif

   preciptotall = preciptotall + preciptot(t)/areaEarth*myconvert
   print('(a,a,F10.5)'),'  ',monname(monref),preciptot(t)/areaEarth*myconvert
ENDDO
print*,''
print('(a)'),'GLOBAL ANNUAL PRECIPITATION (mm)'
print*,preciptotall
print*,''

end

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
