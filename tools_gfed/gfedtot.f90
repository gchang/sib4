!---------------------------------------------
program gfedtot
!---------------------------------------------
!Program to calculate the annual GFED 
!   fire emissions.
!
!To compile:
!>f90nc gfedtot
!
!kdhaynes, 2018/10

implicit none

!------------------------------------------------------
!Enter In Filenames and Variables:

integer, parameter :: nfiles=3

character(len=256), dimension(nfiles) :: &
   filenamesin

filenamesin(1) = &
    '/Users/kdhaynes/Research/sib4_driver/global/gfed4/gfed4_0.5deg_200711.nc'
filenamesin(2) = &
   '/Users/kdhaynes/Test/gfed4_200711.nc'
filenamesin(3) = &
   '/Users/kdhaynes/Dropbox/data/global/gfed_data/gfed4.1s_emis_200711.nc'


!------------------------------------------------------

call gfedtot_output(nfiles, filenamesin)

print*,''
print*,'Finished Calculating Total Fire Emissions.'
print*,''

end program gfedtot


!=========================================
subroutine gfedtot_output(nfiles, filenames)
!=========================================
!Subroutine determining if file is
!   gridded or vector.
!
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames

!netcdf variables
integer :: status, ncid, dimid

!misc variables
integer :: f
integer :: spos1, spos2

!------------------------------------------
DO f=1,nfiles
   !print message
   spos1=index(filenames(f),'/',back=.true.)
   spos2=len_trim(filenames(f))
   print('(2a)'),'Processing File: ',filenames(f)(spos1+1:spos2)

   !open the file
   status = nf90_open(trim(filenames(f)),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Opening File: '
      print*,'  ',trim(filenames(f))
      STOP
   ENDIF  

   !check for nsib/landpoints
   status = nf90_inq_dimid(ncid, 'nsib', dimid)
   IF (status .eq. nf90_noerr) THEN
      status = nf90_close(ncid)
      call gfedtot_sib(filenames(f))
   ELSE
      status = nf90_close(ncid)
      call gfedtot_grid(filenames(f))
   ENDIF
ENDDO !f=1,nfiles

print*,''
print*,'Finished Processing Files.'
print*,''
end subroutine gfedtot_output

!=========================================
subroutine gfedtot_grid(filename)
!=========================================
!Subroutine controlling the sequence of 
!   calls to calculate total fire emissions.
!
!kdhaynes, 10/17

use netcdf
implicit none

!input variables
character(len=256), intent(in) :: filename

!parameters
real, parameter :: gttg=1.E-12

!netcdf variables
integer :: status, ncid, dimid, varid

!area variables
integer :: nlon, nlat
real, dimension(:), allocatable :: lon, lat
real*8, dimension(:,:), allocatable :: areagrid

!data variables
integer :: ntime
real*8, dimension(:,:,:), allocatable :: emisC, emisCO2
real*8 :: totemisC, totemisCO2
real*8, dimension(:,:), allocatable :: tottemp

!misc variables
integer :: t

!------------------------------------------

   !open the file
   status = nf90_open(trim(filename),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Opening File: '
      print*,'  ',trim(filename)
      STOP
   ENDIF  

   !get dimensions
   status = nf90_inq_dimid(ncid, 'longitude', dimid)
   IF (status .ne. nf90_noerr) THEN
       status = nf90_inq_dimid(ncid,'lon',dimid)
       IF (status .ne. nf90_noerr) THEN
            print*,'Unknown Longitude Dimension Name.'
            print*,'Stopping.'
            STOP
       ENDIF
    ENDIF
    status = nf90_inquire_dimension(ncid, dimid, len=nlon)
   
    status = nf90_inq_dimid(ncid, 'latitude', dimid)
    IF (status .ne. nf90_noerr) THEN
        status = nf90_inq_dimid(ncid,'lat',dimid)
        IF (status .ne. nf90_noerr) THEN
             print*,'Unknown Latitude Dimension Name.'
             print*,'Stopping.'
             STOP
        ENDIF
    ENDIF
    status = nf90_inquire_dimension(ncid, dimid, len=nlat)

    !get lon/lat
    allocate(lon(nlon))
    status = nf90_inq_varid(ncid, 'longitude', varid)
    IF (status .ne. nf90_noerr) THEN
          print*,'Unknown Longitude Name.'
          print*,'Stopping.'
          STOP
    ENDIF
    status = nf90_get_var(ncid, varid, lon)

    allocate(lat(nlat))
    status = nf90_inq_varid(ncid, 'latitude', varid)
    IF (status .ne. nf90_noerr) THEN
          print*,'Unknown Latitude Name.'
          print*,'Stoping.'
          STOP
       ENDIF
    status = nf90_get_var(ncid, varid, lat)

    allocate(areagrid(nlon,nlat))
    call calc_gridarea(nlon, nlat, &
             lon, lat, areagrid)

   !get time
   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)
   print('(a,3i6)'), '   Dimensions (nlat/nlon/ntime): ', &
        nlat,nlon,ntime

   !get the fire C emissions
   allocate(emisc(nlon,nlat,ntime))
   status = nf90_inq_varid(ncid, 'emis_C', varid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Missing C Emissions.'
      print*,'Stopping.'
      STOP
   ENDIF
   status = nf90_get_var(ncid, varid, emisc)

   !get the fire CO2 emissions
   allocate(emisco2(nlon,nlat,ntime))
   status = nf90_inq_varid(ncid, 'emis_CO2', varid)
   IF (status .ne. nf90_noerr) THEN
      status = nf90_inq_varid(ncid, 'resp_fire', varid)
      IF (status .ne. nf90_noerr) THEN
          print*,'Missing CO2 Emissions.'
          print*,'Stopping.'
          STOP
       ENDIF
   ENDIF
   status = nf90_get_var(ncid, varid, emisco2)
   status = nf90_close(ncid)

   !calculate total emissions
   allocate(tottemp(nlon,nlat))
   totemisC = 0.
   totemisCO2 = 0.
   do t=1,ntime
       tottemp(:,:) = emisc(:,:,t)/8.
       totemisC = totemisC + SUM(tottemp*areagrid)

       tottemp(:,:) = emisco2(:,:,t)/8.
       totemisCO2 = totemisCO2 + SUM(tottemp*areagrid)
   enddo
   totemisC = totemisC*gttg
   totemisCO2 = totemisCO2*gttg
   print*,'  Total C Emissions (Tg):',totemisC
   print*,'  Total CO2 Emissions (Tg):',totemisCO2
   print*,''


   !deallocate arrays for next files
   deallocate(areagrid,lon,lat)
   deallocate(emisc,emisco2)
   deallocate(tottemp)


end subroutine gfedtot_grid


!=========================================
subroutine calc_gridarea(nlon, nlat, &
      lon, lat, area)
!=========================================
!Subroutine to calculate gridcell areas.
!
!kdhaynes, 06/16
!
implicit none

! input variables
integer, intent(in) :: nlon, nlat
real, dimension(nlon), intent(in) :: lon
real, dimension(nlat), intent(in) :: lat
real*8, dimension(nlon,nlat), intent(inout) :: area

! areal variables
real :: deltalon, deltalat
real*8 :: pi
real*8 :: lone, lonw, loner, lonwr
real*8, dimension(nlon) :: londiff
real*8 :: lats, latn, latsr, latnr
real*8 :: sinlatsr, sinlatnr
real*8, dimension(nlat) :: sinlatdiff
real*8 :: areatot

! parameters
logical, parameter :: area_check=.false.
real*8, parameter :: radius = 6370000.  !Earth radius (m)
character(len=30), parameter :: err_message='calc_gridarea_error'

! local variables
integer :: i,j

!----------------------------
!set local variables
pi = acos(-1.0)
areatot = 0.

!calculate areal variables
deltalon = (lon(3)-lon(2))*0.5
deltalat = (lat(3)-lat(2))*0.5

do i=1,nlon
   lone=lon(i)+deltalon
   lonw=lon(i)-deltalon
   loner=lone*pi/180.
   lonwr=lonw*pi/180.
   londiff(i)=loner-lonwr
enddo

do j=1,nlat
   lats=lat(j)-deltalat
   latn=lat(j)+deltalat
   latsr=lats*pi/180.
   latnr=latn*pi/180.
   sinlatsr=sin(latsr)
   sinlatnr=sin(latnr)
   sinlatdiff(j)=sinlatnr-sinlatsr
enddo

do i=1, nlon
   do j=1, nlat
      area(i,j) = radius*radius*londiff(i)*sinlatdiff(j)
      areatot = areatot + area(i,j)
   enddo !j
enddo !i

if (area_check) then
   print*,''
   print('(a,E15.8,a)'),'Area used vs actual:',areatot,'  0.50990436E+15 m2'
endif

 
end subroutine calc_gridarea


!=========================================
!=========================================
subroutine gfedtot_sib(filename)
!=========================================
!Subroutine controlling the sequence of 
!   calls to calculate total fire emissions.
!
!kdhaynes, 10/17

use netcdf
implicit none

!input variables
character(len=256), intent(in) :: filename

!parameters
real, parameter :: gttg=1.E-12
real*8, parameter :: convertc=3600.*3.*12.*1.E-6
real*8, parameter :: convertco2=3600.*3.*44.*1.E-6
logical, parameter :: isglobal=.true.

!netcdf variables
integer :: status, ncid, dimid, varid

!area variables
integer :: nsib
real, dimension(:), allocatable :: lonsib, latsib
real*8, dimension(:), allocatable :: areasib

!data variables
integer :: ntime
real*8, dimension(:,:), allocatable :: firec, fireco2
real*8, dimension(:), allocatable :: tottemp
real*8 :: totemisC, totemisCO2

!misc variables
integer :: t

!------------------------------------------

   !open the file
   status = nf90_open(trim(filename),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Opening File: '
      print*,'  ',trim(filename)
      STOP
   ENDIF  

   !get lon/lat
   status = nf90_inq_dimid(ncid, 'nsib', dimid)
   IF (status .ne. nf90_noerr) THEN
       print*,'Unknown Dimension Name.'
       print*,'Stopping.'
       STOP
    ENDIF
    status = nf90_inquire_dimension(ncid, dimid, len=nsib)
   
    allocate(lonsib(nsib))
    status = nf90_inq_varid(ncid, 'lonsib', varid)
    IF (status .ne. nf90_noerr) THEN
          print*,'Unknown Longitude Name.'
          print*,'Stopping.'
          STOP
    ENDIF
    status = nf90_get_var(ncid, varid, lonsib)

    allocate(latsib(nsib))
    status = nf90_inq_varid(ncid, 'latsib', varid)
    IF (status .ne. nf90_noerr) THEN
          print*,'Unknown Latitude Name.'
          print*,'Stoping.'
          STOP
       ENDIF
    status = nf90_get_var(ncid, varid, latsib)

    allocate(areasib(nsib))
    call calc_sibarea(nsib, &
             lonsib, latsib, isglobal, areasib)

   !get time
   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)
   print('(a,3i6)'), '   Dimensions (nsib/ntime): ', &
        nsib,ntime

   !get the fire C emissions
   allocate(firec(nsib,ntime))
   status = nf90_inq_varid(ncid, 'emis_C', varid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Missing Fire C Emissions.'
      print*,'Stopping.'
      STOP
   ENDIF
   status = nf90_get_var(ncid, varid, firec)

   !get the fire CO2 emissions
   allocate(fireco2(nsib,ntime))
   status = nf90_inq_varid(ncid, 'emis_CO2', varid)
   IF (status .ne. nf90_noerr) THEN
      status = nf90_inq_varid(ncid, 'resp_fire', varid)
      IF (status .ne. nf90_noerr) THEN
          print*,'Missing Fire CO2 Emissions.'
          print*,'Stopping.'
          STOP
      ENDIF
   ENDIF
   status = nf90_get_var(ncid, varid, fireco2)
   status = nf90_close(ncid)

   !calculate total emissions
   allocate(tottemp(nsib))
   totemisC = 0.
   totemisCO2 = 0.
   do t=1,ntime
       tottemp(:) = firec(:,t)*convertc
       totemisC = totemisC + SUM(tottemp*areasib)

       tottemp(:) = fireco2(:,t)*convertco2
       totemisCO2 = totemisCO2 + SUM(tottemp*areasib)
   enddo
   totemisC = totemisC*gttg
   totemisCO2 = totemisCO2*gttg

   print*,'  Total C Loss (Tg):',totemisC
   print*,'  Total CO2 Emissions (Tg):',totemisCO2
   print*,''

   !deallocate arrays for next files
   deallocate(areasib,lonsib,latsib)
   deallocate(firec, fireco2)
   deallocate(tottemp)


end subroutine gfedtot_sib


!=========================================
subroutine calc_sibarea(  &
      nsib, lon, lat, isglobal, area)
!=========================================
!Subroutine to calculate sib areas.
!
!kdhaynes, 06/16
!
implicit none

! input variables
integer, intent(in) :: nsib
real, dimension(nsib), intent(in) :: lon
real, dimension(nsib), intent(in) :: lat
logical, intent(in) :: isglobal
real*8, dimension(nsib), intent(inout) :: area

! areal variables
real :: deltalon, deltalat
real*8 :: pi
real*8 :: lone, lonw, loner, lonwr
real*8 :: londiff
real*8 :: lats, latn, latsr, latnr
real*8 :: sinlatsr, sinlatnr
real*8 :: sinlatdiff
real*8 :: areatot

! parameters
logical, parameter :: area_check=.false.
real*8, parameter :: radius = 6370000.  !Earth radius (m)

! local variables
integer :: n
real :: minll, nextll
real, dimension(nsib) :: lltemp

!----------------------------
!set local variables
pi = acos(-1.0)
areatot = 0.

!calculate areal variables
IF (isglobal) THEN
   lltemp = lon
   minll = minval(lltemp)
   where(lltemp .eq. minll) lltemp=999.
   nextll = minval(lltemp)
   deltalon = (nextll-minll)*0.5
ELSE
   deltalon = 0.25
ENDIF

IF (isglobal) THEN
   lltemp = lat
   minll = minval(lltemp)
   where(lltemp .eq. minll) lltemp=999.
   nextll = minval(lltemp)
   deltalat = (nextll-minll)*0.5
ELSE
   deltalat = 0.25
ENDIF

do n=1,nsib
   lone=lon(n)+deltalon
   lonw=lon(n)-deltalon
   loner=lone*pi/180.
   lonwr=lonw*pi/180.
   londiff=loner-lonwr

   lats=lat(n)-deltalat
   latn=lat(n)+deltalat
   latsr=lats*pi/180.
   latnr=latn*pi/180.

   sinlatsr=sin(latsr)
   sinlatnr=sin(latnr)
   sinlatdiff=sinlatnr-sinlatsr

   area(n) = radius*radius*londiff*sinlatdiff
   areatot = areatot + area(n)

enddo !n

if (area_check) then
   print*,''
   print('(a,E15.8,a)'),'Area used vs actual:',areatot,'  0.50990436E+15 m2'
endif

 
end subroutine calc_sibarea
