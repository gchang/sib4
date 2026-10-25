!Program to calculate the mean annual maximum LAI.
!
!kdhaynes, 04/16
!
program MODIS_calclai

use netcdf
implicit none

!File specific information
character(len=100), parameter :: &
    specfile= './MODIS_calclai.txt'
integer, parameter :: nMODIS=2
integer, parameter :: nmaxyrs=20
integer, parameter :: startyr=2000
character(len=8), dimension(2), parameter :: &
    modisprefix=['MOD15A2_','MYD15A2_']
character(len=3), parameter :: &
    suffix='.nc'

!Input information
character(len=7) :: sitename
character(len=120) :: datadir, trash
character(len=120) :: datafile

!...data variables
integer :: ntime, yrmin, yrmax, numyrs
integer :: ndatamdone
real :: datamdone, meandata
real, dimension(nMODIS) :: datadone
real, dimension(:), allocatable :: time, data

real, dimension(nMODIS,nmaxyrs) :: datamax
real, dimension(nmaxyrs) :: timemax

!...netcdf variables
integer :: ncid, status
integer :: dimid, varid

!...misc variables
integer :: nf, t, yr
integer :: yrminref, yrmaxref, yrref

!--------------------------------------
!Read the spec file
open(unit=11,file=trim(specfile),form='formatted')
read(11,('(a7,a)')),sitename,trash
read(11,*) trash
read(11,*) datadir
close(11)

print*,''
print*,'Processing site: ',sitename

!Loop over max years
timemax(1) = startyr
do nf=1,nmaxyrs-1
   timemax(nf+1) = timemax(nf)+1
enddo

!Loop over MODIS files
datamax(:,:) = 0.
do nf=1,nMODIS
   !...read in data
   write(datafile,'(4a)') trim(datadir), trim(modisprefix(nf)), &
            trim(sitename), trim(suffix)
   status = nf90_open(trim(datafile),nf90_nowrite,ncid)
   status = nf90_inq_dimid( ncid,'time', dimid )   
   status = nf90_inquire_dimension( ncid, dimid, len=ntime)

   allocate(time(ntime))
   status = nf90_inq_varid( ncid, 'time', varid )
   status = nf90_get_var( ncid, varid, time )

   allocate(data(ntime))
   status = nf90_inq_varid( ncid, 'lai', varid )
   status = nf90_get_var( ncid, varid, data )
   status = nf90_close(ncid)   

   !...process data
   yrmin = floor(minval(time))
   yrmax = ceiling(maxval(time))-1
   numyrs = yrmax - yrmin + 1
   print('(a3,a3,a,i4,a,i4)'),'   ',modisprefix(nf),' years: ',yrmin, ' - ',yrmax
   
   do t=1,ntime
      yrref=floor(time(t)) - startyr + 1
      if (datamax(nf,yrref) .lt. data(t)) datamax(nf,yrref)=data(t)
   enddo

   yrminref = yrmin - startyr + 1
   yrmaxref = yrmax - startyr + 1
   datadone(nf) = sum(datamax(nf,yrminref:yrmaxref)) / numyrs
   !print('(a,f8.4)'),'     Mean LAI Maximum: ',datadone(nf)
  
   !...deallocate arrays
   deallocate(time,data)
enddo

print*,''
print('(a)'),'Year    MOD_LAI   MYD_LAI   MEAN'
datamdone=0.
ndatamdone=0
do yr=0,nmaxyrs-1
   if ((datamax(1,yr+1) .gt. 0.) .and. (datamax(2,yr+1) .gt. 0.)) then
      meandata = sum(datamax(:,yr+1))/2.
   elseif (datamax(1,yr+1) .eq. 0.) then
      meandata = datamax(2,yr+1)
   elseif (datamax(2,yr+1) .eq. 0.) then
      meandata = datamax(1,yr+1)
   else
      meandata=0.
   endif

   if (meandata .gt. 0.) then
      print('(i4,3f10.4)'), startyr+yr, datamax(1,yr+1), datamax(2,yr+1), meandata
      datamdone = datamdone + meandata
      ndatamdone = ndatamdone + 1
   endif
enddo
datamdone = datamdone / ndatamdone

print*,''
print('(a,3f10.4)'),'MEAN', datadone(1), datadone(2), datamdone
print*,''

end
