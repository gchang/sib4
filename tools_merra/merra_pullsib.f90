!Program to read in gridded MERRA monthly files (lonxlat),
!and write out monthly SiB driver files (nsib).
!
!To compile, use:
!>pgf90 merra_pullsib.f90 -L/usr/local/netcdf3-pgi/lib -lnetcdf 
!   -I/usr/local/netcdf3-pgi/include -o merra_pullsib
!
!>-or-
!>make merra_pullsib
!
!kdhaynes, 2019/03
!
program  merra_pullsib
  
use netcdf

implicit none

!!!!! user defined variables
integer, parameter :: startYr=2018, stopYr=2018
integer, parameter :: startmon=12, stopmon=12

character(len=100), parameter :: inputdir= &
     '/spinning-scratch/kdhaynes/data/merra2_proc/'
character(len=7), parameter :: inprefix='merra2_'

character(len=100), parameter :: outputdir= &
     '/ssd-scratch/kdhaynes/sib4_driver/global/merra2/'
character(len=24), parameter :: outprefix='merra2_0.5deg_'

character(len=100), parameter :: sibfile= &
     '/ssd-scratch/kdhaynes/sib4_driver/global/sib_vs_0.5deg.nc'

!!!!! MERRA-specific parameters
integer, parameter :: nvar = 10
character(len=12), parameter, dimension(nvar) :: &
   varname = ['tm          ','sh          ','ps          ', &
              'spdm        ','swd         ','lwd         ', &
              'lspr        ','cupr        ','lspr_scaled ', &
              'cupr_scaled ']
character(len=3), parameter, dimension(12) :: &
   monname = ['Jan','Feb','Mar','Apr','May','Jun', &
              'Jul','Aug','Sep','Oct','Nov','Dec']

!input variables
integer :: nlon, nlat, ntime
real, dimension(:), allocatable :: longitude, latitude
real*8, dimension(:), allocatable :: time
integer, dimension(:), allocatable :: year, month, doy, day
real, dimension(:), allocatable :: hour
real, dimension(:,:,:), allocatable :: values_temp
real, dimension(:,:,:,:), allocatable :: values

!output variables
integer :: nsib
real, dimension(:), allocatable :: lonsib, latsib
real, dimension(:,:), allocatable :: valuessib_temp
real, dimension(:,:,:), allocatable :: valuessib

!comparison variables
real :: minlon, minlat
real, dimension(:), allocatable :: londiff, latdiff
integer, dimension(:), allocatable :: lonref, latref

!netcdf variables
integer ncid, dimid, varid, varidin(nvar)
integer ncidsib, varidsib(nvar)
integer status
integer timedid, landid
integer timeid, latid, lonid
integer yrid, monid, doyid, dayid, hrid
integer valuesid(nvar)
character(len=30) :: monunit, dayunit

!misc variables
integer j,k,v
integer mon, yr
integer mystartmon, mystopmon
character(len=120) :: filename
character(len=20) :: varname_temp

!---------------------------------------------------
!Read in the SiB lat/lon information for land points
call check ( nf90_open( trim(sibfile), nf90_nowrite, ncid ) )
print*,''
print('(a)'),'Getting SiB Information: '
print('(a,a)'),'  ',trim(sibfile)
print*,''

call check ( nf90_inq_dimid( ncid, 'nsib', dimid ) )
call check ( nf90_inquire_dimension( ncid, dimid, len=nsib ) )

allocate(lonsib(nsib), latsib(nsib))
call check ( nf90_inq_varid( ncid, 'lonsib', varid ) )
call check ( nf90_get_var( ncid, varid, lonsib ) )

call check ( nf90_inq_varid( ncid, 'latsib', varid ) )
call check ( nf90_get_var( ncid, varid, latsib ) )

call check ( nf90_close(ncid) )

!Process the files:
DO yr=startyr, stopyr
   mystartmon=1
   mystopmon=12
   if (yr .eq. startyr) mystartmon=startmon
   if (yr .eq. stopyr) mystopmon=stopmon

   DO mon=mystartmon, mystopmon

      print('(a,a,a,i4)'), 'Processing ',monname(mon),' ',yr
      
      !Read in MERRA lat/lon
      write(filename,'(a,a,i4.4,i2.2,a3)') &
           trim(inputdir), trim(inprefix), yr, mon, '.nc'
      print('(a)'),' Opening MERRA file: '
      print('(a,a)'),'  ',trim(filename)

      call check ( nf90_open( trim(filename), nf90_nowrite, ncid ) )

      IF ((yr .EQ. startyr) .AND. (mon .EQ. startmon)) THEN
          call check ( nf90_inq_dimid( ncid, 'lon', dimid ) )
          call check ( nf90_inquire_dimension( ncid, dimid, len=nlon))
          call check ( nf90_inq_dimid( ncid, 'lat', dimid ) )
          call check ( nf90_inquire_dimension( ncid, dimid, len=nlat))

          allocate(longitude(nlon),latitude(nlat))
          call check ( nf90_inq_varid( ncid, 'longitude', varid ) )
          call check ( nf90_get_var( ncid, varid, longitude ) )
          call check ( nf90_inq_varid( ncid, 'latitude', varid ) )
          call check ( nf90_get_var( ncid, varid, latitude ) )
      ENDIF

      call check ( nf90_inq_dimid( ncid, 'time', dimid ) )
      call check ( nf90_inquire_dimension( ncid, dimid, len=ntime ) )
      allocate(time(ntime))
      allocate(year(ntime), month(ntime), doy(ntime), &
               day(ntime), hour(ntime))
      call check ( nf90_inq_varid( ncid, 'time', varid ) )
      call check ( nf90_get_var( ncid, varid, time ) )
      call check ( nf90_inq_varid( ncid, 'year', varid ) )
      call check ( nf90_get_var( ncid, varid, year ) )
      call check ( nf90_inq_varid( ncid, 'month', varid ) )
      call check ( nf90_get_var( ncid, varid, month ) )
      call check ( nf90_inq_varid( ncid, 'doy', varid ) )
      call check ( nf90_get_var( ncid, varid, doy))
      call check ( nf90_inq_varid( ncid, 'day', varid ) )
      call check ( nf90_get_var( ncid, varid, day ) )
      call check ( nf90_inq_varid( ncid, 'hour', varid ) )
      call check ( nf90_get_var( ncid, varid, hour ) )

      allocate(values_temp(nlon,nlat,ntime))
      allocate(values(nvar,nlon,nlat,ntime))
      DO v=1,nvar
         varname_temp = varname(v)
         call check ( nf90_inq_varid( ncid, varname_temp, varidin(v) ) )
         call check ( nf90_get_var( ncid, varidin(v), values_temp) )
         values(v,:,:,:) = values_temp(:,:,:)
      ENDDO

      !Calculate SiB4 points and references
      IF ((yr .EQ. startyr) .AND. (mon .EQ. startmon)) THEN
          allocate(lonref(nsib),latref(nsib))
          allocate(londiff(nlon),latdiff(nlat))
         
          minlon=0.
          minlat=0.
          do j=1,nsib
             londiff = abs(longitude-lonsib(j))
             latdiff = abs(latitude-latsib(j))
             minlon=minval(londiff)
             minlat=minval(latdiff)

             do k=1,nlon
                if (londiff(k) .eq. minlon) lonref(j)=k
             enddo
             do k=1,nlat
                if (latdiff(k) .eq. minlat) latref(j)=k
             enddo
          enddo !nsib
      ENDIF

      !Save the data on SiB gridpoints
      allocate(valuessib_temp(nsib,ntime))
      allocate(valuessib(nvar,nsib,ntime))
      DO j=1,nsib
         valuessib(:,j,:) = values(:,lonref(j),latref(j),:)
      ENDDO !nsib

      !Write out data to netcdf file   
      write(filename,'(a,a,I4.4,I2.2,a3)') &
          trim(outputdir), trim(outprefix), yr, mon, '.nc'
      print('(a)'),' Writing output file: '
      print('(a,a)'),'  ',trim(filename)

      call check ( nf90_create(trim(filename), nf90_64bit_offset, ncidsib) )
      call check ( nf90_def_dim(ncidsib, 'nsib', nsib, landid ) )
      call check ( nf90_def_dim(ncidsib, 'time', ntime, timedid) )

      call check ( nf90_def_var(ncidsib, 'lonsib', nf90_float, &
                   (/landid/), lonid) )
      call check ( nf90_put_att(ncidsib,lonid,'long_name','Vector Longitude'))
      call check ( nf90_put_att(ncidsib,lonid,'units','degrees_east'))

      call check ( nf90_def_var(ncidsib, 'latsib', nf90_float, &
                   (/landid/), latid) )
      call check ( nf90_put_att(ncidsib,latid,'long_name','Vector Latitude'))
      call check ( nf90_put_att(ncidsib,latid,'units','degrees_north'))

      call check ( nf90_def_var(ncidsib, 'time', nf90_double, &
                   (/timedid/), timeid) )
      call check ( nf90_put_att(ncidsib,timeid,'long_name','Time'))
      call check ( nf90_put_att(ncidsib,timeid,'units','years since 0000-00-00'))

      call check ( nf90_def_var(ncidsib, 'year', nf90_int, &
                   (/timedid/), yrid) )
      call check ( nf90_put_att(ncidsib,yrid,'long_name','Year'))
      call check ( nf90_put_att(ncidsib,yrid,'units','year since 0000-00-00'))

      call check ( nf90_def_var(ncidsib, 'month', nf90_int, &
                   (/timedid/), monid) )
      write(monunit,'(a,i4,a)') &
           'month since ', yr, '-00-00'
      call check ( nf90_put_att(ncidsib,monid,'long_name','Month'))
      call check ( nf90_put_att(ncidsib,monid,'units',monunit) )

      call check ( nf90_def_var(ncidsib, 'doy', nf90_int, &
                   (/timedid/), doyid) )
      write(dayunit,'(a,i4.4,a)') &
           'days since ', yr, '-00-00'
      call check ( nf90_put_att(ncidsib,doyid,'long_name','Day Of Year'))
      call check ( nf90_put_att(ncidsib,doyid,'units',dayunit))

      call check ( nf90_def_var(ncidsib, 'day', nf90_int, &
                   (/timedid/), dayid) )
      write(dayunit,'(a,i4.4,a,i2.2,a)') &
           'days since ', yr, '-', mon, '-00'
      call check ( nf90_put_att(ncidsib,dayid,'long_name','Day Of Month'))
      call check ( nf90_put_att(ncidsib,dayid,'units',dayunit))

      call check ( nf90_def_var(ncidsib, 'hour', nf90_float, &
                   (/timedid/), hrid) )
      call check ( nf90_put_att(ncidsib,hrid,'long_name','Hour'))
      call check ( nf90_put_att(ncidsib,hrid,'units','hour of day (GMT)'))

      do j=1, nvar
         varname_temp = varname(j)
         call check ( nf90_def_var(ncidsib, trim(varname_temp), &
                        nf90_float, (/landid,timedid/), varidsib(j)) )
         call check ( nf90_copy_att(ncid, varidin(j), &
                        'long_name', ncidsib, varidsib(j)))
         call check ( nf90_copy_att(ncid, varidin(j), &
                        'units', ncidsib, varidsib(j)))
         status = nf90_copy_att(ncid, varidin(j), &
                        'title', ncidsib, varidsib(j))
      enddo

      status = nf90_enddef(ncidsib)

      call check ( nf90_put_var(ncidsib, timeid, time) )
      call check ( nf90_put_var(ncidsib, lonid, lonsib) )
      call check ( nf90_put_var(ncidsib, latid, latsib) )
      call check ( nf90_put_var(ncidsib, yrid, year) )
      call check ( nf90_put_var(ncidsib, monid, month) )
      call check ( nf90_put_var(ncidsib, doyid, doy) )
      call check ( nf90_put_var(ncidsib, dayid, day) )
      call check ( nf90_put_var(ncidsib, hrid, hour) )

      do j=1, nvar
          valuessib_temp(:,:) = valuessib(j,:,:)
          call check ( nf90_put_var(ncidsib, varidsib(j), valuessib_temp) )
      enddo

      call check ( nf90_close(ncid) )
      call check ( nf90_close(ncidsib) )

     deallocate(time,year,month,doy,day,hour)
     deallocate(values, values_temp)
     deallocate(valuessib, valuessib_temp)

   enddo !mon
enddo !yr

print('(a)'), 'Finished processing.'
print*,''
	
end program merra_pullsib

!***********************************************************************
!***********************************************************************

subroutine check(status)

use netcdf
use typeSizes

implicit none

integer, intent(in) :: status

   if (status /= nf90_noerr) then
      stop "Error with netcdf.  Stopped."
   endif

end subroutine check
