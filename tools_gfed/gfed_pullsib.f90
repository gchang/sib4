!Program to read in gridded GFED4 monthly files (lonxlat),
!and write out monthly SiB driver files (nsib).
!
!To compile, use:
!>pgf90 gfed4_pullsib.f90 -L/usr/local/netcdf4-pgi/lib -lnetcdf -I/usr/local/netcdf4-pgi/include -o merra_pullsib
!
!-or- (if defined)
!>f90nc gfed_pullsib.f90
!>f90nco gfed_pullsib.f90 gfed_pullsib
!
!kdhaynes, 2018/10
!
program  gfed_pullsib
  
use netcdf

implicit none

!!!!! user defined variables
integer, parameter :: startYr=2018, stopYr=2018
integer, parameter :: startmon=1, stopmon=12

character(len=100), parameter :: inputdir= &
     '/Users/kdhaynes/Test/'
character(len=24), parameter :: inprefix='gfed4.1s_emis_'

character(len=100), parameter :: outputdir= &
     '/Users/kdhaynes/Research/sib4_driver/global/gfed4/'
character(len=24), parameter :: outprefix='gfed4_0.5deg_'

character(len=100), parameter :: sibfile= &
     '/Users/kdhaynes/Research/sib4_driver/global/sib_vs_0.5deg.nc'

!!!!! GFED4-specific parameters
integer, parameter :: ntperday = 8
integer, parameter :: nvar = 2
real*8, parameter :: myconvertc = 1./12./8./3./3600.*1.E6
real*8, parameter :: myconvertco2 = 1./44./8./3./3600.*1.E6

character(len=12), parameter, dimension(nvar) :: &
   varnamein = ['emis_C      ','emis_CO2    ']
character(len=12), parameter, dimension(nvar) :: &
   varnameout= ['emis_C      ','emis_CO2    ']
character(len=30), parameter, dimension(nvar) :: &
   vartitleout= ['Total Carbon Fire Emissions', &
                 'CO2 Fire Emissions         ']
character(len=30), parameter, dimension(nvar) :: &
   varunitsout = ['micromoles C/m2/s         ', &
                  'micromoles C/m2/s         '] 

character(len=3), parameter, dimension(12) :: &
   monname = ['Jan','Feb','Mar','Apr','May','Jun', &
              'Jul','Aug','Sep','Oct','Nov','Dec']
integer, parameter, dimension(12) :: &
    dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
integer, parameter, dimension(12) :: &
    doypermon=[1,32,60,91,121,152,182,213,244,274,305,335]

!input variables
integer :: nlon, nlat, ntime, ndays, ndate
real, dimension(:), allocatable :: longitude, latitude
integer*2, dimension(:,:), allocatable :: itime
integer*2, dimension(:), allocatable :: &
   year, month, day, doy
real, dimension(:), allocatable :: hour
real*8, dimension(:), allocatable :: time
real*8, dimension(:,:,:), allocatable :: values_temp
real*8, dimension(:,:,:,:), allocatable :: values
real*8, dimension(:,:), allocatable :: firec,fireco2

!output variables
integer :: nsib
real, dimension(:), allocatable :: lonsib, latsib
real, dimension(:,:), allocatable :: valuessib_temp
real, dimension(:,:,:), allocatable :: valuessib

!comparison variables
real :: minlon, minlat
real, dimension(:), allocatable :: londiff, latdiff
integer, dimension(:), allocatable :: lonref, latref

!monthly total variables
real*8 totc, totco2

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
integer j,k,v, t
integer mon, yr
integer mystartmon, mystopmon
integer mydoy, myyrlen
real*8 doyfrac
character(len=120) :: filename
character(len=12) :: varname_temp

!---------------------------------------------------
!Read in the SiB lat/lon information for land points
call check ( nf90_open( trim(sibfile), nf90_nowrite, ncid ) )
print*,''
print('(a)'),'Getting SiB Information: '
print('(a,a)'),'  ',trim(sibfile)

call check ( nf90_inq_dimid( ncid, 'nsib', dimid ) )
call check ( nf90_inquire_dimension( ncid, dimid, len=nsib ) )

allocate(lonsib(nsib), latsib(nsib))
call check ( nf90_inq_varid( ncid, 'lonsib', varid ) )
call check ( nf90_get_var( ncid, varid, lonsib ) )

call check ( nf90_inq_varid( ncid, 'latsib', varid ) )
call check ( nf90_get_var( ncid, varid, latsib ) )

call check ( nf90_close(ncid) )
print('(a,i8)'),'  Number of Land Points: ',nsib
print*,''

!Process the files:
DO yr=startyr, stopyr
   myyrlen=365
   if (mod(yr,4) == 0) myyrlen=myyrlen+1

   mystartmon=1
   mystopmon=12
   if (yr .eq. startyr) mystartmon=startmon
   if (yr .eq. stopyr) mystopmon=stopmon

   DO mon=mystartmon, mystopmon

      print('(a,a,a,i4)'), 'Processing ',monname(mon),' ',yr
      !Calculate day of year
      mydoy = doypermon(mon)-1
      if ((mod(yr,4) == 0)  .and. (mon .ge. 2)) mydoy=mydoy+1

      !Read in GFED lat/lon
      write(filename,'(a,a,i4.4,i2.2,a3)') &
           trim(inputdir), trim(inprefix), yr, mon, '.nc'
      print('(a)'),' Opening GFED file: '
      print('(a,a)'),'  ',trim(filename)

      status = nf90_open( trim(filename), nf90_nowrite, ncid )
      IF (status .ne. nf90_noerr) THEN
          print*,' Error Opening File!!'
          RETURN
      ENDIF

      IF ((yr .EQ. startyr) .AND. (mon .EQ. startmon)) THEN
          status = nf90_inq_dimid( ncid, 'longitude', dimid )
          IF (status .ne. nf90_noerr) THEN
             status = nf90_inq_dimid(ncid,'lon',dimid)
             IF (status .ne. nf90_noerr) THEN
                status = nf90_inq_dimid(ncid,'nlon',dimid)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Finding Longitude Dimension'
                   RETURN
                ENDIF
             ENDIF
          ENDIF
          status = nf90_inquire_dimension( ncid, dimid, len=nlon)

          status = nf90_inq_dimid( ncid, 'latitude', dimid )
          IF (status .ne. nf90_noerr) THEN
             status = nf90_inq_dimid(ncid,'lat',dimid)
             IF (status .ne. nf90_noerr) THEN
                status = nf90_inq_dimid(ncid,'nlat',dimid)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Finding Latitude Dimension'
                   RETURN
                ENDIF
             ENDIF
         ENDIF
         status = nf90_inquire_dimension( ncid, dimid, len=nlat)

          allocate(longitude(nlon),latitude(nlat))
          status = nf90_inq_varid( ncid, 'longitude', varid )
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Finding Longitude'
             RETURN
          ENDIF
          status = nf90_get_var( ncid, varid, longitude )

          status = nf90_inq_varid( ncid, 'latitude', varid )
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Finding Latitude'
             RETURN
          ENDIF
          status = nf90_get_var( ncid, varid, latitude )
      ENDIF

      status = nf90_inq_dimid( ncid, 'time', dimid )
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Finding Time Dimension'
         RETURN
      ENDIF
      status = nf90_inquire_dimension( ncid, dimid, len=ntime )
      
      status = nf90_inq_dimid( ncid, 'date_comps', dimid )
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Finding Date Dimension'
          RETURN
      ENDIF
      status = nf90_inquire_dimension( ncid, dimid, len=ndate )

      allocate(itime(ndate,ntime), time(ntime))
      allocate(year(ntime), month(ntime), doy(ntime), &
               day(ntime), hour(ntime))
      status = nf90_inq_varid( ncid, 'int_time', varid )
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Finding int_time array'
         RETURN
      ENDIF
      status = nf90_get_var( ncid, varid, itime )

      year(:) = itime(1,:)
      month(:) = itime(2,:)
      day(:) = itime(3,:)
      hour(:) = itime(4,:) + itime(5,:)/60.

      do t=1,ntime 
          doy(t) = mydoy+day(t)
          doyfrac = (doy(t)-1) + hour(t)/24.
          time(t) = yr + doyfrac/myyrlen
      enddo

      allocate(values_temp(nlon,nlat,ntime))
      allocate(values(nvar,nlon,nlat,ntime))
      DO v=1,nvar
         varname_temp = varnamein(v)
         call check ( nf90_inq_varid( ncid, varname_temp, varidin(v) ) )
         call check ( nf90_get_var( ncid, varidin(v), values_temp) )
         values(v,:,:,:) = values_temp(:,:,:)
      ENDDO
      call check (nf90_close(ncid))

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

      !Save the data on SiB gridpoints, 
      !  changing to proper times/units
      if (mod(ntime,ntperday) /= 0) then
          print*,'Odd number of timesteps.'
          RETURN
      endif
      ndays = ntime/ntperday
      allocate(firec(nsib,ntime))
      allocate(fireco2(nsib,ntime))
      DO j=1,nsib
         DO t=1,ntime
            firec(j,t) = &
                    values(1,lonref(j),latref(j),t)*myconvertc
            fireco2(j,t) = &
                    values(2,lonref(j),latref(j),t)*myconvertco2
          ENDDO
       ENDDO !nsib

      !Write out data to netcdf file   
      write(filename,'(a,a,I4.4,I2.2,a3)') &
          trim(outputdir), trim(outprefix), yr, mon, '.nc'
      print('(a)'),' Writing output file: '
      print('(a,a)'),'  ',trim(filename)

      status = nf90_create(trim(filename), nf90_64bit_offset, ncidsib)
      if (status .ne. nf90_noerr) then
         print*,'Error Creating File: '
         print*,'  ',trim(filename)
         RETURN
      endif

      status = nf90_def_dim(ncidsib, 'nsib', nsib, landid )
      if (status .ne. nf90_noerr) then
         print*,'Error Defining Landpoint.'
         RETURN
      endif
      status = nf90_def_dim(ncidsib, 'time', ntime, timedid)
      IF (status .ne. nf90_noerr) then
         print*,'Error Defining Time.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'lonsib', nf90_float, &
                   (/landid/), lonid)
      status = nf90_put_att(ncidsib,lonid,'long_name','Vector Longitude')
      status = nf90_put_att(ncidsib,lonid,'units','degrees_east')
      IF (status .ne. nf90_noerr) then
         print*,'Error Defining Longitude.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'latsib', nf90_float, &
                   (/landid/), latid)
      status = nf90_put_att(ncidsib,latid,'long_name','Vector Latitude')
      status = nf90_put_att(ncidsib,latid,'units','degrees_north')
      IF (status .ne. nf90_noerr) then
          print*,'Error Defining Latitude.'
          RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'time', nf90_double, &
                   (/timedid/), timeid)
      status = nf90_put_att(ncidsib,timeid,'long_name','Time')
      status = nf90_put_att(ncidsib,timeid,'units',&
                    'years since 0000-00-00')
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Defining Time.'
         RETURN
      ENDIF
      
      status = nf90_def_var(ncidsib, 'year', nf90_int, &
                   (/timedid/), yrid)
      status = nf90_put_att(ncidsib,yrid,'long_name','Year')
      status = nf90_put_att(ncidsib,yrid,'units','year since 0000-00-00')
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Defining Year.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'month', nf90_int, &
                   (/timedid/), monid)
      write(monunit,'(a,i4,a)') &
           'month since ', yr, '-00-00'
      status = nf90_put_att(ncidsib,monid,'long_name','Month')
      status = nf90_put_att(ncidsib,monid,'units',monunit)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Defining Month.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'doy', nf90_int, &
                   (/timedid/), doyid)
      write(dayunit,'(a,i4.4,a)') &
           'days since ', yr, '-00-00'
      status = nf90_put_att(ncidsib,doyid,'long_name','Day Of Year')
      status = nf90_put_att(ncidsib,doyid,'units',dayunit)
      IF (status .ne. nf90_noerr) then
         print*,'Error Defining Day of Year.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'day', nf90_int, &
                   (/timedid/), dayid)
      write(dayunit,'(a,i4.4,a,i2.2,a)') &
           'days since ', yr, '-', mon, '-00'
      status = nf90_put_att(ncidsib,dayid,'long_name','Day Of Month')
      status = nf90_put_att(ncidsib,dayid,'units',dayunit)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Defining Day of Month.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, 'hour', nf90_float, &
                   (/timedid/), hrid)
      status = nf90_put_att(ncidsib,hrid,'long_name','Hour')
      status = nf90_put_att(ncidsib,hrid,'units','hour of day (GMT)')
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Defining Hour of Day.'
         RETURN
      ENDIF

      status = nf90_def_var(ncidsib, trim(varnameout(1)), &
                 nf90_double, (/landid,timedid/), varidsib(1))
      status = nf90_put_att(ncidsib, varidsib(1), &
                        'long_name', trim(vartitleout(1)))
      status = nf90_put_att(ncid, varidsib(1), &
                        'units', trim(varunitsout(1)))
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Defining Variable: ',trim(varnameout(1))
          RETURN
      ENDIF

      status = nf90_def_var(ncidsib, trim(varnameout(2)), &
                 nf90_double, (/landid,timedid/), varidsib(2))
      status = nf90_put_att(ncidsib, varidsib(2), &
                        'long_name', trim(vartitleout(2)))
      status = nf90_put_att(ncid, varidsib(2), &
                        'units', trim(varunitsout(2)))
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Defining Variable: ',trim(varnameout(2))
          RETURN
      ENDIF
      status = nf90_enddef(ncidsib)

      status = nf90_put_var(ncidsib, timeid, time)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Writing Time.'
         RETURN
      ENDIF

      status = nf90_put_var(ncidsib, lonid, lonsib)
      status = nf90_put_var(ncidsib, latid, latsib)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Writing Lon/Lat.'
         RETURN
      ENDIF

      status = nf90_put_var(ncidsib, yrid, year)
      status = nf90_put_var(ncidsib, monid, month)
      status = nf90_put_var(ncidsib, doyid, doy)
      status = nf90_put_var(ncidsib, dayid, day)
      status = nf90_put_var(ncidsib, hrid, hour)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Writing Time Information.'
         RETURN
      ENDIF

     status = nf90_put_var(ncidsib, varidsib(1), firec)
     IF (status .ne. nf90_noerr) THEN
         print*,'Error Writing Variable: ',trim(varnameout(1))
         RETURN
     ENDIF

     status = nf90_put_var(ncidsib, varidsib(2), fireco2)
     IF (status .ne. nf90_noerr) THEN
         print*,'Error Writing Variable: ',trim(varnameout(2))
         RETURN
     ENDIF
     status = nf90_close(ncidsib)

     deallocate(itime,time,year,month,doy,day,hour)
     deallocate(values, values_temp)
     deallocate(firec, fireco2)

   enddo !mon
enddo !yr

print('(a)'), 'Finished processing.'
print*,''
	
end program gfed_pullsib

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
