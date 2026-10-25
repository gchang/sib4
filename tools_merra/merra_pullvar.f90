!Program to read in daily MERRA files in netcdf4,
!and to write out gridded driver files in netcdf4,
!including only the variables necessary for SiB.
!
!To compile, use:
!>make merra_pullvar
!
!kdhaynes, 2019/03
program  merra_pullvar
  
use netcdf

implicit none

!!!!! user defined variables
integer, parameter :: startYr=2018, stopYr=2018
integer, parameter :: startMon=1, stopMon=12
character*100, parameter :: filedir='/spinning-scratch/kdhaynes/data/merra_orig/'
character*100, parameter :: outdir='/spinning-scratch/kdhaynes/data/merra_proc/'
character*20, parameter :: outprefix='merra_'

!!!!! MERRA defined variables for 2/3-degree by 1/2-degree hourly data
integer, parameter :: numtimesteps = 24
integer, parameter :: lat = 361
integer, parameter :: lon = 576
integer, parameter :: startDay=1
real, parameter :: startHr = 0.5

real, parameter :: initLon = -180.0
real, parameter :: initLat = -90.0

character*30, parameter :: fname_flx = 'MERRA2_400.tavg1_2d_flx_Nx.'
character*30, parameter :: fname_rad = 'MERRA2_400.tavg1_2d_rad_Nx.'
character*30, parameter :: fname_slv = 'MERRA2_400.tavg1_2d_slv_Nx.'
character*4,  parameter :: suffix = '.nc4'

integer, parameter :: nvar_flx = 4
integer, parameter :: nvar_rad = 3
integer, parameter :: nvar_slv = 5
integer, parameter :: nvar_out = 8

! file variables
character(len=100) filename
character(len=20), dimension(nvar_flx) :: varname_flx, units_flx
character(len=300), dimension(nvar_flx) :: description_flx
character(len=20), dimension(nvar_rad) :: varname_rad, units_rad
character(len=300), dimension(nvar_rad) :: description_rad
character(len=20), dimension(nvar_slv) :: varname_slv, units_slv
character(len=300), dimension(nvar_slv) :: description_slv
character(len=20), dimension(nvar_out) :: varname_out, units_out
character(len=300), dimension(nvar_out) :: description_out

real*8, dimension(lon) :: longitudein
real*8, dimension(lat) :: latitudein
real*4, dimension(lon) :: longitude
real*4, dimension(lat) :: latitude
real, dimension(lon,lat,numtimesteps) :: valuesday
real, dimension(:,:,:,:), allocatable :: &
   valuesmon_flx, valuesmon_rad, valuesmon_slv

!output variables
real, dimension(:,:,:), allocatable :: valuesout_temp
real, dimension(:,:,:,:), allocatable :: valuesout

!netcdf variables
integer ncid, dimid, varid
integer timedid, latdid, londid
integer timeid, latid, lonid
integer yrid, monid, doyid, dayid, hrid
integer varid_out(nvar_out)
integer status, dims(3)


!time variables
integer :: ntime
real timeinc,dayfrac
real*8, dimension(:), allocatable :: time
integer, dimension(:), allocatable :: &
      timeyr, timemon, timeday, timedoy
real, dimension(:), allocatable :: timehr
integer, dimension(12)  ::  dayspermon, numdays  
character*13, dimension(12) :: monnames

!misc variables
integer j,k
integer yr, mon, day, var
integer count, countstart, countstop

!!!!!data values
DATA varname_flx /'PRECLSC','PRECANV','PRECCON','PRECSNO'/
DATA varname_rad /'ALBEDO','SWGDN','LWGAB'/
DATA varname_slv /'T2M','QV2M','PS','U10M','V10M'/

DATA varname_out /'tm','sh','ps','spdm','lspr','cupr','swd','lwd'/
DATA dayspermon /31,28,31,30,31,30,31,31,30,31,30,31/
DATA monnames /'Jan','Feb','Mar','Apr','May','Jun', &
               'Jul','Aug','Sep','Oct','Nov','Dec'/
!!!!!

!Set misc values
timeinc = numtimesteps/24.

!Read in MERRA lat/lon
write(filename,'(a,a,i4.4,2i2.2,a)') &
     trim(filedir), &  !startYr,'/', &
     trim(fname_rad), startYr, startMon, startDay, suffix
!print*,'Opening MERRA file: ',trim(filename)

status = nf90_open(trim(filename), nf90_nowrite, ncid)
if (status .ne. nf90_noerr) then
   print*,'Error Opening File.'
   print*,'File Name: '
   print*,' ',trim(filename)
   print*,'Status: ',status
   stop
endif
status = nf90_inq_varid(ncid,'lon',varid)
status = nf90_get_var(ncid,varid,longitudein)
status = nf90_inq_varid(ncid,'lat',varid)
status = nf90_get_var(ncid,varid,latitudein)
status = nf90_close(ncid)


!Process all specified years
valuesday=0.
print*,''
do yr = startYr,stopYr

   numdays = dayspermon
   if (mod(yr,4) == 0) numdays(2) = numdays(2) + 1

   do mon = startMon, stopMon

      print('(a,a3,a,i4)'),'Processing ',monnames(mon),' ',yr 
      ntime = numtimesteps*numdays(mon)
      allocate(valuesmon_flx(nvar_flx,lon,lat,ntime))
      allocate(valuesmon_rad(nvar_rad,lon,lat,ntime))
      allocate(valuesmon_slv(nvar_slv,lon,lat,ntime))

      valuesmon_flx = 0.
      valuesmon_rad = 0.
      valuesmon_slv = 0.

      allocate(time(ntime),timeyr(ntime),timemon(ntime))
      allocate(timedoy(ntime),timeday(ntime),timehr(ntime))
       
      time = 0.
      timeyr = 0.
      timemon = 0.
      timedoy = 0.
      timeday = 0.
      timehr = 0.

      count=1
      do day = 1, numdays(mon)

         !Set time variables
         countstart=count
         do j=1, numtimesteps
            timehr(countstart) = startHr + (j-1)*timeinc
            timeyr(countstart) = yr
            timemon(countstart) = mon
            timeday(countstart) = day
            if (mon .eq. 1) then 
               timedoy(countstart) = day
            else
               timedoy(countstart) = day + sum(dayspermon(1:mon-1))
            endif
            dayfrac = timedoy(countstart) + timehr(countstart)/24.
            timedoy(countstart) = dayfrac
            time(countstart) = yr + dayfrac/(sum(numdays))

            countstart = countstart+1
         enddo
         countstart=count
         countstop=count+numtimesteps-1
    
         !Open flx file
         write(filename,'(a,a,i4.4,2i2.2,a)') &
               trim(filedir), &  !yr, '/', & 
               trim(fname_flx), yr, mon, day, suffix
         !print*, 'READING: ', trim(filename)

         status = nf90_open(trim(filename),nf90_nowrite,ncid)
         if (status .ne. nf90_noerr) then
             print*,'Error Opening File.'
             print*,' ',trim(filename)
             print*,'Status: ',status
             stop
         endif
	 do var=1,nvar_flx
	    status = nf90_inq_varid(ncid, trim(varname_flx(var)), varid)
            !get variable description and units only once
            if (day .eq. 1) then
               status = nf90_get_att(ncid,varid,'long_name',description_flx(var))
               status = nf90_get_att(ncid,varid,'units',units_flx(var))
           endif
            status = nf90_get_var(ncid,varid,valuesday)
            if (status < 0) then
                print*,'Error reading flx data, status =', status
	        print*,var,trim(varname_flx(var))
	        stop
            endif

            valuesmon_flx(var,:,:,countstart:countstop) = &
                 valuesday(:,:,1:numtimesteps)

	 enddo  !var
         status = nf90_close(ncid)

         !Open rad file
         write(filename,'(a,a,i4.4,2i2.2,a)') &
               trim(filedir), &  !yr, '/', &
               trim(fname_rad), yr, mon, day, suffix
         !print*,'READING: ',trim(filename)

         status = nf90_open(trim(filename), nf90_nowrite, ncid)
         if (status .ne. nf90_noerr) then
            print*,'Error Opening File.'
            print*,' ',trim(filename)
            print*,'Status: ',status
         endif
         do var=1,nvar_rad
            status = nf90_inq_varid(ncid,trim(varname_rad(var)),varid)

            !get variable description and units only once
            if (mon .eq. 1 .and. day .eq. 1) then
               status = nf90_get_att(ncid,varid,'long_name',description_rad(var))
               status = nf90_get_att(ncid,varid,'units',units_rad(var))
            endif
            status = nf90_get_var(ncid,varid,valuesday)
            if (status < 0) then
               print*,'Error reading rad data, status=',status
               print*,var,trim(varname_rad(var))
               stop
            endif

            valuesmon_rad(var,:,:,countstart:countstop) = &
                valuesday(:,:,1:numtimesteps)

         enddo !var
         status = nf90_close(ncid)

         !Open slv file
         write(filename,'(a,a,i4.4,2i2.2,a)') &
               trim(filedir), &  !yr, '/', &
               trim(fname_slv), yr, mon, day, suffix
         !print*,'READING: ', trim(filename)

         status = nf90_open(trim(filename),nf90_nowrite,ncid)
         if (status .ne. nf90_noerr) then
             print*,'Error Opening File.'
             print*,' ',trim(filename)
             print*,'Status: ',status
         endif
         do var=1,nvar_slv
            status = nf90_inq_varid(ncid,trim(varname_slv(var)),varid)
 
            !get variable description and units only once
            if (mon .eq. 1 .and. day .eq. 1) then
               status = nf90_get_att(ncid,varid,'long_name',description_slv(var))
               status = nf90_get_att(ncid,varid,'units',units_slv(var))
            endif
            status = nf90_get_var(ncid,varid,valuesday)
            if (status < 0) then
               print*,'Error reading slv data, status =',status
               print*, var, trim(varname_slv(var))
               stop
            endif
            valuesmon_slv(var,:,:,countstart:countstop) = &
                valuesday(:,:,1:numtimesteps)

         enddo  !var
         status = nf90_close(ncid)

         count = count + numtimesteps
      enddo !day

      !Save the gridded data
      allocate(valuesout_temp(lon,lat,ntime))
      allocate(valuesout(nvar_out,lon,lat,ntime))

      units_out(1) = 'K'
      description_out(1) = description_slv(1)
      units_out(2) = 'kg/kg'
      description_out(2) = description_slv(2)
      units_out(3) = 'mb'
      description_out(3) = description_slv(3)
      units_out(4) = 'm/s'
      description_out(4) = 'Wind Speed at 10 m'
      units_out(5) = 'mm/s'
      description_out(5) = 'Large Scale Precipitation (with snow)'
      units_out(6) = 'mm/s'
      description_out(6) = 'Convective Precipitation'
      units_out(7) = 'W/m2'
      description_out(7) = 'Surface solar radiation downwards'
      units_out(8) = 'W/m2'
      description_out(8) = 'Surface thermal radiation downwards'

      valuesout(1,:,:,:) = valuesmon_slv(1,:,:,:)
      valuesout(2,:,:,:) = valuesmon_slv(2,:,:,:)
      !convert pressure from Pa to mb    
      valuesout(3,:,:,:) = valuesmon_slv(3,:,:,:)/100.
      !calculate wind from components
      valuesout(4,:,:,:) = SQRT( &
         valuesmon_slv(4,:,:,:)*valuesmon_slv(4,:,:,:) + &
         valuesmon_slv(5,:,:,:)*valuesmon_slv(5,:,:,:))
      !combine large scale and snow precipitation
      valuesout(5,:,:,:) = valuesmon_flx(1,:,:,:) + &
                           valuesmon_flx(4,:,:,:)
      !combine convective and anvil precipitation
      valuesout(6,:,:,:) = valuesmon_flx(2,:,:,:) + &
                           valuesmon_flx(3,:,:,:)
      valuesout(7,:,:,:) = valuesmon_rad(2,:,:,:)
         !...if necessary, calculate net downward shortwave radiation 
         !...by dividing by (1.-albedo)
         !valuesout(7,j,:) = valuesmon_rad(2,lonref(j),latref(j),:) / &
         !                   (1-valuesmon_rad(1,lonref(j),latref(j),:))
      valuesout(8,:,:,:) = valuesmon_rad(3,:,:,:)

      !Write out data to netcdf file   
      write(filename,'(a,a,I4.4,I2.2,a3)') &
          trim(outdir), trim(outprefix), yr, mon, '.nc'
      print*,'Writing output file: ',trim(filename)

      call check ( nf90_create(trim(filename), &
                   nf90_64bit_offset, ncid) )

      call check ( nf90_def_dim(ncid,'lon',lon,londid))
      call check ( nf90_def_dim(ncid,'lat',lat,latdid))
      call check ( nf90_def_dim(ncid, 'time', ntime, timedid) )

      call check ( nf90_def_var(ncid, 'longitude', nf90_float, &
                   (/londid/), lonid) )
      call check ( nf90_def_var(ncid, 'latitude', nf90_float, &
                   (/latdid/), latid) )
      call check ( nf90_def_var(ncid, 'time', nf90_double, &
                   (/timedid/), timeid))

      call check ( nf90_def_var(ncid, 'year', nf90_int, &
                   (/timedid/), yrid) )
      call check ( nf90_def_var(ncid, 'month', nf90_int, &
                   (/timedid/), monid) )
      call check ( nf90_def_var(ncid, 'doy', nf90_int, &
                   (/timedid/), doyid) )

      call check ( nf90_def_var(ncid, 'day', nf90_int, &
                   (/timedid/), dayid) )
      call check ( nf90_def_var(ncid, 'hour', nf90_float, &
                   (/timedid/), hrid) )

      dims(1) = londid
      dims(2) = latdid
      dims(3) = timedid

      do j=1, nvar_out
         call check ( nf90_def_var(ncid, trim(varname_out(j)), &
                        nf90_float, dims, varid_out(j)) )
         call check ( nf90_put_att(ncid, varid_out(j), &
                        'long_name', trim(description_out(j))) )
         call check ( nf90_put_att(ncid, varid_out(j), &
                        'units',trim(units_out(j))) )
         call check ( nf90_put_att(ncid, varid_out(j), &
                        'missing_value', 1.e15) )
         call check ( nf90_put_att(ncid, varid_out(j), &
                        '_FillValue', 1.e15) )
      enddo

     call check ( nf90_enddef(ncid) )

     longitude=longitudein
     call check ( nf90_put_var(ncid, lonid, longitude) )

     latitude=latitudein
     call check ( nf90_put_var(ncid, latid, latitude) )

     call check ( nf90_put_var(ncid, timeid, time))
     call check ( nf90_put_var(ncid, yrid, timeyr) )
     call check ( nf90_put_var(ncid, monid, timemon) )
     call check ( nf90_put_var(ncid, doyid, timedoy) )
     call check ( nf90_put_var(ncid, dayid, timeday) )
     call check ( nf90_put_var(ncid, hrid, timehr) )

     do j=1, nvar_out
        valuesout_temp(:,:,:) = valuesout(j,:,:,:)
        call check ( nf90_put_var(ncid, varid_out(j), valuesout_temp) )
     enddo

     call check ( nf90_close(ncid) )

     deallocate(valuesmon_flx, valuesmon_rad, valuesmon_slv)
     deallocate(time,timeyr,timemon,timedoy,timeday,timehr)
     deallocate(valuesout,valuesout_temp)

   enddo !mon
enddo !yr
	
print*,'Finished Processing'
print*,''

end program merra_pullvar

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
