!Program to prepare GPCP 2.3 data to be used
!   to scale weather (MERRA2) driver data.
!
! -- Converts longitude to go from -180 to 180
! -- Reorders latitude to go from -90 to 90
! -- Conjoins all months into a single file for a year
! -- Regrids to weather data resolution.
!
!Use the makefile in this directory
!>make gpcp_prep23 (NC=3 for netcdf3)
!
!NOTE: This leaves the precipitation in mm/day.
! If you want mm/month, set the flag_mmpmon=.true.,
! which multiplies the gpcp data by dayspermon.
!
!kdhaynes, 2019/03
!
program gpcp_prep23

use netcdf
implicit none

integer, parameter :: yrstart=2018, yrstop=2018
integer, parameter :: monstart=1, monstop=12

character*180, parameter :: &  !initial GPCP file
   indir='/Users/kdhaynes/gpcp_v2.3/gpcp_cdr_v23rB1_'
character*180, parameter :: &  !regridded GPCP files
   outdir='/Users/kdhaynes/gpcp_v2.3/gpcp_v2.3_'
character*180, parameter :: &  !regrid file
   regridfile='/Users/kdhaynes/Research/sib4_data/grid/grid_0.625x0.5.nc'

logical, parameter :: flag_mmpmon=.false.
real, parameter :: badval = -9999.
integer, parameter :: maxng=4

!!!Netcdf variables
integer :: status
integer :: ncid,ncid2,dimid,varid
integer :: nlatdid, nlondid, ntimedid
integer :: latid, lonid, timeid, monthid
integer :: precipid
character*20 :: dimname
character*180 :: filenamein, filenameout

!!!GPCP variables
integer :: nlong, nlatg, ntime
real, dimension(:), allocatable :: lonino, latino, lonin, latin
real, dimension(:,:), allocatable :: gpcpin
real, dimension(:,:,:), allocatable :: gpcpall, gpcpout

!!!Regrid variables
integer :: nlon, nlat
real, dimension(:), allocatable :: lon, lat
real*8, dimension(:,:), allocatable :: areag
real*8, dimension(:,:), allocatable :: areagg

!!!Areal variables
integer, dimension(:,:), allocatable :: ngmin
integer, dimension(:,:,:), allocatable :: refiin,refjin
real*8, dimension(:), allocatable :: arlonin, arlatin
real*8, dimension(:), allocatable :: arlonout, arlatout
real*8, dimension(:,:,:), allocatable :: contribin
logical :: regridup

!!!Time variables
integer, dimension(:), allocatable :: month
real, dimension(:), allocatable :: time

!!!Local variables
integer :: i,j,ii,jj
integer :: yr, mon, yrref
integer :: startref, lonref
integer :: numatts, att
character(len=8) :: gpcpunits
character(len=30) :: attname

integer :: dayspermon(12)
real :: radius
real*8 :: pi

!------------------
!Set local variables
data dayspermon/31,28,31,30,31,30,31,31,30,31,30,31/
pi = acos(-1.0)
radius = 6371000.

!-----------------------
!Get regrid information
print*,''
print('(a)'),'Regrid File Information: '
print('(a,a)'), '   ',trim(regridfile)
status = nf90_open( trim(regridfile), nf90_nowrite, ncid )
if (status .ne. nf90_noerr) then
   print*,'Error Opening File. Stopping.'
   stop
endif

status = nf90_inq_dimid(ncid, 'lon', dimid )
status = nf90_inquire_dimension(ncid, dimid, dimname, nlon)
status = nf90_inq_dimid(ncid, 'lat', dimid )
status = nf90_inquire_dimension(ncid, dimid, dimname, nlat)
print('(a,i3.3,a,i3.3)'),'   Lon/Lat:  ',nlon,'  ',nlat

allocate(lon(nlon),lat(nlat))
allocate(gpcpout(nlon,nlat,12))
regridup = .true.

status = nf90_inq_varid(ncid, 'lon', varid )
status = nf90_get_var(ncid, varid, lon )
status = nf90_inq_varid(ncid, 'lat', varid )
status = nf90_get_var(ncid, varid, lat )
status = nf90_close(ncid)


!------------------
!Loop over years/months
do yr=yrstart, yrstop
   yrref = yr-yrstart+1

   do mon=monstart, monstop

      !Get the GPCP data
      write(filenamein,'(a,a,i4.4,a,i2.2,a)') trim(indir), &
            'y', yr, '_m', mon, '.nc'            
      print('(a,i2.2,a,i4.4)'),'Processing: ',mon,' ',yr
      print('(a)'),'   GPCP File: '
      print('(a,a)'), '     ',trim(filenamein)
      status = nf90_open( trim(filenamein), nf90_nowrite, ncid )
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening File! Stopping!'
         RETURN
      ENDIF

      if ((yrref .eq. 1) .and. (mon .eq. 1)) then
          status = nf90_inq_dimid(ncid, 'nlon', dimid )
          status = nf90_inquire_dimension(ncid, dimid, dimname, nlong)
          status = nf90_inq_dimid(ncid, 'nlat', dimid )
          status = nf90_inquire_dimension(ncid, dimid, dimname, nlatg)
          status = nf90_inq_dimid(ncid, 'time', dimid )
          status = nf90_inquire_dimension(ncid, dimid, dimname, ntime)
          IF (ntime .ne. 1) THEN 
              print*,'Only expecting one time per file!'
              RETURN
          ENDIF

          print('(a,i3.3,a,i3.3)'), '     Lon/Lat:  ',nlong, '  ', nlatg
          IF (nlon .GT. nlong) regridup = .false.

          allocate(lonino(nlong),lonin(nlong))
          allocate(latino(nlatg), latin(nlatg))
          status = nf90_inq_varid(ncid, 'longitude', varid )
          status = nf90_get_var(ncid, varid, lonino )
          status = nf90_inq_varid(ncid, 'latitude', varid )
          status = nf90_get_var(ncid, varid, latino )

          !....fix GPCP longitude
          ii = nlong/2.+1
          do i=1,nlong
             if (lonino(ii) > 180.) then
                lonin(i) = lonino(ii) - 360.0
             else
                lonin(i) = lonino(ii)
             endif

             ii = ii + 1
             if (ii == nlong+1) ii=1
          enddo

          !....fix GPCP latitude
          ii = nlatg
          do i=1,nlatg
             latin(i) = latino(ii)
             ii = ii -1
          enddo

          allocate(time(12),month(12))
          allocate(gpcpin(nlong,nlatg))
          allocate(gpcpall(nlong,nlatg,12))
      endif !yr == yrstart and mon ==1

      status = nf90_inq_varid(ncid, 'precip', varid )
      status = nf90_get_var(ncid, varid, gpcpin )
      status = nf90_close(ncid)
      where(gpcpin .lt. 0.) gpcpin=0.

      !...save the precip
      ii = nlong/2.+1
      do i=1,nlong
         jj = nlatg
         do j=1,nlatg
            gpcpall(i,j,mon) = gpcpin(ii,jj) 
            jj = jj -1
         enddo

         ii = ii + 1
         if (ii == nlong+1) ii=1
      enddo
      if (flag_mmpmon) then
          gpcpall(:,:,mon) = gpcpall(:,:,mon) * dayspermon(mon)
          gpcpunits = 'mm/month'
      else
          gpcpunits = 'mm/day'
      endif

      !...save the time
      month(mon) = mon
      time(mon) = yr + (mon-0.5)/12.

   enddo  !mon=monstart,monstop

   !!!Regrid
   allocate(arlonin(nlong),arlatin(nlatg))
   allocate(arlonout(nlon),arlatout(nlat))
   allocate(areagg(nlong,nlatg))
   allocate(areag(nlon,nlat))

   IF (regridup) THEN
       allocate(ngmin(nlong,nlatg))
       allocate(refiin(nlong,nlatg,maxng), &
                refjin(nlong,nlatg,maxng))
       allocate(contribin(nlong,nlatg,maxng))

       call regrid_upinfo( &
            nlong, nlatg, &
            nlon, nlat, maxng, &
            lonin, latin, lon, lat, &
            ngmin, refiin, refjin, &
            arlonin, arlatin, arlonout, arlatout, &
            contribin, areagg, areag)

      call regrid_varup( yr, flag_mmpmon, &
            nlong, nlatg, &
            nlon, nlat, &
            12, maxng, &
            lon, lat, ngmin, refiin, refjin, &
            arlonin, arlatin, arlonout, arlatout, &
            contribin, areag, gpcpall, gpcpout)
            
   ELSE
      allocate(ngmin(nlon,nlat))
      allocate(refiin(nlon,nlat,maxng), &
               refjin(nlon,nlat,maxng))
      allocate(contribin(nlon,nlat,maxng))

      call regrid_downinfo( &
           nlong, nlatg, &
           nlon, nlat, maxng, &
           lonin, latin, lon, lat, &
           ngmin, refiin, refjin, &
           arlonin, arlatin, arlonout, arlatout, &
           contribin, areagg, areag)

      call regrid_vardown( flag_mmpmon, &
            nlong, nlatg, 12, &
            nlon, nlat, maxng, &
            ngmin, refiin, refjin, &
            arlonin, arlatin, arlonout, arlatout, &
            contribin, gpcpall, gpcpout)
   ENDIF

   deallocate(arlonin,arlatin,arlonout,arlatout)
   deallocate(areagg,areag)

   !!!Write out file
   write(filenameout,'(a,i4.4,a)') trim(outdir), yr, '.nc'
   print('(a)')
   print('(a)'), 'Writing file: '
   print('(2a)'),'  ', trim(filenameout)
   status = nf90_create( trim(filenameout), nf90_clobber, ncid2)

   !...copy global attributes....
   status = nf90_open( trim(filenamein), nf90_nowrite, ncid )
   status = nf90_inquire(ncid, nAttributes=numatts)
   do att=1,numatts
      status = nf90_inq_attname(ncid,nf90_global,att,attname)
      if ((attname .eq. 'title') .or. (attname .eq. 'references')) then
          status = nf90_copy_att(ncid,nf90_global,trim(attname),ncid2,nf90_global)
      endif
   enddo

   !...dimensions...
   status = nf90_def_dim( ncid2, 'nlon', nlon, nlondid )
   status = nf90_def_dim( ncid2, 'nlat', nlat, nlatdid )
   status = nf90_def_dim( ncid2, 'time', 12, ntimedid )

   !...variables...
   status = nf90_def_var( ncid2,'longitude',nf90_float,(/nlondid/), lonid )
   status = nf90_put_att( ncid2, lonid, 'units','degrees_north')

   status = nf90_def_var( ncid2,'latitude',nf90_float,(/nlatdid/), latid )
   status = nf90_put_att( ncid2, latid, 'units','degrees_east')

   status = nf90_def_var( ncid2,'time',nf90_float,(/ntimedid/), timeid )
   status = nf90_put_att( ncid2, timeid, 'units','years since 0000-00-00')

   write(attname,'(a,i4.4,a)') 'months since ',yr,'-00-00'
   status = nf90_def_var( ncid2,'month',nf90_float,(/ntimedid/), monthid)
   status = nf90_put_att( ncid2, monthid, 'units', trim(attname))

   status = nf90_def_var( ncid2,'precip',nf90_float,(/nlondid,nlatdid,ntimedid/), precipid )
   status = nf90_put_att( ncid2, precipid, 'long_name', &
            'Monthly Precipitation' )
   status = nf90_put_att( ncid2, precipid, 'units', gpcpunits)
   status = nf90_put_att( ncid2, precipid, 'dataset', &
            'GPCP Version 2.3 Combined Precipitation Dataset')

   !...take file out of define mode, into data mode
   status = nf90_enddef( ncid2 )

   !...load the variables...
   status = nf90_put_var( ncid2, lonid, lon )
   status = nf90_put_var( ncid2, latid, lat )
   status = nf90_put_var( ncid2, monthid, month )
   status = nf90_put_var( ncid2, timeid, time )
   status = nf90_put_var( ncid2, precipid, gpcpout)

   !...close the files
   status = nf90_close( ncid )
   status = nf90_close( ncid2 )


enddo


print*,''
print('(a)'),'Finished Processing.'
print*,''

end program gpcp_prep23
