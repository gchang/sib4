!Program to change the precipitation values for the SiB driver files
!and scale them to GPCP precipitation.
!
!Program finds the ratio between the MERRA and GPCP precipitation
!and multiplies by this ratio to 'scale' the precip.
!
!MERRA precip units assumed to be mm/s, with hourly data
!GPCP precip units assumed to be mm/day, with monthly data
!  --Expects GPCP to be regridded to MERRA resolution!!
!
!Program will not change the timing nor 'create' precip in MERRA.
! 
!NOTE:  This will actually change the netcdf file!
!
!To compile, use:
!>pgf90 scale_merra_gpcp.f90 -L/usr/local/netcdf3-pgi/lib -lnetcdf 
!   -I/usr/local/netcdf3-pgi/include -o scale_merra_gpcp
!
!-or-
!>make scale_merra_gpcp
!
!kdhaynes, 2019/03

program scale_merra_gpcp

use netcdf
implicit none

!!!user defined variables
integer, parameter :: startYr=2018, stopYr=2018
integer, parameter :: startMon=1, stopMon=12

!.....GPCP v2.3 
!.....(2.5-degree, monthly, units of mm/day)
character*180, parameter :: &
    gpcpfile='/spinning-scratch/kdhaynes/data/gpcp/gpcp_v2.3_2018.nc'
logical, parameter :: flag_mmpmon=.false.

!....MERRA-2
character*100, parameter :: &
    merradir='/spinning-scratch/kdhaynes/data/merra2_proc/'
character*20, parameter :: &
    merraprefix='merra2_'

!!!other MERRA parameters 
!!!assuming 2/3-degree lon by 1/2-degree lat, hourly data
real*4, parameter :: timeconvert=3600.

!space variables
integer nlon, nlat
real, dimension(:), allocatable :: lon, lat

!time variables
character*3 :: monname(12)
character*100 :: monlabel

!merra variables
integer nlonm, nlatm, ntimem
integer :: merrauaf, merraba

real, dimension(:,:,:), allocatable :: lsprin, cuprin
real, dimension(:,:,:), allocatable :: lsprout, cuprout
real, dimension(:,:), allocatable :: merraprecipin, merraprecipout
real, dimension(:,:), allocatable :: ratio

!gpcp variables
integer :: ntimeg, gpcpref
real :: myyrstart, myyrnext
real, dimension(:), allocatable :: timeg
real, dimension(:,:,:), allocatable :: gpcpin
real, dimension(:,:), allocatable :: gpcpprecip

!areal variables
real :: deltalon, deltalat
real, dimension(:), allocatable :: lonE, lonW, arlon
real, dimension(:), allocatable :: latN, latS, arlat

!comparison variables
integer :: ratioa1, ratiob1, ratio1
real*8 :: preciptotg, preciptotmin, preciptotmout

!netcdf variables
integer ncid,dimid,varid,status
integer nlondid, nlatdid, ntimedid
integer lsprid, cuprid
character*20 :: dimname
character*120 :: merraname

!misc variables
integer :: i,j, yr, mon
integer :: mystartmon, mystopmon

!local variables
integer :: dayspermon(12)
real :: radius,myconvert
real*8 :: areaEarth, pi

!Define/set variables
data monname/'Jan','Feb','Mar','Apr','May','Jun', &
    'Jul','Aug','Sep','Oct','Nov','Dec'/
data dayspermon/31,28,31,30,31,30,31,31,30,31,30,31/
pi = acos(-1.0)
radius = 6371000.
areaEarth = 510072000000000.

!Get the GPCP data
print*,''
print('(a)'),'GPCP File Information: '
print('(a,a)'), '   ',trim(gpcpfile)
status = nf90_open( trim(gpcpfile), nf90_nowrite, ncid )
IF (status .ne. nf90_noerr) THEN
   print*,'Error Finding GPCP File. Stopping.'
   STOP
ENDIF

status = nf90_inq_dimid(ncid, 'nlon', dimid )
status = nf90_inquire_dimension(ncid, dimid, dimname, nlon)
status = nf90_inq_dimid(ncid, 'nlat', dimid )
status = nf90_inquire_dimension(ncid, dimid, dimname, nlat)
status = nf90_inq_dimid(ncid, 'time', dimid )
status = nf90_inquire_dimension(ncid, dimid, dimname, ntimeg)

allocate(lon(nlon),lat(nlat),timeg(ntimeg))
status = nf90_inq_varid(ncid, 'longitude', varid )
status = nf90_get_var(ncid, varid, lon )

status = nf90_inq_varid(ncid, 'latitude', varid )
status = nf90_get_var(ncid, varid, lat )

status = nf90_inq_varid(ncid, 'time', varid )
status = nf90_get_var(ncid, varid, timeg)

allocate(gpcpin(nlon,nlat,ntimeg))
allocate(gpcpprecip(nlon,nlat))
status = nf90_inq_varid(ncid, 'precip', varid )
status = nf90_get_var(ncid, varid, gpcpin )
status = nf90_close(ncid)

!....get GPCP time reference start
IF (startyr .LT. floor(timeg(1))) THEN
    print*,'Requesting GPCP Data Prior To Available.'
    print*,'Stopping.'
ELSEIF (startyr .GT. ceiling(timeg(ntimeg))) THEN
    print*,'Requesting GPCP Data After Available.'
    print*,'Stopping.'
ELSE
   myyrstart = startyr + real(startMon-0.5)/12.
   gpcpref = -999
   DO i=1,ntimeg
      myyrnext = timeg(i) + 0.5/12.
      IF ((myyrstart .GE. timeg(i)) .AND. &
          (myyrstart .LT. myyrnext)) THEN
          gpcpref = i
      ENDIF
   ENDDO
   IF (gpcpref .lt. 1) THEN
      print*,'Invalid GPCP Time Requested.'
      print*,'Stopping.'
   ENDIF
ENDIF
print('(a,i6,f10.2)'),'   GPCP Start Ref and Time: ', &
    gpcpref, timeg(gpcpref)


!...get areal variables for precipitation totals
deltalon=(lon(3)-lon(2))/2.
deltalat=(lat(3)-lat(2))/2.

allocate(lonE(nlon),lonW(nlon),arlon(nlon))
do i=1,nlon
   lonE(i)=lon(i)+deltalon
   lonW(i)=lon(i)-deltalon
   arlon(i)=(lonE(i)-lonW(i))/360.
enddo
arlon = arlon*2*pi*radius*radius

allocate(latS(nlat),latN(nlat),arlat(nlat))
do j=1,nlat
   if (j .eq. 1) then
      latS(j) = -90.
      latN(j) = (lat(j+1) + lat(j))*0.5
   elseif (j .eq. 2) then
      latS(j) = (lat(j-1) + lat(j))*0.5
      latN(j) = lat(j) + deltalat
   elseif (j .eq. nlat-1) then
      latS(j) = lat(j) - deltalat
      latN(j) = (lat(j) + lat(j+1))*0.5
   elseif (j .eq. nlat) then
      latS(j) = (lat(j-1) + lat(j))*0.5
      latN(j) = 90.
   else
      latS(j) = lat(j) - deltalat
      latN(j) = lat(j) + deltalat
   endif
   arlat(j) = -sin(pi*latS(j)/180.) + &
               sin(pi*latN(j)/180.)
enddo

!Loop over years/months
do yr=startYr,stopYr
   mystartmon=1
   mystopmon=12
   IF (yr .eq. startyr) mystartmon=startMon
   IF (yr .eq. stopyr) mystopmon=stopMon

   do mon=mystartmon,mystopmon

      !Get the GPCP data
      IF ((gpcpref .GE. 1) .and. (gpcpref .LE. ntimeg)) THEN
          !....convert from mm/day to mm/month
          if (flag_mmpmon) then
              myconvert = 1.0
          else
              myconvert = dayspermon(mon)
              if ((mod(yr,4) .eq. 0) .and. (gpcpref .eq. 2)) &
                   myconvert = myconvert + 1
          endif

          gpcpprecip(:,:) = gpcpin(:,:,gpcpref) * myconvert
          gpcpref = gpcpref + 1
      ELSE
          print*,'Requested GPCP Data After Available.'
          print*,'Stopping.'
          STOP
      ENDIF

      !Open the MERRA file to modify
      write(merraname,'(a,a,i4.4,i2.2,a)') &
          trim(merradir), trim(merraprefix), yr, mon, '.nc'
      call check ( nf90_open( trim(merraname), nf90_write, ncid ) )

      call check ( nf90_inq_dimid(ncid, 'time', ntimedid ) )
      call check ( nf90_inquire_dimension(ncid, ntimedid, dimname, ntimem) )

      !check lon/lat info for MERRA only if first time through loop
      if (mon .eq. startMon .and. yr .eq. startYr) then
         call check ( nf90_inq_dimid(ncid, 'lon', nlondid ) )
         call check ( nf90_inquire_dimension(ncid, nlondid, len=nlonm) )
         call check ( nf90_inq_dimid(ncid, 'lat', nlatdid ) )
         call check ( nf90_inquire_dimension(ncid, nlatdid, len=nlatm) )
 
         IF ((nlonm .NE. nlon) .OR. (nlatm .NE. nlat)) THEN
             print*,'Mismatching GPCP/MERRA Dimensions'
             print*,' GPCP Lon/Lat: ',nlon,nlat
             print*,' MERRA Lon/Lat: ',nlonm,nlatm
             print*,'Stopping.'
             STOP
         ELSE
             print('(a,2I6,2F10.4)'), &
                '   Dimensions (nlon/nlat/dlon/dlat): ', &  
                nlonm,nlatm,(lon(3)-lon(2)),lat(3)-lat(2)
         ENDIF

         allocate(merraprecipin(nlonm,nlatm))
         allocate(merraprecipout(nlonm,nlatm))
         allocate(ratio(nlonm,nlatm))
      ENDIF !yr==startyr, mon==startmon

      print*,''
      write(monlabel,'(a,a,a,i4.4)') 'Processing ',monname(mon), ' ', yr
      print('(a)'),trim(monlabel)
      print('(a)'),'  Modifying MERRA File: '
      print('(a,a)'),'   ',trim(merraname)

      allocate(lsprin(nlonm,nlatm,ntimem),cuprin(nlonm,nlatm,ntimem))
      allocate(lsprout(nlonm,nlatm,ntimem),cuprout(nlonm,nlatm,ntimem))
      call check ( nf90_inq_varid(ncid, 'lspr', varid ) )
      call check ( nf90_get_var(ncid, varid, lsprin ) )
      call check ( nf90_inq_varid(ncid, 'cupr', varid ) )
      call check ( nf90_get_var(ncid, varid, cuprin ) )
      lsprout=lsprin
      cuprout=cuprin

      !Compare the precipitation
      ratioa1 = 0
      ratiob1 = 0
      ratio1 = 0
      DO i=1,nlon
         DO j=1,nlat
            merraprecipin(i,j) = (sum(cuprin(i,j,:)) + sum(lsprin(i,j,:))) &
                                      *timeconvert
            IF (merraprecipin(i,j) .GT. 0.) THEN
               ratio(i,j) = gpcpprecip(i,j) / merraprecipin(i,j) 
            ELSE
               ratio(i,j) = 0.
            ENDIF

            IF (ratio(i,j) .GT. 1.001) THEN
                ratioa1 = ratioa1 + 1
            ELSEIF (ratio(i,j) .LT. 0.999) THEN
                ratiob1 = ratiob1 + 1
            ELSE
                ratio1 = ratio1 + 1
            ENDIF

            cuprout(i,j,:) = cuprout(i,j,:)*ratio(i,j)
            lsprout(i,j,:) = lsprout(i,j,:)*ratio(i,j)
            merraprecipout(i,j) = (sum(cuprout(i,j,:)) + sum(lsprout(i,j,:))) &
                                     *timeconvert
         ENDDO !j
      ENDDO !i 

      !Check total precipitations
      preciptotmin=0.0
      preciptotmout=0.0
      preciptotg=0.0
      DO i=1,nlon
         DO j=1,nlat
            preciptotmin = preciptotmin + &
                    merraprecipin(i,j)*arlon(i)*arlat(j)
            preciptotmout = preciptotmout + &
                    merraprecipout(i,j)*arlon(i)*arlat(j)
            preciptotg = preciptotg + &
                    gpcpprecip(i,j)*arlon(i)*arlat(j)

         ENDDO
      ENDDO

      !Print out stats
      print*,''
      print('(a)'),' Monthly Precipitation Totals (mm/month)'
      print('(a,F15.8)'),'  MERRA Original: ',preciptotmin/areaEarth
      print('(a,F15.8)'),'  MERRA Scaled: ',preciptotmout/areaEarth
      print('(a,F15.8)'),'  GPCP: ',preciptotg/areaEarth

      print*,''
      print('(a)'),' Ratio Information: '
      print('(a,2E14.5)'),'  Ratio Min/Max: ',minval(ratio), maxval(ratio)
      print('(a,I8)'), '    # > 1.0: ', ratioa1
      print('(a,I8)'), '    # < 1.0: ', ratiob1
      print('(a,I8)'), '    # = 1.0: ', ratio1
      print('(a,I8)'), '      TOTAL: ', ratioa1+ratiob1+ratio1

      !Write out the new scaled data
      status = nf90_inq_varid(ncid, 'lspr_scaled', lsprid )
      if (status .ne. 0) then
         call check ( nf90_redef(ncid) )
         call check ( nf90_def_var( ncid, 'lspr_scaled', nf90_float, &
              (/nlondid,nlatdid,ntimedid/), lsprid ) )
         call check ( nf90_put_att(ncid, lsprid, &
              'long_name','Scaled Large Scale Precip'))
         call check ( nf90_put_att(ncid, lsprid, &
              'title', 'LSPR Scaled To GPCP v2.3'))
         call check ( nf90_put_att(ncid, lsprid, &
              'units', 'mm/s'))
         call check ( nf90_enddef( ncid ) )
      endif

      status = nf90_inq_varid(ncid, 'cupr_scaled', cuprid )
      if (status .ne. 0) then
         call check ( nf90_redef(ncid) )
         call check ( nf90_def_var( ncid, 'cupr_scaled', nf90_float, &
             (/nlondid,nlatdid,ntimedid/), cuprid ) )
         call check ( nf90_put_att( ncid, cuprid, &
              'long_name','Scaled Convective Precip'))
         call check ( nf90_put_att( ncid, cuprid, &
              'title', 'CUPR Scaled to GPCP v2.3'))
         call check ( nf90_put_att( ncid, cuprid, &
              'units', 'mm/s'))
         call check ( nf90_enddef( ncid ) )
      endif

      !!!Setting all precip to large-scale!!!
      !lsprout = lsprout + cuprout
      !cuprout = 0.

      call check ( nf90_put_var( ncid, lsprid, lsprout) )
      call check ( nf90_put_var( ncid, cuprid, cuprout) )
      call check ( nf90_close(ncid) )

      deallocate(lsprin,cuprin,lsprout,cuprout)

   enddo !mon=startMon,stopMon
enddo !yr=startYr,stopYr

print*,''
print*,'Finished Processing'
print*,''

end


!**********************************************************
!**********************************************************

subroutine check(status)

use netcdf
use typeSizes

implicit none

integer, intent(in) :: status

   if (status /= nf90_noerr) then
      stop "Error with netcdf.  Stopping."
   endif

end subroutine check
