!Program to calculate the mean annual maximum LAI
!   for list of sites.
!
!Writes mean annual maximum to netcdf file.
!
!kdhaynes, 05/16
!
program MODIS_maxlai

use netcdf
implicit none

!File specific information
character(len=100), parameter :: &
    specfile= './MODIS_maxlai.txt'
integer, parameter :: nMODIS=2
integer, parameter :: nmaxyrs=16
integer, parameter :: startyr=2000
character(len=8), dimension(2), parameter :: &
    modisprefix=['MOD15A2_','MYD15A2_']
character(len=3), parameter :: suffix='.nc'
real, parameter :: badval=-999

!Input information
integer :: numsites
character(len=6), dimension(:), allocatable :: sitenames
character(len=120) :: datadir, datafile
character(len=120) :: outfile

!...data variables
integer :: ntime, yrmin, yrmax, numyrs
integer :: ndatamdone
real :: datamdone
real, dimension(nMODIS) :: datadone
real, dimension(:), allocatable :: time, data

real, dimension(nMODIS,nmaxyrs) :: datamax
real, dimension(nmaxyrs) :: timemax
real, dimension(:,:), allocatable :: meandata

!...netcdf variables
integer :: ncid, ncido, status
integer :: dimid, timedid, did, vid
integer, dimension(:), allocatable :: varid

!...misc variables
integer :: nf, ns, t, yr
integer :: yrminref, yrmaxref, yrref

!--------------------------------------
!Read the spec file
open(unit=11,file=trim(specfile),form='formatted')
read(11,*) datadir
read(11,*) outfile
read(11,*) numsites
allocate(sitenames(numsites))
do ns=1,numsites
   read(11,*) sitenames(ns)
enddo
close(11)

!Loop over max years
timemax(1) = startyr+0.5
do yr=1,nmaxyrs-1
   timemax(yr+1) = timemax(yr)+1
enddo

!Create Output File
allocate(varid(numsites))
status = nf90_create(trim(outfile), nf90_clobber, ncido)
status = nf90_def_dim(ncido,'numt',nmaxyrs,timedid)
status = nf90_def_dim(ncido,'numsites',numsites,did)

status = nf90_def_var(ncido,'time',nf90_float,timedid,vid)
do ns=1,numsites
   status = nf90_def_var(ncido,sitenames(ns),nf90_float,timedid,varid(ns))
enddo
status = nf90_enddef(ncido)

status = nf90_put_var(ncido,vid,timemax)

!Loop over sites
allocate(meandata(numsites,nmaxyrs))
meandata(:,:) = 0.
do ns=1,numsites
   !Loop over MODIS files
   datamax(:,:) = 0.
   do nf=1,nMODIS
      !...read in data
      write(datafile,'(4a)') trim(datadir), trim(modisprefix(nf)), &
            trim(sitenames(ns)), trim(suffix)
      status = nf90_open(trim(datafile),nf90_nowrite,ncid)
      status = nf90_inq_dimid( ncid,'time', dimid )   
      status = nf90_inquire_dimension( ncid, dimid, len=ntime)

      allocate(time(ntime))
      status = nf90_inq_varid( ncid, 'time', vid )
      status = nf90_get_var( ncid, vid, time )

      allocate(data(ntime))
      status = nf90_inq_varid( ncid, 'lai', vid )
      status = nf90_get_var( ncid, vid, data )
      status = nf90_close(ncid)   

      !...process data
      yrmin = floor(minval(time))
      yrmax = ceiling(maxval(time))-1
      !print('(a3,a3,a,i4,a,i4)'),'   ',modisprefix(nf),' years: ',yrmin, ' - ',yrmax
   
      do t=1,ntime
         yrref=floor(time(t)) - startyr + 1
         if (yrref .le. nmaxyrs) then
            if (datamax(nf,yrref) .lt. data(t)) &
                datamax(nf,yrref)=data(t)
         endif
      enddo

      yrminref = yrmin - startyr + 1
      yrmaxref = MIN(nmaxyrs,yrmax - startyr + 1)
      numyrs=yrmaxref-yrminref+1
      datadone(nf) = sum(datamax(nf,yrminref:yrmaxref)) / numyrs
      !print('(a,f8.4)'),'     Mean LAI Maximum: ',datadone(nf)
  
      !...deallocate arrays
      deallocate(time,data)
   enddo

   !print*,''
   !print('(a)'),'Year    MOD_LAI   MYD_LAI   MEAN'
   datamdone=0.
   ndatamdone=0
   do yr=0,nmaxyrs-1
      if ((datamax(1,yr+1) .gt. 0.) .and. (datamax(2,yr+1) .gt. 0.)) then
         meandata(ns,yr+1) = sum(datamax(:,yr+1))/2.
      elseif (datamax(1,yr+1) .eq. 0.) then
         meandata(ns,yr+1) = datamax(2,yr+1)
      elseif (datamax(2,yr+1) .eq. 0.) then
         meandata(ns,yr+1) = datamax(1,yr+1)
      else
         meandata(ns,yr+1)=badval
      endif

      if (meandata(ns,yr+1) .gt. 0.) then
         !print('(i4,3f10.4)'), startyr+yr, datamax(1,yr+1), datamax(2,yr+1), meandata(ns,yr+1)
         datamdone = datamdone + meandata(ns,yr+1)
         ndatamdone = ndatamdone + 1
      endif
   enddo
   datamdone = datamdone / ndatamdone

   if (ns .eq. 1) then
      print*,''
      print('(a)'), '-----Mean LAI Maximum (m2/m2)-----'
      print('(a)'), 'Site        MOD      MYD       AVE'
   endif
   print('(a,3f10.4)'),sitenames(ns), datadone(1), datadone(2), datamdone
   status=nf90_put_var(ncido,varid(ns),meandata(ns,:))

enddo 

status=nf90_close(ncido)

end
