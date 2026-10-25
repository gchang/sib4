!Program to convert filled text data into a netcdf file.
!
!kdhaynes, 05/16

program caflux_csv_to_netcdf

use netcdf
implicit none

character*120, parameter :: specfile='caflux_csv_to_netcdf.txt'
integer, dimension(12), parameter :: &
    dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
integer, parameter :: badval=-999
integer, parameter :: nvarsbadb=3
integer, parameter :: nvarsbade=2

integer :: hrperday, timeout
integer :: startyr, stopyr, numyr

integer, parameter :: ltrash=1000
character(len=12) :: myformat
character(len=ltrash) :: trash
character*120 :: dirin
character*50 :: prefixin, suffixin
character*120 :: dirout

!time variables
integer :: yrlen
real*8 :: hrfrac, doyfrac
integer, dimension(:), allocatable :: numdayperyr

!file variables
integer :: vlen, vstart, numvars
character(len=36), dimension(:), allocatable :: varsname, varsunit
character(len=5) :: suffixout
character(len=2) :: ssstartyr, ssstopyr
character(len=4) :: sstartyr, sstopyr
character*150 :: filein, fileout
real*8, dimension(:), allocatable :: time


!netcdf variables
integer :: ncid, timedid
integer :: timeid
integer, dimension(:), allocatable :: varsid

!data variables
integer :: numt, numtref, numttemp, tdays
real*8 :: timetemp
real, dimension(:), allocatable :: varstemp
real, dimension(:,:), allocatable :: varssave
character(len=9) :: datatype
character(len=6) :: sitename
character(len=6) :: subsite

!time-mean data variables
integer :: numtmean, newnumt
real*8 :: delt, startt
real*8, dimension(:), allocatable :: timemean
integer, dimension(:), allocatable :: varsanum
real, dimension(:,:), allocatable :: varsmean
real, dimension(:), allocatable :: varsout

!misc variables
integer :: count, countdoy, countv
integer :: countnew, counttmean
integer*4 :: yr, mon, day, hr
integer :: v

!----------------------------------------------------------
!----------------
!Read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,*) startyr
read(11,*) stopyr
numyr=stopyr-startyr+1

read(11,*) hrperday
read(11,*) timeout
read(11,*) trash
read(11,*) dirin
read(11,*) trash
read(11,*) prefixin
read(11,*) trash
read(11,*) suffixin
read(11,*) trash
read(11,*) dirout
close(11)


!-------------------
!Read input file to find number of variables
write(filein,'(a,a,i4,a,i2.2,a)') trim(dirin),trim(prefixin),startyr,'-',1,trim(suffixin)
!print*,'Opening the file: ',trim(filein)
open(unit=11,file=trim(filein),form='formatted')

write(myformat,'(a,i4.4,a)') '(a',ltrash,')'
read(11,trim(myformat)) trash
close(11)

vlen=len(trash)
numvars=1
do v=1,vlen
  if (trash(v:v) .eq. ',') numvars = numvars + 1
enddo
numvars=numvars-nvarsbadb-nvarsbade

!----------------
!Allocate arrays
allocate(numdayperyr(numyr))
do yr=startyr, startyr+numyr-1
    if (mod(yr,4) .eq. 0) then
        numdayperyr(yr-startyr+1)=366
    else
        numdayperyr(yr-startyr+1)=365
    endif
enddo
numt=sum(numdayperyr)*hrperday

allocate(time(numt))
allocate(varsname(numvars),varsunit(numvars),varsid(numvars))
varsname(:)=''
varsunit(:)=''
varsid(:)=0

allocate(varstemp(numvars))
varstemp(:)=0.
allocate(varssave(numvars,numt))
varssave(:,:)=0.

!---------------------
!Read the input files
count=1
do yr=startyr,stopyr
   yrlen=365
   if (mod(yr,4) .eq. 0) yrlen=366

   countdoy=0
   do mon=1,12
      write(filein,'(a,a,i4.4,a,i2.2,a)') trim(dirin),trim(prefixin),yr,'-',mon,trim(suffixin)
      !print*,'Opening the file: ',trim(filein)
      open(unit=11,file=trim(filein),form='formatted')
  
      read(11,trim(myformat)) trash
      vlen=len(trash)
      vstart=1
      countv=-1*(nvarsbadb-1)
      do v=1,vlen
         if (trash(v:v) .eq. ',') then
            if ((countv .ge. 1) .and. (countv .le. numvars)) then
                varsname(countv) = trash(vstart:v-1)
             endif
             vstart=v+1
             countv=countv+1
         endif
      enddo
      !print*,'Variables: ',trim(varsname(1)),trim(varsname(numvars))

      read(11,trim(myformat)) trash
      vlen=len(trash)
      vstart=1
      countv=-1*(nvarsbadb-1)
      do v=1,vlen
         if (trash(v:v) .eq. ',') then
             if ((countv .ge. 1) .and. (countv .le. numvars)) then
                 varsunit(countv) = trash(vstart:v-1)
             endif
             vstart=v+1
             countv=countv+1
         endif
      enddo
      !print*,'Units: ',trim(varsunit(1)),trim(varsunit(numvars))

      tdays=dayspermon(mon)
      if ((mod(yr,4) .eq. 0) .and. &
          (mon .eq. 2)) then
          tdays=tdays+1
      endif

      do day=1,tdays
          do hr=1,hrperday
             hrfrac = (hr-0.5)/dble(hrperday)
             doyfrac = (countdoy + hrfrac)/yrlen
             time(count) = yr + doyfrac

             read(11,*) datatype,sitename,subsite,varstemp
             varssave(:,count) = varstemp(:)

             count=count+1
          enddo  !hr
          countdoy = countdoy + 1
      enddo !day
   enddo !mon
enddo  !yr
close(11)


!----------------------------------------------------
!Calculate the output
   if (timeout == 0) then
      if (hrperday .eq. 48.) then
          numtmean=1
      else
          print*,'Cannot Output Half-Hourly Data'
          print*,'Stopping.'
          stop
      endif
   elseif (timeout == 1) then
      if (hrperday .eq. 24.) then
         numtmean=1
      elseif (hrperday .eq. 48.) then
         numtmean=2
      else
         print*,'Unexpected Data Timestep'
         print*,'  Hr_Per_Day: ',hrperday
         print*,'Stopping.'
         stop
      endif
   elseif (timeout == 2) then
      numtmean=hrperday
   elseif (timeout == 3) then
      numtmean=hrperday*7
   endif

   if (timeout < 4) then
       newnumt=numt/numtmean
   else
       newnumt=numyr*12.
   endif

   allocate(timemean(newnumt))
   allocate(varsanum(numvars))
   allocate(varsmean(numvars,newnumt))

   timetemp = 0.
   timemean(:) = 0.
   varstemp(:) = 0.
   varsanum(:) = 0.
   varsmean(:,:) = 0.

   count=1
   countnew=1
   counttmean=0
   do yr=startyr,startyr+numyr-1
      do mon=1,12
         if (timeout .eq. 4) numtmean=dayspermon(mon)*hrperday

         tdays=dayspermon(mon)
         if ((mod(yr,4) .eq. 0) .and. &
             (mon .eq. 2)) then
              tdays=tdays+1
         endif

         do day=1,tdays
            do hr=1,hrperday

               timetemp = timetemp + time(count)
               do v=1,numvars
                  if (varssave(v,count) .gt. badval) then
                      varsanum(v) = varsanum(v) + 1
                      varstemp(v) = varstemp(v) + varssave(v,count)
                  endif
               enddo

               count=count + 1
               counttmean=counttmean + 1

               if (counttmean .eq. numtmean) then
                  timemean(countnew) = timetemp/numtmean
                  do v=1,numvars
                     if (varsanum(v) > 0) then
                         varsmean(v,countnew) = varstemp(v)/varsanum(v)
                     else
                         varsmean(v,countnew) = badval
                     endif
                  enddo

                  countnew=countnew+1
                  counttmean=0
                  timetemp=0.
                  varstemp(:) = 0.
                  varsanum(:) = 0
                endif !counttmean == numtmean
             enddo  !hr
           enddo !day
        enddo !mon
    enddo !yr

!----------------------------------------------------
!Write out the file
if (timeout == 0) then
   write(suffixout,'(a)') '_f.nc'
elseif (timeout == 1) then
   write(suffixout,'(a)') '_h.nc'
elseif (timeout == 2) then
   write(suffixout,'(a)') '_d.nc'
elseif (timeout == 3) then
   write(suffixout,'(a)') '_w.nc'
elseif (timeout == 4) then
   write(suffixout,'(a)') '_m.nc'
else
   print*,'Invalid Time For Output.  Stopping'
   stop
endif
write(sstartyr,('(i4)')) startyr
write(sstopyr,('(i4)')) stopyr
ssstartyr=sstartyr(3:4)
ssstopyr=sstopyr(3:4)
if (startyr .lt. stopyr) then
   write(fileout,('(4a)')) trim(dirout), ssstartyr, ssstopyr, trim(suffixout)
elseif (startyr .eq. stopyr) then
   write(fileout,('(3a)')) trim(dirout), ssstartyr, trim(suffixout)
endif

print*,''
print*,'Writing out the file: ',trim(fileout)
print*

call check ( nf90_create(trim(fileout), nf90_clobber, ncid) )

call check ( nf90_def_dim( ncid, 'numt', newnumt, timedid ) )
call check ( nf90_def_var( ncid, 'time', nf90_double,timedid,timeid ) )

do v=1,numvars
   call check ( nf90_def_var( ncid, trim(varsname(v)), nf90_float,timedid,varsid(v)))
   call check ( nf90_put_att( ncid, varsid(v), 'units', trim(varsunit(v))))
   !print*,'Defined variable: ',varsname(v)
enddo
call check ( nf90_enddef( ncid ) )

call check ( nf90_put_var( ncid, timeid, timemean ) )

allocate(varsout(newnumt))
do v=1,numvars
   varsout(:) = varsmean(v,:)
   call check ( nf90_put_var( ncid, varsid(v), varsout ) )
enddo

call check ( nf90_close(ncid) )

end program caflux_csv_to_netcdf

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
