!Program to convert filled text data into a netcdf file.
!
!kdhaynes, 11/15

program asiaflux_csv_to_netcdf

use netcdf
implicit none

character*120, parameter :: specfile='asiaflux_csv_to_netcdf.txt'
integer, dimension(12), parameter :: &
    dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
integer, parameter :: badval=-9999

integer :: hrperday, timeout
integer :: startyr, stopyr, numyr
integer :: numvars

character*120 :: trash
character*120 :: dirin, fnamein
character(len=5000) :: trashall

!time variables
integer, dimension(:), allocatable :: numdayperyr

!file variables
character(len=36), dimension(:), allocatable :: varsname, varsunit
character(len=5) :: suffixout
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

!time-mean data variables
integer :: numtmean, newnumt
real*8 :: delt, startt
real*8, dimension(:), allocatable :: timemean
integer, dimension(:), allocatable :: varsanum
real, dimension(:,:), allocatable :: varsmean
real, dimension(:), allocatable :: varsout

!misc variables
integer :: count, countnew, counttmean
integer*4 :: yr, mon, day, hr, t
integer :: v, nlen

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
read(11,*) fnamein
read(11,*) trash
read(11,*) fileout
close(11)


!---------------------
!Find the number of variables
write(filein,'(a,a)') trim(dirin),trim(fnamein)
print*,'Opening the file: ',trim(filein)
open(unit=11,file=trim(filein),form='formatted')
read(11,('(a)')) trashall
nlen=len(trim(trashall))
numvars=1
do v=1,nlen
   if (trashall(v:v) .eq. ',') then
      numvars=numvars+1
   endif
enddo

print*,'  Number of Variables: ',numvars
close(11)

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
allocate(varsname(numvars),varsunit(numvars))
allocate(varsid(numvars))
allocate(varstemp(numvars))
allocate(varssave(numvars,numt))

!---------------------
!Read the input file
write(filein,'(a,a)') trim(dirin),trim(fnamein)
!print*,'Opening the file: ',trim(filein)
open(unit=11,file=trim(filein),form='formatted')
read(11,*) varsname
!print*,'Variables: ',varsname
read(11,*) varsunit

do v=1, numvars
   if (trim(varsname(v)) .eq. 'LE') then
      print*,'  Changing variable LE to LH.'
      varsname(v) = 'LH'
   endif

   if (trim(varsname(v)) .eq. 'TIME') then
      print*,'  Changing variable TIME to TIMEH.'
      varsname(v) = 'TIMEH'
   endif

   if (trim(varsname(v)) .eq. 'time') then
     print*,'  Changing variable time to timef.'
     varsname(v) = 'timef'
   endif
enddo

count=1
do yr=startyr,stopyr
   numtref=yr-startyr+1
   numttemp=numdayperyr(numtref)*hrperday
   delt=1./(hrperday*numdayperyr(yr-startyr+1))
   startt=yr + delt/2.
   do day=1,numdayperyr(yr-startyr+1)  !numttemp
      do hr=1,hrperday
         read(11,*) varstemp
         varssave(:,count) = varstemp(:)

         if (varssave(4,count) .gt. 20) varssave(4,count)=badval

         if (count .gt. 1) then
             time(count) = time(count-1) + delt
         elseif (count .eq. 1) then
             time(count) = startt
         endif
         count=count+1
      enddo
   enddo  !hr
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
                  if (varssave(v,count) > badval) then
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
write(fileout,'(a,a)') trim(fileout),trim(suffixout)

print*,''
print*,'Writing out the file: ',trim(fileout)
print*

call check ( nf90_create(trim(fileout), nf90_clobber, ncid) )

call check ( nf90_def_dim( ncid, 'numt', newnumt, timedid ) )
call check ( nf90_def_var( ncid, 'time', nf90_double,timedid,timeid ) )

do v=1,numvars
   !print*,'Defining variable: ',varsname(v)
   call check ( nf90_def_var( ncid, trim(varsname(v)), nf90_float,timedid,varsid(v)))
enddo
call check ( nf90_enddef( ncid ) )

call check ( nf90_put_var( ncid, timeid, timemean ) )

allocate(varsout(newnumt))
do v=1,numvars
   varsout(:) = varsmean(v,:)

   !print*,'Putting var: ',varsname(v)
   call check ( nf90_put_var( ncid, varsid(v), varsout ) )
enddo

call check ( nf90_close(ncid) )

end program asiaflux_csv_to_netcdf

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
