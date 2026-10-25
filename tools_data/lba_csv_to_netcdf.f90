!Program to convert filled text data into a netcdf file.
!
!kdhaynes, 08/15

program lba_csv_to_netcdf

use netcdf
implicit none


character*120, parameter :: &
  specfile='lba_csv_to_netcdf.txt'

!parameters
integer, parameter :: numhrperday=24
integer, dimension(12), parameter :: &
    doymonstart=[1,32,60,91,121,152,182,213,244,274,305,335]
integer, dimension(12), parameter :: &
    dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
integer, parameter :: badval=-9999

!time variables
integer :: startyr, stopyr, numyr
integer :: yrtemp, doytemp, doynow, doyadd, yrlen
real :: hrfrac
integer, allocatable, dimension(:) :: numdayperyr

!file variables
integer :: filetype, numvars, iostatus
character(len=8), allocatable, dimension(:) :: varsname
character*150 :: filein, fileout, trash
real*8, dimension(:), allocatable :: time

!netcdf variables
integer :: ncid, timedid
integer :: timeid
integer, allocatable, dimension(:) :: varsid

!data variables
integer :: numt, numtref, numttemp, tdays
real, dimension(:), allocatable :: varstemp, varsout
real, dimension(:,:), allocatable :: varssave

!misc variables
integer*4 :: yr, yrref, doy, count, hr
integer :: v

!----------------------------------------------------------
!----------------
!Read in input/output files
open(unit=11,file=trim(specfile),form='formatted')
read(11,*) filetype
read(11,*) numvars
read(11,*) startyr
read(11,*) stopyr
read(11,*) trash
read(11,*) filein
read(11,*) trash
read(11,*) fileout

!Allocate arrays
numyr=stopyr-startyr+1
allocate(numdayperyr(numyr))
do yr=startyr, startyr+numyr-1
   yrref=yr-startyr+1

   if (filetype .eq. 3) then
      numdayperyr(yrref) = 12
    else
      if (mod(yr,4) .eq. 0) then
          numdayperyr(yrref)=366
      else
          numdayperyr(yrref)=365
      endif
    endif
enddo

if (filetype .eq. 1) then
   numt=sum(numdayperyr)*24.
elseif (filetype .eq. 2) then
    numt=sum(numdayperyr)
elseif (filetype .eq. 3) then
    numt=numyr*12.
else
    print*,'Unexepected File Type.'
    stop
endif

allocate(time(numt))
allocate(varsname(numvars))
allocate(varstemp(numvars))
allocate(varssave(numvars,numt))

!---------------------
!Read the data
open(unit=11,file=trim(filein),form='formatted')
read(11,*) varsname
!print*,'Variables: ',varsname

do v=1,numvars
   if (varsname(v) .eq. 'LE') varsname(v)='LH'
enddo

read(11,*,iostat=iostatus) varstemp

count=1
do yr=startyr,stopyr
   yrref=yr-startyr+1

   do doy=1,numdayperyr(yrref)
       yrtemp=varstemp(1)
       doytemp=varstemp(2)

       if (filetype .eq. 1) then
           do hr=0,numhrperday-1
              hrfrac = (hr+0.5)/numhrperday
              time(count) = yr + (doy+hrfrac)/dble(numdayperyr(yrref)+1)
  
              if ((yrtemp .EQ. yr) .and. (doytemp .EQ. doy) .and. &
                  (hr .EQ. varstemp(3))) then
                   varssave(:,count) = varstemp(:)
                   read(11,*,iostat=iostatus) varstemp
              else
                   varssave(:,count) = badval
              endif
              count=count+1
            enddo
       else

          if (filetype .eq. 3) then
              doynow=doymonstart(doy)
              doyadd=dayspermon(doy)/2.

              if ((mod(yr,4) .eq. 0) .and. &
                  (doy .gt. 2)) then
                   doynow=doymonstart(doy)+1
                   yrlen=367
              else
                  doynow=doymonstart(doy)
                  yrlen=366
              endif
              time(count) = yr + (doynow+doyadd)/dble(yrlen)
              !time(count) = yr + (doynow+15)/dble(numdayperyr(yrref))
          else
              doynow=doy
              time(count) = yr + (doynow-0.5)/dble(numdayperyr(yrref)+1)
          endif

          if ((yrtemp .EQ. yr) .and. (doytemp .EQ. doynow)) then
              varssave(:,count) = varstemp(:)
              read(11,*,iostat=iostatus) varstemp
          else
              varssave(:,count) = badval
          endif

          count = count + 1
     endif !filetype ne 1
   enddo
enddo
close(11)

!----------------------------------------------------
!Write out the file
print*,''
print*,'Writing out the file: '
print*,' ',trim(fileout)
print*,''

call check ( nf90_create(trim(fileout), nf90_clobber, ncid) )

call check ( nf90_def_dim( ncid, 'numt', numt, timedid ) )
call check ( nf90_def_var( ncid, 'time', nf90_double,timedid,timeid ) )

allocate(varsid(numvars))
do v=1,numvars
   call check ( nf90_def_var( ncid, trim(varsname(v)), nf90_float,timedid,varsid(v)))
   !call check ( nf90_put_att( ncid, varsid(v), 'long_name', varslname(v)))
   !call check ( nf90_put_att( ncid, varsid(v), 'units', varsunits(v)))
enddo
call check ( nf90_enddef( ncid ) )

call check ( nf90_put_var( ncid, timeid, time ) )

allocate(varsout(numt))
do v=1,numvars
   varsout(:) = varssave(v,:)

   if (varsname(v) .eq. 'Fc') then
       where (varsout .GT. 80) 
           varsout=badval
       endwhere
       where (varsout .LT. -40) 
            varsout=badval
       endwhere
   endif

   call check ( nf90_put_var( ncid, varsid(v), varsout ) )
enddo

call check ( nf90_close(ncid) )

end program lba_csv_to_netcdf

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
