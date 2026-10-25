!Program to convert FLUXNET2015 data into a netcdf file.
!
!Assumes the data contains complete years.
!   --If it starts/stops mid-year, please fill
!     file timesteps with missing data.
!
!kdhaynes, 2018/02

program fluxnet15_csv_to_netcdf

use netcdf
implicit none

character*120, parameter :: specfile='fluxnet15_csv_to_netcdf.txt'
integer, parameter :: badval=-9999

!input variables
integer :: startyr, stopyr
character(len=2) :: filetype
character(len=6) :: sitename
character(len=180) :: trash
character(len=180) :: fdirin, sdirin, fnamein
character(len=180) :: fdirout
character(len=10000) :: trashall

!time variables
integer :: hrperday, numt, numyr
integer, dimension(:), allocatable :: &
   numdayperyr, numtperyr
real*8, dimension(:), allocatable :: time

!file name variables
character(len=2) :: shortyrstart,shortyrstop
character(len=4) :: ayrstart,ayrstop
character(len=3) :: shortsite
character(len=5) :: suffixout
character(len=256) :: filein, fileout

!file variables
integer :: numvars
character(len=12) :: tstart, tend
character(len=15) :: tstart_name, tend_name
character(len=36), dimension(:), allocatable :: varsname
logical :: ttest2

!netcdf variables
integer :: ncid, status
integer :: timedid, clendid
integer :: timeid, tstartid, tendid
integer, dimension(:), allocatable :: varsid

!data variables
character(len=12), dimension(:), allocatable :: tstartsave, tendsave
real, dimension(:), allocatable :: varstemp, varsout
real, dimension(:,:), allocatable :: varssave

!misc variables
integer :: indx, nlen
integer :: t, v, yr
integer*8 :: count
real*8 :: yrcount

!parameters
character(len=6), parameter :: ad='(a180)'

!----------------------------------------------------------
!----------------
!Read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,*) startyr
read(11,*) stopyr
numyr=stopyr-startyr+1

read(11,('(a2)')) filetype
read(11,('(a6)')) sitename
read(11,ad) trash
read(11,ad) fdirin
read(11,ad) trash
read(11,ad) sdirin
read(11,ad) trash
read(11,ad) fnamein
read(11,ad) trash
read(11,ad) fdirout
close(11)

!---------------------
!Create references
write(ayrstart,'(i4)') startyr
write(ayrstop,'(i4)') stopyr
call to_lower(sitename(4:6), shortsite)
shortyrstart = ayrstart(3:4)
shortyrstop = ayrstop(3:4)

!Find the number of timesteps
allocate(numdayperyr(numyr))
allocate(numtperyr(numyr))
do yr=startyr, startyr+numyr-1
    if (mod(yr,4) .eq. 0) then
        numdayperyr(yr-startyr+1)=366
    else
        numdayperyr(yr-startyr+1)=365
    endif
enddo

if (filetype .eq. 'HH') then
   hrperday=48.
   numt = sum(numdayperyr)*hrperday
   do yr=1, numyr
      numtperyr(yr) = numdayperyr(yr)*hrperday
   enddo
   write(suffixout,'(a)') '_f.nc'
   ttest2=.TRUE.
elseif (filetype .eq. 'HR') then
   hrperday=24.
   numt = sum(numdayperyr)*hrperday
   do yr=1, numyr
      numtperyr(yr) = numdayperyr(yr)*hrperday
   enddo
   write(suffixout,'(a)') '_h.nc'
   ttest2=.TRUE.
elseif (filetype .eq. 'DD') then
   hrperday=1.
   numt = sum(numdayperyr)*hrperday
   numtperyr(:) = numdayperyr(:)
   write(suffixout,'(a)') '_d.nc'
   ttest2=.FALSE.
elseif (filetype .eq. 'WW') then
   hrperday=1.
   numt = numyr*52.
   numtperyr(:) = 52.
   write(suffixout,'(a)') '_w.nc'
   ttest2=.TRUE.
elseif (filetype .eq. 'MM') then
   hrperday=1.
   numt = numyr*12.
   numtperyr(:) = 12.
   write(suffixout,'(a)') '_m.nc'
   ttest2=.FALSE.
elseif (filetype .eq. 'YY') then
   hrperday=1.
   numt = numyr
   numtperyr(:) = 1.
   write(suffixout,'(a)') '_y.nc'
   ttest2=.FALSE.
else
   print*,'Unknown File Type. Stopping.'
   STOP
endif

!Create the total time array
allocate(time(numt))
count=1
do yr=1,numyr
   yrcount=0.5
   do t=1,numtperyr(yr)
       time(count) = (yr + startyr -1) &
                     + yrcount/dble(numtperyr(yr))
       count=count+1
       yrcount=yrcount+1
   enddo
enddo

!Find the number of variables
indx = index(sdirin,'xxxxxx')
if (indx .ge. 1) sdirin(indx:indx+5) = sitename
indx = index(sdirin,'year')
if (indx .ge. 1) sdirin(indx:indx+3) = ayrstart
indx = index(sdirin,'year')
if (indx .ge. 1) sdirin(indx:indx+3) = ayrstop

indx = index(fnamein,'xxxxxx')
if (indx .ge. 1) fnamein(indx:indx+5) = sitename
indx = index(fnamein,'year')
if (indx .ge. 1) fnamein(indx:indx+3) = ayrstart
indx = index(fnamein,'year')
if (indx .ge. 1) fnamein(indx:indx+3) = ayrstop

indx = index(fnamein,'tt')
if (indx .ge. 1) fnamein(indx:indx+1) = filetype

filein = trim(fdirin) // trim(sdirin) // trim(fnamein)

open(unit=11,file=trim(filein),form='formatted')
print('(a)'),'Opening File: '
print('(2a)'),'   ',trim(fnamein)

read(11,('(a)')) trashall
nlen=len(trim(trashall))
numvars=1
do v=1,nlen
   if (trashall(v:v) .eq. ',') then
      numvars=numvars+1
   endif
enddo

if (ttest2) then
    numvars=numvars-2 !remove timestamps
else
    numvars=numvars-1
endif
print('(a,i6)'),'   Number of Variables: ',numvars
print('(a,i8)'),'   Number of Times: ',numt
close(11)

!----------------
!Allocate variable arrays
allocate(tstartsave(numt),tendsave(numt))
allocate(varsname(numvars),varsid(numvars))
allocate(varstemp(numvars),varsout(numt))
allocate(varssave(numvars,numt))

!---------------------
!Read the input file
!print*,'Opening the file: ',trim(filein)
open(unit=11,file=trim(filein),form='formatted')
if (ttest2) then
   read(11,*) tstart_name, tend_name, varsname
   print*,'  Times: ',tstart_name,'  ',tend_name
else
   read(11,*) tstart_name, varsname
   print*,'  Times: ',tstart_name
endif
print*,'  Variables: ',trim(varsname(1)),'  ',trim(varsname(2)), &
        '  ...  ', trim(varsname(numvars-1)),'  ', trim(varsname(numvars))

do t=1, numt
   if (ttest2) then
       read(11,*) tstart, tend, varstemp(:)
       tstartsave(t) = tstart
       tendsave(t) = tend
   else
       read(11,*) tstart, varstemp(:)
       tstartsave(t) = tstart
   endif
   varssave(:,t) = varstemp(:)
enddo

close(11)

!----------------------------------------------------
!Modify any data

!----------------------------------------------------
!Write out the file
fileout = trim(fdirout) // sitename // &
       '/' // shortsite // '_' // &
       shortyrstart // shortyrstop // trim(suffixout)

status = nf90_create(trim(fileout), nf90_clobber, ncid)
if (status .ne. nf90_noerr) then
   print*,'Error Creating File: '
   print*,'  ',trim(fileout)
else
   print*,''
   print('(a)'),'Writing File: '
   print('(2a)'),'  ',trim(fileout)
endif

status = nf90_put_att( ncid, nf90_global, 'Source', &
         'FLUXNET2015')
status = nf90_def_dim( ncid, 'numt', numt, timedid )
status = nf90_def_dim( ncid, 'clen', 12, clendid )

status = nf90_def_var( ncid, 'time', nf90_double,timedid,timeid )
status = nf90_def_var( ncid,trim(tstart_name),nf90_char,(/clendid,timedid/),tstartid )
if (ttest2) then
    status = nf90_def_var( ncid,trim(tend_name),nf90_char,(/clendid,timedid/),tendid )
    if (status .ne. nf90_noerr) then
        print*,'Error Defining TimeStamp: ',nf90_strerror(status)
        print*,'Stopping.'
        STOP
    endif
endif

do v=1,numvars
   status = nf90_def_var( ncid, trim(varsname(v)), nf90_float,timedid,varsid(v))
   if (status .ne. nf90_noerr) then
       print*,'Error Defining Variable: ',varsname(v)
       print*,'Stopping.'
       STOP
   endif
enddo
status = nf90_enddef( ncid )

status = nf90_put_var( ncid, timeid, time )
status = nf90_put_var( ncid, tstartid, tstartsave)
if (ttest2) then
    status = nf90_put_var( ncid, tendid, tendsave)
endif

do v=1,numvars
   varsout(:) = varssave(v,:)
   status = nf90_put_var( ncid, varsid(v), varsout )
enddo

status = nf90_close(ncid)

print*,''
print('(5a)'),'Finished Processing ',sitename,' (',filetype,')'
print*,''
end program fluxnet15_csv_to_netcdf


!==========================================
! Function to create lower case letters
subroutine to_lower(strIn, strOut)

implicit none

character(len=*), intent(in) :: strIn
character(len=len(strIn)) :: strOut
integer :: i,j

do i= 1, len(strIn)
   j = iachar(strIn(i:i))
   if (j >= iachar("A") .and. j <= iachar("Z")) then
      strOut(i:i) = achar(iachar(strIn(i:i))+32)
   else
      strOut(i:i) = strIn(i:i)
   endif
enddo

end subroutine to_lower
