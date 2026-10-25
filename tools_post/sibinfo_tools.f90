!===========================================
!This file contains miscellaneous subroutines
!   for the post processing of SiB4 output.
!===========================================


!=========================================
subroutine calc_gridarea(nlon, nlat, &
      lon, lat, area)
!=========================================
!Subroutine to calculate gridcell areas.
!
!kdhaynes, 06/16
!
implicit none

! input variables
integer, intent(in) :: nlon, nlat
real, dimension(nlon), intent(in) :: lon
real, dimension(nlat), intent(in) :: lat
real*8, dimension(nlon,nlat), intent(inout) :: area

! areal variables
real :: deltalon, deltalat
real*8 :: pi
real*8 :: lone, lonw, loner, lonwr
real*8, dimension(nlon) :: londiff
real*8 :: lats, latn, latsr, latnr
real*8 :: sinlatsr, sinlatnr
real*8, dimension(nlat) :: sinlatdiff
real*8 :: areatot

! parameters
logical, parameter :: area_check=.false.
real*8, parameter :: radius = 6370000.  !Earth radius (m)
character(len=30), parameter :: err_message='calc_gridarea_error'

! local variables
integer :: i,j

!----------------------------
!set local variables
pi = acos(-1.0)
areatot = 0.

!calculate areal variables
deltalon = (lon(3)-lon(2))*0.5
deltalat = (lat(3)-lat(2))*0.5

do i=1,nlon
   lone=lon(i)+deltalon
   lonw=lon(i)-deltalon
   loner=lone*pi/180.
   lonwr=lonw*pi/180.
   londiff(i)=loner-lonwr
enddo

do j=1,nlat
   lats=lat(j)-deltalat
   latn=lat(j)+deltalat
   latsr=lats*pi/180.
   latnr=latn*pi/180.
   sinlatsr=sin(latsr)
   sinlatnr=sin(latnr)
   sinlatdiff(j)=sinlatnr-sinlatsr
enddo

do i=1, nlon
   do j=1, nlat
      area(i,j) = radius*radius*londiff(i)*sinlatdiff(j)
      areatot = areatot + area(i,j)
   enddo !j
enddo !i

if (area_check) then
   print*,''
   print('(a,E15.8,a)'),'Area used vs actual:',areatot,'  0.50990436E+15 m2'
endif

 
end subroutine calc_gridarea



!=========================================
subroutine calc_sibarea(  &
      nsib, lon, lat, isglobal, area)
!=========================================
!Subroutine to calculate sib areas.
!
!kdhaynes, 06/16
!
implicit none

! input variables
integer, intent(in) :: nsib
real, dimension(nsib), intent(in) :: lon
real, dimension(nsib), intent(in) :: lat
logical, intent(in) :: isglobal
real*8, dimension(nsib), intent(inout) :: area

! areal variables
real :: deltalon, deltalat
real*8 :: pi
real*8 :: lone, lonw, loner, lonwr
real*8 :: londiff
real*8 :: lats, latn, latsr, latnr
real*8 :: sinlatsr, sinlatnr
real*8 :: sinlatdiff
real*8 :: areatot

! parameters
logical, parameter :: area_check=.false.
real*8, parameter :: radius = 6370000.  !Earth radius (m)

! local variables
integer :: n
real :: minll, nextll
real, dimension(nsib) :: lltemp

!----------------------------
!set local variables
pi = acos(-1.0)
areatot = 0.

!calculate areal variables
IF (isglobal) THEN
   lltemp = lon
   minll = minval(lltemp)
   where(lltemp .eq. minll) lltemp=999.
   nextll = minval(lltemp)
   deltalon = (nextll-minll)*0.5
ELSE
   deltalon = 0.25
ENDIF

IF (isglobal) THEN
   lltemp = lat
   minll = minval(lltemp)
   where(lltemp .eq. minll) lltemp=999.
   nextll = minval(lltemp)
   deltalat = (nextll-minll)*0.5
ELSE
   deltalat = 0.25
ENDIF

do n=1,nsib
   lone=lon(n)+deltalon
   lonw=lon(n)-deltalon
   loner=lone*pi/180.
   lonwr=lonw*pi/180.
   londiff=loner-lonwr

   lats=lat(n)-deltalat
   latn=lat(n)+deltalat
   latsr=lats*pi/180.
   latnr=latn*pi/180.

   sinlatsr=sin(latsr)
   sinlatnr=sin(latnr)
   sinlatdiff=sinlatnr-sinlatsr

   area(n) = radius*radius*londiff*sinlatdiff
   areatot = areatot + area(n)

enddo !n

if (area_check) then
   print*,''
   print('(a,E15.8,a)'),'Area used vs actual:',areatot,'  0.50990436E+15 m2'
endif

 
end subroutine calc_sibarea


!=========================================
subroutine calc_tconvert(ntime, tconvert)
!=========================================
!Subroutine to calculate time conversion
!   factor from the number of timesteps in
!   MONTHLY output files.
!
!kdhaynes, 10/17
!
implicit none

! input variables
integer, intent(in) :: ntime
real*8, intent(inout) :: tconvert

!----------------------------
!calculate time factor
select case (ntime)
case (1)  !monthly output: !assume 30-day months!
   tconvert = 3600.*24.*30.  !convert from s-1 to mon-1
case (12) !monthly output
   tconvert = 3600.*24.*30.  

case (28) !daily output
   tconvert = 3600.*24.  !convert from s-1 to day-1
case (29) !daily output
   tconvert = 3600.*24.
case (30) !daily output
   tconvert = 3600.*24.
case (31) !daily output
   tconvert = 3600.*24.

case (224) !3-hourly output
   tconvert = 3600.*3.  !convert from s-1 do 3hr-1
case (240) !3-hourly output
   tconvert = 3600.*3.
case (248) !3-hourly output
   tconvert = 3600.*3.

case (336) !2-hourly output
   tconvert = 3600.*2.  !convert from s-1 to 2hr-1
case (360) !2-hourly output
   tconvert = 3600.*2.
case (372) !2-hourly output
   tconvert = 3600.*2.

case (24)  !hourly output, daily files
   tconvert = 3600.    !convert from s-1 to hr-1
case (672) !hourly output
   tconvert = 3600.    !convert from s-1 to hr-1
case (720) !hourly output
   tconvert = 3600.
case (744) !hourly output
   tconvert = 3600.

case default
   print*,'Unknown number of timesteps: ',ntime
   print*,'Stopping'
   STOP

end select

end subroutine calc_tconvert


!=========================================
subroutine calc_cando_daily(filename, flag_daily)
!=========================================
!Subroutine to determine if the original
!   file has small enough time resoluation
!   for breaking up into daily files.
!
!kdhaynes, 10/17

use netcdf
implicit none

! input variables
character(len=*), intent(in) :: filename
logical, intent(inout) :: flag_daily

! local variables
integer :: ntime
integer :: status, ncid
integer :: dimid, varid
integer :: spos
character(len=20) :: att_text

!---------------------------------------------------
status = nf90_open(trim(filename),nf90_nowrite,ncid)
IF (status .NE. nf90_noerr) THEN
   print*,'Error Opening File To Determine Time Units.'
   print*,'   ',trim(filename)
   print*,'Stopping.'
   STOP
ENDIF

status = nf90_inq_dimid(ncid,'time',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=ntime)

status = nf90_inq_varid(ncid,'time',varid)
status = nf90_get_att(ncid,varid,'units',att_text)
status = nf90_close(ncid)

flag_daily=.false.

spos = index(att_text,'day')
IF (spos .GE. 0) THEN
   IF (ntime .gt. 12) flag_daily=.true.
ENDIF

spos = index(att_text,'hour')
IF (spos .GE. 0) THEN
   IF (ntime .gt. 1) flag_daily=.true.
ENDIF

spos = index(att_text,'sec')
IF (spos .GE. 0) THEN
   IF (ntime .gt. 1) flag_daily=.true.
ENDIF

end subroutine calc_cando_daily


!=========================================
subroutine check_gridded(filename, grid_flag)
!=========================================
!Subroutine to check to see if a file
!   is gridded or in vector format.
!
!Returns true if gridded.
!
!kdhaynes, 06/16
!
use netcdf
implicit none

! input variables
character(len=256), intent(in) :: filename
logical, intent(inout) :: grid_flag

! local variables
integer :: ncid,dimid,status
logical :: file_exist

!-----------------------

inquire(file=trim(filename), exist=file_exist)         
if (.not. file_exist) then
   print*,'File not found: '
   print*,'  ',trim(filename)
   stop 'check_gridded_error'
endif

grid_flag=.false.
status = nf90_open(trim(filename), nf90_nowrite, ncid)

status = nf90_inq_dimid(ncid,'lon',dimid)
IF (status .eq. nf90_noerr) grid_flag=.true.

status = nf90_inq_dimid(ncid,'nlon',dimid)
IF (status .eq. nf90_noerr) grid_flag=.true.

status = nf90_close(ncid)

end subroutine check_gridded


!=========================================
subroutine create_sib_eqname( &
   nprocs, nspin, &
   fileindir, fileoutdir, &
   prefix, suffix,  &
   yrstart, yrstop, &
   filesineq, filesouteq, numeqs)
!=========================================
!Subroutine to get the name of SiB4 
!  equilibrium files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
integer, intent(in) :: nprocs, nspin
character(len=180), intent(in) :: fileindir
character(len=180), intent(in) :: fileoutdir
character(len=*), intent(in) :: prefix, suffix
integer, intent(in) :: yrstart, yrstop
character(len=256), dimension(nprocs,nspin), intent(inout) :: &
      filesineq
character(len=256), dimension(nspin), intent(inout) :: &
      filesouteq
integer, intent(inout) :: numeqs

!local variables
integer :: p, s, yr, mon
character(len=256) :: tempfilename
logical :: file_exist

integer, parameter :: ntries=8
character(len=25), dimension(ntries), parameter :: &
     pformat = [ &
     '(a,a,a,i9.9,a)', &
     '(a,a,a,i8.8,a)', &
     '(a,a,a,i7.7,a)', &
     '(a,a,a,i6.6,a)', &
     '(a,a,a,i5.5,a)', &
     '(a,a,a,i4.4,a)', &
     '(a,a,a,i3.3,a)', &
     '(a,a,a,i2.2,a)']

integer :: testfnum, usefnum

!-------------------------------------


!Test all p numbers:
file_exist = .false.
usefnum=0
testfnum = ntries
do while (testfnum .gt. 0)
   write( tempfilename, pformat(testfnum) )  &
          trim(fileindir),trim(prefix), 'p', 1,   &
          trim(suffix)
    inquire(file=trim(tempfilename), exist=file_exist)
    if (file_exist) then
       usefnum=testfnum
       testfnum = 0
    endif
    testfnum = testfnum - 1
enddo

!Create the file names
IF (usefnum .gt. 0) THEN
   do p = 1, nprocs
      file_exist = .false.   
      write( tempfilename, pformat(usefnum) )  &
             trim(fileindir),trim(prefix), 'p', p,   &
             trim(suffix)
          inquire(file=trim(tempfilename), exist=file_exist)
          if (file_exist) then
             numeqs=1
             filesineq(p,numeqs) = tempfilename
             write( filesouteq(numeqs), '(a,a,a)' ) &
                  trim(fileoutdir),trim(prefix), trim(suffix)
             print*,'  EQ File: ',trim(filesineq(p,numeqs))
          endif
    enddo
ENDIF


end subroutine create_sib_eqname

!=========================================
subroutine create_sib_eqnames( &
   nprocs, nspin, &
   fileindir, fileoutdir, &
   prefix, suffix,  &
   yrstart, yrstop, &
   filesineq, filesouteq, numeqs)
!=========================================
!Subroutine to get the name of SiB4 
!  equilibrium files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
integer, intent(in) :: nprocs, nspin
character(len=180), intent(in) :: fileindir
character(len=180), intent(in) :: fileoutdir
character(len=*), intent(in) :: prefix, suffix
integer, intent(in) :: yrstart, yrstop
character(len=256), dimension(nprocs,nspin), intent(inout) :: &
      filesineq
character(len=256), dimension(nspin), intent(inout) :: &
      filesouteq
integer, intent(inout) :: numeqs

!local variables
integer :: p, s, yr, mon
character(len=256) :: tempfilename
logical :: file_exist

integer, parameter :: ntries=8
character(len=25), dimension(ntries), parameter :: &
     pformats = [ &
     '(a,a,a,i9.9,a,i2.2,a)', &
     '(a,a,a,i8.8,a,i2.2,a)', &
     '(a,a,a,i7.7,a,i2.2,a)', &
     '(a,a,a,i6.6,a,i2.2,a)', &
     '(a,a,a,i5.5,a,i2.2,a)', &
     '(a,a,a,i4.4,a,i2.2,a)', &
     '(a,a,a,i3.3,a,i2.2,a)', &
     '(a,a,a,i2.2,a,i2.2,a)']
character(len=20), dimension(ntries), parameter :: &
     pformata = [ &
     '(a,a,i4.4,a,i9.9,a)', &
     '(a,a,i4.4,a,i8.8,a)', &
     '(a,a,i4.4,a,i7.7,a)', &
     '(a,a,i4.4,a,i6.6,a)', &
     '(a,a,i4.4,a,i5.5,a)', &
     '(a,a,i4.4,a,i4.4,a)', &
     '(a,a,i4.4,a,i3.3,a)', &
     '(a,a,i4.4,a,i2.2,a)']
character(len=25), dimension(ntries), parameter :: &
     pformatm = [ &
     '(a,a,i4.4,i2.2,a,i9.9,a)', &
     '(a,a,i4.4,i2.2,a,i8.8,a)', &
     '(a,a,i4.4,i2.2,a,i7.7,a)', &
     '(a,a,i4.4,i2.2,a,i6.6,a)', &
     '(a,a,i4.4,i2.2,a,i5.5,a)', &
     '(a,a,i4.4,i2.2,a,i4.4,a)', &
     '(a,a,i4.4,i2.2,a,i3.3,a)', &
     '(a,a,i4.4,i2.2,a,i2.2,a)']
     
integer :: testfnum, usefnum

!-------------------------------------

!Create the file names
numeqs = 0

!!check all spinup iterations
s = nspin
file_exist = .false.
usefnum=0
do while (s .gt. 0)
   testfnum = ntries
   do while (testfnum .gt. 0)
      write( tempfilename, pformats(testfnum) )  &
          trim(fileindir),trim(prefix), 'p', 1,   &
          's', s, trim(suffix)
       inquire(file=trim(tempfilename), exist=file_exist)
       if (file_exist) then
          usefnum=testfnum
          testfnum = 0
          s = 0
       endif
       testfnum = testfnum - 1
    enddo
    s = s - 1
enddo

IF (usefnum .gt. 0) THEN
   do p = 1, nprocs
      s = nspin
      file_exist = .false.   
      do while (s .gt. 0)
          write( tempfilename, pformats(usefnum) )  &
             trim(fileindir),trim(prefix), 'p', p,   &
             's', s, trim(suffix)
          inquire(file=trim(tempfilename), exist=file_exist)
          if (file_exist) then
             numeqs=1
             filesineq(p,numeqs) = tempfilename
             write( filesouteq(numeqs), '(a,a,a,i2.2,a)' ) &
                  trim(fileoutdir),trim(prefix), trim(suffix)
             print*,'  EQ File: ',trim(filesineq(p,numeqs))
             s = 0
          else
             s = s - 1
          endif
       enddo  !while file doesn't exist keep checking
    enddo
ENDIF


!check annual output
file_exist = .false.
usefnum=0
testfnum = ntries
do while (testfnum .gt. 0)
   write( tempfilename, pformata(testfnum) )  &
          trim(fileindir),trim(prefix), yrstart, 'p', 1,   &
          trim(suffix)
    inquire(file=trim(tempfilename), exist=file_exist)
    if (file_exist) then
       usefnum=testfnum
       testfnum = 0
    endif
    testfnum = testfnum - 1
enddo

IF (usefnum .gt. 0) THEN
   do yr = yrstart, yrstop+1
      do p = 1, nprocs
          write( tempfilename, pformata(usefnum) ) &
              trim(fileindir),trim(prefix), yr, &
              'p', p,trim(suffix)
          inquire(file=trim(tempfilename), exist=file_exist)
          if (file_exist) then
              if (p .eq. 1) then
                   numeqs = numeqs + 1
                   write(filesouteq(numeqs),'(a,a,i4.4,a)') &
                        trim(fileoutdir),trim(prefix),yr,trim(suffix)
              endif
              filesineq(p,numeqs) = tempfilename
          endif
      enddo !p=1,nprocs
    enddo
ENDIF
    
!check annual output with month in filename
file_exist = .false.
usefnum=0
testfnum = ntries
do while (testfnum .gt. 0)
   mon = 12
   do while (mon .gt. 0)
       write( tempfilename, pformatm(testfnum) )  &
          trim(fileindir),trim(prefix), yrstart, mon, 'p', 1,   &
          trim(suffix)
       inquire(file=trim(tempfilename), exist=file_exist)
       if (file_exist) then
          usefnum=testfnum
          testfnum = 0
          mon = 0
       endif
       mon = mon - 1
    enddo
    testfnum = testfnum - 1
enddo

IF (usefnum .gt. 0) THEN
   do yr=yrstart,yrstop
      do mon=1,12
         do p=1,nprocs
             write( tempfilename, pformatm(usefnum) ) &
                  trim(fileindir),trim(prefix), yr, mon, &
                  'p', p, trim(suffix)
             inquire(file=trim(tempfilename), exist=file_exist)
             if (file_exist) then
                if (p .eq. 1) then
                    numeqs = numeqs + 1
                    write(filesouteq(numeqs),'(a,a,i4.4,i2.2,a)') &
                        trim(fileoutdir),trim(prefix),yr,mon,trim(suffix)
                endif
                filesineq(p,numeqs) = tempfilename
              endif
          enddo !nprocs
       enddo !mon
   enddo !yr
ENDIF

end subroutine create_sib_eqnames


!=========================================
subroutine create_sib_nameannual(filename,  &
   filedir, prefix, suffix, &
   yr, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  annual output files.
!
!kdhaynes, 03/18
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
integer,intent(in) :: yr
logical,intent(in) :: testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
write(filename,'(a,a,i4.4,a)') &
       trim(filedir),trim(prefix), &
       yr,trim(suffix)

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) filename=''
endif

end subroutine create_sib_nameannual


!=========================================
subroutine create_sib_nameannualc(filename,  &
   filedir, prefix, suffix, &
   text, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  monthly output files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
character(len=*), intent(in) :: text
logical,intent(in) :: testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
write(filename,'(a,a,a,a)') &
       trim(filedir),trim(prefix), &
       text,trim(suffix)

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) filename=''
endif

end subroutine create_sib_nameannualc


!=========================================
subroutine create_sib_nameday(filename,  &
   filedir, prefix, suffix, &
   yr, mon, day, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  monthly output files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
integer,intent(in) :: yr, mon, day
logical,intent(in) :: testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
write(filename,'(a,a,i4.4,i2.2,i2.2,a)') &
       trim(filedir),trim(prefix), &
       yr,mon,day,trim(suffix)

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) then
      write(filename,'(a,a,a1,i4.4,a1,i2.2,a1,i2.2,a)') &
           trim(filedir),prefix(1:len(prefix)-1), &
           '-',yr,'-',mon,'-',day,trim(suffix)

      inquire(file=trim(filename), exist=file_exist)
      if (.not. file_exist) then
         filename=''
      endif
   endif
endif

end subroutine create_sib_nameday

!=========================================
subroutine create_sib_namedayc(filename,  &
   filedir, prefix, suffix, &
   text, mon, day, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  monthly output files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
character(len=*), intent(in) :: text
integer,intent(in) :: mon, day
logical,intent(in) :: testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
write(filename,'(a,a,a,i2.2,i2.2,a)') &
       trim(filedir),trim(prefix), &
       text,mon,day,trim(suffix)

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) filename=''
endif

end subroutine create_sib_namedayc


!=========================================
subroutine create_sib_namedayp(filename,  &
   filedir, prefix, suffix, &
   yr, mon, day, proc, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  daily output files.
!
!kdhaynes, 10/19
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
integer,intent(in) :: yr, mon, day, proc
logical,intent(in) :: testflag

!local variables
integer, parameter :: ntries=8
character(len=30), dimension(ntries), parameter :: &
     daypformats = [ &
     '(a,a,i4.4,i2.2,i2.2,a1,i9.9,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i8.8,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i7.7,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i6.6,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i5.5,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i4.4,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i3.3,a)', &
     '(a,a,i4.4,i2.2,i2.2,a1,i2.2,a)']

integer :: testfnum
logical :: file_exist

!-------------------------------------

!Requires testflag to be true to use this routine
if (.not. testflag) THEN
   print*,'Expecting filenames to be tested!!'
   STOP
endif

!Create the file name
file_exist = .FALSE.
testfnum = 1
do while (.not. file_exist)
     write(filename,daypformats(testfnum)) &
          trim(filedir),trim(prefix), &
          yr,mon,day,'p',proc,trim(suffix)

     inquire(file=trim(filename), exist=file_exist)
     testfnum = testfnum + 1
     if (testfnum .gt. ntries) then
        if (.not. file_exist) then
           print*,'!!No Files Found!!'
           STOP
        endif
     endif
enddo


end subroutine create_sib_namedayp

!=========================================
subroutine create_sib_namemon(filename,  &
   filedir, prefix, suffix, &
   yr, mon, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  monthly output files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
integer,intent(in) :: yr, mon
logical,intent(in) :: testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
write(filename,'(a,a,i4.4,i2.2,a)') &
       trim(filedir),trim(prefix), &
       yr,mon,trim(suffix)

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) then
       write(filename,'(a,a,i4.4,i2.2,a,a)') &
             trim(filedir),trim(prefix), &
             yr,mon,'p001',trim(suffix)
       inquire(file=trim(filename), exist=file_exist)   
   endif

   if (.not. file_exist) filename=''
endif

end subroutine create_sib_namemon


!=========================================
subroutine create_sib_namemonc(filename,  &
   filedir, prefix, suffix, &
   text, mon, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  monthly output files.
!Instead of using a year, use text.
!
!kdhaynes, 06/16
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
character(len=*), intent(in) :: text
integer,intent(in) :: mon
logical,intent(in) :: testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
write(filename,'(a,a,a,i2.2,a)') &
       trim(filedir),trim(prefix), &
       trim(text),mon,trim(suffix)

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) filename=''
endif

end subroutine create_sib_namemonc


!=========================================
subroutine create_sib_namemonp(filename,  &
   filedir, prefix, suffix, &
   yr, mon, proc, testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  input files run on different processors.
!
!kdhaynes, 10/19
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix, suffix
integer,intent(in) :: yr, mon, proc
logical,intent(in) :: testflag

!local variables
integer, parameter :: ntries=8
character(len=25), dimension(ntries), parameter :: &
     daypformats = [ &
     '(a,a,i4.4,i2.2,a1,i9.9,a)', &
     '(a,a,i4.4,i2.2,a1,i8.8,a)', &
     '(a,a,i4.4,i2.2,a1,i7.7,a)', &
     '(a,a,i4.4,i2.2,a1,i6.6,a)', &
     '(a,a,i4.4,i2.2,a1,i5.5,a)', &
     '(a,a,i4.4,i2.2,a1,i4.4,a)', &
     '(a,a,i4.4,i2.2,a1,i3.3,a)', &
     '(a,a,i4.4,i2.2,a1,i2.2,a)']

integer :: testfnum
logical :: file_exist

!-------------------------------------

!Requires testflag to be true to use this routine
if (.not. testflag) THEN
   print*,'Expecting filenames to be tested!!'
   STOP
endif

!Create the file name
file_exist = .FALSE.
testfnum = 1
do while (.not. file_exist)
     write(filename,daypformats(testfnum)) &
          trim(filedir),trim(prefix), &
          yr,mon,'p',proc,trim(suffix)

     inquire(file=trim(filename), exist=file_exist)
     testfnum = testfnum + 1
     if (testfnum .gt. ntries) then
        if (.not. file_exist) then
           print*,'!!No Files Found!!'
           STOP
        endif
     endif
enddo


end subroutine create_sib_namemonp


!=========================================
subroutine create_sib_nameyr( &
   filename,  &
   filedir, prefix, suffix, &
   startmon, startyr, stopmon, stopyr, &
   testflag)
!=========================================
!Subroutine to get the name of SiB4 
!  crop output file.
!
!kdhaynes, 10/17
!
implicit none

!input variables
character(len=256),intent(inout) :: filename
character(len=180),intent(in) :: filedir
character(len=*), intent(in) :: prefix
character(len=*), intent(in) :: suffix
integer,intent(in) :: startmon, startyr
integer,intent(in) :: stopmon, stopyr
logical,intent(in) :: testflag

!misc variables
logical :: file_exist

!-------------------------------------

!Create the file name
IF ((startyr .eq. stopyr) .and. &
     (startmon .eq. 1) .and. &
     (stopmon .eq. 12)) THEN
   write(filename,'(2a,i4.4,a)') &
        trim(filedir),trim(prefix), &
        startyr,trim(suffix)
ELSE
   write(filename,'(2a,i4.4,i2.2,a,i4.4,i2.2,a)') &
       trim(filedir),trim(prefix), &
       startyr,startmon,'_', &
       stopyr,stopmon,trim(suffix)
ENDIF
   
if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) filename=''
endif

end subroutine create_sib_nameyr


!================================================
subroutine create_sib_nameyro(filename, filedir, &
   prefix, suffix, yrstart, yrstop, testflag)
!=================================================
!Subroutine to get the name of SiB4 annual files.
!
!kdhaynes, 06/16
!
implicit none

!input variables
character(len=256), intent(inout) :: filename
character(len=180), intent(in) :: filedir
character(len=*), intent(in) :: prefix, suffix
integer,intent(in) :: yrstart, yrstop
logical,intent(in) ::  testflag

!local variables
logical :: file_exist

!-------------------------------------

!Create the file name
if (yrstart .eq. yrstop) then
   write(filename,'(a,a,i4.4,a)') &
       trim(filedir),trim(prefix), &
       yrstart,trim(suffix)
else
   write(filename,'(a,a,i4.4,a,i4.4,a)') &
       trim(filedir),trim(prefix), &
       yrstart,'_',yrstop,trim(suffix)
endif

if (testflag) then
   inquire(file=trim(filename), exist=file_exist)         
   if (.not. file_exist) filename=''
endif

end subroutine create_sib_nameyro


!=========================================
subroutine get_sib_dimsize( &
       filename, dimname, dimsize)
!=========================================
!Subroutine to get a SiB4 dimension.
!
!kdhaynes, 06/16

use netcdf
implicit none

! input variables
character(len=256), intent(in) :: filename
character(len=*), intent(in) :: dimname
integer, intent(inout) :: dimsize

! local variables 
integer :: ncid, dimid, status

!---------------------------
 
status = nf90_open(trim(filename), nf90_nowrite, ncid)
status = nf90_inq_dimid(ncid, dimname, dimid)
IF (status .ne. nf90_noerr) THEN
   print*,'Dimension ',dimname,' not in output file.'
   print*,'Stopping.'
   stop
ENDIF
status = nf90_inquire_dimension(ncid,dimid,len=dimsize)

status = nf90_close(ncid)

end subroutine get_sib_dimsize


!==========================================================
subroutine grid_sib_nlonlat(nsib,lonsib,latsib,nlon,nlat, &
           deltalon,deltalat,truelonmin,truelatmin)
!==========================================================
! Subroutine to get new grid number of lat/lon
! kdhaynes, 02/16

implicit none

!...input variables
integer, intent (in) :: nsib
real, dimension(nsib), intent(in) :: lonsib, latsib
integer, intent(inout) :: nlon,nlat
real, intent(inout) :: deltalon, deltalat
real, intent(inout) :: truelonmin, truelatmin

!...grid information
logical, parameter :: print_gridinfo=.false.
real, dimension(nsib) :: ltemp
real :: latnext,latmin,latmax,truelatmax
real :: lonnext,lonmin,lonmax,truelonmax

!---------------------------------
  !Figure out the grid information
  !....Latitude
  latmin=minval(latsib)
  latmax=maxval(latsib)
  ltemp = latsib
  where(ltemp .eq. latmin) ltemp=latmax
  latnext=minval(ltemp)
  deltalat=latnext-latmin
  if (deltalat .le. 0.) then
     print*,'Expecting more than a single point'
     print*,'  to grid to global grid.'
     print*,'Please set output global to False.'
     STOP
  endif
  
  truelatmin=latmin
  do while (truelatmin-deltalat .gt. -90.)
     truelatmin=truelatmin-deltalat
  enddo

  truelatmax=truelatmin
  nlat=1
  do while (truelatmax+deltalat .lt. 90.)
     truelatmax=truelatmax+deltalat
     nlat=nlat+1
  enddo

  !...Longitude
  lonmin=minval(lonsib)
  lonmax=maxval(lonsib)
  ltemp = lonsib
  where(ltemp .eq. lonmin) ltemp=lonmax
  lonnext=minval(ltemp)
  deltalon=lonnext-lonmin
  if (deltalon .le. 0.) then
     print*,'Expecting more than a single point'
     print*,'  to grid to global grid.'
     print*,'Please set output global to False.'
     STOP
  endif

  truelonmin=lonmin
  do while (truelonmin-deltalon .gt. -180.)
     truelonmin=truelonmin-deltalon
  enddo

  truelonmax=truelonmin
  nlon=1
  do while (truelonmax+deltalon .lt. 180.)
     truelonmax=truelonmax+deltalon
     nlon=nlon+1
  enddo

  if (print_gridinfo) then
     print('(a)'),'      Gridded Points: '
     print('(a,2i5)'),'        Num Lon/Lat:',nlon,nlat
     print('(a,2f8.4)'),'        Resolution: ',deltalon,deltalat
     print('(a)'),''
  endif

end subroutine grid_sib_nlonlat


!==========================================================
subroutine grid_sib_nllloc(nsib,lonsib,latsib,nlon,nlat, &
           deltalon,deltalat,truelonmin,truelatmin)
!==========================================================
! Subroutine to get new grid number of lat/lon
! kdhaynes, 05/2019

implicit none

!...input variables
integer, intent (in) :: nsib
real, dimension(nsib), intent(in) :: lonsib, latsib
integer, intent(inout) :: nlon,nlat
real, intent(inout) :: deltalon, deltalat
real, intent(inout) :: truelonmin, truelatmin

!...grid information
logical, parameter :: print_gridinfo=.false.
real, dimension(nsib) :: ltemp
real :: latnext,latmin,latmax,truelatmax
real :: lonnext,lonmin,lonmax,truelonmax

!---------------------------------
  !Figure out the grid information
  !....Latitude
  latmin=minval(latsib)
  latmax=maxval(latsib)
  ltemp = latsib
  where(ltemp .eq. latmin) ltemp=latmax
  latnext=minval(ltemp)
  deltalat=latnext-latmin
  
  truelatmin=latmin
  truelatmax=latmax
  if (deltalat .gt. 0.) then
     nlat = (latmax-latmin)/deltalat + 1
  else
     nlat = 1
  endif
  
  !...Longitude
  lonmin=minval(lonsib)
  lonmax=maxval(lonsib)
  ltemp = lonsib
  where(ltemp .eq. lonmin) ltemp=lonmax
  lonnext=minval(ltemp)
  deltalon=lonnext-lonmin

  truelonmin=lonmin
  truelonmax=lonmax
  if (deltalon .gt. 0.) then
     nlon = (lonmax-lonmin)/deltalon + 1
  else
     nlon = 1
  endif

  if (print_gridinfo) then
     print('(a)'),'      Gridded Points: '
     print('(a,2i5)'),'        Num Lon/Lat:',nlon,nlat
     print('(a,2f8.4)'),'        Resolution: ',deltalon,deltalat
     print('(a)'),''
  endif

end subroutine grid_sib_nllloc


!========================================================
subroutine grid_sib_ref(nsib,siblon,siblat,nlon,nlat, &
                    deltalon, deltalat, lonmin, latmin, &
                    lon,lat, lonref,latref)
!========================================================
! Subroutine to get new lat/lon references
! kdhaynes, 02/16

implicit none

!...input variables
integer, intent (in) :: nsib
real, dimension(nsib), intent(in) :: siblon, siblat
integer, intent(in) :: nlon,nlat
real, intent(in) :: deltalon, deltalat
real, intent(in) :: lonmin, latmin
real, dimension(nlon), intent(inout) :: lon
real, dimension(nlat), intent(inout) :: lat
integer, dimension(nsib), intent(inout) :: lonref, latref

!...misc variables
integer :: i,x

  !Put the data onto the grid
  lon(1) = lonmin
  do i=2,nlon
     lon(i) = lonmin + deltalon*(i-1)
  enddo

  lat(1) = latmin
  do i=2,nlat
     lat(i) = latmin + deltalat*(i-1)
  enddo

  latref(:)=-999
  lonref(:)=-999

  do i=1,nsib

     do x=1,nlat
        if (siblat(i) .eq. lat(x)) then
           latref(i)=x
           exit
        endif
     enddo

     do x=1,nlon
       if (siblon(i) .eq. lon(x)) then
          lonref(i)=x
          exit
       endif
     enddo
   
     if (latref(i) .lt. 1 .or. lonref(i) .lt. 1) then
          print*,'Bad Lat/Lon Regrid Value.  Stopping.'
          print*,'Original Lat/Lon: ',siblat(i),siblon(i)
          stop
     endif

  enddo  !i=1,nsib

end subroutine grid_sib_ref

!========================================================
subroutine ungrid_dataf( &
           nlon, nlat, lon, lat, datain, &
           nsib, siblon, siblat, &
           lonref, latref, dataout)
!========================================================
! Subroutine to put gridded input onto sib vector.
!   - If the sib vector does not include the
!      lat/lon, the reference will be -999.
!
! kdhaynes, 10/17

implicit none

!input variables
integer, intent(in) :: nlon, nlat
real, dimension(nlon), intent(in) :: lon
real, dimension(nlat), intent(in) :: lat
real, dimension(nlon,nlat), intent(in) :: datain

integer, intent(in) :: nsib
real, dimension(nsib), intent(in) :: siblon, siblat
integer, dimension(nsib), intent(inout) :: lonref, latref
real, dimension(nsib), intent(inout) :: dataout

!local variables
integer :: n, x, y
real :: diff, mindist

!---------------------------------------------------
DO n=1,nsib
   !Find references if necessary
   IF (lonref(n) .le. 0) THEN
       mindist=999.
       do x=1,nlon
          diff = abs(lon(x) - siblon(n))
          IF (diff .lt. mindist) THEN
             lonref(n) = x
             mindist = diff
          ENDIF
       enddo

       IF ((lonref(n) .LE. 0) .OR. &
           (lonref(n) .GT. nlon)) THEN
           print*,'Error Finding Matching SiB/Grid Longitude.'
           print*,'Stopping.'
           STOP
       ENDIF
   ENDIF

   IF (latref(n) .le. 0) THEN
      mindist=999.
      do y=1,nlat
         diff = abs(lat(y) - siblat(n))
         IF (diff .lt. mindist) THEN
             latref(n) = y
             mindist = diff
         ENDIF
      enddo

      IF ((latref(n) .LE. 0) .OR. &
          (latref(n) .GT. nlat)) THEN
          print*,'Error Finding Matching SiB/Grid Latitude.'
          print*,'   nsib/nlat: ',nsib,nlat
          print*,'   min/max siblat : ',minval(siblat),maxval(siblat)
          print*,'   min/max datalat: ',minval(lat),maxval(lat)
          print*,'Stopping.'
          STOP
      ENDIF
    ENDIF 

   !Ungrid the data onto specified vector
   dataout(n) = datain(lonref(n),latref(n))
ENDDO


end subroutine ungrid_dataf



!========================================================
subroutine zcopyf90( ncid, ncido)
!========================================================
! Subroutine to copy file from ncid to ncido.
!
! kdhaynes, 05/18

use netcdf
use sibinfo_module, only: &
    use_deflate, deflev
implicit none

! input variables
integer, intent(in) :: &
    ncid, &  ! input file id
    ncido    ! output file id

! netcdf variables
integer :: status                              ! netcdf function return value
integer :: numvars                             ! # variables in input files
integer :: numatts                             ! # attributes in input files
character(len=20) :: name                      ! variable name
integer, dimension(:), allocatable :: dimids   ! dimension id#s
integer, dimension(:), allocatable :: dimlens  ! dimension values
integer, dimension(:), allocatable :: vardims  ! variable dimensions
integer :: xtype                               ! variable data type
integer :: numdims                             ! # dimensions in input files
integer :: varid                               ! variable id#

! local variables
integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7),  dimension(:), allocatable :: cvalues7 
character(len=10), dimension(:), allocatable :: cvalues10
double precision, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues1
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4
integer, dimension(:), allocatable :: ivalues1
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3

! indices
integer :: a, d, v

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! get number of dims, vars, and atts
status = nf90_inquire(ncid, nDimensions=numdims, &
          nVariables=numvars, nAttributes=numatts)

! copy global dimensions to output file
allocate(dimids(numdims))
allocate(dimlens(numdims))
do d=1,numdims
   status = nf90_inquire_dimension(ncid, d, name=name, len=dimlens(d))
   status = nf90_def_dim(ncido, trim(name), dimlens(d), dimids(d))
enddo

! copy global attributes to output file
do a=1, numatts
   status = nf90_inq_attname(ncid, nf90_global, a, name)
   status = nf90_copy_att(ncid, nf90_global, trim(name), &
            ncido, nf90_global)
enddo

! define variables in output file
allocate(vardims(numdims))
do v=1,numvars
   status = nf90_inquire_variable(ncid,v,name=name, &
            xtype=xtype, ndims=numdims, dimids=vardims, &
            natts=numatts)
   status = nf90_def_var(ncido, trim(name), xtype, &
            vardims(1:numdims), varid)
   if (use_deflate) then
      status = nf90_def_var_deflate(ncido, varid, 1, 1, deflev)
   endif

   do a=1,numatts
      status = nf90_inq_attname(ncid,v,a,name=name)
      status = nf90_copy_att(ncid,v,trim(name),ncido,varid)
   enddo !a=1,numatts
enddo !v=1,numvars

! stop defining variables
status = nf90_enddef(ncido)

! copy variables to output file
do v=1,numvars
   status = nf90_inquire_variable(ncid,v, &
            name=name, xtype=xtype, &
            ndims=numdims, dimids=vardims)

   if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
      allocate(dvalues(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,dvalues)
      status = nf90_put_var(ncido,v,dvalues)
      deallocate(dvalues)

   elseif((xtype .eq. 5) .and. (numdims .eq. 1)) then
      allocate(fvalues1(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,fvalues1)
      status = nf90_put_var(ncido,v,fvalues1)
      deallocate(fvalues1)

   elseif((xtype .eq. 5) .and. (numdims .eq. 2)) then
      allocate(fvalues2(dimlens(vardims(1)), &
                        dimlens(vardims(2))))
      status = nf90_get_var(ncid,v,fvalues2)
      status = nf90_put_var(ncido,v,fvalues2)
      deallocate(fvalues2)

   elseif((xtype .eq. 5) .and. (numdims .eq. 3)) then
      allocate(fvalues3(dimlens(vardims(1)), &
                        dimlens(vardims(2)), &
                        dimlens(vardims(3))))
      status = nf90_get_var(ncid,v,fvalues3)
      status = nf90_put_var(ncido,v,fvalues3)
      deallocate(fvalues3)

   elseif((xtype .eq. 5) .and. (numdims .eq. 4)) then
      allocate(fvalues4(dimlens(vardims(1)), &
                        dimlens(vardims(2)), &
                        dimlens(vardims(3)), &
                        dimlens(vardims(4))))
      status = nf90_get_var(ncid,v,fvalues4)
      status = nf90_put_var(ncido,v,fvalues4)
      deallocate(fvalues4)

   elseif((xtype .eq. 4) .and. (numdims .eq. 1)) then
      allocate(ivalues1(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,ivalues1)
      status = nf90_put_var(ncido,v,ivalues1)
      deallocate(ivalues1)

   elseif((xtype .eq. 4) .and. (numdims .eq. 2)) then
      allocate(ivalues2(dimlens(vardims(1)), &
                        dimlens(vardims(2))))
      status = nf90_get_var(ncid,v,ivalues2)
      status = nf90_put_var(ncido,v,ivalues2)
      deallocate(ivalues2)

   elseif((xtype .eq. 4) .and. (numdims .eq. 3)) then
      allocate(ivalues3(dimlens(vardims(1)), &
                        dimlens(vardims(2)), &
                        dimlens(vardims(3))))
      status = nf90_get_var(ncid,v,ivalues3)
      status = nf90_put_var(ncido,v,ivalues3)
      deallocate(ivalues3)

   elseif((xtype .eq. 2) .and. (numdims .eq. 2)) then
      if (dimlens(vardims(1)) .eq. 3) then
          allocate(cvalues3(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues3)
          status = nf90_put_var(ncido,v,cvalues3)
          deallocate(cvalues3)

      elseif (dimlens(vardims(1)) .eq. 6) then
          allocate(cvalues6(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues6)
          status = nf90_put_var(ncido,v,cvalues6)
          deallocate(cvalues6)

      elseif (dimlens(vardims(1)) .eq. 7) then
          allocate(cvalues7(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues7)
          status = nf90_put_var(ncido,v,cvalues7)
          deallocate(cvalues7)

      elseif (dimlens(vardims(1)) .eq. 10) then
          allocate(cvalues10(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues10)
          status = nf90_put_var(ncido,v,cvalues10)
          deallocate(cvalues10)

          else
              print*,'OutputMerge: Unexpected Character Array.'
              print*,'  Var name and type: ',name,xtype
              print*, '  Number of dimensions: ',numdims
              print*, '  Dimension IDs: ',vardims
              stop
          endif

   elseif((xtype .eq. 1) .and. (numdims .eq. 1)) then
      allocate(bvalues1(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,bvalues1)
      status = nf90_put_var(ncido,v,bvalues1)
      deallocate(bvalues1)

   else
      print*,'Copy Error: Unexpected Output Type and/or Dimension.'
      print*,'  Var name and type: ',name,xtype
      print*,'  Number of dimensions: ',numdims
      print*,'  Dimension IDs: ',vardims
      stop
   endif

   if (status .ne. nf90_noerr) then
      print*,'Copy Error: Problem Writing Variable.'
      print*,'   Var name and type: ',name,xtype
      print*,'   Number of dimensions: ',numdims
      print*,'   Dimension IDs: ',vardims
    endif

enddo !v=1,numvars

end subroutine zcopyf90
