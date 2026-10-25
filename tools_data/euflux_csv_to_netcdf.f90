!Program to convert filled text data into a netcdf file.
!
!kdhaynes, 07/17

program euflux_csv_to_netcdf

use netcdf
implicit none

character*120, parameter :: specfile='euflux_csv_to_netcdf.txt'
integer, dimension(12), parameter :: &
    dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
integer, parameter :: mminval=-900
integer, parameter :: badval=-9999
integer, parameter :: vlen=36, slen=6
integer, parameter :: vfimax=30

!input variables
character(len=slen) :: sitename
integer :: yrstart, yrstop, numyr
integer :: hrperday, timeout
character*120 :: dirin, fnamein, dirout, fnameout
integer :: numvarstosave
character(len=vlen), dimension(:), allocatable :: vnamesave

character*120 :: trash
character(len=5000) :: trashall

!time variables
integer, dimension(:), allocatable :: numdayperyr

!file variables
integer :: numvars
character(len=vlen), dimension(:), allocatable :: varsname
character(len=6) :: suffixout
character*150 :: filein, fileincopy, fileout
real*8, dimension(:), allocatable :: time
integer :: indx, vfi
character(len=3) :: cvfi
character(len=4) :: cvfi4
character(len=4) :: cyr
logical :: exist
character(len=2) :: shortyrstart, shortyrstop
character(len=3) :: shortsite
character(len=4) :: ayrstart, ayrstop

!netcdf variables
integer :: ncid, timedid
integer :: timeid
integer, dimension(:), allocatable :: varsid

!data variables
integer :: numt, numtref, numttemp, tdays
real*8 :: timetemp
real, dimension(:), allocatable :: varstemp
integer, dimension(:), allocatable :: varsref
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
integer :: v, vv, nlen, vcount
integer :: tout, toutstart, toutstop

!----------------------------------------------------------
!----------------
!Read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,'(a6)') sitename
read(11,*) yrstart
read(11,*) yrstop
numyr=yrstop-yrstart+1

read(11,*) hrperday
read(11,*) timeout
read(11,*) trash
read(11,*) dirin
read(11,*) trash
read(11,*) fnamein
read(11,*) trash
read(11,*) dirout

read(11,*) trash
read(11,*) trash

read(11,('(a)')) trashall
nlen=len(trim(trashall))
numvarstosave=1
do v=1,nlen
   if (trashall(v:v) .eq. ',') then
      numvarstosave=numvarstosave+1
   endif
enddo
!print('(a,i3)'),'Number of Vars Saved: ', numvarstosave
close(11)

allocate(vnamesave(numvarstosave))
vnamesave(:) = ''

count=1
vcount=1
do v=1,nlen
   if (trashall(v:v) .eq. ',') then
      vnamesave(count) = trashall(vcount:v-1)
      count=count+1
      vcount=v+1
   endif
enddo
vnamesave(numvarstosave) = trashall(vcount:nlen)

!print*,'  Names of Vars Saved: '
!do v=1,numvarstosave
!    print*,'     ',trim(vnamesave(v))
!enddo

write(ayrstart,'(i4)') yrstart
write(ayrstop,'(i4)') yrstop
call to_lower(sitename(4:6), shortsite)
shortyrstart = ayrstart(3:4)
shortyrstop = ayrstop(3:4)


!----------------
!Allocate arrays
allocate(numdayperyr(numyr))
do yr=yrstart, yrstart+numyr-1
    if (mod(yr,4) .eq. 0) then
        numdayperyr(yr-yrstart+1)=366
    else
        numdayperyr(yr-yrstart+1)=365
    endif
enddo
numt=sum(numdayperyr)*hrperday
allocate(time(numt))
startt=yr + delt/2.

allocate(varssave(numvarstosave,numt))
allocate(varsref(numvarstosave))
varssave(:,:) = badval

!---------------------
!Get the data by looping over the years
indx = index(dirin,'xxxxx')
if (indx .ge. 1) then
   dirin(indx:indx+indx+1) = sitename(1:2)
   dirin(indx+2:indx+4) = sitename(4:6)
endif

indx = index(fnamein,'xxxxx')
if (indx .ge. 1) then
   fnamein(indx:indx+1) = sitename(1:2)
   fnamein(indx+2:indx+4) = sitename(4:6)
endif

count = 1
filein=''
do yr = yrstart, yrstop
    !Set time
    delt=1./(hrperday*numdayperyr(yr-yrstart+1))

   !Create file name
   write(filein,'(a,a)') trim(dirin),trim(fnamein)

   indx = index(filein,'year')
   if (indx .ge. 1) then
      write(cyr,'(i4)') yr
      filein(indx:indx+3) = cyr
   endif

   vfi = 1
   indx = index(filein,'vvv')
   if (indx .ge. 1) then
       fileincopy = filein
       exist = .false.
       do while ((.not. exist) .and. &
                (vfi .lt. vfimax))

                if (vfi .lt. 10) then
                   write(cvfi,'(a,i2.2)') 'v',vfi
                   filein(indx:indx+2) = cvfi
                else
                   write(cvfi4,'(a,i3.3)') 'v',vfi
                   filein(indx+3:150) = fileincopy(indx+2:149)
                   filein(indx:indx+3) = cvfi4
               endif  

                inquire(file=trim(filein), exist=exist)
                vfi = vfi + 1
       enddo
   endif
   if (.not. exist) then
       print*,''
       print*,'File Does Not Exist.'
       print*,'Last File Name Tried: '
       print*,'  ',trim(filein)
       print*,'Stopping.'
       print*,''
       stop
    endif

   !Find the number of variables in file
   print*,'Opening the file: '
   print*,' ',trim(filein)
   open(unit=11,file=trim(filein),form='formatted')
   read(11,('(a)')) trashall
   close(11)

   nlen=len(trim(trashall))
   numvars=1
   do v=1,nlen
      if (trashall(v:v) .eq. ',') then
         numvars=numvars+1
      endif
   enddo
   print('(a,i3)'),'  Num Vars: ',numvars
   allocate(varsname(numvars))
   allocate(varstemp(numvars))

   !Open file and get variable names
   open(unit=11,file=trim(filein),form='formatted')
   read(11,*) varsname
   !print*,'Variables: ',varsname

   do v=1,numvarstosave
      varsref(v) = 0
      do vv=1,numvars
         if (trim(vnamesave(v)) .eq. trim(varsname(vv))) then
            varsref(v) = vv
         endif
      enddo
    enddo
         
   !Process the file
   do day=1,numdayperyr(yr-yrstart+1)
      do hr=1,hrperday
         !Read
         read(11,*) varstemp

         !Save
         do v=1,numvarstosave
            if (varsref(v) .gt. 0) then
               varssave(v,count) = varstemp(varsref(v))
            endif
         enddo

         !Set time
         if (count .gt. 1) then
             time(count) = time(count-1) + delt
        elseif (count .eq. 1) then
             time(count) =  yr + delt/2.
         endif

         !Update counter
         count=count+1
      enddo
   enddo
   close(11)

   deallocate(varsname,varstemp)

enddo !yr=yrstart, yrstop


!----------------------------------------------------
!Calculate the output and write out to file
allocate(varstemp(numvarstosave))
allocate(varsanum(numvarstosave))

toutstart=timeout
toutstop=timeout
if (timeout .eq. 5) then
   toutstart=1
   toutstop=4
endif

do tout=toutstart,toutstop
   if (tout == 0) then
      if (hrperday .eq. 48.) then
          numtmean=1
      else
          print*,'Cannot Output Half-Hourly Data'
          print*,'Stopping.'
          stop
      endif
   elseif (tout == 1) then
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

   elseif (tout == 2) then
      numtmean=hrperday
   elseif (tout == 3) then
      numtmean=hrperday*7
   endif

   if (tout < 4) then
       newnumt=numt/numtmean
   else
       newnumt=numyr*12.
   endif

   allocate(timemean(newnumt))
   allocate(varsmean(numvarstosave,newnumt))

   timetemp = 0.
   timemean(:) = 0.
   varstemp(:) = 0.
   varsanum(:) = 0.
   varsmean(:,:) = 0.

   count=1
   countnew=1
   counttmean=0
   do yr=yrstart,yrstop
      do mon=1,12
         if (tout .eq. 4) numtmean=dayspermon(mon)*hrperday

         tdays=dayspermon(mon)
         if ((mod(yr,4) .eq. 0) .and. &
             (mon .eq. 2)) then
              tdays=tdays+1
         endif
         do day=1,tdays
            do hr=1,hrperday

               timetemp = timetemp + time(count)
               do v=1,numvarstosave
                  if (varssave(v,count) > mminval) then
                      varsanum(v) = varsanum(v) + 1
                      varstemp(v) = varstemp(v) + varssave(v,count)
                  endif
               enddo

               count=count + 1
               counttmean=counttmean + 1

               if (counttmean .eq. numtmean) then
                  timemean(countnew) = timetemp/numtmean
                  do v=1,numvarstosave
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

   !Write out the file
   write(fnameout,'(6a)') sitename, '/', &
      shortsite, '_', shortyrstart, shortyrstop

   if (tout == 0) then
      write(suffixout,'(a)') '_f.nc'
   elseif (tout == 1) then
      write(suffixout,'(a)') '_h.nc'
   elseif (tout == 2) then
      write(suffixout,'(a)') '_d.nc'
   elseif (tout == 3) then
      write(suffixout,'(a)') '_w.nc'
   elseif (tout == 4) then
      write(suffixout,'(a)') '_m.nc'
   else
      print*,'Invalid Time For Output.  Stopping'
      stop
   endif
   write(fileout,'(3a)') trim(dirout),trim(fnameout),trim(suffixout)

   if (tout .eq. toutstart) print*,''
   print*,'Writing: ',trim(fileout)
   if (tout .eq. toutstop) print*

   call check ( nf90_create(trim(fileout), nf90_clobber, ncid) )

   call check ( nf90_def_dim( ncid, 'numt', newnumt, timedid ) )
   call check ( nf90_def_var( ncid, 'time', nf90_double,timedid,timeid ) )

   allocate(varsid(numvarstosave))
   do v=1,numvarstosave
      call check ( nf90_def_var( ncid, trim(vnamesave(v)), &
                nf90_float,timedid,varsid(v)))
      !print*,'Defined variable: ',varsname(v)
   enddo
   call check ( nf90_enddef( ncid ) )

   call check ( nf90_put_var( ncid, timeid, timemean ) )

   allocate(varsout(newnumt))
   do v=1,numvarstosave
      varsout(:) = varsmean(v,:)
      call check ( nf90_put_var( ncid, varsid(v), varsout ) )
   enddo

   call check ( nf90_close(ncid) )

   deallocate(timemean,varsmean)
   deallocate(varsout,varsid)

enddo !tout

end program euflux_csv_to_netcdf

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

