!Program to combine yearly OzFlux data in netcdf files.
!
!Converts output from typically 1/2-hourly data to
!   either hourly, daily, or monthly
!
!kdhaynes, 08/15
! 
program ozflux_nc_to_netcdf

use netcdf
implicit none

!File specific information
integer, parameter :: filehrperday=24
real, parameter :: badval=-9999.
integer, parameter, dimension(12) :: &
    midmondoy=[15,46,77,107,138,167,198,229,259,290,320,350]
character(len=120), parameter :: specfile='ozflux_nc_to_netcdf.txt'

integer :: startyr, stopyr  !starting and stopping years
integer :: filehroutperday  !48=1/2 hourly 24=hourly, 1=daily, -1=monthly  
                            !(number of hours per day to output)
character(len=120) :: dirout, fileout
character(len=120) :: dirin
character(len=60)  :: suffix

!misc file info
character(len=120) :: filein, trash
integer :: numyr
integer, dimension(:), allocatable :: numdayperyr
integer :: numtin, numtout, numtfile

character(len=6) :: suffixout
character(len=2) :: ssstartyr, ssstopyr
character(len=4) :: sstartyr, sstopyr

!data variables
integer :: nvars
character(len=nf90_max_name) :: tempname, varattname
character(len=nf90_max_name), dimension(:), allocatable :: varname
real*8, dimension(:), allocatable :: timein, timetemp, timeout
real*8, dimension(:), allocatable :: datatemp, dataout
real*8, dimension(:,:), allocatable :: datain

!netcdf variables
integer :: ncid,ncidout,tdid,tid
integer :: varidin, status
integer :: xtype,ndimsin,nvarsin,ngatts,nvatts
integer :: udid,idimids(nf90_max_var_dims)
integer, dimension(:), allocatable :: varid


!misc variables
integer :: i, doy, hr, v, yr, yrref
integer :: count
integer :: vtstart, vtstop
integer :: countd1, countd2
real*8  :: hrfrac

!----------------------------------------------------------
!read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,*) startyr
!print*,'Starting Year: ',startyr
read(11,*) stopyr
!print*,'Stopping Year: ',stopyr
read(11,*) filehroutperday
!print*,'Hours Per Day: ',filehroutperday
read(11,*) trash
read(11,*) trash
read(11,*) dirin
!print*,'Input Directory: ',trim(dirin)
read(11,*) trash
read(11,*) suffix
!print*,'Input Suffix: ',trim(suffix)
read(11,*) trash
read(11,*) dirout
!print*,'Output File: ',fileout
read(11,*) nvars
allocate(varname(nvars))
allocate(varid(nvars))
do v=1,nvars
   read(11,*) varname(v)
enddo
close(11)

!create the output file name
if (filehroutperday .eq. 0) then
   suffixout='_hh.nc'
elseif (filehroutperday .eq. 1) then
   suffixout='_h.nc'
elseif (filehroutperday .eq. 2) then
   suffixout='_d.nc'
elseif (filehroutperday .eq. 3) then
   suffixout='_m.nc'
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


!calculate the total length of the combined data
numyr=stopyr-startyr+1
allocate(numdayperyr(numyr))
do yr=startyr,stopyr
   yrref=yr-startyr+1
    if (mod(yr,4) .eq. 0) then
       numdayperyr(yrref)=366
    else
       numdayperyr(yrref)=365
    endif
enddo
numtin = sum(numdayperyr*filehrperday)
allocate(timein(numtin))
allocate(datain(nvars,numtin))
datain(:,:) = badval

count=1
do yr=startyr,stopyr
   yrref=yr-startyr+1
   do doy=1,numdayperyr(yrref)
      do hr=1,filehrperday
         hrfrac = (hr-0.5)/dble(filehrperday)
         timein(count) = yr + (doy+hrfrac)/ &
             dble(numdayperyr(yrref)+1)
         count=count+1
      enddo
   enddo
enddo

if (filehroutperday .eq. 0) then
   numtout = sum(numdayperyr)*48.
elseif (filehroutperday .eq. 1) then
   numtout = sum(numdayperyr)*24.
elseif (filehroutperday .eq. 2) then
   numtout = sum(numdayperyr)
elseif (filehroutperday .eq. 3) then
   numtout = numyr*12.
else
   print*,'Unexpected output time frequency.'
   stop
endif
allocate(timeout(numtout))
call calc_ave(numtin,numtout,timein,timeout)

!------------------------------
!Read the data
vtstart=0
do yr=startyr,stopyr
    yrref=yr-startyr+1

   !...open the file
    print*,'Processing ',yr
    write(filein,'(a,i4,a)') trim(dirin),yr,trim(suffix)
    call check ( nf90_open(trim(filein),nf90_nowrite,ncid))
    print*,'  Opening File: '
    print*,'  ',trim(filein)

    call check ( nf90_inquire_dimension(ncid,1,len=numtfile) )

    IF (yr .eq. startyr) THEN
       !find starting time
       allocate(timetemp(numtfile))
       call check ( nf90_inq_varid(ncid,'Ddd',varidin) )
       call check ( nf90_get_var(ncid,varidin,timetemp, &
             start=(/1,1,1/), count=(/1,1,numtfile/)) )

       vtstart=(timetemp(1)-1)*filehrperday

       call check ( nf90_create(trim(fileout),nf90_64bit_offset,ncidout))
       call check ( nf90_def_dim(ncidout,'numt',numtout,tdid))
       call check ( nf90_def_var(ncidout,'time',nf90_double,(/tdid/),tid))
       call check ( nf90_inquire(ncid,ndimsin,nvarsin,ngatts,udid,xtype))
       do i=1,ngatts
          call check (nf90_inq_attname(ncid,nf90_global,i,varattname))
          call check (nf90_copy_att(ncid,nf90_global,trim(varattname),ncidout,nf90_global))
       enddo

       do v=1,nvars
          !print*,'  Variable: ',trim(varname(v))
          call check(nf90_inq_varid(ncid,trim(varname(v)),varidin))
          call check(nf90_inquire_variable( &
               ncid,varidin,tempname,xtype,ndimsin,idimids,nvatts))
          call check(nf90_def_var(ncidout,trim(varname(v)),nf90_float,tdid,varid(v)))
          do i=1,nvatts
             call check(nf90_inq_attname(ncid,varidin,i,varattname))
             call check(nf90_copy_att(ncid,varidin,trim(varattname),ncidout,varid(v)))
          enddo
        enddo !v=1,nvars

        call check(nf90_enddef(ncidout))
      ENDIF !yr eq startyear

      !...update the time counts
      vtstart=vtstart+1
      vtstop=vtstart+numtfile-1

      !...get the data
      allocate(datatemp(numtfile))
      do v=1,nvars
         call check(nf90_inq_varid(ncid,trim(varname(v)),varidin))
         call check(nf90_inquire_variable( &
              ncid,varidin,tempname,xtype,ndimsin,idimids,nvatts))
         if (ndimsin .eq. 1) then
             call check(nf90_get_var(ncid,varidin,datatemp))
         elseif (ndimsin .eq. 3) then
             call check(nf90_get_var(ncid,varidin,datatemp, &
                 start=(/1,1,1/),count=(/1,1,numtfile/)))
         else
             print*,'Unexpected variable dimensions.'
             stop
         endif
         datain(v,vtstart:vtstop) = datatemp(:)
      enddo

      deallocate(datatemp)

      call check (nf90_close(ncid))
      vtstart=vtstop

enddo !yr=startyr,stopyr


!write out the variables
print*,''
print*,'Writing File:'
print*,'  ',trim(fileout)

call check(nf90_put_var(ncidout,tid,timeout))
allocate(datatemp(numtin),dataout(numtout))
do v=1,nvars
   datatemp(:)=datain(v,:)
   call calc_ave(numtin,numtout,datatemp,dataout)
   call check(nf90_put_var(ncidout,varid(v),dataout))
enddo

call check(nf90_close(ncidout))

print*,''
print*,'Finished processing.'
print*,''

end program ozflux_nc_to_netcdf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine calc_ave(numt1,numt2,din,dout)

implicit none

integer :: numt1,numt2
real*8, dimension(numt1) :: din
real*8, dimension(numt2) :: dout
real, parameter :: badval=-9999.
real, parameter :: badvalout=-9999.

real*8 :: newval
integer :: i,j
integer :: newcount,count,numcombo

numcombo=numt1/numt2
count=1
do i=1,numt2
   newval=0.
   newcount=0
   do j=1,numcombo
      if (din(count) .gt. badval) then
         newval = newval + din(count)
         newcount = newcount + 1
      endif
      count=count+1
   enddo
 
   if (newcount .gt. 0) then
      dout(i) = newval/newcount
   else
      dout(i) = badvalout
   endif
enddo


return
end subroutine calc_ave

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine calc_timeave(numt1,numt2,timein,timeout,din,dout)

implicit none

integer :: numt1,numt2
real, dimension(numt1), intent(in)  :: timein, din
real, dimension(numt2), intent(in)  :: timeout
real, dimension(numt2), intent(out) :: dout
integer, parameter :: ibadval=-999
real, parameter :: badval=-6999.
real, parameter :: badvalout=-999.

integer, dimension(numt1) :: timeoutref
real, dimension(numt2) :: timediff
real :: timemin

integer, dimension(numt2) :: numgoodout

integer :: t1,t2

!...create an array with indices of the closest new times
timeoutref=ibadval
do t1=1,numt1
   timediff(:) = abs(timein(t1) - timeout(:))
   timemin = minval(timediff)
   do t2=1,numt2
      if (timediff(t2) == timemin) then
          timeoutref(t1) = t2
       endif
   enddo
   if (timeoutref(t1) == ibadval) then
       print*,'Incorrect Time Calculation'
       stop
    endif
enddo

numgoodout(:) = 0
dout(:) = 0.0
do t1=1,numt1
   if (din(t1) .gt. badval) then
      dout(timeoutref(t1)) = dout(timeoutref(t1)) + din(t1)
      numgoodout(timeoutref(t1)) = numgoodout(timeoutref(t1)) + 1
   endif
enddo

do t2=1,numt2
   if (numgoodout(t2) .gt. 0) then
      dout(t2) = dout(t2) / numgoodout(t2)
   else
      dout(t2) = badvalout
   endif
enddo

return
end subroutine calc_timeave


!===================================
! Subroutine to check netcdf routines
! kdcorbin, 02/11
 
subroutine check(status)

use netcdf
use typeSizes
implicit none

integer, intent ( in ) :: status
 
     if (status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop "Error with netcdf.  Stopped."
      end if
end subroutine check   
