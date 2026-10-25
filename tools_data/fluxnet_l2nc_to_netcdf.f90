!Program to combine yearly FLUXNET data in netcdf files.
!
!Converts output from typically 1/2-hourly data to
!   either hourly, daily, or monthly
!
!kdhaynes, 12/15
! 
program fluxnet_l2nc_to_netcdf

use netcdf
implicit none

!File specific information
integer, parameter :: vstart=7
integer, parameter :: filehrperday=48
character(len=10), parameter :: timename='DTIME'
real, parameter :: mminval=-399.
real, parameter :: badvalout=-999.
integer, parameter, dimension(12) :: midmondoy=[15,46,77,107,138,167,198,229,259,290,320,350]
character(len=120), parameter :: specfile='fluxnet_l2nc_to_netcdf.txt'

integer :: startyr, stopyr  !starting and stopping years
integer :: filehroutperday  !number of hours per day to output
                            !  48=1/2 hourly 24=hourly, 1=daily, -1=monthly
integer :: stepshift          !steps to shift the time
character(len=120) :: dirin, dirout
character(len=60)  :: suffix

!misc file info
character(len=120) :: trash, filein, fileout
character(len=6) :: suffixout
character(len=2) :: ssstartyr, ssstopyr
character(len=4) :: sstartyr, sstopyr

integer :: numyr,yrlen,yrlenhrin
integer :: numt, totlenin, totlenout
integer :: numhrcombo

!data variables
real, dimension(:), allocatable :: dtime
real*8, dimension(:), allocatable :: timein, dtimein, timeout
real, dimension(:), allocatable :: datatempin
real, dimension(:), allocatable :: datatemptot, datatempout
real, dimension(:,:), allocatable :: datain, datashift, dataout

!netcdf variables
integer :: ncid,ncidout,tdid,tid,dtid
integer, dimension(:), allocatable :: varid
integer :: xtype,ndims,nvars,ngatts,nvatts
integer :: udid,idimids(nf90_max_var_dims)
character(len=nf90_max_name) :: varname,varattname

!misc variables
integer :: count, vtstart, vtstop
real*8  :: hrfrac, dayfrac, yrfrac, ptime
integer :: yr,mon,day,hr
integer :: i,v,t

!----------------------------------------------------------
!read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,*) trash
read(11,*) startyr
!print*,'Starting Year: ',startyr
read(11,*) trash
read(11,*) stopyr
!print*,'Stopping Year: ',stopyr
read(11,*) trash
read(11,*) filehroutperday
!print*,'Hours Per Day: ',filehroutperday
read(11,*) trash
read(11,*) stepshift
read(11,*) trash
read(11,*) dirin
!print*,'Input Directory: ',trim(dirin)
read(11,*) trash
read(11,*) suffix
!print*,'Input Suffix: ',trim(suffix)
read(11,*) trash
read(11,*) dirout
!print*,'Output File: ',fileout
close(11)

!create the output file name
if (filehroutperday .eq. 48) then
   suffixout='_hh.nc'
elseif (filehroutperday .eq. 24) then
   suffixout='_h.nc'
elseif (filehroutperday .eq. 1) then
   suffixout='_d.nc'
elseif (filehroutperday .eq. -1) then
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
numhrcombo=filehrperday/filehroutperday

totlenin=0
totlenout=0
do yr=startyr,stopyr
   yrlen=365
   if (mod(yr,4) .eq. 0) yrlen=366
   yrlenhrin = filehrperday*yrlen
   totlenin = totlenin + yrlenhrin
enddo

if (filehroutperday .gt. 0) then
   totlenout=totlenin/dble(numhrcombo)
else
   numhrcombo=0
   totlenout=12*numyr
endif

allocate(timein(totlenin))
count=1
do yr=startyr,stopyr
   yrlen=365
   if (mod(yr,4) .eq. 0) yrlen=366
   do day=0,yrlen-1
      do hr=0,filehrperday-1
         hrfrac = (hr + 0.5)/filehrperday
         dayfrac=(day + hrfrac)/yrlen
         timein(count) = yr + dayfrac
         count=count+1
      enddo
    enddo
enddo

!read the input files:
vtstart=0
vtstop=0
do yr=startyr,stopyr

   !...open the file
   print*,'Processing ',yr
   write(filein,'(a,i4,a)') trim(dirin),yr,trim(suffix)
   print*,'  ','Opening File: '
   print*,'    ',trim(filein)
   call check ( nf90_open(trim(filein),nf90_nowrite,ncid ) )

   IF (yr .eq. startyr) THEN
       !find starting time
       call check ( nf90_inquire_dimension(ncid,1,len=numt) )
       allocate(dtime(numt))
       allocate(dtimein(numt))

       call check ( nf90_inq_varid(ncid,trim(timename),dtid) )
       call check ( nf90_get_var(ncid,dtid,dtime) )

       if (filehrperday .eq. 48) then
            ptime=15.
       elseif (filehrperday .eq. 24) then
            ptime=30.
       else
            ptime=0.
       endif
       dtime=dtime+ptime/dble(24*60.)

       if (dtime(1) .lt. 1) then
          ptime=0.
       else
          ptime=1.
       endif

       dtimein(:) = yr + dble(dtime(:)-ptime)/dble(yrlen)
       do t=1,yrlenhrin-1
          if ((dtimein(1) .ge. timein(t)) .and. &
              (dtimein(1) .le. timein(t+1))) then
              vtstart=vtstart+t
              exit
           endif
       enddo

       if (vtstart .eq. 0) then
          if (dtimein(1) .le. timein(2)) then
             vtstart=1
          endif
       endif

       !open the output file and create variables
       !print*,'Creating file: ',trim(fileout)
       call check ( nf90_create(trim(fileout),nf90_64bit_offset,ncidout))
       call check ( nf90_def_dim( ncidout,'numt',totlenout,tdid))
       call check ( nf90_def_var( ncidout,'time',nf90_double,(/tdid/),tid))
       
        call check ( nf90_inquire(ncid,ndims,nvars,ngatts,udid,xtype) )
        allocate(varid(nvars))

       do v=vstart,nvars
          call check ( nf90_inquire_variable( &
                       ncid,v,varname,xtype,ndims,idimids,nvatts))
          if (trim(varname) == 'LE') varname='LH'
          call check ( nf90_def_var( ncidout,trim(varname),nf90_float,(/tdid/),varid(v)))
          do i=1,nvatts
             call check ( nf90_inq_attname( ncid, v, i, varattname) )
             if (trim(varattname) == '_FillValue') then
                 call check ( nf90_put_att( ncidout, varid(v), trim(varattname), badvalout))
             else
                 call check ( nf90_copy_att( ncid, v, trim(varattname), ncidout, varid(v)))
             endif
          enddo
        enddo

       do i=1,ngatts
           call check ( nf90_inq_attname(ncid,nf90_global,i,varattname))
           call check ( nf90_copy_att(ncid,nf90_global,trim(varattname),ncidout,nf90_global))
       enddo

       call check ( nf90_enddef(ncidout))
       !print*,'  ','Defined variables: ',nvars

       allocate(datain(nvars,totlenin))
       datain(:,:)=badvalout
    ENDIF

    call check ( nf90_inquire_dimension(ncid,1,len=numt) )
    allocate(datatempin(numt))

    !...get the variables and save for output
    vtstop=MIN(totlenin,vtstart+numt-1)

    do v=vstart,nvars
       call check ( nf90_get_var(ncid,v,datatempin) )
       datain(v,vtstart:vtstop) = datatempin(:)
    enddo       

   call check ( nf90_close(ncid))
   vtstart=vtstop
   deallocate(datatempin)

ENDDO !yr=startyr,stopyr

!shift the data if necessary
allocate(datashift(nvars,totlenin))
do v=vstart,nvars
   if (stepshift .gt. 0.) then
       datashift(v,1:stepshift) = badvalout
       datashift(v,stepshift+1:totlenin) = datain(v,1:totlenin-stepshift)
   elseif (stepshift .lt. 0.) then
       datashift(v,1:totlenin+stepshift+1) = datain(v,abs(stepshift):totlenin)
       datashift(v,totlenin+stepshift+2:totlenin) = badvalout
   else
       datashift=datain
  endif
enddo

!process the data
allocate(timeout(totlenout))
allocate(dataout(nvars,totlenout))
allocate(datatemptot(totlenin),datatempout(totlenout))

call calc_timeave(totlenin,totlenout,timein,timeout)

do v=vstart,nvars
   datatemptot(:) = datashift(v,:)
   call calc_ave(totlenin,totlenout,datatemptot,datatempout)
   dataout(v,:) = datatempout(:)
enddo
where(dataout .lt. mminval) dataout=badvalout
   
!write out the variables
print*,''
print*,'Writing out file: '
print*,'   ',trim(fileout)
call check ( nf90_put_var( ncidout, tid, timeout ) )
do v=vstart,nvars
   datatempout(:) = dataout(v,:)
   call check ( nf90_put_var( ncidout, varid(v), datatempout ) )
enddo

call check ( nf90_close(ncidout) )

print*,''
print*,'Finished processing.'
print*,''

end program fluxnet_l2nc_to_netcdf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine calc_ave(numt1,numt2,din,dout)

implicit none

integer :: numt1,numt2
real, dimension(numt1) :: din
real, dimension(numt2) :: dout
real, parameter :: badval=-6999.
real, parameter :: badvalout=-999.

real :: newval
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
subroutine calc_timeave(numtin,numtout,timein,timeout)

implicit none

integer, intent(in) :: numtin,numtout
real*8, dimension(numtin), intent(in)  :: timein
real*8, dimension(numtout), intent(inout)  :: timeout
real, parameter :: badval=-6999.
real, parameter :: badvalout=-999.

real*8 :: newval
integer :: i,j
integer :: newcount,count,numcombo

numcombo=numtin/numtout
count=1
do i=1, numtout
   newval=0.
   newcount=0
   do j=1,numcombo
      if (timein(count) .gt. badval) then
         newval = newval + timein(count)
         newcount = newcount + 1
      endif
      count=count+1
   enddo
 
   if (newcount .gt. 0) then
      timeout(i) = newval/newcount
   else
      timeout(i) = badvalout
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
