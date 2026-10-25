!==================================================
subroutine write_merra_outfile(infile, &
   nsites, siteref, ntstart, ntime)
!==================================================

use ncmodule
use netcdf
implicit none

!input variables
character(len=256), intent(in) :: infile
integer, intent(in) :: nsites
integer, dimension(nsites), intent(in) :: siteref
integer, intent(in) :: ntstart, ntime

!file variables
integer :: ntimedr
real, dimension(:), allocatable :: &
     precip, swd
real, dimension(:,:), allocatable :: &
     lsprall, cuprall, swdall

!misc variables
integer :: countall, day, hr
integer :: ns, sref

!-------------------------------------------------
status=nf90_open(trim(infile),nf90_nowrite,inid)
if (status .ne. nf90_noerr) then
   print*,'Error Opening File: '
   print*,'  ',trim(infile)
   print*,'Stopping.'
   STOP
!else
!   print*,'Opening File: '
!   print*,'  ',trim(infile)
!   STOP
endif

status=nf90_inq_dimid(inid,'nsib',dimid)
status=nf90_inquire_dimension(inid,dimid,len=nsibdr)
IF (nsibdr .ne. nsib) THEN
   print*,'!!Mismatching SiB4 and MERRA!!'
   print*,'   SiB4 Points: ',nsib
   print*,'   MERRA Points:',nsibdr
   STOP
ENDIF

status=nf90_inq_dimid(inid,'time',dimid)
status=nf90_inquire_dimension(inid,dimid,len=ntimedr)
IF (status .ne. nf90_noerr) THEN
   print*,'Error getting time.'
   STOP
ENDIF

allocate(lsprall(nsib,ntimedr))
allocate(cuprall(nsib,ntimedr))
allocate(swdall(nsib,ntimedr))

status = nf90_inq_varid(inid,'lspr_scaled',varid)
if (status .ne. nf90_noerr) then
   print*,'MERRA file missing lspr_scaled! Stopping.'
   STOP
endif
status=nf90_get_var(inid,varid,lsprall)
if (status .ne. nf90_noerr) then
   print*,'Error getting lspr_scaled! Stopping.'
   STOP
endif

status = nf90_inq_varid(inid,'cupr_scaled',varid)
if (status .ne. nf90_noerr) then
   print*,'MERRA file missing cupr_scaled! Stopping.'
   STOP
endif
status=nf90_get_var(inid,varid,cuprall)
if (status .ne. nf90_noerr) then
   print*,'Error getting cupr_scaled! Stopping.'
   STOP
endif

status = nf90_inq_varid(inid,'swd',varid)
if (status .ne. nf90_noerr) then
   print*,'MERRA file missing swd! Stopping.'
   STOP
endif
status=nf90_get_var(inid,varid,swdall)
if (status .ne. nf90_noerr) then
   print*,'Error getting swd! Stopping.'
   STOP
endif
status=nf90_close(inid)

!loop through the sites and pull out data
allocate(precip(ntime),swd(ntime))
do ns=1,nsites
   precip(:) = 0.
   swd(:) = 0.

   sref = siteref(ns)

   if (ntime .le. 1) then
      precip(1) = (sum(lsprall(sref,:))/ntimedr &
                 + sum(cuprall(sref,:))/ntimedr)*3600.
      swd(1) = sum(swdall(sref,:))/ntimedr
   elseif (ntime .le. 31) then
      countall=1
      do day=1,ntime
         do hr=1,24
            precip(day) = precip(day) + &
                 (lsprall(sref,countall) + cuprall(sref,countall))*3600.
            countall = countall + 1
         enddo
         precip(day) = precip(day) / 24.
      enddo
   else
      precip(:) = (lsprall(sref,:) + cuprall(sref,:))*3600.
      swd(:) = swdall(sref,:)
   endif

   status=nf90_put_var(outid,sprid,precip,   &
        start=(/ns,ntstart/), count=(/1,ntime/))
   IF (status .ne. nf90_noerr) THEN
      print*,'Error writing precip!!'
      STOP
   endif
   
   status=nf90_put_var(outid,sswid,swd, &
          start=(/ns,ntstart/), count=(/1,ntime/))
   if (status .ne. nf90_noerr) then
      print*,'Error writing swd!!'
      STOP
   endif
   
enddo !ns=1,nsites

deallocate(lsprall,cuprall,swdall)

end subroutine write_merra_outfile



