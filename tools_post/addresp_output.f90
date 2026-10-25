subroutine addresp_output(nfiles, &
     filenamesin, filenamesout, ratiofile)

use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filenamesin, filenamesout
character(len=256), intent(in) :: ratiofile

!netcdf variables
integer :: ncid, status, dimid

!open the first file
status = nf90_open(trim(filenamesin(1)), nf90_nowrite, ncid)

!get dimensions
status = nf90_inq_dimid(ncid, 'nsib', dimid)
if (status .ne. nf90_noerr) then
   status = nf90_inq_dimid(ncid, 'landpoints', dimid)
endif

if (status .eq. nf90_noerr) then
   call addresp_outsib(nfiles, filenamesin, filenamesout, ratiofile)
else
   call addresp_outgrid(nfiles, filenamesin, filenamesout, ratiofile)
endif

status = nf90_close(ncid)

end subroutine addresp_output


!=========================================
subroutine addresp_outsib(nfiles, &
    filenamesin, filenamesout, ratiofile)
!=========================================
!Subroutine controlling the sequence of 
!   calls to add balanced respiration.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
     filenamesin, filenamesout
character(len=256), intent(in) :: ratiofile

!netcdf variables
integer :: ncid, ncido
integer :: status, dimid, varid
integer :: respid, respbid

integer :: ndims
integer, dimension(:), allocatable :: &
    myvardims

!data variables
integer :: nsib, nlu, ntime
integer :: nsibr, nlur
real, dimension(:,:), allocatable :: ratio
real, dimension(:,:,:), allocatable :: data, respf

!local variables
integer :: fref
integer :: l, n

!-----------------------------------------------------------------
!Get the ratio
status = nf90_open(trim(ratiofile),nf90_nowrite,ncid)
IF (status .NE. nf90_noerr) THEN
    print*,'Error Opening Ratio File: '
    print*,'  ',trim(ratiofile)
    print*,'Stopping.'
    STOP
ENDIF

status = nf90_inq_dimid(ncid,trim(nsibnamed),dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nsibr)
status = nf90_inq_dimid(ncid,trim(nluname),dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlur)

allocate(ratio(nsibr,nlur))
status = nf90_inq_varid(ncid,'ratio',varid)
status = nf90_get_var(ncid,varid,ratio)
IF (status .NE. nf90_noerr) THEN
   print*,'Error Reading Ratio.'
   print*,'Stopping.'
   STOP
ENDIF

status = nf90_close(ncid)


!Add balanced respiration to file
DO fref=1,nfiles
   status = nf90_open(trim(filenamesin(fref)),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Opening File: '
       print*,'  ',trim(filenamesin(fref))
       print*,'Stopping.'
       STOP
   ENDIF

   if (use_deflate) THEN
       status = nf90_create(trim(filenamesout(fref)),nf90_netcdf4,ncido)
   else
       status = nf90_create(trim(filenamesout(fref)),nf90_64bit_offset,ncido)
   endif 
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Creating File: '
      print*,'  ',trim(filenamesout(fref))
      print*,'Message:',nf90_strerror(status)
      print*,'Stopping.'
      STOP
   ENDIF

   status = nf90_inq_dimid(ncid,trim(nsibnamed),dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nsib)
   IF (nsib .ne. nsibr) THEN
       print*,'Mismatching number of points: '
       print*,'    Ratio File: ',nsibr
       print*,'    Diagnostic File: ',nsib
       print*,'Stopping.'
       STOP
   ENDIF

   status = nf90_inq_dimid(ncid,trim(nluname),dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlu)
   IF (nlu .ne. nlur) THEN
        print*,'Mismatching number of land units: '
        print*,'    Ratio File: ',nlur
        print*,'    Diagnostic File: ',nlu
        print*,'Stopping.'
        STOP
   ENDIF

   call zcopyf90(ncid, ncido)

   !Get respiration
   status = nf90_inq_varid(ncid, respname, respid)
   IF (status .NE. nf90_noerr) THEN
       print(fa),'Error Getting Original Respiration.'
       print(fa),'Stopping.'
       STOP
   ENDIF

   status = nf90_inq_varid(ncido, respbname, respbid)
   IF (status .NE. nf90_noerr) THEN
       status = nf90_inquire_variable(ncid, respid, &     
                ndims=ndims)
       if (ndims .gt. 0) then
          allocate(myvardims(ndims))
       else
          print(fa),'Error Getting Original Respiration.'
          print(fa),'Stopping.'
          STOP
       endif
       status = nf90_inquire_variable(ncid,respid, &
                dimids=myvardims)

       status = nf90_redef(ncido)
       status = nf90_def_var(ncido, trim(respbname), nf90_float, &
                 myvardims, respbid)
       if (use_deflate) THEN
           status = nf90_def_var_deflate(ncido, respbid, 1, 1, deflev)
       endif
       IF (status .NE. nf90_noerr) THEN
           print(fa), 'Error Adding New Balanced Respiration'
           print(f2a),'  Error: ',trim(nf90_strerror(status))
           print(f2a),'  Resp_Bal Name: ',trim(respbname)
           print('(a,3i4)'),'  Dimension IDs: ',myvardims
           print(fa),'Stopping.'
           stop
        ENDIF
        deallocate(myvardims)

        status = nf90_put_att(ncido,respbid,'long_name',trim(respblong))
        status = nf90_put_att(ncido,respbid,'title',trim(respbtitle))
        status = nf90_put_att(ncido,respbid,'units',trim(respbunits))
        status = nf90_enddef(ncido)
   ENDIF

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)

   allocate(data(nsib,nlu,ntime))
   allocate(respf(nsib,nlu,ntime))
   status = nf90_get_var(ncid, respid, data)
   do l=1,nlu
      do n=1,nsib
         respf(n,l,:) = data(n,l,:) * ratio(n,l)
      enddo
    enddo
    status = nf90_put_var(ncido, respbid, respf)
    IF (status .ne. nf90_noerr) THEN
         print*,'Error Writing Balanced Respiration.'
         print*,'   Error: ',trim(nf90_strerror(status))
         print*,'Stopping.'
         STOP
    ENDIF
  deallocate(data)
  deallocate(respf)
  
  status = nf90_close(ncid)
  status = nf90_close(ncido)

ENDDO

end subroutine addresp_outsib


!=========================================
subroutine addresp_outgrid(nfiles, &
    filenamesin, filenamesout, ratiofile)
!=========================================
!Subroutine controlling the sequence of 
!   calls to add balanced respiration.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
     filenamesin, filenamesout
character(len=256), intent(in) :: ratiofile

!netcdf variables
integer :: ncid, ncido
integer :: status, dimid, varid
integer :: respid, respbid

integer :: ndims
integer, dimension(:), allocatable :: &
    myvardims

!data variables
integer :: nlon, nlat, nlu, ntime
integer :: nlonr, nlatr, nlur
real, dimension(:,:,:), allocatable :: ratio
real, dimension(:,:,:,:), allocatable :: data, respf

!local variables
integer :: fref
integer :: i,j,l

!-----------------------------------------------------------------
!Get the ratio
status = nf90_open(trim(ratiofile),nf90_nowrite,ncid)
IF (status .NE. nf90_noerr) THEN
    print*,'Error Opening Ratio File: '
    print*,'  ',trim(ratiofile)
    print*,'Stopping.'
    STOP
ENDIF

status = nf90_inq_dimid(ncid,trim('lon'),dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlonr)
status = nf90_inq_dimid(ncid,trim('lat'),dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlatr)
status = nf90_inq_dimid(ncid,trim(nluname),dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlur)

allocate(ratio(nlonr,nlatr,nlur))
status = nf90_inq_varid(ncid,'ratio',varid)
status = nf90_get_var(ncid,varid,ratio)
IF (status .NE. nf90_noerr) THEN
   print*,'Error Reading Ratio.'
   print*,'Stopping.'
   STOP
ENDIF

status = nf90_close(ncid)


!Add balanced respiration to file
DO fref=1,nfiles
   status = nf90_open(trim(filenamesin(fref)),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Opening File: '
       print*,'  ',trim(filenamesin(fref))
       print*,'Stopping.'
       STOP
   ENDIF

   if (use_deflate) THEN
       status = nf90_create(trim(filenamesout(fref)),nf90_netcdf4,ncido)
   else
       status = nf90_create(trim(filenamesout(fref)),nf90_64bit_offset,ncido)
   endif
   if (status .ne. nf90_noerr) then
      print*,'Error Creating File: '
      print*,'  ',trim(filenamesout(fref))
      print*,'Message: ',nf90_strerror(status)
      print*,'Stopping.'
      STOP
   endif

   status = nf90_inq_dimid(ncid,trim('lon'),dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlon)
   status = nf90_inq_dimid(ncid,trim('lat'),dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlat)
   IF ((nlon .ne. nlonr) .or. (nlat .ne. nlatr)) THEN
       print*,'Mismatching number of points: '
       print*,'    Ratio File: ',nlonr,nlatr
       print*,'    Diagnostic File: ',nlon,nlat
       print*,'Stopping.'
       STOP
   ENDIF

   status = nf90_inq_dimid(ncid,trim(nluname),dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlu)
   IF (nlu .ne. nlur) THEN
        print*,'Mismatching number of land units: '
        print*,'    Ratio File: ',nlur
        print*,'    Diagnostic File: ',nlu
        print*,'Stopping.'
        STOP
   ENDIF

   call zcopyf90(ncid,ncido)

   !Get respiration
   status = nf90_inq_varid(ncid, respname, respid)
   IF (status .ne. nf90_noerr) then
      print(fa),'Error Getting Original Respiration.'
      print(fa),'Stopping.'
      STOP
   ENDIF

   status = nf90_inq_varid(ncido, respbname, respbid)
   IF (status .NE. nf90_noerr) THEN
       status = nf90_inquire_variable(ncid, respid, &     
                ndims=ndims)
       if (ndims .gt. 0) then
          allocate(myvardims(ndims))
       else
          print(fa),'Error Getting Original Respiration.'
          print(fa),'Stopping.'
          STOP
       endif
       status = nf90_inquire_variable(ncid,respid, &
                dimids=myvardims)

       status = nf90_redef(ncido)
       status = nf90_def_var(ncido, respbname, nf90_float, &
                 myvardims, respbid)
       if (use_deflate) then
           status = nf90_def_var_deflate(ncido, respbid, 1, 1, deflev)
       endif
       IF (status .NE. nf90_noerr) THEN
           print(fa),'Error Creating New Balanced Respiration'
           print(f2a),'   Error: ',trim(nf90_strerror(status))
           print(fa),'Stopping.'
           stop
       ENDIF
       deallocate(myvardims)

       status = nf90_put_att(ncido,respbid,'long_name',trim(respblong))
       status = nf90_put_att(ncido,respbid,'title',trim(respbtitle))
       status = nf90_put_att(ncido,respbid,'units',trim(respbunits))
       status = nf90_enddef(ncido)
  ENDIF

  status = nf90_inq_dimid(ncid, 'time', dimid)
  status = nf90_inquire_dimension(ncid, dimid, len=ntime)

  allocate(data(nlon,nlat,nlu,ntime))
  allocate(respf(nlon,nlat,nlu,ntime))
  status = nf90_get_var(ncid, respid, data)
  do l=1,nlu
     do j=1,nlat
        do i=1,nlon
           respf(i,j,l,:) = data(i,j,l,:) * ratio(i,j,l)
        enddo
     enddo
   enddo
   status = nf90_put_var(ncido, respbid, respf)
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Writing Balanced Respiration.'
       print*,'Stopping.'
       STOP
   ENDIF
   deallocate(data,respf)

   status = nf90_close(ncid)
   status = nf90_close(ncido)

ENDDO

end subroutine addresp_outgrid


