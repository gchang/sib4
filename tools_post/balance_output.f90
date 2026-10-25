subroutine balance_output(nfiles, filenames, &
     ratiofile, write_ratio, write_bresp)

  use netcdf
  use sibinfo_module

  implicit none

  !input variables
  integer, intent(in) :: nfiles
  character(len=256), dimension(nfiles), intent(in) :: filenames
  character(len=256), intent(in) :: ratiofile
  logical, intent(in) :: write_ratio, write_bresp

  !netcdf variables
  integer :: ncid, status, dimid

  !open the first file
  status = nf90_open(trim(filenames(1)), nf90_nowrite, ncid)

  !get dimensions
  status = nf90_inq_dimid(ncid, nsibnamed, dimid)

  if (status .eq. nf90_noerr) then
     call balance_outsib(nfiles, filenames, &
          ratiofile, write_ratio, write_bresp)
  else
     call balance_outgrid(nfiles, filenames, &
          ratiofile, write_ratio, write_bresp)
  endif

  status = nf90_close(ncid)

end subroutine balance_output


!=========================================
subroutine balance_outsib(nfiles, filenames, &
    ratiofile, write_ratio, write_bresp)
!=========================================
!Subroutine controlling the sequence of 
!   calls to balance fluxes.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames
character(len=256), intent(in) :: ratiofile
logical, intent(in) :: write_ratio, write_bresp

!parameters
logical :: print_files=.false.

!netcdf variables
integer :: ncid, rfid
integer :: status, dimid
integer :: gppid, respid, respbid, varid, varinid
integer, dimension(:), allocatable :: &
    mydims, mydlens, myvardims

integer :: xtype, dimlen
integer :: numatts, numdims
integer :: ratioid
character(len=20) :: name, attname

!data variables
integer :: ndims
integer :: nsib, nlu, ntime, nttot
integer, dimension(:,:), allocatable :: lu_pref
real, dimension(:,:,:), allocatable :: dataa, datar, respb
real*8, dimension(:,:), allocatable :: assim, resp, ratio

integer*1, dimension(:), allocatable :: bvalues
integer, dimension(:,:), allocatable :: ivalues2
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6

!local variables
integer :: dim, a, n, l, v
integer :: fref
integer :: spos1, spos2

!-----------------------------------------------------------------
nttot = 0
DO fref=1,nfiles
   !write message if requested
   IF (print_files) THEN
       spos1 = index(filenames(fref),'/',back=.true.)
       spos2 = len_trim(filenames(fref))
       IF (fref .eq. 1) THEN
          print(f2a),'  Opening: ',filenames(fref)(spos1+1:spos2)
       ELSE
          print(f2a),'             ',filenames(fref)(spos1+1:spos2)
       ENDIF
   ENDIF

   !open the files
   status = nf90_open(trim(filenames(fref)), nf90_nowrite, ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'  Error Opening File: '
      print*,'    ',trim(filenames(fref))
      print*,'  Message: ',nf90_strerror(status)
      STOP
   ENDIF

   IF (fref .EQ. 1) THEN
      !get dimensions
      status = nf90_inq_dimid(ncid, nsibnamed, dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nsib)
   
      status = nf90_inq_dimid(ncid, nluname, dimid)
      IF (status .NE. nf90_noerr) THEN
         nlu=1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlu)
      ENDIF

      print('(a,2i10)'), '  Dimensions (nsib/nlu): ', nsib, nlu
      allocate(assim(nsib,nlu),resp(nsib,nlu),ratio(nsib,nlu))
      assim(:,:) = 0.
      resp(:,:) = 0.
      ratio(:,:) = 1.

      IF (nlu .EQ. 1) THEN
         ndims=2

         allocate(lu_pref(nsib,nlu))
         lu_pref(:,:) = npftstart
      ELSE
         ndims=3

         !get PFTs
         allocate(lu_pref(nsib,nlu))
         status = nf90_inq_varid(ncid, trim(lprefname), varid)
         status = nf90_get_var(ncid, varid, lu_pref) 
      ENDIF

   ENDIF

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Retrieving Time!'
      STOP
   !ELSE
   !   print*,'  Number of Timesteps: ',ntime
   ENDIF
   nttot = nttot + ntime

   !get the GPP and RE
   status = nf90_inq_varid(ncid, 'assim', gppid)
   IF (status .NE. nf90_noerr) THEN
      status = nf90_inq_varid(ncid, 'gpp', gppid)
      IF (status .NE. nf90_noerr) THEN
         print(fa),''
         print(fa),'GPP Not Included In Output.'
         print(fa),'Stopping.'
         print(fa),''
         stop
     ENDIF
   ENDIF
     
   status = nf90_inq_varid(ncid, respname, respid)
   IF (status .NE. nf90_noerr) THEN
      print(fa),''
      print(fa),'RESP Not Included In Output.'
      print(fa),'Stopping.'
      print(fa),''
      stop
   ENDIF

   allocate(dataa(nsib,nlu,ntime))
   allocate(datar(nsib,nlu,ntime))
   status = nf90_get_var(ncid, gppid, dataa)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Getting Assimilation!'
      STOP
   ENDIF

   status = nf90_get_var(ncid, respid, datar)
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Getting Respiration!'
       STOP
   ENDIF

   do l=1,nlu
      do n=1,nsib
         assim(n,l) = assim(n,l) + SUM(dataa(n,l,:))
         resp(n,l) = resp(n,l) + SUM(datar(n,l,:))
      enddo
   enddo

   deallocate(dataa,datar)
   status = nf90_close(ncid)

ENDDO !fref=1,nfiles

!calculate the ratios
print(fa),'  Calculating Ratios'
DO l=1,nlu
   DO n=1,nsib
      IF ((lu_pref(n,l) .GE. npftstart) .AND. &
          (lu_pref(n,l) .LT. npftcropstart) .AND. &
          (resp(n,l) .GT. 1.E-10)) THEN
          ratio(n,l) = assim(n,l) / resp(n,l)
      ENDIF
   ENDDO
ENDDO
print(faf),'     Min: ',minval(ratio)
print(faf),'     Max: ',maxval(ratio)

!write the ratio
IF (write_ratio) THEN
     print(fa),'  Writing Balance Ratio: '
     spos1=index(ratiofile,'/',back=.true.)
     spos2=len_trim(ratiofile)
     print(f2a),'     ',ratiofile(spos1+1:spos2)

     status = nf90_create(trim(ratiofile),nf90_64bit_offset, rfid)
     IF (status .ne.nf90_noerr) THEN
         print*,'Error Creating Ratio File: '
         print*,'  ',trim(ratiofile)
         print*,'Stopping.'
         STOP
     ENDIF

     !copy dimensions to output file
     status = nf90_open(trim(filenames(1)), nf90_nowrite, ncid)
     status = nf90_inquire(ncid, nDimensions=numdims)
     IF (status .ne. nf90_noerr) THEN
        print*,'Error Getting Dimensions.'
        print*,'   Message: ',nf90_strerror(status)
        STOP
     ENDIF

     allocate(mydlens(numdims))
     allocate(mydims(numdims))
     mydlens(:) = 0
     mydims(:) = 0
     do dim=1,numdims
        status = nf90_inquire_dimension(ncid,dim,name=name,len=dimlen)
        IF (trim(name) .ne. 'time') THEN
           mydlens(dim) = dimlen
           status = nf90_def_dim(rfid, trim(name), dimlen, mydims(dim))
           IF (status .ne. nf90_noerr) THEN
               print*,'Error Writing Dimension: ',trim(name)
               print*,'Stopping.'
               STOP
           ENDIF
        ENDIF
     enddo

     !add required variables to output file
     allocate(myvardims(ndims))
     do v=1,nvarsreq-1
        status = nf90_inq_varid(ncid,trim(varsreq(v)),varinid)
        IF (status .eq. nf90_noerr) THEN
            status = nf90_inquire_variable(ncid, varinid, &
                     name=name, xtype=xtype, ndims=numdims, &
                     dimids=myvardims, natts=numatts)
            status = nf90_def_var(rfid, trim(name), xtype, &
                     mydims(myvardims(1:numdims)), varid)
            IF (status .NE. nf90_noerr) THEN
               print*,'Error Defining Variable: ',trim(name)
               print*,'    code: ',status
               print*,'   xtype: ',xtype
               print*,'   ndims: ',numdims
               print*,' odimids: ',myvardims(1:numdims)
               print*,' ndimids: ',mydims(myvardims(1:numdims))
               print*,'Stopping.'
               STOP
           ENDIF

           do a=1,numatts
              status = nf90_inq_attname(ncid,varinid,a,name=attname)
              status = nf90_copy_att(ncid,varinid,trim(attname),rfid,varid)
           enddo

        ENDIF
     enddo !v=1,nvarsreq

     status = nf90_def_var(rfid,'ratio',nf90_float, &
              mydims(1:2),ratioid)

     status = nf90_enddef(rfid)

     !copy required variables to output file
     do v=1,nvarsreq-1
        status = nf90_inq_varid(ncid,trim(varsreq(v)),varinid)
        status = nf90_inquire_variable(ncid, varinid, &
                 name=name, xtype=xtype, ndims=numdims, &
                 dimids=myvardims)

        if ((xtype .eq. 5) .and. (numdims .eq. 1)) then
           allocate(fvalues(mydlens(myvardims(1))))
           status = nf90_get_var(ncid, varinid, fvalues)
           status = nf90_put_var(rfid, v, fvalues)
           deallocate(fvalues)

        elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
           allocate(fvalues2(mydlens(myvardims(1)),mydlens(myvardims(2))))
           status = nf90_get_var(ncid, varinid, fvalues2)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Getting Variable: ',trim(name)
              print*,'   ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           status = nf90_put_var(rfid, v, fvalues2)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Writing Variable: ',trim(name)
              print*,'   ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           deallocate(fvalues2)
        elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
           allocate(ivalues2(mydlens(myvardims(1)),mydlens(myvardims(2))))
           status = nf90_get_var(ncid, varinid, ivalues2)
           status = nf90_put_var(rfid, v, ivalues2)
           deallocate(ivalues2)

        elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
           IF (mydlens(myvardims(1)) .eq. 3) THEN
              allocate(cvalues3(mydlens(myvardims(2))))
              status = nf90_get_var(ncid, varinid, cvalues3)
              status = nf90_put_var(rfid, v, cvalues3)
              deallocate(cvalues3)
           ELSEIF (mydlens(myvardims(1)) .eq. 6) THEN
              allocate(cvalues6(mydlens(myvardims(2))))
              status = nf90_get_var(ncid, varinid, cvalues6)
              status = nf90_put_var(rfid, v, cvalues6)
              deallocate(cvalues6)
           ELSE
              print*,'Unexpected Character Array: ',trim(varsreq(v))
              print*,'   Dimension Lengths: ', &
                     mydlens(myvardims(1)),mydlens(myvardims(2))
              print*,'Stopping.'
              STOP
           ENDIF

        elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
           allocate(bvalues(mydlens(myvardims(1))))
           status = nf90_get_var(ncid, varinid, bvalues)
           status = nf90_put_var(rfid, v, bvalues)
           deallocate(bvalues)

        else
           print*,'Unexpected Output Type and/or Dimension.'
           print*,'   Var name and type: ',trim(varsreq(v)),xtype
           print*,'   Number of dimensions: ',numdims
           print*,'   Dimension IDs: ',myvardims
           !print*,'Stopping.'
           !STOP
       endif
     enddo

     status = nf90_put_var(rfid, ratioid, real(ratio))
     IF (status .ne. nf90_noerr) THEN
        print*,'Error writing ratio.'
        print*,'Stopping.'
        STOP
     ENDIF

     status = nf90_close(ncid)
     status = nf90_close(rfid)
     deallocate(mydlens,mydims,myvardims)
ENDIF

!write the balanced respiration
IF (write_bresp) THEN

   print(fa),'  Writing Balanced Respiration'
   DO fref=1,nfiles
      status = nf90_open(trim(filenames(fref)), nf90_write, ncid)
      IF (status .ne. nf90_noerr) THEN
         print*,''
         print*,' Error Adding Balanced Respiration.'
         print*,'   Message: ',trim(nf90_strerror(status))
         print*,'   Please use AddCropResp.'
         RETURN
      ENDIF

      !add balanced respiration to file if appropriate
      status = nf90_inq_varid(ncid, respname, respid)
      status = nf90_inq_varid(ncid, respbname, respbid)

      IF (status .NE. nf90_noerr) THEN
         allocate(myvardims(ndims))
         status = nf90_inquire_variable(ncid, respid, &     
                  ndims=ndims,dimids=myvardims)

         status = nf90_redef(ncid)
         IF (status .NE. nf90_noerr) THEN
             print*,'Error Moving To Redefine Mode'
             stop
         ENDIF
         status = nf90_def_var(ncid, respbname, nf90_float, &
                  myvardims, respbid)
         IF (status .NE. nf90_noerr) THEN
             print(fa),'Error Creating New Balanced Respiration'
             print(fa),'Stopping.'
             stop
         ENDIF
         IF (use_deflate) THEN
             status = nf90_def_var_deflate(ncid,varid,1,1,deflev)
         ENDIF

         status = nf90_put_att(ncid,respbid,'long_name',trim(respblong))
         status = nf90_put_att(ncid,respbid,'title',trim(respbtitle))
         status = nf90_put_att(ncid,respbid,'units',trim(respbunits))
         status = nf90_enddef(ncid)

         deallocate(myvardims)
      ENDIF

      status = nf90_inq_dimid(ncid, 'time', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=ntime)
      allocate(datar(nsib,nlu,ntime))
      allocate(respb(nsib,nlu,ntime))
      status = nf90_get_var(ncid, respid, datar)
      IF (status .ne. NF90_noerr) THEN
         print*,'Error Getting Respiration To Modify For Balance.'
         STOP
      ENDIF

      do l=1,nlu
         do n=1,nsib
            respb(n,l,:) = datar(n,l,:) * real(ratio(n,l))
         enddo
      enddo

      status = nf90_put_var(ncid, respbid, respb)
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Writing Balanced Respiration.'
          print*,'Stopping.'
          STOP
      ENDIF
      deallocate(datar,respb)
      status = nf90_close(ncid)
   ENDDO

endif !write_bresp

end subroutine balance_outsib



!=========================================
subroutine balance_outgrid(nfiles, filenames, &
    ratiofile, write_ratio, write_bresp)
!=========================================
!Subroutine controlling the sequence of 
!   calls to balance fluxes.
!
!kdhaynes, 10/17

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames
character(len=256), intent(in) :: ratiofile
logical, intent(in) :: write_ratio, write_bresp

!parameters
logical :: print_files=.false.

!netcdf variables
integer :: ncid, rfid
integer :: status, dimid
integer :: gppid, respid, varid, varinid
integer, dimension(:), allocatable :: &
    mydims, mydlens, myvardims

integer :: xtype, dimlen
integer :: numatts, numdims
integer :: ratioid
character(len=20) :: name, attname

!data variables
integer :: ndims
integer :: nlon, nlat, nlu, ntime, nttot
integer, dimension(:,:,:), allocatable :: lu_pref
real, dimension(:,:,:,:), allocatable :: dataa, datar
real*8, dimension(:,:,:), allocatable :: assim, resp, ratio

integer*1, dimension(:), allocatable :: bvalues
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
character(len=3), dimension(:), allocatable :: cvalues3

!local variables
integer :: dim, a, i, j, l, v
integer :: fref
integer :: spos1, spos2

!-----------------------------------------------------------------
nttot = 0
DO fref=1,nfiles
   !write message if requested
   IF (print_files) THEN
       spos1 = index(filenames(fref),'/',back=.true.)
       spos2 = len_trim(filenames(fref))
       IF (fref .eq. 1) THEN
          print(f2a),'  Opening: ',filenames(fref)(spos1+1:spos2)
       ELSE
          print(f2a),'             ',filenames(fref)(spos1+1:spos2)
       ENDIF
   ENDIF

   !open the files
   status = nf90_open(trim(filenames(fref)), nf90_nowrite, ncid)
   IF (status .ne. nf90_nowrite) THEN
      print*,'Error Opening File: '
      print*,'   ',trim(filenames(fref))
   ENDIF

   IF (fref .EQ. 1) THEN
      !get dimensions
      status = nf90_inq_dimid(ncid, 'lon', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nlon)

      status = nf90_inq_dimid(ncid, 'lat', dimid)
      status = nf90_inquire_dimension(ncid, dimid, len=nlat)
      
      status = nf90_inq_dimid(ncid, nluname, dimid)
      IF (status .NE. nf90_noerr) THEN
         nlu=1
      ELSE
         status = nf90_inquire_dimension(ncid, dimid, len=nlu)
      ENDIF

      print('(a,3i10)'), '  Dimensions (nlon/nlat/nlu): ', &
           nlon,nlat,nlu
      allocate(assim(nlon,nlat,nlu), &
           resp(nlon,nlat,nlu), &
           ratio(nlon,nlat,nlu))
      assim(:,:,:) = 0.
      resp(:,:,:) = 0.
      ratio(:,:,:) = 1.

      IF (nlu .EQ. 1) THEN
         ndims=3
         allocate(lu_pref(nlon,nlat,nlu))
         lu_pref(:,:,:) = npftstart
      ELSE
         ndims=4

         !get PFTs
         allocate(lu_pref(nlon,nlat,nlu))
         status = nf90_inq_varid(ncid, trim(lprefname), varid)
         status = nf90_get_var(ncid, varid, lu_pref) 
      ENDIF

   ENDIF

   status = nf90_inq_dimid(ncid, 'time', dimid)
   status = nf90_inquire_dimension(ncid, dimid, len=ntime)
   IF (status .ne. nf90_noerr) THEN
       print*,'Error Getting Time.'
       STOP
   ENDIF
   nttot = nttot + ntime

   !get the GPP and RE
   status = nf90_inq_varid(ncid, 'assim', gppid)
   IF (status .NE. nf90_noerr) THEN
      status = nf90_inq_varid(ncid, 'gpp', gppid)
      IF (status .NE. nf90_noerr) THEN
         print(fa),''
         print(fa),'GPP Not Included In Output.'
         print(fa),'Stopping.'
         print(fa),''
         stop
     ENDIF
   ENDIF
     
   status = nf90_inq_varid(ncid, respname, respid)
   IF (status .NE. nf90_noerr) THEN
      print(fa),''
      print(fa),'RESP Not Included In Output.'
      print(fa),'Stopping.'
      print(fa),''
      stop
   ENDIF

   allocate(dataa(nlon,nlat,nlu,ntime))
   allocate(datar(nlon,nlat,nlu,ntime))
   status = nf90_get_var(ncid, gppid, dataa)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Getting Assimilation!'
      STOP
   ENDIF

   status = nf90_get_var(ncid, respid, datar)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Getting Respiration!'
      STOP
   ENDIF

   do l=1,nlu
      do j=1,nlat
         do i=1,nlon
            assim(i,j,l) = assim(i,j,l) + SUM(dataa(i,j,l,:))
            resp(i,j,l) = resp(i,j,l) + SUM(datar(i,j,l,:))
         enddo
      enddo
   enddo

   deallocate(dataa,datar)
   status = nf90_close(ncid)

ENDDO !fref=1,nfiles

!calculate the ratios
print(fa),'  Calculating Ratios'
DO l=1,nlu
   DO j=1,nlat
      DO i=1,nlon
          IF ((lu_pref(i,j,l) .GE. npftstart) .AND. &
              (lu_pref(i,j,l) .LT. npftcropstart) .AND. &
              (resp(i,j,l) .GT. 1.E-10)) THEN
              ratio(i,j,l) = assim(i,j,l) / resp(i,j,l)
          ENDIF

       ENDDO
    ENDDO
ENDDO
 
print(faf),'     Min: ',minval(ratio)
print(faf),'     Max: ',maxval(ratio)

!write the ratio
IF (write_ratio) THEN
     print(fa),'  Writing Balance Ratio: '
     spos1=index(ratiofile,'/',back=.true.)
     spos2=len_trim(ratiofile)
     print(f2a),'     ',ratiofile(spos1+1:spos2)

     status = nf90_create(trim(ratiofile),nf90_64bit_offset, rfid)
     IF (status .ne.nf90_noerr) THEN
         print*,'Error Creating Ratio File: '
         print*,'  ',trim(ratiofile)
         print*,'Stopping.'
         STOP
     ENDIF

     !copy dimensions to output file
     status = nf90_open(trim(filenames(1)),nf90_nowrite,ncid)
     status = nf90_inquire(ncid, nDimensions=numdims)
     IF (status .ne. nf90_noerr) THEN
        print*,'Error Getting Dimensions.'
        print*,'  Message: ',nf90_strerror(status)
        STOP
     ENDIF

     allocate(mydlens(ndims))
     allocate(mydims(numdims))
     mydlens(:) = 0
     mydims(:) = 0
     do dim=1,numdims
        status = nf90_inquire_dimension(ncid,dim,name=name,len=dimlen)
        IF (trim(name) .ne. 'time') THEN
           mydlens(dim) = dimlen
           status = nf90_def_dim(rfid, trim(name), dimlen, mydims(dim))
           IF (status .ne. nf90_noerr) THEN
               print*,'Error Writing Dimension: ',trim(name)
               print*,'Stopping.'
               STOP
           ENDIF
        ENDIF
     enddo

     !add required variables to output file
     allocate(myvardims(ndims))
     do v=1,nvarsreq-1
        if (v .eq. 1) then
           status = nf90_inq_varid(ncid,'lon',varinid)
        elseif (v .eq. 2) then
           status = nf90_inq_varid(ncid,'lat',varinid)
        else
           status = nf90_inq_varid(ncid,trim(varsreq(v)),varinid)
        endif
        
        IF (status .eq. nf90_noerr) THEN
            status = nf90_inquire_variable(ncid, varinid, &
                     name=name, xtype=xtype, ndims=numdims, &
                     dimids=myvardims, natts=numatts)
            status = nf90_def_var(rfid, trim(name), xtype, &
                     mydims(myvardims(1:numdims)), varid)
            IF (status .NE. nf90_noerr) THEN
               print*,'Error Defining Variable: ',trim(name)
               print*,'   xtype: ',xtype
               print*,'   ndims: ',numdims
               print*,'   dimids: ',myvardims(1:numdims)
               print*,'Stopping.'
               STOP
           ENDIF

           do a=1,numatts
              status = nf90_inq_attname(ncid,varinid,a,name=attname)
              status = nf90_copy_att(ncid,varinid,trim(attname),rfid,varid)
           enddo

        ENDIF
     enddo !v=1,nvarsreq

     status = nf90_def_var(rfid,'ratio',nf90_float, &
              mydims(1:3),ratioid)

     status = nf90_enddef(rfid)

     !copy required variables to output file
     do v=1,nvarsreq-1
        if (v .eq. 1) then
           status = nf90_inq_varid(ncid,'lon',varinid)
        elseif (v .eq. 2) then
           status = nf90_inq_varid(ncid,'lat',varinid)
        else
           status = nf90_inq_varid(ncid,trim(varsreq(v)),varinid)
        endif
        status = nf90_inquire_variable(ncid, varinid, &
                 name=name, xtype=xtype, ndims=numdims, &
                 dimids=myvardims)

        if ((xtype .eq. 5) .and. (numdims .eq. 1)) then
           allocate(fvalues(mydlens(myvardims(1))))
           status = nf90_get_var(ncid, varinid, fvalues)
           status = nf90_put_var(rfid, v, fvalues)
           deallocate(fvalues)

        elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
           allocate(fvalues2(mydlens(myvardims(1)),mydlens(myvardims(2))))
           status = nf90_get_var(ncid, varinid, fvalues2)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Getting Variable: ',trim(name)
              print*,'   ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           status = nf90_put_var(rfid, v, fvalues2)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Writing Variable: ',trim(name)
              print*,'   ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           deallocate(fvalues2)

        elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
           allocate(fvalues3(mydlens(myvardims(1)), &
                mydlens(myvardims(2)), mydlens(myvardims(3))))
           status = nf90_get_var(ncid, varinid, fvalues3)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Getting Variable: ',trim(name)
              print*,'   ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           status = nf90_put_var(rfid, v, fvalues3)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Writing Variable: ',trim(name)
              print*,'  ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           deallocate(fvalues3)

        elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
           allocate(ivalues2(mydlens(myvardims(1)),mydlens(myvardims(2))))
           status = nf90_get_var(ncid, varinid, ivalues2)
           status = nf90_put_var(rfid, v, ivalues2)
           deallocate(ivalues2)

        elseif ((xtype .eq. 4) .and. (numdims .eq. 3)) then
           allocate(ivalues3(mydlens(myvardims(1)), &
                mydlens(myvardims(2)),mydlens(myvardims(3))))
           status = nf90_get_var(ncid, varinid, ivalues3)
           status = nf90_put_var(rfid, v, ivalues3)
           IF (status .NE. nf90_noerr) THEN
              print*,'Error Writing Variable: ',trim(name)
              print*,'  ',trim(nf90_strerror(status))
              print*,'Stopping.'
              STOP
           ENDIF
           deallocate(ivalues3)
           
        elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
           IF (mydlens(myvardims(1)) .eq. 3) THEN
              allocate(cvalues3(mydlens(myvardims(2))))
              status = nf90_get_var(ncid, varinid, cvalues3)
              status = nf90_put_var(rfid, v, cvalues3)
              deallocate(cvalues3)
           ELSE
              print*,'Unexpected Character Array: ',trim(varsreq(v))
              print*,'   Dimension Lengths: ', &
                     mydlens(myvardims(1)),mydlens(myvardims(2))
              print*,'Stopping.'
              STOP
           ENDIF

        elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
           allocate(bvalues(mydlens(myvardims(1))))
           status = nf90_get_var(ncid, varinid, bvalues)
           status = nf90_put_var(rfid, v, bvalues)
           deallocate(bvalues)

        else
           print*,'Unexpected Output Type and/or Dimension.'
           print*,'   Var name and type: ',trim(varsreq(v)),xtype
           print*,'   Number of dimensions: ',numdims
           print*,'   Dimension IDs: ',myvardims
           !print*,'Stopping.'
           !STOP
       endif
     enddo

     status = nf90_put_var(rfid, ratioid, real(ratio))
     IF (status .ne. nf90_noerr) THEN
        print*,'Error writing ratio.'
        print*,'Stopping.'
        STOP
     ENDIF

     status = nf90_close(ncid)
     status = nf90_close(rfid)

ENDIF

!write the balanced respiration
IF (write_bresp) THEN

   print(fa),'  Currently Unable To Add Balanced Respiration.'

endif !write_bresp


end subroutine balance_outgrid


