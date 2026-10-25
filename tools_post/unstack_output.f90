!========================================
subroutine unstack_output( &
    nfiles, filesin, filebresp, filecrop, &
    filesout, nselvar, selvarnamesin, area_flag)
!========================================

use sibinfo_module
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filesin, filesout
character(len=256), intent(in) :: filebresp, filecrop
integer, intent(in) :: nselvar
character(len=20), dimension(nselvar), intent(in) :: &
    selvarnamesin
logical, intent(in) :: area_flag

!parameters
logical, parameter :: print_files=.true.
logical, parameter :: print_varinfo=.false.

!balance respiration variables
integer :: nlonb, nlatb, nlub
real, dimension(:,:,:), allocatable :: &
      resp_ratio
real, dimension(:,:,:,:), allocatable :: &
      resp_orig
logical :: add_bresp

!crop respiration variables
integer :: nlonc, nlatc, ncropc
real, dimension(:,:,:), allocatable :: resp_crop
logical :: add_cresp

!PFT variables
integer, dimension(:,:,:), allocatable :: lu_pref
real, dimension(:,:,:), allocatable :: lu_area

!data variables
integer :: nsibd, nlond, nlatd, nlud, npftd, ncropd, clend, slend, ntimed
integer :: nsibdid, nlondid, nlatdid, npftdid, ncropdid
integer :: clendid, slendid, ntimedid
integer :: nsibdidin, nlondidin, nlatdidin, nludidin, npftdidin
integer :: ncropdidin, clendidin, slendidin, ntimedidin
integer :: respbid, respcid

character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues61
character(len=6), dimension(:,:), allocatable :: cvalues6

integer, dimension(:,:), allocatable :: ivalues
real*8, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2,fvalues2out
real, dimension(:,:,:), allocatable :: fvalues3,fvalues3out
real, dimension(:,:,:,:), allocatable :: fvalues4,fvalues4out

!netcdf variables
integer :: status, inid, outid
integer :: dimid, dimlen, varid, varinid
integer :: numatts, numdims, ndimso
integer :: numvars, nvarso
integer :: xtype
integer, dimension(:), allocatable :: &
   dimlens, vardims, vardimso
integer :: dimlenin1,dimlenin2,dimlenin3,dimlenin4
character(len=20) :: name, attname
character(len=60) :: mystring
character(len=20), dimension(:), allocatable :: varnames

!misc variables
integer :: a, d
integer :: f, v, t
integer :: i, j, l
integer :: spos1, spos2
logical :: file_exist

!---------------------------------
!set local variables
nsibd=-1
nlond=-1
nlatd=-1
nlud=-1
npftd=-1
ncropd=-1
clend=-1
slend=-1
ntimed=-1

nsibdid=-1
nlondid=-1
nlatdid=-1
npftdid=-1
ncropdid=-1
clendid=-1
slendid=-1
ntimedid=-1

nsibdidin=-1
nlondidin=-1
nlatdidin=-1
npftdidin=-1
ncropdidin=-1
clendidin=-1
slendidin=-1
ntimedidin=-1

!get balanced respiration ratio, if available
add_bresp = .false.
inquire(file=trim(filebresp), exist=file_exist)

IF (file_exist) THEN
    status = nf90_open(trim(filebresp),nf90_nowrite,inid)
    IF (status .ne. nf90_noerr) THEN
        print*,'Error Opening Balanced Respiration File.'
        STOP
    ENDIF
    print*,'  Adding Balanced Respiration'

    add_bresp = .true.
    status = nf90_inq_dimid(inid,'lon',dimid)
    status = nf90_inquire_dimension(inid,dimid,len=nlonb)
    status = nf90_inq_dimid(inid,'lat',dimid)
    status = nf90_inquire_dimension(inid,dimid,len=nlatb)

    status = nf90_inq_dimid(inid,'nlu',dimid)
    IF (status .ne. nf90_noerr) THEN
       print*,'Expecting unstacked respiration ratios.'
       print*,'Stopping.'
       STOP
    ELSE
       status = nf90_inquire_dimension(inid,dimid,len=nlub)
       allocate(resp_ratio(nlonb,nlatb,nlub))
       status = nf90_inq_varid(inid,trim('ratio'),varid)
       status = nf90_get_var(inid,varid,resp_ratio)
       IF (status .ne. nf90_noerr) THEN
          print*,''
          print*,'Error Getting Balanced Respiration Ratios.'
          print*,'Stopping.'
          STOP
       ENDIF
    ENDIF
 
    status = nf90_close(inid)
ELSE
   print*,'  Not Adding Balanced Respiration.'
ENDIF

!get crop respiration, if available
add_cresp = .false.
inquire(file=trim(filecrop), exist=file_exist)
IF (file_exist) THEN
    status = nf90_open(trim(filecrop),nf90_nowrite,inid)
    IF (status .ne. nf90_noerr) THEN
        print*,'Error Opening Crop File'
        stop
    ENDIF
    print*,'  Adding Crop Respiration'

    add_cresp = .true.
    status = nf90_inq_dimid(inid,'lon',dimid)
    status = nf90_inquire_dimension(inid,dimid,len=nlonc)
    status = nf90_inq_dimid(inid,'lat',dimid)
    status = nf90_inquire_dimension(inid,dimid,len=nlatc)
    status = nf90_inq_dimid(inid,'ncrop',dimid)
    status = nf90_inquire_dimension(inid,dimid,len=ncropc)

    allocate(resp_crop(nlonc,nlatc,ncropc))
    status = nf90_inq_varid(inid,trim(respcname),varid)
    status = nf90_get_var(inid,varid,resp_crop)
    IF (status .ne. nf90_noerr) THEN
       print*,''
       print*,'Error Getting Crop Respiration.'
       print*,'Stopping.'
       STOP
    ELSE
       print*,'     Min/Max Crop Respiration Rate: ', &
           minval(resp_crop),maxval(resp_crop)
    ENDIF
 
    status = nf90_close(inid)
ELSE
    print*,'  Not Adding Crop Harvest Respiration.'
ENDIF

!process files
do f = 1, nfiles
   !write message if requested
   IF (print_files) THEN
       spos1 = index(filesin(f),'/',back=.true.)
       spos2 = len_trim(filesin(f))
       print*,''
       print*,'  Unstacking: ',filesin(f)(spos1+1:spos2)

       spos1 = index(filesout(f),'/',back=.true.)
       spos2 = len_trim(filesout(f))
       print*,'  Creating: ',filesout(f)(spos1+1:spos2)
   ENDIF

   !open input and output files
   status = nf90_open(trim(filesin(f)),nf90_nowrite,inid)
   IF (status .NE. nf90_noerr) THEN
      print*,'Error Opening Input File: '
      print*,' ',trim(filesin(f))
      print*,'Stopping.'
      STOP
   ENDIF

   if (use_deflate) then
      status = nf90_create(trim(filesout(f)),nf90_netcdf4,outid)
      !status = nf90_create(trim(filesout(f)), nf90_hdf5, outid)
   else
      status = nf90_create(trim(filesout(f)), nf90_64bit_offset, outid)
   endif
   
   IF (status .NE. nf90_noerr) THEN
       print*,'Error Opening Output File: '
       print*,' ',trim(filesout(f))
       print*,'Stopping.'
       STOP
   ENDIF

   !get dimensions and attributes
   status = nf90_inquire(inid, nDimensions=numdims, &
            nVariables=numvars, nAttributes=numatts)

   !copy global attributes
   do a=1,numatts
      status = nf90_inq_attname(inid,nf90_global,a,name)
      status = nf90_copy_att(inid,nf90_global,trim(name), &
               outid,nf90_global)
   enddo

   !add convention attribute
   status = nf90_put_att(outid,nf90_global,'Conventions','CF-1.6')

   ! copy dimensions over to output file
   ndimso = numdims-1
   allocate(dimlens(numdims))
   dimlens=0
   do d=1,numdims
      status = nf90_inquire_dimension(inid, d, name=name, len=dimlen)
      dimlens(d) = dimlen

      IF (trim(name) .EQ. 'time') THEN
          ntimed = dimlen
          ntimedidin = d
      ENDIF

      IF (trim(name) .EQ. 'landpoints') THEN
          nsibd = dimlen
          nsibdidin = d
          status = nf90_def_dim(outid,trim(name),nsibd,nsibdid)
      ENDIF

      IF (trim(name) .EQ. 'lon') THEN
         nlond = dimlen
         nlondidin = d
         status = nf90_def_dim(outid,trim(name),nlond,nlondid)
      ENDIF

      IF (trim(name) .EQ. 'lat') THEN
         nlatd = dimlen
         nlatdidin = d
         status = nf90_def_dim(outid,trim(name),nlatd,nlatdid)
      ENDIF

      IF (trim(name) .EQ. 'nlu') THEN
         nlud = dimlen
         nludidin = d
      ENDIF

      IF (trim(name) .EQ. 'npft') THEN
         npftd = dimlen
         npftdidin = d
         status = nf90_def_dim(outid,trim(name),npftd,npftdid)
      ENDIF

      IF (trim(name) .EQ. 'ncrop') THEN
         ncropd = dimlen
         ncropdidin = d
         status = nf90_def_dim(outid,trim(name),ncropd,ncropdid)
      ENDIF

      IF (trim(name) .EQ. 'clen') THEN
         clend = dimlen
         clendidin = d
         status = nf90_def_dim(outid,trim(name),clend,clendid)
      ENDIF

      IF (trim(name) .EQ. 'slen') THEN
         slend = dimlen
         slendidin = d
         status = nf90_def_dim(outid,trim(name),slend,slendid)
      ENDIF
   enddo
   IF (ntimed .gt. 0) &
       status = nf90_def_dim(outid,'time',nf90_unlimited,ntimedid)

   !define variables in output file
    IF ((nselvar .le. 1) .AND. &
        (trim(selvarnamesin(1)) .EQ. '')) THEN
       !count variables, removing unnecessary
       nvarso=0
       do v=1,numvars
          status = nf90_inquire_variable(inid, v, name=name)
          IF ((trim(name) .NE. trim(pftrefs)) .AND. &
              (trim(name) .NE. trim(lprefname))) nvarso=nvarso+1
       enddo

       allocate(varnames(nvarso))
       varnames=''
       nvarso=0
       do v=1,numvars
          status = nf90_inquire_variable(inid, v, name=name)
          IF ((trim(name) .NE. trim(pftrefs)) .AND. &
              (trim(name) .NE. trim(lprefname))) THEN
              nvarso=nvarso+1
              varnames(nvarso) = name
          ENDIF
       enddo

    ELSE
       !check variable names
       nvarso=0
       do v=1,nvarsreq
          IF ((varsreq(v) .NE. trim(pftrefs)) .AND. &
              (varsreq(v) .NE. trim(lprefname))) THEN
              status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
              IF (status .EQ. nf90_noerr) nvarso=nvarso+1
          ENDIF
       enddo

       do v=1,nselvar
          status = nf90_inq_varid(inid,trim(selvarnamesin(v)),varid)
          IF (status .EQ. nf90_noerr) nvarso=nvarso+1
       enddo

       !save variable names
       allocate(varnames(nvarso))
       varnames=''
       nvarso=0
       do v=1,nvarsreq
          IF ((varsreq(v) .NE. trim(pftrefs)) .AND. &
              (varsreq(v) .NE. trim(lprefname))) THEN
              status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
              IF (status .EQ. nf90_noerr) THEN
                 nvarso=nvarso+1
                 varnames(nvarso) = varsreq(v)
                 IF (varnames(nvarso) .eq. 'resp_fire') THEN
                    print*,'RESP_FIRE WARNING: THIS IS ALREADY WEIGHTED,'
                    print*,'   AND THIS ROUTINE HAS NOT BEEN MODIFIED.'
                    print*,'PLEASE BE WARNED RESP_FIRE WILL BE INCORRECT.'
                 ENDIF
              ENDIF
          ENDIF
       enddo

       do v=1,nselvar
          status = nf90_inq_varid(inid,trim(selvarnamesin(v)),varid)
          IF (status .EQ. nf90_noerr) THEN
             nvarso=nvarso+1
             varnames(nvarso) = selvarnamesin(v)
          ENDIF
       enddo
   ENDIF
   print('(a,i4)'),'   Number of variables saving: ',nvarso

   allocate(vardims(numdims))
   vardims(:) = 0
   allocate(vardimso(ndimso))
   vardimso(:) = 0
   do v=1,nvarso
      status = nf90_inq_varid(inid,trim(varnames(v)),varinid)
      status = nf90_inquire_variable(inid,varinid,name=name, &
                xtype=xtype, ndims=numdims, dimids=vardims, &
                natts=numatts)
      do d=1,numdims
         dimid=vardims(d)
         dimlen=dimlens(vardims(d))
         IF (dimid .EQ. nsibdidin) THEN
             vardimso(d) = nsibdid
         ELSEIF (dimid .EQ. nlondidin) THEN
             vardimso(d) = nlondid
         ELSEIF (dimid .EQ. nlatdidin) THEN
             vardimso(d) = nlatdid
         ELSEIF (dimid .EQ. nludidin) THEN
             vardimso(d) = npftdid
         ELSEIF (dimid .EQ. npftdidin) THEN
             vardimso(d) = npftdid
         ELSEIF (dimid .EQ. ncropdidin) THEN
             vardimso(d) = ncropdid
         ELSEIF (dimid .EQ. clendidin) THEN
             vardimso(d) = clendid
         ELSEIF (dimid .EQ. slendidin) THEN
             vardimso(d) = slendid
         ELSEIF (dimid .EQ. ntimedidin) THEN
             vardimso(d) = ntimedid
         ELSE
              print*,'Unknown Variable Dimension.'
              print*,'  Variable: ',trim(name)
              print*,'  Type: ',xtype
              print*,'  NumDims: ',numdims
              print*,'  Dimension #: ',d
              print*,'  Dimension Length: ',dimlen
              print*,'Stopping.'
              STOP
         ENDIF
       enddo

       if (trim(name) .eq. 'lu_area') then
          status = nf90_def_var(outid, 'pft_area', xtype, &
               vardimso(1:numdims), varid)
       else
          status = nf90_def_var(outid, trim(name), xtype, &
                   vardimso(1:numdims), varid)
       endif
       if (use_deflate) then
           status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
       endif
       IF (status .NE. nf90_noerr) THEN
          print*,'Error Defining Variable: ',trim(name)
          print*,' message: ',trim(nf90_strerror(status))
          print*,'   xtype: ',xtype
          print*,'   ndims: ',numdims
          print*,'   dimids: ',vardimso(1:numdims)
          print*,'Stopping.'
          STOP
       ENDIF

       do a=1,numatts
          status = nf90_inq_attname(inid,varinid,a,name=attname)
          IF (trim(attname) .EQ. 'quantity') THEN
              status = nf90_get_att(inid,varinid,trim(attname),mystring)
              status = nf90_put_att(outid,varid,'long_name',mystring)
          ENDIF
          IF (trim(attname) .EQ. 'title') THEN
             status = nf90_get_att(inid,varinid,trim(attname),mystring)
             status = nf90_put_att(outid,varid,'long_name',mystring)
          ENDIF
          IF (trim(attname) .EQ. 'units') THEN
             status = nf90_get_att(inid,varinid,trim(attname),mystring)
             IF (trim(mystring) .EQ. '-') THEN
                status = nf90_put_att(outid,varid,trim(attname),'1')
             ELSEIF (trim(mystring) .EQ. 'Fraction (0-1)') THEN
                status = nf90_put_att(outid,varid,trim(attname),'1')
             ELSE
                 status = nf90_copy_att(inid,varinid,trim(attname),outid,varid)
             ENDIF
          ENDIF
          IF ((trim(attname) .EQ. 'long_name') .AND. (numatts .LE. 2)) THEN
              status = nf90_copy_att(inid,varinid,trim(attname),outid,varid)
          ENDIF
       enddo

       IF (trim(name) .EQ. 'lu_area') THEN
          status = nf90_put_att(outid,varid,'long_name', &
               'PFT Fractional Areal Coverage')
       ENDIF
       IF (trim(name) .EQ. 'gpp') THEN
          status = nf90_put_att(outid,varid,'long_name', &
               'Gross Primary Production')
       ENDIF
       IF (trim(name) .EQ. 'ratio') THEN
          status = nf90_put_att(outid,varid,'long_name', &
               'Ratio To Balance Respiration')
          status = nf90_put_att(outid,varid,'units', '1')
       ENDIF
       IF (trim(name) .EQ. 'time') THEN
          status = nf90_put_att(outid,varid,'calendar','standard')
       ENDIF

   enddo !v=1,nvarso

   !add balanced respiration
   IF (add_bresp) THEN
      IF ((nlond .ne. nlonb) .or. (nlatd .ne. nlatd)) THEN
         print*,'Mismatching SiB4 and Ratio Files.'
         print*,'Stopping.'
         STOP
      ENDIF
      
      status = nf90_inq_varid(outid, trim(respbname), respbid)
      if (status .ne. nf90_noerr) then
          status = nf90_def_var(outid, trim(respbname), nf90_float, &
                   (/nlondid,nlatdid,npftdid,ntimedid/),respbid)
          if (use_deflate) then
              status = nf90_def_var_deflate(outid, respbid, 1, 1, deflev)
          endif
          status = nf90_put_att(outid,respbid,'long_name',trim(respblong))
          status = nf90_put_att(outid,respbid,'units',trim(respbunits))
      endif
   ENDIF

   !add crop respiration
   IF (add_cresp) THEN
       status = nf90_inq_varid(outid, trim(respcname), respcid)
       if (status .ne. nf90_noerr) then
           status = nf90_def_var(outid, trim(respcname), nf90_float, &
                    (/nlondid,nlatdid,npftdid,ntimedid/),respcid)
           if (use_deflate) then
               status = nf90_def_var_deflate(outid, respcid, 1, 1, deflev)
           endif
           status = nf90_put_att(outid,respcid,'long_name',trim(respclong))
           status = nf90_put_att(outid,respcid,'units',trim(respcunits))
       endif
   ENDIF

   !stop defining variables
   status = nf90_enddef(outid)

   !get lu_pref and lu_area to unpack PFts
   IF (nsibdid .ge. 0) THEN
      allocate(lu_pref(nsibd,1,nlud))
      allocate(ivalues(nsibd,nlud))
      status = nf90_inq_varid(inid,'lu_pref',varinid)
      status = nf90_get_var(inid,varinid,ivalues)
      lu_pref(:,1,:) = ivalues(:,:)
      deallocate(ivalues)
   ELSE
      allocate(lu_pref(nlond,nlatd,nlud))
      status = nf90_inq_varid(inid,'lu_pref',varinid)
      status = nf90_get_var(inid,varinid,lu_pref)
   ENDIF
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Getting PFT Reference Values.'
      print*,'Stopping.'
      STOP
   ENDIF

   IF (nsibdid .ge. 0) THEN
      allocate(lu_area(nsibd,1,nlud))
      allocate(fvalues2(nsibd,nlud))
      status = nf90_inq_varid(inid,'lu_area',varinid)
      status = nf90_get_var(inid,varinid,fvalues2)
      lu_area(:,1,:) = fvalues2(:,:)
      deallocate(fvalues2)
   ELSE
      allocate(lu_area(nlond,nlatd,nlud))
      status = nf90_inq_varid(inid,'lu_area',varinid)
      status = nf90_get_var(inid,varinid,lu_area)
   ENDIF
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Getting PFT Areas.'
      print*,'Stopping.'
      STOP
   ENDIF

   !copy variables to output file
   do v=1,nvarso
      status = nf90_inq_varid(inid,trim(varnames(v)),varinid)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Obtaining Variable ID: ',trim(varnames(v))
         print*,trim(nf90_strerror(status))
         print*,'Stopping.'
         STOP
      ENDIF
      status = nf90_inquire_variable(inid, varinid, &
              name=name, xtype=xtype, &
              ndims=numdims, dimids=vardims)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Inquiring Variable: ',trim(varnames(v))
         print*,trim(nf90_strerror(status))
         print*,'Stopping.'
         STOP
      ENDIF

     dimlenin1 = dimlens(vardims(1))
     if (numdims .ge. 2) dimlenin2 = dimlens(vardims(2))
     if (numdims .ge. 3) dimlenin3 = dimlens(vardims(3))
     if (numdims .ge. 4) dimlenin4 = dimlens(vardims(4))
     
     if ((xtype .eq. 6) .and. (numdims .eq. 1)) THEN
         allocate(dvalues(dimlenin1))
         status = nf90_get_var(inid, varinid, dvalues)
         status = nf90_put_var(outid, v, dvalues)
         deallocate(dvalues)

     elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
       allocate(fvalues(dimlenin1))
       status = nf90_get_var(inid, varinid, fvalues)
       status = nf90_put_var(outid, v, fvalues)
       deallocate(fvalues)

     elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
       allocate(fvalues2(dimlenin1,dimlenin2))
       status = nf90_get_var(inid,varinid,fvalues2)

       if (dimlenin2 .eq. nlud) THEN
          allocate(fvalues2out(dimlenin1,npftd))
          fvalues2out(:,:) = 0.
          IF (trim(varnames(v)) .EQ. 'lu_area') THEN
             call unstack_pft2(dimlenin1,dimlenin2,npftd, &
                  lu_pref(:,1,:), lu_area(:,1,:), .false., &
                  fvalues2, fvalues2out)
          ELSE
             call unstack_pft2(dimlenin1,dimlenin2,npftd, &
                  lu_pref(:,1,:), lu_area(:,1,:), area_flag, &
                  fvalues2, fvalues2out)
          ENDIF
          status = nf90_put_var(outid, v, fvalues2out)
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Writing Variable: ',trim(varnames(v))
             print*,'Stopping.'
             STOP
          ENDIF
          deallocate(fvalues2out)
       ENDIF
       deallocate(fvalues2)

     elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
       allocate(fvalues3(dimlenin1,dimlenin2,dimlenin3))
       status = nf90_get_var(inid,varinid,fvalues3)

       IF (nsibd .ge. 1) THEN
          IF (dimlenin2 .eq. nlud) THEN
             allocate(fvalues3out(dimlenin1,npftd,dimlenin3))
             fvalues3out(:,:,:) = 0.
             do d=1,dimlenin3
                call unstack_pft2(dimlenin1, dimlenin2, npftd, &
                     lu_pref(:,1,:), lu_area(:,1,:), area_flag,&
                     fvalues3(:,:,d), fvalues3out(:,:,d))
             enddo
             status = nf90_put_var(outid, v, fvalues3out)
             IF (status .ne. nf90_noerr) THEN
                 print*,'Error Writing Variable: ',trim(varnames(v))
                 print*,'Stopping.'
                 STOP
             ENDIF
             deallocate(fvalues3out)
          ELSE
            status = nf90_put_var(outid, v, fvalues3)
          ENDIF
          deallocate(fvalues3)
       ELSE

          if (dimlenin3 .eq. nlud) THEN
              allocate(fvalues3out(dimlenin1,dimlenin2,npftd))       
              fvalues3out(:,:,:) = 0.
              IF (trim(varnames(v)) .EQ. 'lu_area') THEN
                  call unstack_pft3(dimlenin1, dimlenin2, dimlenin3, &
                       npftd, lu_pref, lu_area, .false., &
                       fvalues3, fvalues3out)
              ELSE
                  call unstack_pft3(dimlenin1, dimlenin2, dimlenin3, &
                       npftd, lu_pref, lu_area, area_flag, &
                       fvalues3, fvalues3out)
              ENDIF
              status = nf90_put_var(outid, v, fvalues3out)
              deallocate(fvalues3out)
         else
              status = nf90_put_var(outid, v, fvalues3)
         endif
         deallocate(fvalues3)
      ENDIF

     elseif ((xtype .eq. 5) .and. (numdims .eq. 4)) then
         allocate(fvalues4(dimlenin1,dimlenin2,dimlenin3,dimlenin4))
         status = nf90_get_var(inid,varinid,fvalues4)

         IF (dimlenin3 .eq. nlud) THEN
            allocate(fvalues4out(dimlenin1,dimlenin2,npftd,dimlenin4))
            fvalues4out(:,:,:,:) = 0.
            call unstack_pft4(dimlenin1, dimlenin2, dimlenin3, dimlenin4, &
                 npftd, lu_pref, lu_area, area_flag, &
                 fvalues4, fvalues4out)
            status = nf90_put_var(outid, v, fvalues4out)
            deallocate(fvalues4out)
         ELSE
            status = nf90_put_var(outid, v, fvalues4)
         ENDIF
         deallocate(fvalues4)

     elseif ((xtype .eq. 2) .and. (numdims .eq. 3)) then
         IF ((dimlenin1 .eq. slend) .and. (slend .eq. 6)) THEN
            allocate(cvalues6(dimlenin2,dimlenin3))
            status = nf90_get_var(inid,varinid,cvalues6)
            status = nf90_put_var(outid, v, cvalues6)
            deallocate(cvalues6)
         ELSE
            print*,'Unexpected Character Array: ',trim(varnames(v))
            print*,'   Dimension Lengths: ',dimlenin1, dimlenin2, dimlenin3
            print*,'Stopping.'
            STOP
         ENDIF

         
     elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
         IF ((dimlenin1 .eq. clend) .and. (clend .eq. 3)) THEN
            allocate(cvalues3(dimlenin2))
            status = nf90_get_var(inid,varinid,cvalues3)
            status = nf90_put_var(outid, v, cvalues3)
            deallocate(cvalues3)
         ELSE
            IF ((dimlenin1 .eq. slend) .and. (slend .eq. 6)) THEN
                allocate(cvalues61(dimlenin2))
                status = nf90_get_var(inid,varinid,cvalues61)
                status = nf90_put_var(outid, v, cvalues61)
                deallocate(cvalues61)
            ELSE
                print*,'Unexpected Character Array: ',trim(varnames(v))
                print*,'   Dimension Lengths: ',dimlenin1, dimlenin2
                print*,'Stopping.'
                STOP
            ENDIF
        ENDIF
     else
         print*,'Unexpected Output Type and/or Dimension.'
         print*,'   Var name and type: ',trim(varnames(v)),xtype
         print*,'   Number of dimensions: ',numdims
         print*,'   Dimension IDs: ',vardims
         print*,'Stopping.'
         STOP
     endif

      IF (status .NE. nf90_noerr) THEN
          print*,'Error Writing Variable: ',varnames(v)
          print*,'Stopping.'
          STOP
      ENDIF

      IF (print_varinfo) THEN
          print*,'  ',trim(varnames(v)),' min/max: ', &
               minval(fvalues4out),maxval(fvalues4out)
      ENDIF
   enddo !v=1,nvarso

   !add balanced respiration
   IF (add_bresp) THEN
      allocate(resp_orig(nlond,nlatd,nlud,ntimed))
      allocate(fvalues4(nlond,nlatd,nlud,ntimed))
      allocate(fvalues4out(nlond,nlatd,npftd,ntimed))
      fvalues4(:,:,:,:) = 0.
      fvalues4out(:,:,:,:) = 0.

      status = nf90_inq_varid(inid,respname,varid)
      IF (status .ne. nf90_noerr) THEN
          print*,'Total Respiration Not Included In File.'
          print*,'Cannot Scale Respiration To Balance Fluxes.'
          print*,'Stopping.'
          STOP
      ENDIF
      status = nf90_get_var(inid,varid,resp_orig)
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Getting Total Respiration.'
          print*,'Stopping.'
         STOP
      ENDIF

      do l=1,nlud
         do j=1,nlatd
            do i=1,nlond
               fvalues4(i,j,l,:) = resp_orig(i,j,l,:) * resp_ratio(i,j,l)
            enddo
         enddo
       enddo
       call unstack_pft4(nlond,nlatd,nlud,ntimed, &
              npftd, lu_pref, lu_area, area_flag, &
              fvalues4, fvalues4out)

       status = nf90_put_var(outid,respbid,fvalues4out)
       IF (status .ne. nf90_noerr) THEN
           print*,'Error Adding Balanced Respiration.'
           print*,'Stopping.'
          STOP
        ENDIF

        deallocate(resp_orig,fvalues4,fvalues4out)
   ENDIF

   !add crop respiration
   IF (add_cresp) THEN
      allocate(fvalues4(nlond,nlatd,npftd,ntimed))
      fvalues4(:,:,:,:) = 0.

      DO t=1,ntimed
         fvalues4(:,:,pftnum(npftcropstart):npftd,t) = &
          resp_crop(:,:,:)
      ENDDO
      status = nf90_put_var(outid,respcid,fvalues4)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Adding Crop Respiration.'
         print*,'Stopping.'
         STOP
      ENDIF

      deallocate(fvalues4)
   ENDIF

   !close files
   status = nf90_close(inid)
   status = nf90_close(outid)

   !deallocate arrays
   deallocate(dimlens)
   deallocate(varnames)
   deallocate(vardims,vardimso)
   deallocate(lu_area,lu_pref)

enddo !f=1,nfiles

end subroutine unstack_output


!========================================
subroutine unstack_pft2( &
    nsib, nlu, npft, &
    lu_pref, lu_area, area_flag, &
    valuesin, valuesout)
!========================================

use sibinfo_module, only: pftnum
implicit none

!input variables
integer, intent(in) :: nsib, nlu, npft
integer, dimension(nsib,nlu), intent(in) :: lu_pref
real, dimension(nsib,nlu), intent(in) :: lu_area
logical, intent(in) :: area_flag
real, dimension(nsib,nlu), intent(in) :: valuesin
real, dimension(nsib,npft), intent(inout) :: valuesout

!local variables
integer :: l, n
integer :: pnum, pref

!--------------------

IF ((nsib .LT. 1) .OR. &
    (nlu .LT. 1) .OR. (npft .LT. 1)) THEN
    print*,'Bad Dimensions To Unstack.'
    print*,'   nsib: ',nsib
    print*,'   nlu/npft:  ',nlu,npft
    print*,'Stopping.'
    STOP
ENDIF


do n=1,nsib
   do l=1,nlu
      pref = lu_pref(n,l)
      IF (pref .GT. 0) THEN
          pnum = pftnum(lu_pref(n,l))
          IF (area_flag) THEN
             valuesout(n,pnum) = valuesin(n,l)*lu_area(n,l)
          ELSE
             valuesout(n,pnum) = valuesin(n,l)
          ENDIF
      ENDIF
  enddo
enddo

end subroutine unstack_pft2





!========================================
subroutine unstack_pft3( &
    nlon, nlat, nlu, npft, &
    lu_pref, lu_area, area_flag, &
    valuesin, valuesout)
!========================================

use sibinfo_module, only: pftnum
implicit none

!input variables
integer, intent(in) :: nlon, nlat, nlu, npft
integer, dimension(nlon,nlat,nlu), intent(in) :: lu_pref
real, dimension(nlon,nlat,nlu), intent(in) :: lu_area
logical, intent(in) :: area_flag
real, dimension(nlon,nlat,nlu), intent(in) :: valuesin
real, dimension(nlon,nlat,npft), intent(inout) :: valuesout

!local variables
integer :: i,j,l
integer :: pnum, pref

!--------------------

IF ((nlon .LT. 1) .OR. (nlat .LT. 1) .OR. &
    (nlu .LT. 1) .OR. (npft .LT. 1)) THEN
    print*,'Bad Dimensions To Unstack.'
    print*,'   nlon/nlat: ',nlon,nlat
    print*,'   nlu/npft:  ',nlu,npft
    print*,'Stopping.'
    STOP
ENDIF


do i=1,nlon
   do j=1,nlat
      do l=1,nlu
         pref = lu_pref(i,j,l)
         IF (pref .GT. 0) THEN
             pnum = pftnum(lu_pref(i,j,l))
             IF (area_flag) THEN
                 valuesout(i,j,pnum) = valuesin(i,j,l)*lu_area(i,j,l)
             ELSE
                 valuesout(i,j,pnum) = valuesin(i,j,l)
             ENDIF
         ENDIF
      enddo
    enddo
enddo

end subroutine unstack_pft3


!========================================
subroutine unstack_pft4( &
    nlon, nlat, nlu, ntime, &
    npft, lu_pref, lu_area, area_flag, &
    valuesin, valuesout)
!========================================

use sibinfo_module, only: pftnum
implicit none

!input variables
integer, intent(in) :: nlon, nlat, nlu, ntime, npft
integer, dimension(nlon,nlat,nlu), intent(in) :: lu_pref
real, dimension(nlon,nlat,nlu), intent(in) :: lu_area
logical, intent(in) :: area_flag
real, dimension(nlon,nlat,nlu,ntime), intent(in) :: valuesin
real, dimension(nlon,nlat,npft,ntime), intent(inout) :: valuesout

!local variables
integer :: i,j,l
integer :: pnum, pref

!--------------------

IF ((nlon .LT. 1) .OR. (nlat .LT. 1) .OR. &
    (nlu .LT. 1) .OR. (ntime .LT. 1) .OR. &
    (npft .LT. 1)) THEN
    print*,'Bad Dimensions To Unstack.'
    print*,'   nlon/nlat: ',nlon,nlat
    print*,'   nlu/npft:  ',nlu,npft
    print*,'   ntime:     ',ntime
    print*,'Stopping.'
    STOP
ENDIF

do i=1,nlon
   do j=1,nlat
      do l=1,nlu
         pref = lu_pref(i,j,l)
         IF (pref .GT. 0) THEN
             pnum = pftnum(lu_pref(i,j,l))
             IF (area_flag) THEN
                 valuesout(i,j,pnum,:) = valuesin(i,j,l,:)*lu_area(i,j,l)
             ELSE
                 valuesout(i,j,pnum,:) = valuesin(i,j,l,:)
             ENDIF
         ENDIF
      enddo
    enddo
enddo

end subroutine unstack_pft4
