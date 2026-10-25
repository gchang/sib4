!========================================
subroutine vec2vec_output(nfiles, &
    filesin, filesout, regridfile)
!========================================

use sibinfo_module, only: &
    lareaname, lprefname
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filesin, filesout
character(len=*), intent(in) :: regridfile

!parameters
integer, parameter :: maxng=4

!file variables
integer :: stringloc, stringend
character(len=180) :: shortname

!netcdf variables
integer :: inid
integer :: status
integer :: dimid, varid

!original variables
integer :: nsibin
real, dimension(:), allocatable :: &
   lonin, latin

!new variables
integer :: nsibout
real, dimension(:), allocatable :: &
   lonout, latout

!misc variables
integer :: f
logical :: regridup


!----------------------------------------
DO f=1, nfiles

   IF (f .eq. 1) THEN
      !Get the new grid
      status = nf90_open(trim(regridfile),nf90_nowrite,inid)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening Regrid File: '
         print*,'  ',trim(regridfile)
         print*,'Stopping.'
         STOP
      ENDIF

      status = nf90_inq_dimid(inid,'nsib',dimid)
      IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(inid,'landpoints',dimid)
          IF (status .ne. nf90_noerr) THEN
              print*,'Error Finding Number of Points.'
              print*,'Stopping.'
              STOP
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(inid,dimid,len=nsibout)
      allocate(lonout(nsibout))
      status = nf90_inq_varid(inid,'lonsib',varid)
      status = nf90_get_var(inid,varid,lonout)
      IF (status .ne. nf90_noerr) THEN
          print*,'Error Getting Output Lons.'
          STOP
      ENDIF

      allocate(latout(nsibout))
      status = nf90_inq_varid(inid,'latsib',varid)
      status = nf90_get_var(inid,varid,latout)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Getting Output Lats.'
         STOP
      ENDIF
      status = nf90_close(inid)

      !Get the original grid
      status = nf90_open(trim(filesin(f)),nf90_nowrite,inid)
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Opening File: '
         print*,'  ',trim(filesin(f))
         print*,'Stopping.'
         STOP
      ENDIF

      status = nf90_inq_dimid(inid,'nsib',dimid)
      IF (status .NE. nf90_noerr) THEN
          status = nf90_inq_dimid(inid,'landpoints',dimid)
          IF (status .NE. nf90_noerr) THEN
              print*,'Error Finding Original Number of Points.'
              print*,'Stopping.'
              STOP
          ENDIF
      ENDIF
      status = nf90_inquire_dimension(inid,dimid,len=nsibin)
      allocate(lonin(nsibin))
      status = nf90_inq_varid(inid,'lonsib',varid)
      IF (status .NE. nf90_noerr) THEN
         print*,'File Does Not Contain Longitude.'
         STOP
      ENDIF
      status = nf90_get_var(inid,varid,lonin)
      IF (status .NE. nf90_noerr) THEN
         print*,'Error Getting Original Lons.'
         STOP
      ENDIF

      allocate(latin(nsibin))
      status = nf90_inq_varid(inid,'latsib',varid)
      IF (status .ne. nf90_noerr) THEN
         print*,'File Does Not Contain Latitude.'
         STOP
      ENDIF
      status = nf90_get_var(inid,varid,latin)
      IF (status .NE. nf90_noerr) THEN
         print*,'Error Getting Original Lats.'
         STOP
      ENDIF

      status = nf90_close(inid)

      !-----------------------
      !Compare the dimensions   
      regridup = .true.
      IF (nsibout .GT. nsibin) regridup = .false.

   ENDIF !f==1

   IF (regridup) THEN
      print*,'   !!!Not Yet Done!!!'
      RETURN
   ELSE

      stringloc = index(filesin(f),'/',back=.true.)
      stringend = len_trim(filesin(f))
      shortname = (filesin(f)(stringloc+1:stringend))
      print*,''
      print('(a)'), '   Orig File: '
      print('(2a)'),  '       ',trim(filesin(f))
      print('(a,i8)'),'       Points: ', nsibin

      stringloc = index(filesout(f),'/',back=.true.)
      stringend = len_trim(filesout(f))
      shortname = (filesout(f)(stringloc+1:stringend))
      print('(1a)'),'   New File: '
      print('(2a)'),'      ',trim(filesout(f))
      print('(a,i8)'),'       Points: ', nsibout

      call vec2vec_down( &
           filesin(f), filesout(f), &
           nsibin, nsibout, &
           lonin, latin, lonout, latout)
   ENDIF

ENDDO !f=1,nfiles


end subroutine vec2vec_output


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine vec2vec_down( &
          filenamein, filenameout, &
          nsibin, nsibout, &
          lonin, latin, lonout, latout)

use netcdf
use sibinfo_module, only: &
   use_deflate, deflev
implicit none

! input variables
character(len=*), intent(in) :: filenamein
character(len=*), intent(in) :: filenameout
integer, intent(in) :: nsibin, nsibout
real, dimension(nsibin), intent(in) :: lonin, latin
real, dimension(nsibout), intent(in) :: lonout, latout

! netcdf variables
integer :: ncid, outid
integer :: status, varid
integer :: numdims, numvars, numatts
integer :: xtype
integer, dimension(:), allocatable :: &
   dimids, dimlens, vardims
character(len=20) :: name

! data variables
real :: fval

real*8, dimension(:), allocatable :: dvalues, dvaluesout
real*8, dimension(:,:), allocatable :: dvalues2, dvalues2out
real*8, dimension(:,:,:), allocatable :: dvalues3, dvalues3out

real, dimension(:), allocatable :: fvalues, fvaluesout
real, dimension(:,:), allocatable :: fvalues2, fvalues2out
real, dimension(:,:,:), allocatable :: fvalues3

integer :: ival
integer, dimension(:), allocatable :: ivalues, ivaluesout
integer, dimension(:,:), allocatable :: ivalues2, ivalues2out

! grid variables
integer :: nlonout, nlatout
real :: dlatout, dlonout, tlatminout, tlonminout
real, dimension(:), allocatable :: longout, latgout
integer, dimension(:), allocatable :: lonrefout, latrefout

! regrid variables
integer :: jref
integer, dimension(:), allocatable :: myref
real :: distance, distsave

! misc variables
integer :: a, d, v
integer :: i, j
integer :: dlen1, dlen2, dlen3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! get array references
allocate(myref(nsibout))
myref = 0

do i=1,nsibout
   jref = 0
   distsave = 9999.
   do j=1,nsibin
      distance = abs(lonout(i) - lonin(j)) + &
                 abs(latout(i) - latin(j))
      IF (distance .lt. distsave) THEN
          jref = j
          distsave = distance
      ENDIF
    enddo
    myref(i) = jref
    !print*,'Lat/Lon In: ',lonin(jref),latin(jref)
    !print*,'Lat/Lon Out: ',lonout(i),latout(i)
enddo

!get grid information
call grid_sib_nlonlat(nsibout,lonout,latout, &
     nlonout, nlatout, dlonout, dlatout, &
     tlonminout, tlatminout)
print*,'      Num Lon/Lat: ',nlonout,nlatout
print*,'      Delta Lon/Lat: ',dlonout,dlatout
print*,'      Center LL Lon/Lat: ',tlonminout,tlatminout

allocate(longout(nlonout))
allocate(latgout(nlatout))
allocate(lonrefout(nsibout))
allocate(latrefout(nsibout))
call grid_sib_ref(nsibout, lonout, latout, &
     nlonout, nlatout, dlonout, dlatout, &
     tlonminout, tlatminout, &
     longout, latgout, lonrefout, latrefout)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! open input file
status = nf90_open(trim(filenamein),nf90_nowrite,ncid)
IF (status .ne. nf90_noerr) THEN
   print*,'Error Opening File: '
   print*,'  ',trim(filenamein)
   STOP
ENDIF

! create output file
IF (use_deflate) THEN
   status = nf90_create(trim(filenameout),nf90_hdf5,outid)
ELSE
   status = nf90_create(trim(filenameout),nf90_64bit_offset,outid)
ENDIF
IF (status .ne. nf90_noerr) THEN
   print*,'Error Creating File: '
   print*,'  ',trim(filenameout)
   STOP
ENDIF

! get dimensions and attributes
status = nf90_inquire(ncid, nDimensions=numdims, nVariables=numvars, &
          nAttributes=numatts)

! copy global attributes to output file
do a = 1, numatts
   status = nf90_inq_attname(ncid, nf90_global, a, name)
   status = nf90_copy_att(ncid, nf90_global, trim(name), &
             outid, nf90_global)
enddo

! copy dimensions over to output file
allocate(dimids(numdims))
allocate(dimlens(numdims))

do d = 1, numdims
   status = nf90_inquire_dimension(ncid, d, name=name, len=dimlens(d))
   IF ((trim(name) .eq. 'nsib') .or. (trim(name) .eq. 'landpoints')) THEN
      status = nf90_def_dim(outid, trim(name), nsibout, dimids(d))
   ELSEIF (trim(name) .eq. 'latitude') THEN
      status = nf90_def_dim(outid, trim(name), nlatout, dimids(d))
   ELSEIF (trim(name) .eq. 'longitude') THEN
      status = nf90_def_dim(outid, trim(name), nlonout, dimids(d))
   ELSE
      status = nf90_def_dim(outid, trim(name), dimlens(d), dimids(d))
   ENDIF
enddo

! define variables in output file
allocate(vardims(numdims))
do v = 1, numvars
   status = nf90_inquire_variable(ncid, v, name=name, &
            xtype=xtype, ndims=numdims, dimids=vardims)
   status = nf90_def_var(outid, trim(name), xtype, &
            vardims(1:numdims), varid)

   if (use_deflate) THEN
      status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
   endif
enddo

! stop defining variables
status = nf90_enddef(outid)

! copy variables to output file
do v = 1, numvars
   status = nf90_inquire_variable( &
            ncid, v, name=name, xtype=xtype, &
            ndims=numdims, dimids=vardims)

   dlen1 = dimlens(vardims(1))
   if (numdims .ge. 2) dlen2 = dimlens(vardims(2))
   if (numdims .ge. 3) dlen3 = dimlens(vardims(3))

   if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
      if (dlen1 .eq. nsibin) then
         allocate(dvalues(dlen1))
         allocate(dvaluesout(nsibout))
         status = nf90_get_var(ncid, v, dvalues)

         do i=1,nsibout
            dvaluesout(i) = dvalues(myref(i))
         enddo

         status = nf90_put_var(outid, v, dvaluesout)
         deallocate(dvalues,dvaluesout)
     else
         allocate(dvalues(dlen1))
         status = nf90_get_var(ncid, v, dvalues)
         status = nf90_put_var(outid, v, dvalues)
         deallocate(dvalues)
     endif

   elseif ((xtype .eq. 6) .and. (numdims .eq. 2)) then
     if (dlen1 .eq. nsibin) then
        allocate(dvalues2(dlen1,dlen2))
        allocate(dvalues2out(nsibout,dlen2))
        status = nf90_get_var(ncid, v, dvalues2)
        
        do i=1,nsibout
           dvalues2out(i,:) = dvalues2(myref(i),:)
        enddo
        status = nf90_put_var(outid, v, dvalues2out)
        deallocate(dvalues2,dvalues2out)

     elseif (dlen2 .eq. nsibin) then
        allocate(dvalues2(dlen1,dlen2))
        allocate(dvalues2out(dlen1,nsibout))
        status = nf90_get_var(ncid, v, dvalues2)
     
        do i=1,nsibout
           dvalues2out(:,i) = dvalues2(:,myref(i))
        enddo
        status = nf90_put_var(outid, v, dvalues2out)
        deallocate(dvalues2,dvalues2out)
     else
        allocate(dvalues2(dlen1,dlen2))
        status = nf90_get_var(ncid, v, dvalues2)
        status = nf90_put_var(outid, v, dvalues2)
        deallocate(dvalues2)
     endif

   elseif ((xtype .eq. 6) .and. (numdims .eq. 3)) then
      if (dlen1 .eq. nsibin) then
         print*,'Help Me, Double 3-D 1'
      elseif (dlen2 .eq. nsibin) then
         allocate(dvalues3(dlen1,dlen2,dlen3))
         allocate(dvalues3out(dlen1,nsibout,dlen3))
         status = nf90_get_var(ncid, v, dvalues3)
         
         do i=1,nsibout
            dvalues3out(:,i,:) = dvalues3(:,myref(i),:)
         enddo
         status = nf90_put_var(outid, v, dvalues3out)
         deallocate(dvalues3,dvalues3out)

      elseif (dlen3 .eq. nsibin) then
         print*,'Help Me, Double 3-D 3'
      else
         allocate(dvalues3(dlen1,dlen2,dlen3))
         status = nf90_get_var(ncid, v, dvalues3)
         status = nf90_put_var(outid, v, dvalues3)
         deallocate(dvalues3)
      endif

   elseif ((xtype .eq. 5) .and. (numdims .eq. 0)) then
      if (trim(name) .eq. 'dlat') then
         status = nf90_put_var(outid, v, dlatout)
      elseif (trim(name) .eq. 'dlon') then
         status = nf90_put_var(outid, v, dlonout)
      !elseif (trim(name) .eq. 'lllat') then
      !   status = nf90_put_var(outid, v, tlatminout)
      !elseif (trim(name) .eq. 'lllon') then
      !   status = nf90_put_var(outid, v, tlonminout)
      else
          status = nf90_get_var(ncid, v, fval)
          status = nf90_put_var(outid, v, fval)
      endif

   elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
      if (dlen1 .eq. nsibin) then

          allocate(fvalues(dlen1))
          status = nf90_get_var(ncid, v, fvalues)

          allocate(fvaluesout(nsibout))
          fvaluesout(:) = 0.

          do i=1,nsibout
             fvaluesout(i) = fvalues(myref(i))
          enddo
          status = nf90_put_var(outid, v, fvaluesout)
          deallocate(fvalues,fvaluesout)

      elseif (trim(name) .eq. 'latitude') then
          status = nf90_put_var(outid, v, latgout)
      elseif (trim(name) .eq. 'longitude') then
          status = nf90_put_var(outid, v, longout)

      else
          allocate(fvalues(dlen1))
          status = nf90_get_var(ncid, v, fvalues)
          status = nf90_put_var(outid, v, fvalues)
          deallocate(fvalues)
      endif

   elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
      if (dlen1 .eq. nsibin) then
         allocate(fvalues2(dlen1,dlen2))
         allocate(fvalues2out(nsibout,dlen2))
         status = nf90_get_var(ncid, v, fvalues2)
         
         do i=1,nsibout
            fvalues2out(i,:) = fvalues2(myref(i),:)
         enddo
         status = nf90_put_var(outid, v, fvalues2out)
         deallocate(fvalues2,fvalues2out)

      elseif (dlen2 .eq. nsibin) then
         allocate(fvalues2(dlen1,dlen2))
         allocate(fvalues2out(dlen1,nsibout))
         status = nf90_get_var(ncid, v, fvalues2)

         do i=1,nsibout
            fvalues2out(:,i) = fvalues2(:,myref(i))
         enddo
         status = nf90_put_var(outid, v, fvalues2out)
         deallocate(fvalues2,fvalues2out)

      else
         allocate(fvalues2(dlen1,dlen2))
         status = nf90_get_var(ncid, v, fvalues2)
         status = nf90_put_var(outid, v, fvalues2)
         deallocate(fvalues2)
      endif

   elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
      if (dlen1 .eq. nsibin) then
          print*,'Help Me, Float 3-D, 1'
          STOP
      elseif (dlen2 .eq. nsibin) then
          print*,'Help Me, Float 3-D, 2'
          STOP
      elseif (dlen3 .eq. nsibin) then
          print*,'Help Me, Float 3-D, 3'
      else
          allocate(fvalues3(dlen1,dlen2,dlen3))
          status = nf90_get_var(ncid, v, fvalues3)
          status = nf90_put_var(outid, v, fvalues3)
          deallocate(fvalues3)
      endif

   elseif ((xtype .eq. 4) .and. (numdims .eq. 0)) then
      if ((trim(name) .eq. 'nsib') .or. &
          (trim(name) .eq. 'subcount')) then
         status = nf90_put_var(outid, v, nsibout)
      elseif (trim(name) .eq. 'numlat') then
         status = nf90_put_var(outid, v, nlatout)
      elseif (trim(name) .eq. 'numlon') then
         status = nf90_put_var(outid, v, nlonout)
      else
         status = nf90_get_var(ncid, v, ival)
         status = nf90_put_var(outid, v, ival)
      endif      
   
   elseif ((xtype .eq. 4) .and. (numdims .eq. 1)) then
      if (dlen1 .eq. nsibin) then
         if (trim(name) .eq. 'lonindex') then
            status = nf90_put_var(outid, v, lonrefout)
         elseif (trim(name) .eq. 'latindex') then
            status = nf90_put_var(outid, v, latrefout)
         elseif (trim(name) .eq. 'sibindex') then
            allocate(ivalues(nsibout))
            do i=1,nsibout
               ivalues(i) = i
            enddo
            status = nf90_put_var(outid, v, ivalues)
            deallocate(ivalues)
         else
            allocate(ivalues(dlen1))
            allocate(ivaluesout(nsibout))
            status = nf90_get_var(ncid, v, ivalues)

            do i=1,nsibout
               ivaluesout(i) = ivalues(myref(i))
            enddo
            status = nf90_put_var(outid, v, ivaluesout)
            deallocate(ivalues,ivaluesout)
         endif

      else
         allocate(ivalues(dlen1))
         status = nf90_get_var(ncid, v, ivalues)
         status = nf90_put_var(outid, v, ivalues)
         deallocate(ivalues)
      endif

   elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
       if (dlen1 .eq. nsibin) then
           allocate(ivalues2(dlen1,dlen2))
           allocate(ivalues2out(nsibout,dlen2))
           status = nf90_get_var(ncid, v, ivalues2)

           do i=1,nsibout
              ivalues2out(i,:) = ivalues2(myref(i),:)
           enddo
           status = nf90_put_var(outid, v, ivalues2out)
           deallocate(ivalues2,ivalues2out)

       elseif (dlen2 .eq. nsibin) then
           allocate(ivalues2(dlen1,dlen2))
           allocate(ivalues2out(dlen1,nsibout))
           status = nf90_get_var(ncid, v, ivalues2)
 
           do i=1,nsibout
              ivalues2out(:,i) = ivalues2(:,myref(i))
           enddo
           status = nf90_put_var(outid, v, ivalues2out)
           deallocate(ivalues,ivalues2out)

       else
            allocate(ivalues2(dlen1,dlen2))
            status = nf90_get_var(ncid, v, ivalues2)
            status = nf90_put_var(outid, v, ivalues2)
            deallocate(ivalues,ivalues2out)
       endif

   else
      print*,'Unexpected Output Type and/or Dimensions.'
      print*,'  Var name and type: ',trim(name),xtype
      print*,'  Number of dimensions: ',numdims
      print*,'  Dimension IDs: ',vardims
   endif

   IF (status .ne. nf90_noerr) THEN
      print*,'Error Putting Variable: ',trim(name)
      STOP
   ENDIF

enddo !n=1,numvars

status = nf90_close(ncid)
status = nf90_close(outid) 


end subroutine vec2vec_down

