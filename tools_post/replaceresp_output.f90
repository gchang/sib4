!========================================
subroutine replaceresp_output(nfiles, &
    filenamesin, filenamesout, balfile)
!========================================

use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenamesin
character(len=256), dimension(nfiles), intent(in) :: filenamesout
character(len=256), intent(in) :: balfile

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
   call replaceresp_outsib(nfiles, filenamesin, filenamesout, balfile)
else
   print*,'Replace Respiration for Gridded Files Not Implemented!'
   print*,'Please create routine replaceresp_outgrid'
endif

status = nf90_close(ncid)

end subroutine replaceresp_output

!=========================================
subroutine replaceresp_outsib(nfiles, &
    filenamesin, filenamesout, ratiofile)
!=========================================
!Subroutine controlling the sequence of 
!   calls to replace original respiration.
!   with balanced respiration.
!
!kdhaynes, 2018/02

use netcdf
use sibinfo_module

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenamesin
character(len=256), dimension(nfiles), intent(in) :: filenamesout
character(len=256), intent(in) :: ratiofile

!netcdf variables
integer :: ncid, outid, status
integer :: dimid, varid

!balance ratio variables
integer :: nsibr, nlur
real, dimension(:,:), allocatable :: ratio

!file variables
integer :: numatts, numdims, numvars
integer, dimension(:), allocatable :: dimids, dimlens

character(len=20) :: name
integer :: xtype
integer, dimension(:), allocatable :: vardims

!data variables
real*8, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3
integer*1, dimension(:), allocatable :: bvalues1

character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7), dimension(:), allocatable :: cvalues7

!local variables
integer :: fref, tref
integer :: att, dim, var

!----------------------------------
!Get the ratio
status = nf90_open(trim(ratiofile),nf90_nowrite,ncid)
IF (status .NE. nf90_noerr) THEN
   print*,'Error Opening Balanced Ratio File: '
   print*,'   ',trim(ratiofile)
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

!Replace respiration with balanced respiration in files
DO fref=1,nfiles
   !...open input and output files
   status = nf90_open(trim(filenamesin(fref)),nf90_nowrite,ncid)
   IF (status .ne. nf90_noerr) THEN
      print*,'Error Opening Input File: '
      print*,'  ',trim(filenamesin(fref))
      print*,'Stopping.'
      STOP
   ENDIF

   if (use_deflate) then
       status = nf90_create(trim(filenamesout(fref)),nf90_hdf5,outid)
   else
       status = nf90_create(trim(filenamesout(fref)),nf90_64bit_offset,outid)
   endif

   IF (status .EQ. nf90_noerr) THEN
      print('(a)'),'   Creating output file: '
      print('(2a)'),'     ',trim(filenamesout(fref))
   ELSE
      print('(a)'),'Error Creating Output File: '
      print('(2a)'),'     ',trim(filenamesout(fref))
      print('(a)'),'Stopping.'
      STOP
   ENDIF
   
   !...get dimensions and attributes
   status = nf90_inquire( ncid, nDimensions=numdims, &
       nVariables=numvars, nAttributes=numatts)

   !...copy global attributes over to output file
   do att = 1, numatts
      status = nf90_inq_attname( ncid, nf90_global, att, name)
      status = nf90_copy_att( ncid, nf90_global, trim(name), &
               outid, nf90_global)
   enddo

   !...copy dimensions over to output file
   allocate(dimids(numdims))
   allocate(dimlens(numdims))
   do dim=1,numdims
      status = nf90_inquire_dimension(ncid, dim, name=name, len=dimlens(dim))
      status = nf90_def_dim(outid,trim(name),dimlens(dim),dimids(dim))
   enddo

   !...define variables in output file
   allocate(vardims(numdims))
   do var=1,numvars
      status = nf90_inquire_variable(ncid,var,name=name, &
                xtype=xtype, ndims=numdims, dimids=vardims, &
                natts=numatts)
      if (status .ne. nf90_noerr) then
          print('(a,i4)'),'Error Inquiring Variable Number ',var
          print('(2a)'),  'Error Message: ',nf90_strerror(status)
          print('(a)'),'Stopping.'
          STOP
      endif

      if (trim(name) .eq. trim(respname)) then
         status = nf90_def_var(outid,trim(respbname), xtype, &
                   vardims(1:numdims),varid)
         if (use_deflate) then
             status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
         endif
         status = nf90_put_att(outid,varid,'long_name',trim(respblong))
         status = nf90_put_att(outid,varid,'title',trim(respbtitle))
         status = nf90_put_att(outid,varid,'units',trim(respbunits))
     else
        status = nf90_def_var(outid,trim(name), xtype, &
                   vardims(1:numdims),varid)
        if (use_deflate) then
            status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
        endif
        do att = 1, numatts
           status = nf90_inq_attname(ncid, var, att, name=name)
           status = nf90_copy_att(ncid,var,trim(name),outid,varid)
        enddo
     endif
   enddo !var=1,numvars

   !...stop defining variables
   status = nf90_enddef(outid)

   !...copy variables to output file
   do var=1,numvars
      status = nf90_inquire_variable(ncid,var, &
          name=name, xtype=xtype, ndims=numdims, dimids=vardims)

      if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
          allocate(dvalues(dimlens(vardims(1))))
          status = nf90_get_var(ncid,var,dvalues)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         status = nf90_put_var(outid,var,dvalues)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
         deallocate(dvalues)

     elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
         allocate(fvalues(dimlens(vardims(1))))
         status = nf90_get_var(ncid,var,fvalues)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         status = nf90_put_var(outid,var,fvalues)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
         deallocate(fvalues)
     elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
         allocate(fvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
         status = nf90_get_var(ncid,var,fvalues2)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         status = nf90_put_var(outid,var,fvalues2)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
         deallocate(fvalues2)
     elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
         allocate(fvalues3(dimlens(vardims(1)),dimlens(vardims(2)), &
                  dimlens(vardims(3))))
         status = nf90_get_var(ncid,var,fvalues3)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         if (trim(name) .eq. trim(respname)) then
            DO tref=1,dimlens(vardims(3))
                fvalues3(:,:,tref) = fvalues3(:,:,tref) &
                                     * ratio(:,:)
            ENDDO
         endif
         status = nf90_put_var(outid,var,fvalues3)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
         deallocate(fvalues3)

     elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
         allocate(ivalues2(dimlens(vardims(1)),dimlens(vardims(2))))
         status = nf90_get_var( ncid, var, ivalues2)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         status = nf90_put_var( outid, var, ivalues2)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
         deallocate(ivalues2)

     elseif ((xtype .eq. 4) .and. (numdims .eq. 3)) then
         allocate(ivalues3(dimlens(vardims(1)),dimlens(vardims(2)), &
                  dimlens(vardims(3))))
         status = nf90_get_var( ncid, var, ivalues3)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         status = nf90_put_var( outid, var, ivalues3)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
         deallocate(ivalues3)

     elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
         if (dimlens(vardims(1)) .eq. 3) then
            allocate(cvalues3(dimlens(vardims(2))))
            status = nf90_get_var( ncid, var, cvalues3)
            if (status .ne. nf90_noerr) then
               print*,'Error Getting Variable: ',trim(name)
               print*,'Message: ',nf90_strerror(status)
               print*,'Stopping.'
               STOP
            endif

            status = nf90_put_var( outid, var, cvalues3)
            if (status .ne. nf90_noerr) then
               print*,'Error Writing Variable: ',trim(name)
               print*,'Message: ',nf90_strerror(status)
               print*,'Stopping.'
               STOP
            endif
            deallocate(cvalues3)
         elseif (dimlens(vardims(1)) .eq. 6) then
            allocate(cvalues6(dimlens(vardims(2))))
            status = nf90_get_var( ncid, var, cvalues6)
            if (status .ne. nf90_noerr) then
               print*,'Error Getting Variable: ',trim(name)
               print*,'Message: ',nf90_strerror(status)
               print*,'Stopping.'
               STOP
            endif

            status = nf90_put_var( outid, var, cvalues6)
            if (status .ne. nf90_noerr) then
               print*,'Error Writing Variable: ',trim(name)
               print*,'Message: ',nf90_strerror(status)
               print*,'Stopping.'
               STOP
            endif
            deallocate(cvalues6)
         elseif (dimlens(vardims(1)) .eq. 7) then
            allocate(cvalues7(dimlens(vardims(2))))
            status = nf90_get_var( ncid, var, cvalues7)
            if (status .ne. nf90_noerr) then
               print*,'Error Getting Variable: ',trim(name)
               print*,'Message: ',nf90_strerror(status)
               print*,'Stopping.'
               STOP
            endif

            status = nf90_put_var( outid, var, cvalues7)
            if (status .ne. nf90_noerr) then
               print*,'Error Writing Variable: ',trim(name)
               print*,'Message: ',nf90_strerror(status)
               print*,'Stopping.'
               STOP
            endif
            deallocate(cvalues7)
         else
            print*,'Unrecognized Character Dimensons.'
            print*,'   Dimensions: ', &
                   dimlens(vardims(1)),dimlens(vardims(2))
            stop
        endif

     elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
         allocate(bvalues1(dimlens(vardims(1))))
         status = nf90_get_var( ncid, var, bvalues1)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif

         status = nf90_put_var( outid, var, bvalues1)
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(name)
            print*,'Message: ',nf90_strerror(status)
            print*,'Stopping.'
            STOP
         endif
       deallocate(bvalues1)       

    else
       print*,'Unexpected Output Type and/or Dimension.'
       print*,'   Var name and type: ',name,xtype
       print*,'   Number of dimensions: ',numdims
       print*,'   Dimension IDs: ',vardims
    endif

   enddo !var=1,numvars

   !...close files
   status = nf90_close(ncid)
   status = nf90_close(outid)

   !...deallocate variables
   deallocate(dimids,dimlens)
   deallocate(vardims)

ENDDO !fref=1,nfiles

end subroutine replaceresp_outsib

