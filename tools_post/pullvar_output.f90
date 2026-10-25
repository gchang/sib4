!==================================================
subroutine pullvar_output( &
     nfiles, infiles, outfiles, &
     nselvars, varnamesin)
!==================================================

use sibinfo_module
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
     infiles, outfiles

integer, intent(in) :: nselvars
character(len=20), dimension(nselvars), intent(in) :: varnamesin

!selected variable information
character(len=20), dimension(:), allocatable :: selvarnames

!netcdf variables
integer :: status, inid, outid
integer :: numatts
integer, dimension(:), allocatable :: &
   dimids, dimlens, vardims

character(len=20) :: name
integer :: xtype
integer :: varid, vartid
integer :: numdims, numvars, nvarso

!data variables
integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7),dimension(:), allocatable :: cvalues7
real*8, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3

!misc variables
integer :: att, dim
integer :: f, v


!--------------------------------------------------  
DO f=1,nfiles
   ! Open input file
   status = nf90_open(trim(infiles(f)), nf90_nowrite, inid)
   IF (status .NE. nf90_noerr) THEN
       print*,'    Error Opening Input File: '
       print*,'     ',trim(infiles(f))
       print*,'    Stopping.'
       STOP
   ENDIF

   ! Open output file
   if (use_deflate) then
      status = nf90_create(trim(outfiles(f)),nf90_netcdf4,outid)
   else
      status = nf90_create(trim(outfiles(f)), &
               nf90_64bit_offset, outid)
   endif
   IF (status .NE. nf90_noerr) THEN
       print*,'    Error Opening Output File: '
       print*,'     ',trim(outfiles(f))
       print*,'    Stopping.'
       STOP
   ENDIF

   ! Get dimensions and attributes
   status = nf90_inquire(inid, nDimensions=numdims, &
            nVariables=numvars, nAttributes=numatts)

   ! Copy global attributes to output file
   DO att = 1, numatts
      status = nf90_inq_attname( inid, nf90_global, att, name)
      status = nf90_copy_att( inid, nf90_global, trim(name), &
               outid, nf90_global)
   ENDDO

   ! Copy dimensions to output file
   allocate( dimids(numdims) )
   allocate( dimlens(numdims))
   DO dim = 1, numdims
      status = nf90_inquire_dimension(inid, &
               dim, name=name, len=dimlens(dim))
      status = nf90_def_dim(  outid, trim(name), dimlens(dim), dimids(dim))
   ENDDO

   !check variable names
   nvarso = 0
   do v = 1, nvarsreq
      status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
      IF (status .eq. nf90_noerr) nvarso = nvarso + 1
   enddo

   do v = 1, nselvars
      status = nf90_inq_varid(inid,trim(varnamesin(v)),varid)
      IF (status .EQ. nf90_noerr) nvarso = nvarso + 1
   enddo

   !save variable names
   allocate(selvarnames(nvarso))
   selvarnames(:) = ''
   nvarso = 0
   do v = 1, nvarsreq
      status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
      IF (status .eq. nf90_noerr) THEN
          nvarso = nvarso + 1
          selvarnames(nvarso) = varsreq(v)
      ENDIF
   enddo

   do v = 1, nselvars
      status = nf90_inq_varid(inid,trim(varnamesin(v)),varid)
      IF (status .eq. nf90_noerr) THEN
          nvarso = nvarso + 1
          selvarnames(nvarso) = varnamesin(v)
      ENDIF
   enddo

   allocate(vardims(numdims))
   do v = 1, nvarso
      status = nf90_inq_varid(inid,trim(selvarnames(v)),vartid)
      status = nf90_inquire_variable(inid,vartid,name=name, &
               xtype=xtype, ndims=numdims, dimids=vardims, natts=numatts)
      status = nf90_def_var(outid,trim(name),xtype, &
               vardims(1:numdims), varid)
      if (use_deflate) then
          status = nf90_def_var_deflate(outid,varid,1,1,deflev)
      endif

      do att=1,numatts
         status = nf90_inq_attname(inid,vartid,att,name=name)
         status = nf90_copy_att(inid,vartid,trim(name),outid,varid)
      enddo
   enddo

   ! stop defining variables
   status = nf90_enddef(outid)

   ! copy variables to output file
   do v = 1, nvarso
      status = nf90_inq_varid(inid,trim(selvarnames(v)),vartid)
      status = nf90_inquire_variable(inid, vartid, &
         name=name, xtype=xtype, &
         ndims=numdims, dimids=vardims)

      if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
         allocate(dvalues(dimlens(vardims(1))))
         status = nf90_get_var(inid,vartid,dvalues)
         status = nf90_put_var(outid, v, dvalues)
         deallocate(dvalues)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
         allocate(fvalues(dimlens(vardims(1))))
         status = nf90_get_var(inid,vartid,fvalues)
         status = nf90_put_var(outid, v, fvalues)
         deallocate(fvalues)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
         allocate(fvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
         status = nf90_get_var(inid,vartid,fvalues2)
         status = nf90_put_var(outid,v,fvalues2)
         deallocate(fvalues2)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 4)) then
          allocate(fvalues4( &
                   dimlens(vardims(1)),dimlens(vardims(2)), &
                   dimlens(vardims(3)),dimlens(vardims(4))))
          status = nf90_get_var(inid,vartid,fvalues4)
          status = nf90_put_var(outid,v,fvalues4)
          deallocate(fvalues4)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
          allocate(fvalues3(dimlens(vardims(1)), &
                   dimlens(vardims(2)),dimlens(vardims(3))))
          status = nf90_get_var(inid,vartid,fvalues3)
          status = nf90_put_var(outid,v,fvalues3)
          deallocate(fvalues3)

      elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
          allocate(ivalues2(dimlens(vardims(1)),dimlens(vardims(2))))
          status = nf90_get_var(inid,vartid,ivalues2)
          status = nf90_put_var(outid,v,ivalues2)
          deallocate(ivalues2)

      elseif ((xtype .eq. 4) .and. (numdims .eq. 3)) then
          allocate(ivalues3(dimlens(vardims(1)), &
                   dimlens(vardims(2)),dimlens(vardims(3))))
          status = nf90_get_var(inid,vartid,ivalues3)
          status = nf90_put_var(outid,v,ivalues3)
          deallocate(ivalues3)

      elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
          if (dimlens(vardims(1)) .eq. 3) then
             allocate(cvalues3(dimlens(vardims(2))))
             status = nf90_get_var( inid, vartid, cvalues3)
             status = nf90_put_var( outid, v, cvalues3)
             deallocate(cvalues3)
          elseif (dimlens(vardims(1)) .eq. 6) then
             allocate(cvalues6(dimlens(vardims(2))))
             status = nf90_get_var( inid, vartid, cvalues6)
             status = nf90_put_var( outid, v, cvalues6)
             deallocate(cvalues6)
          elseif (dimlens(vardims(1)) .eq. 7) then
              allocate(cvalues7(dimlens(vardims(2))))
              status = nf90_get_var( inid, vartid, cvalues7)
              status = nf90_put_var( outid, v, cvalues7)
              deallocate(cvalues7)
          else
              print*,'Unrecognized Character Dimensons.'
              stop
          endif

      elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
          allocate(bvalues1(dimlens(vardims(1))))
          status = nf90_get_var( inid, vartid, bvalues1)
          status = nf90_put_var( outid, v, bvalues1)
          deallocate(bvalues1)       

      else
         print*,'Unexpected Output Type and/or Dimension.'
         print*,'   Var name and type: ',name,xtype
         print*,'   Number of dimensions: ',numdims
         print*,'   Dimension IDs: ',vardims
      endif
   enddo


   ! close the files
   status = nf90_close(inid)
   status = nf90_close(outid)

   deallocate(dimids,dimlens)
   deallocate(selvarnames,vardims)
ENDDO !f=1,nfiles


end subroutine pullvar_output

