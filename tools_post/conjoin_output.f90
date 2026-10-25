!========================================
subroutine conjoin_output( &
     nfiles, filenames, outfile, &
     nselvar, selvarnamesin)

use sibinfo_module
use netcdf
implicit none


!input variables
integer, intent(in) :: nfiles, nselvar
character(len=20), dimension(nselvar) :: selvarnamesin
character(len=256), dimension(nfiles), intent(in) :: filenames
character(len=256), intent(in) :: outfile

!selected variable information
character(len=20), dimension(:), allocatable :: &
   selvarnames

!netcdf variables
integer :: status, ntid, outid
integer, dimension(nfiles) :: fileids
integer :: numatts
integer, dimension(:), allocatable :: &
   dimids, dimlens, vardims

character(len=20) :: name
integer :: xtype
integer :: varid, vartid
integer :: numdims, numvars, nvarso

!data variables
integer, dimension(nfiles) :: ntimes
integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7),dimension(:), allocatable :: cvalues7
character(len=10),dimension(:), allocatable :: cvalues10
real*8, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3

!misc variables
integer :: ntimetot
integer :: yrpos, myyear, yrlen
integer :: a,d,f,v
logical :: allvars

!---------------------
! Open files and get total number of times
yrpos = index(filenames(1),'_',back=.true.)
DO f=1, nfiles
   status = nf90_open(trim(filenames(f)), nf90_nowrite, fileids(f))
   status = nf90_inq_dimid(fileids(f), 'time', ntid)
   status = nf90_inquire_dimension(fileids(f), ntid, len=ntimes(f))
ENDDO
ntimetot = SUM(ntimes)
print('(a,i7)'),'   Total Number of Times: ',ntimetot

! create output file
if (use_deflate) then
    status = nf90_create(trim(outfile),nf90_hdf5,outid)
else
    status = nf90_create( trim(outfile), nf90_64bit_offset, outid )
endif
IF (status .EQ. nf90_noerr) THEN
   print('(a)'),'   Creating output file: '
   print('(2a)'),'     ',trim(outfile)
ELSE
   print('(a)'),'Error Creating Output File: '
   print('(2a)'),'     ',trim(outfile)
   print('(a)'),'Stopping Processing.'
   RETURN
ENDIF

! get dimensions and attributes
status = nf90_inquire( fileids(1), nDimensions=numdims, &
         nVariables=numvars, nAttributes=numatts)

! copy global attributes over to output file
do a = 1, numatts
    status = nf90_inq_attname( fileids(1), nf90_global, a, name )
    status = nf90_copy_att( fileids(1), nf90_global, trim(name),  &
              outid, nf90_global )
enddo

! copy dimensions over to output file
allocate( dimids(numdims) )
allocate( dimlens(numdims) )
do d = 1, numdims
   status = nf90_inquire_dimension(fileids(1), d, name=name, len=dimlens(d) )
   if (d .eq. ntid) dimlens(d)=ntimetot
   status = nf90_def_dim( outid, trim(name), dimlens(d), dimids(d))
enddo

! define variables in conjoined output file 
if (trim(selvarnamesin(1)) .eq. '') then
   allvars=.true.
   nvarso=numvars
else
   allvars=.false.

   !check variable names
   nvarso=0
   do v = 1, nvarsreq
      status = nf90_inq_varid(fileids(1),trim(varsreq(v)),varid)
      IF (status .eq. nf90_noerr) nvarso=nvarso + 1
   enddo

   do v = 1, nselvar
      status = nf90_inq_varid(fileids(1),trim(selvarnamesin(v)),varid)
      if (status .eq. nf90_noerr) nvarso=nvarso+1
   enddo

   !save variable names
   allocate(selvarnames(nvarso))
   selvarnames(:)=''
   nvarso=0
   do v = 1, nvarsreq
      status = nf90_inq_varid(fileids(1),trim(varsreq(v)),varid)
      if (status .eq. nf90_noerr) then
          nvarso=nvarso+1
          selvarnames(nvarso) = varsreq(v)
      endif
   enddo

   do v = 1, nselvar
      status = nf90_inq_varid(fileids(1),trim(selvarnamesin(v)),varid)
      if (status .eq. nf90_noerr) then
          nvarso=nvarso+1
          selvarnames(nvarso) = selvarnamesin(v)
      endif
   enddo
endif
print('(a,i4)'),'   Number of variables saving: ',nvarso


allocate( vardims(numdims) )
do v = 1, nvarso
   if (allvars) then
       vartid = v
   else
       status = nf90_inq_varid(fileids(1),trim(selvarnames(v)),vartid)
   endif
   status = nf90_inquire_variable( fileids(1), vartid, name=name, &
        xtype=xtype, ndims=numdims, dimids=vardims, natts=numatts )
   status = nf90_def_var( outid, trim(name), xtype,  &
            vardims(1:numdims), varid )
   if (use_deflate) then
       status = nf90_def_var_deflate(outid,varid,1,1,deflev)
   endif

   do a = 1, numatts
      status = nf90_inq_attname( fileids(1), vartid, a, name=name)
      status = nf90_copy_att( fileids(1), vartid, trim(name), outid, varid )
   enddo
enddo

! fix time units
status = nf90_inq_varid(fileids(1), 'time', varid)
if (nfiles .eq. 12) then
   status = nf90_put_att(outid, varid, 'units', 'month')
elseif ((nfiles .eq. 365) .or. (nfiles .eq. 366)) then
   status = nf90_put_att(outid, varid, 'units', 'days since 0000-00-00')
else
   status = nf90_put_att(outid, varid, 'units', 'year fractions')
endif

! stop defining variables
status = nf90_enddef( outid )


! copy variables to output file
do v=1, nvarso
   if (allvars) then
       vartid = v
   else
       status = nf90_inq_varid(fileids(1),trim(selvarnames(v)),vartid)
   endif

   status = nf90_inquire_variable( fileids(1), vartid, &
         name=name, xtype=xtype,  &
         ndims=numdims, dimids=vardims )

   if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
      if (name .eq. 'time') then
         if ((nfiles .eq. 12) .or. &
             (nfiles .eq. 365) .or. (nfiles .eq. 366)) then
            allocate(dvalues(nfiles))
            do f=1,nfiles
               dvalues(f) = f-0.5
            enddo
            status = nf90_put_var( outid, v, dvalues)
            deallocate(dvalues)

         else
            allocate(dvalues(dimlens(vardims(1))))
            d = 1
            do f=1,nfiles
               status = nf90_get_var( fileids(f), vartid, dvalues(d:d+ntimes(f)-1))
               read(filenames(f)(yrpos+1:yrpos+4),*) myyear
               if (mod(myyear,4) .eq. 0) then
                   yrlen=366
               else
                   yrlen=365
               endif
               dvalues(d:d+ntimes(f)-1) = dvalues(d:d+ntimes(f)-1)/dble(yrlen) + myyear

               d = d + ntimes(f)
            enddo

            status = nf90_put_var( outid, v, dvalues )
            deallocate(dvalues)
         endif !nfiles
      else
         print*,'Unexpected Double Array.'
         print*,'Stopping.'
         STOP
      endif !time

    elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
       allocate(fvalues(dimlens(vardims(1))))
       if (vardims(1) .eq. ntid) then
           d = 1
           do f=1,nfiles
              status = nf90_get_var( fileids(f), vartid, fvalues(d:d+ntimes(f)-1))
              d = d + ntimes(f)
           enddo
        else
           status = nf90_get_var( fileids(1), vartid, fvalues)
        endif
       status = nf90_put_var( outid, v, fvalues)
       deallocate(fvalues)

    elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
       allocate(fvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
       if (vardims(2) .eq. ntid) then
           d = 1
           do f=1,nfiles
              status = nf90_get_var(fileids(f), vartid, fvalues2(:,d:d+ntimes(f)-1))
              d = d + ntimes(f)
           enddo
       else
          status = nf90_get_var(fileids(1), vartid, fvalues2)
       endif
       status = nf90_put_var( outid, v, fvalues2)
       deallocate(fvalues2)

   elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
      allocate(fvalues3(dimlens(vardims(1)), &
               dimlens(vardims(2)),dimlens(vardims(3))))
      if (vardims(3) .eq. ntid) then
         d = 1
         do f=1,nfiles
            status = nf90_get_var(fileids(f), vartid, fvalues3(:,:,d:d+ntimes(f)-1))
            d = d+ntimes(f)
         enddo
      else
         status = nf90_get_var(fileids(1), vartid, fvalues3)
      endif
      status = nf90_put_var(outid,v,fvalues3)
      deallocate(fvalues3)

    elseif ((xtype .eq. 5) .and. (numdims .eq. 4)) then
      allocate(fvalues4(dimlens(vardims(1)),dimlens(vardims(2)), &
               dimlens(vardims(3)),dimlens(vardims(4))))
      if (vardims(4) .eq. ntid) then
         d = 1
         do f=1,nfiles
            status = nf90_get_var(fileids(f), vartid, fvalues4(:,:,:,d:d+ntimes(f)-1))
            d = d+ntimes(f)
         enddo
      else
         status = nf90_get_var(fileids(1), vartid, fvalues4)
      endif
      status = nf90_put_var(outid,v,fvalues4)
      deallocate(fvalues4)

    elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
       allocate(ivalues2(dimlens(vardims(1)),dimlens(vardims(2))))
       if (vardims(2) .eq. ntid) then
           d = 1
           do f=1,nfiles
              status = nf90_get_var(fileids(f), vartid, ivalues2(:,d:d+ntimes(f)-1))
              d = d + ntimes(f)
           enddo
       else
          status = nf90_get_var(fileids(1), vartid, ivalues2)
       endif
       status = nf90_put_var( outid, v, ivalues2)
       deallocate(ivalues2)

    elseif ((xtype .eq. 4) .and. (numdims .eq. 3)) then
       allocate(ivalues3(dimlens(vardims(1)),dimlens(vardims(2)), &
                dimlens(vardims(3))))
       if (vardims(3) .eq. ntid) then
          d = 1
          do f=1,nfiles
             status = nf90_get_var(fileids(f), vartid, ivalues3(:,:,d:d+ntimes(f)-1))
             d = d + ntimes(f)
          enddo
       else
          status = nf90_get_var(fileids(1), vartid, ivalues3)
       endif
       status = nf90_put_var( outid, v, ivalues3)
       deallocate(ivalues3)

    elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
       if (dimlens(vardims(1)) .eq. 3) then
          allocate(cvalues3(dimlens(vardims(2))))
          status = nf90_get_var( fileids(1), vartid, cvalues3)
          status = nf90_put_var( outid, v, cvalues3)
          deallocate(cvalues3)
       elseif (dimlens(vardims(1)) .eq. 6) then
          allocate(cvalues6(dimlens(vardims(2))))
          status = nf90_get_var( fileids(1), vartid, cvalues6)
          status = nf90_put_var( outid, v, cvalues6)
          deallocate(cvalues6)
       elseif (dimlens(vardims(1)) .eq. 7) then
           allocate(cvalues7(dimlens(vardims(2))))
           status = nf90_get_var( fileids(1), vartid, cvalues7)
           status = nf90_put_var( outid, v, cvalues7)
           deallocate(cvalues7)
       elseif (dimlens(vardims(1)) .eq. 10) then
           allocate(cvalues10(dimlens(vardims(2))))
           if (vardims(2) .eq. ntid) then
              d = 1
              do  f=1,nfiles
                  status = nf90_get_var(fileids(f), vartid, cvalues10(d:d+ntimes(f)-1))
                  d = d + ntimes(f)
              enddo
           else
              status = nf90_get_var( fileids(1), vartid, cvalues10 )
           endif
           status = nf90_put_var( outid, v, cvalues10)
           deallocate(cvalues10)
       else
           print*,'Unrecognized Character Dimensons.'
           stop
       endif

    elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
       allocate(bvalues1(dimlens(vardims(1))))
       status = nf90_get_var(fileids(1), vartid, bvalues1)
       status = nf90_put_var( outid, v, bvalues1)
       deallocate(bvalues1)       
    else
       print*,'Unexpected Output Type and/or Dimension.'
       print*,'   Var name and type: ',name,xtype
       print*,'   Number of dimensions: ',numdims
       print*,'   Dimension IDs: ',vardims
    endif

enddo !v=1,numvars

! close all files
DO f=1,nfiles
   status = nf90_close(fileids(f))
ENDDO
status = nf90_close(outid)

deallocate( dimids )
deallocate( dimlens )
deallocate( vardims )

end subroutine conjoin_output




