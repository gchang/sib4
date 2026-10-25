subroutine copy_output(nfiles, &
     filenamesin, filenamesout)

use netcdf
use sibinfo_module, only: use_deflate
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    filenamesin, filenamesout

!netcdf variables
integer :: ncid, ncido, status

!misc variables
integer :: fref

!----------
!Copy files
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

   call zcopyf90(ncid, ncido)
  
  status = nf90_close(ncid)
  status = nf90_close(ncido)

ENDDO

end subroutine copy_output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NOTES: IF YOU WANT TO CHANGE SOMETHING WHILE
!COPYING, EDIT THE ROUTINE BELOW
!========================================================
subroutine zcopyf90b( ncid, ncido)
!========================================================
! Subroutine to copy file from ncid to ncido.
!
! kdhaynes, 05/18

use netcdf
use sibinfo_module, only: &
    use_deflate, deflev
implicit none

! input variables
integer, intent(in) :: &
    ncid, &  ! input file id
    ncido    ! output file id

! netcdf variables
integer :: status                              ! netcdf function return value
integer :: numvars                             ! # variables in input files
integer :: numatts                             ! # attributes in input files
character(len=20) :: name                      ! variable name
integer, dimension(:), allocatable :: dimids   ! dimension id#s
integer, dimension(:), allocatable :: dimlens  ! dimension values
integer, dimension(:), allocatable :: vardims  ! variable dimensions
integer :: xtype                               ! variable data type
integer :: numdims                             ! # dimensions in input files
integer :: varid                               ! variable id#

! local variables
integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7),  dimension(:), allocatable :: cvalues7 
character(len=10), dimension(:), allocatable :: cvalues10
double precision, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues1
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4
integer, dimension(:), allocatable :: ivalues1
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3

! indices
integer :: a, d, v

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! get number of dims, vars, and atts
status = nf90_inquire(ncid, nDimensions=numdims, &
          nVariables=numvars, nAttributes=numatts)

! copy global dimensions to output file
allocate(dimids(numdims))
allocate(dimlens(numdims))
do d=1,numdims
   status = nf90_inquire_dimension(ncid, d, name=name, len=dimlens(d))
   status = nf90_def_dim(ncido, trim(name), dimlens(d), dimids(d))
enddo

! copy global attributes to output file
do a=1, numatts
   status = nf90_inq_attname(ncid, nf90_global, a, name)
   status = nf90_copy_att(ncid, nf90_global, trim(name), &
            ncido, nf90_global)
enddo

! define variables in output file
allocate(vardims(numdims))
do v=1,numvars
   status = nf90_inquire_variable(ncid,v,name=name, &
            xtype=xtype, ndims=numdims, dimids=vardims, &
            natts=numatts)
   if (trim(name) .eq. 'time') then
       vardims(1) = 5
   endif
   if (vardims(4) .eq. 1) then
       vardims(4) = 5
   endif
   status = nf90_def_var(ncido, trim(name), xtype, &
            vardims(1:numdims), varid)
   if (use_deflate) then
      status = nf90_def_var_deflate(ncido, varid, 1, 1, deflev)
   endif

   do a=1,numatts
      status = nf90_inq_attname(ncid,v,a,name=name)
      status = nf90_copy_att(ncid,v,trim(name),ncido,varid)
   enddo !a=1,numatts
enddo !v=1,numvars

! stop defining variables
status = nf90_enddef(ncido)

! copy variables to output file
do v=1,numvars
   status = nf90_inquire_variable(ncid,v, &
            name=name, xtype=xtype, &
            ndims=numdims, dimids=vardims)

   if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
      allocate(dvalues(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,dvalues)
      status = nf90_put_var(ncido,v,dvalues)
      deallocate(dvalues)

   elseif((xtype .eq. 5) .and. (numdims .eq. 1)) then
      allocate(fvalues1(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,fvalues1)
      status = nf90_put_var(ncido,v,fvalues1)
      deallocate(fvalues1)

   elseif((xtype .eq. 5) .and. (numdims .eq. 2)) then
      allocate(fvalues2(dimlens(vardims(1)), &
                        dimlens(vardims(2))))
      status = nf90_get_var(ncid,v,fvalues2)
      status = nf90_put_var(ncido,v,fvalues2)
      deallocate(fvalues2)

   elseif((xtype .eq. 5) .and. (numdims .eq. 3)) then
      allocate(fvalues3(dimlens(vardims(1)), &
                        dimlens(vardims(2)), &
                        dimlens(vardims(3))))
      status = nf90_get_var(ncid,v,fvalues3)
      status = nf90_put_var(ncido,v,fvalues3)
      deallocate(fvalues3)

   elseif((xtype .eq. 5) .and. (numdims .eq. 4)) then
      allocate(fvalues4(dimlens(vardims(1)), &
                        dimlens(vardims(2)), &
                        dimlens(vardims(3)), &
                        dimlens(vardims(4))))
      status = nf90_get_var(ncid,v,fvalues4)
      status = nf90_put_var(ncido,v,fvalues4)
      deallocate(fvalues4)

   elseif((xtype .eq. 4) .and. (numdims .eq. 1)) then
      allocate(ivalues1(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,ivalues1)
      status = nf90_put_var(ncido,v,ivalues1)
      deallocate(ivalues1)

   elseif((xtype .eq. 4) .and. (numdims .eq. 2)) then
      allocate(ivalues2(dimlens(vardims(1)), &
                        dimlens(vardims(2))))
      status = nf90_get_var(ncid,v,ivalues2)
      status = nf90_put_var(ncido,v,ivalues2)
      deallocate(ivalues2)

   elseif((xtype .eq. 4) .and. (numdims .eq. 3)) then
      allocate(ivalues3(dimlens(vardims(1)), &
                        dimlens(vardims(2)), &
                        dimlens(vardims(3))))
      status = nf90_get_var(ncid,v,ivalues3)
      status = nf90_put_var(ncido,v,ivalues3)
      deallocate(ivalues3)

   elseif((xtype .eq. 2) .and. (numdims .eq. 2)) then
      if (dimlens(vardims(1)) .eq. 3) then
          allocate(cvalues3(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues3)
          status = nf90_put_var(ncido,v,cvalues3)
          deallocate(cvalues3)

      elseif (dimlens(vardims(1)) .eq. 6) then
          allocate(cvalues6(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues6)
          status = nf90_put_var(ncido,v,cvalues6)
          deallocate(cvalues6)

      elseif (dimlens(vardims(1)) .eq. 7) then
          allocate(cvalues7(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues7)
          status = nf90_put_var(ncido,v,cvalues7)
          deallocate(cvalues7)

      elseif (dimlens(vardims(1)) .eq. 10) then
          allocate(cvalues10(dimlens(vardims(2))))
          status = nf90_get_var(ncid,v,cvalues10)
          status = nf90_put_var(ncido,v,cvalues10)
          deallocate(cvalues10)

          else
              print*,'OutputMerge: Unexpected Character Array.'
              print*,'  Var name and type: ',name,xtype
              print*, '  Number of dimensions: ',numdims
              print*, '  Dimension IDs: ',vardims
              stop
          endif

   elseif((xtype .eq. 1) .and. (numdims .eq. 1)) then
      allocate(bvalues1(dimlens(vardims(1))))
      status = nf90_get_var(ncid,v,bvalues1)
      status = nf90_put_var(ncido,v,bvalues1)
      deallocate(bvalues1)

   else
      print*,'Copy Error: Unexpected Output Type and/or Dimension.'
      print*,'  Var name and type: ',name,xtype
      print*,'  Number of dimensions: ',numdims
      print*,'  Dimension IDs: ',vardims
      stop
   endif

   if (status .ne. nf90_noerr) then
      print*,'Copy Error: Problem Writing Variable.'
      print*,'   Var name and type: ',name,xtype
      print*,'   Number of dimensions: ',numdims
      print*,'   Dimension IDs: ',vardims
    endif

enddo !v=1,numvars

end subroutine zcopyf90b
