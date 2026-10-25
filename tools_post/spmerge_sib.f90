!---------------------------------------------
program spmerge_sib
!---------------------------------------------
!This program merges the SiB4 output files
! from a simulation using multiple processors.  
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile :
!>make merge
!
! kdhaynes, 10/17

use sibinfo_module
implicit none

! input variables
  integer :: start_year       ! beginning year of simulation
  integer :: start_month      ! beginning month of simulation
  integer :: end_year         ! ending year of simulation
  integer :: end_month        ! ending month of simulation
  logical :: merge_requib     ! flag to merge requib files
  logical :: merge_restart    ! flag to merge restart files
  logical, dimension(:), allocatable ::  &
       merge_diagnostic       ! flags to merge diagnostic files
  integer :: nprocs           ! number of processes in parallel run
  character(len=180) :: in_path  ! directory containing input files to be merged
  character(len=180) :: out_path ! directory to write merged output files
  integer :: nselvar          ! number of variables to merge
  character(len=20), dimension(:), allocatable :: &
        varnames
  
! misc variables
  integer :: dref
  character(len=45) :: junk

!--------------------------------------
!Open the input/user specification file
allocate(merge_diagnostic(ndiag))
open(unit=9,file=trim('merge_sib.txt'),form='formatted')
read (9,fj) junk
read (9,fj) junk
read (9,fji) junk, start_month
read (9,fji) junk, start_year
read (9,fji) junk, end_month
read (9,fji) junk, end_year
read (9,fjl) junk, merge_restart
read (9,fjl) junk, merge_requib
do dref = 1, ndiag
   read (9,fjl) junk, merge_diagnostic(dref)
enddo
read (9,fji) junk, nprocs
read (9,fj) junk
read (9,fd) in_path
read (9,fj) junk
read (9,fd) out_path

read(9,fj) junk
read(9,fi) nselvar
IF (nselvar .lt. 1) THEN
   nselvar=1
   allocate(varnames(1))
   varnames(1) = ''
ELSE
   allocate(varnames(nselvar))
   do dref=1,nselvar
      read(11,fv) varnames(dref)
   enddo
ENDIF

close (9)

call spmerge_parse( &
     start_month, start_year, &
     end_month, end_year, &
     merge_restart, merge_requib, &
     merge_diagnostic, &
     nprocs, in_path, out_path, &
     nselvar, varnames)

print(fa),''
print(fa), 'Finished Merging Files'
print(fa),''

end program spmerge_sib
