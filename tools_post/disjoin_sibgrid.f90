!---------------------------------------------
program disjoin_sibgrid
!---------------------------------------------
!Program to disjoin output files into  
!  seaparate netcdf files (over time).
!  - Breaks apart monthly files into
!     daily files
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make disjoin_sib
!
!kdhaynes, 10/17

use sibinfo_module
implicit none

!parameters
character(len=*), parameter :: &
   specfile='disjoin.txt'

!user specified variables
integer :: startmon, startyr, stopmon, stopyr, nselvar
logical :: use_other
logical, dimension(:), allocatable :: sib_diagnostic
character(len=180) :: indir, outdir, indiro, outdiro
character(len=20), dimension(:), allocatable :: varnames

!local variables
integer :: dref
character(len=45) :: junk


!------------------------------------------------------
!Open the input/user specification file
allocate(sib_diagnostic(ndiag))
open(unit=11,file=trim(specfile),form='formatted')
read(11,fj) junk
read(11,fj) junk
read(11,fji) junk, startmon
read(11,fji) junk, startyr
read(11,fji) junk, stopmon
read(11,fji) junk, stopyr
do dref=1,ndiag
   read(11,fjl) junk, sib_diagnostic(dref)
enddo
read(11,fjl) use_other
read(11,fj) junk
read(11,fd) indir
read(11,fj) junk
read(11,fd) outdir
read(11,fj) junk
read(11,fd) indiro
read(11,fj) junk
read(11,fd) outdiro
read(11,fj) junk
read(11,fi) nselvar
if (nselvar .ge. 1) THEn
   allocate(varnames(nselvar))
   do dref=1,nselvar
      read(11,fv) varnames(dref)
  enddo
else
  nselvar=1
  allocate(varnames(1))
  varnames(1) = ''
endif

close(11)

do dref=1,ndiag
   IF (sib_diagnostic(dref)) THEN
       call disjoin_parse( &
            startmon, startyr, stopmon, stopyr, &
            dref, indir, outdir, nselvar, varnames)
  ENDIF
enddo

IF (use_other) THEN
   call disjoin_parse( &
        startmon, startyr, stopmon, stopyr, &
        -1, indiro, outdiro, nselvar, varnames)
ENDIF

print(fa),''
print(fa),'Finished Disjoining Files.'
print(fa),''


end program disjoin_sibgrid
