!---------------------------------------------
program meanclim_sib
!---------------------------------------------
!Program to create a "climatological mean"
!   SiB4 output over a year.
!
!Requires at least two years to be combined.
!Assumes a January start.
!
!Creates new files containing the mean values for
!  a single year from the specified years.
!  - New filname has last two digits
!     of the starting and stopping months.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make meanclim
!
!kdhaynes, 10/17

use sibinfo_module
implicit none

!parameters
character(len=*), parameter :: &
   specfile='meanclim_sib.txt'

!user specified variables
integer :: startmon, startyr, stopmon, stopyr, nselvar
logical, dimension(:), allocatable :: mean_diagnostic
character(len=180) :: indir, outdir
character(len=20), dimension(:), allocatable :: varnames

!local variables
integer :: dref
character(len=45) :: junk


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Open the input/user specification file
allocate(mean_diagnostic(ndiag))
open(unit=11,file=trim(specfile),form='formatted')
read(11,fj) junk
read(11,fj) junk
read(11,fji) junk, startmon
read(11,fji) junk, startyr
read(11,fji) junk, stopmon
read(11,fji) junk, stopyr
do dref=1,ndiag
   read(11,fjl) junk, mean_diagnostic(dref)
enddo
read(11,fj) junk
read(11,fd) indir
read(11,fj) junk
read(11,fd) outdir
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
   IF (mean_diagnostic(dref)) THEN
       call meanclim_parse( trim(siblabel(dref)), &
            startmon, startyr, stopmon, stopyr, &
            indir, outdir, trim(sibprefix(dref)), &
            trim(sibprefix(dref)), &
            trim(sibsuffix(dref)), nselvar, varnames)
  ENDIF
enddo

print(fa),''
print(fa),'Finished Creating Climatological Files.'
print(fa),''


end program meanclim_sib
