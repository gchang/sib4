!---------------------------------------------
program pullvar_sibgrid
!---------------------------------------------
!Program to pull specific variables
!  from a netcdf file.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make pullvar
!
!kdhaynes, 10/17


implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
integer, parameter :: nvars=1

character(len=256), dimension(nfiles) :: &
   filenamesin, filenamesout
character(len=20), dimension(nvars) :: &
   varnames

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200001.lu.qp3.nc'
filenamesout(1) = &
   '/Users/kdhaynes/Output/global/monthly/qrst_200001.lu.qp3.nc'

varnames(1) = 'rstfac2'

!------------------------------------------------------

call pullvar_output(nfiles, &
     filenamesin, filenamesout, &
     nvars, varnames)

print*,''
print*,'Finished Pulling Variables.'
print*,''

end program pullvar_sibgrid

