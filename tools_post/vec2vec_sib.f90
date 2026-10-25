!---------------------------------------------
program vec2vec_sib
!---------------------------------------------
!Program to regrid vector files,
!   moving from one vector list to another.
!
!Notes :
!   - Can be used for SiB4 files.
!
!   - Expects all input files to be in a vector.
!
!   - Expects a vector input file with the
!     new lats and lons.
!     --Assumes all lats/lons are centers.
!
!   - Creates new files.
!     --For SiB4 diagnostic output, 
!       filename prefixes can be specified
!       in the input path
!
!   - This program requires netcdf.
!      -- The netcdf library/include directories  
!         are specified in the Makefile.
!
!To compile:
!>make vec2vec
!
!kdhaynes, 2018/05

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
character(len=256), dimension(nfiles) :: &
  filenamesin, filenamesout
character(len=256) :: regridfile

filenamesin(1) = &
   '/Users/kdhaynes/Research/sib3_driver/1x1/sib_param_1999.nc'

filenamesout(1) = &
  '/Users/kdhaynes/Research/sib3_driver/0.5x0.5/sib_param_1999.nc'

regridfile = &
  '/Users/kdhaynes/Research/sib4_driver/global/sib_vs_0.5deg.nc'

!---------------------------------------------------
call vec2vec_output(nfiles, filenamesin, filenamesout, regridfile)

print*,''
print*, 'Finished Creating New Vector Files.'
print*,''


end program vec2vec_sib
