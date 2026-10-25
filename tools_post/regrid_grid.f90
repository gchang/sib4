!---------------------------------------------
program regrid_grid
!---------------------------------------------
!Program to regrid files.
!
!Notes :
!   - Can be used for SiB4 files or other data.
!
!   - Expects all input files to already be gridded.
!      -- If SiB4 output is not gridded, please use
!         grid_sib.f90
!
!   - Expects a gridded input file with the
!     new lats and lons.
!     --Assumes all lats/lons are grid centers.
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
!>make regrid
!
!kdhaynes, 10/17

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
character(len=256), dimension(nfiles) :: &
  filenamesin, filenamesout
character(len=256) :: regridfile

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/grid/qgrid_200001.g.qp2.nc'

filenamesout(1) = &
  '/Users/kdhaynes/Output/global/grid/qg_1x1_200001.g.qp2.nc'

regridfile = &
  '/Users/kdhaynes/Research/sib4_data/tools_post/grid_1x1.nc'

!---------------------------------------------------
call regrid_output(nfiles, filenamesin, filenamesout, regridfile)

print*,''
print*, 'Finished Re-Gridding Files.'
print*,''


end program regrid_grid
