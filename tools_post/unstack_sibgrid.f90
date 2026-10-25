!---------------------------------------------
program unstack_sibgrid
!---------------------------------------------
!Program to unstack PFTs and to write output
!  into a COARDS-compliant netcdf file.
!
!Notes :
!   - Can be used for SiB4 files or other data.
!
!   - Expects all input files to already be gridded.
!      -- If SiB4 output is not gridded, please use
!         grid_sib.f90
!
!   - This program requires netcdf.
!      -- The netcdf library/include directories  
!         are specified in the Makefile.
!
!To compile:
!>make unstack
!
!kdhaynes, 10/17

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
integer, parameter :: nvars=1
logical, parameter :: area_flag=.true.
logical, parameter :: add_bresp=.true.
logical, parameter :: add_crop=.true.

character(len=256), dimension(nfiles) :: &
   filenamesin, filenamesout
character(len=256), dimension(nfiles) :: &
   filebresp, filecresp
character(len=20), dimension(nvars) :: &
   varnames

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/grid/qsib_200001.lu.nc'
filenamesout(1) = &
   '/Users/kdhaynes/Output/global/test/qsib_200001.pft.nc'

filebresp = '/Users/kdhaynes/Output/global/grid/qbal_200001_200012.lu.nc'
filecresp = '/Users/kdhaynes/Output/global/grid/qcro_200001_200012.lu.nc'

varnames(1) = ''

!------------------------------------------------------

IF (.not. add_bresp) filebresp=''
IF (.not. add_crop) filecresp=''

call unstack_output(nfiles,  &
     filenamesin, filebresp, filecresp, &
     filenamesout, nvars, varnames, &
     area_flag)

print*,''
print*,'Finished Unstacking Files.'
print*,''

end program unstack_sibgrid
