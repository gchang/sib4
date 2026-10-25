!---------------------------------------------
program grid_sib
!---------------------------------------------
!Program to put SiB4 files onto a grid,
!   rather than a vector of points.
!
!INPUT:
!The number of files to process, as well as the
!   input file and output file names.
!   -These are specified below.
!
!   -NOTE: All files MUST have the same format
!      (i.e. same dimensions, variables)
!
!   -To process various SiB4 diagnostic outputs,
!    please use sib4post:
!      -- Set the options in grid_sib.txt
!      -- Make sib4post: >make sib4post
!      -- Run: >sib4post grid
!
!OUTPUT:
!  Creates a new file containing the gridded output.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make grid
!
!kdhaynes, 07/17

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
character(len=256), dimension(nfiles) :: &
  filenamesin, filenamesout

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200001.lu.qp3.nc'

filenamesout(1) = &
  '/Users/kdhaynes/Output/global/grid/qg_200001.lu.qp3.nc'

!---------------------------------------------------
call grid_output(nfiles, filenamesin, filenamesout)

print*,''
print*, 'Finished Gridding Files.'
print*,''


end program grid_sib


