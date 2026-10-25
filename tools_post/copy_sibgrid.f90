!---------------------------------------------
program copy_sibgrid
!---------------------------------------------
!Program to copy files.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make copy
!
!kdhaynes, 2018/05
!
implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1

character(len=256), dimension(nfiles) :: filenamesin
character(len=256), dimension(nfiles) :: filenamesout

filenamesin(1) = &
   '/Users/kdhaynes/sib4_hourly_199804.pft.nc'
filenamesout(1) = &
   '/Users/kdhaynes/sib4_fixed_199804.pft.nc'

!------------------------------------------------------

call copy_output(nfiles, filenamesin, filenamesout)

print*,''
print*, 'Finished Copying Files.'
print*,''

end program copy_sibgrid

