!---------------------------------------------
program combine_sib
!---------------------------------------------
!Program to combine PFT and LU information 
!   to grid cell values.
!
!Currently all the diagnostic variables
!   to be saved are floats.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make combine
!
!kdhaynes, 10/17

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
character(len=256), dimension(nfiles) :: &
   filenamesin, filenamesout

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200001.lu.qp3.nc'

filenamesout(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200001.g.qp2.nc'

!------------------------------------------------------

call combine_output(nfiles, filenamesin, filenamesout)

print*,''
print*,'Finished Balancing Files.'
print*,''

end program combine_sib

