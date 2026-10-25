!---------------------------------------------
program balance_sibgrid
!---------------------------------------------
!Program to balance the non-crop fluxes, 
!   setting the respiration to the assimilation.
!   -Respiration is scaled as necessary, per PFT
!
!Two options for output:
!  1) Write ratio to a file
!  2) Add the balanced respiration to the file
!
!Assumes monthly files for input.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make balance
!
!kdhaynes, 10/17
!

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=12
logical, parameter :: write_ratio=.true.
logical, parameter :: write_bresp=.false.

character(len=256), dimension(nfiles) :: filenamesin
character(len=256) :: ratiofile
filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200001.lu.qp3.nc'
filenamesin(2) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200002.lu.qp3.nc'
filenamesin(3) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200003.lu.qp3.nc'
filenamesin(4) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200004.lu.qp3.nc'
filenamesin(5) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200005.lu.qp3.nc'
filenamesin(6) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200006.lu.qp3.nc'
filenamesin(7) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200007.lu.qp3.nc'
filenamesin(8) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200008.lu.qp3.nc'
filenamesin(9) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200009.lu.qp3.nc'
filenamesin(10) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200010.lu.qp3.nc'
filenamesin(11) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200011.lu.qp3.nc'
filenamesin(12) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200012.lu.qp3.nc'

ratiofile = &
   '/Users/kdhaynes/Output/global/monthly/qratio_2000.lu.qp3.nc'
!------------------------------------------------------

call balance_output(nfiles, filenamesin, ratiofile, &
     write_ratio, write_bresp)

print*,''
print*, 'Finished Balancing Files.'
print*,''

end program balance_sibgrid

