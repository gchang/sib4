!---------------------------------------------
program addresp_sibgrid
!---------------------------------------------
!Program to add the balanced respiration 
!   into the output files.
!   - Creates a new balanced respiration variable
!   - Multiplies the respiration by the
!     saved ratio calculated from balance_sib.f90.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make balaadd
!
!kdhaynes, 10/17
!

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1

character(len=256), dimension(nfiles) :: filenamesin
character(len=256), dimension(nfiles) :: filenamesout
character(len=256) :: ratiofile

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_199801.lu.qp3.nc'
filenamesout(1) = &
   '/Users/kdhaynes/Output/global/monthlyb/qsib_199801.lu.qp3.nc'
ratiofile = &
   '/Users/kdhaynes/Output/global/monthly/qbal_199801_201712.lu.qp3.nc'
!------------------------------------------------------

call addresp_output(nfiles, filenamesin, filenamesout, ratiofile)

print*,''
print*, 'Finished Adding Balanced Respiration To Files.'
print*,''

end program addresp_sibgrid

