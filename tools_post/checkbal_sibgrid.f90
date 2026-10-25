!---------------------------------------------
program checkbal_sibgrid
!---------------------------------------------
!Program that checks the balance of fluxes.
!
!Assumes the input/output files are
!  monthly SiB4 diagnostic output files.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make checkbal
!
!kdhaynes, 10/17

implicit none


!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=12
character(len=256), dimension(nfiles) :: filenamesin
character(len=256) :: respfile
character(len=256) :: cropfile

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

respfile = ''
cropfile = &
   '/Users/kdhaynes/Output/global/test/qcrop_200001_200012.cro.qp3.nc'

!------------------------------------------------------

call checkbal_output(nfiles, filenamesin, respfile, cropfile)

print*,''
print*, 'Finished Checking Carbon Balance.'
print*,''

end program checkbal_sibgrid

