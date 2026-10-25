!---------------------------------------------
program conjoin_sib
!---------------------------------------------
!Program to conjoin output files into a 
!  single netcdf file (conjoining over time).
!  - Combines monthly files into either
!     annual or multi-year files
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make conjoin
!
!kdhaynes, 10/17

implicit none

!------------------------------------------------------
!Enter In Filenames and Variables:

integer, parameter :: nfiles=12
integer, parameter :: nvars=1   !if no variables are wanted, 
                                !set nvars=1 and varnames=''

character(len=20), dimension(nvars) :: varnames
character(len=256), dimension(nfiles) :: &
   filenamesin
character(len=256) :: filenameout

varnames(1) = ''

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

filenameout = &
   '/Users/kdhaynes/Output/global/monthly/qtst_2000.lu.qp3.nc'
!------------------------------------------------------

call conjoin_output(nfiles, filenamesin, filenameout, &
     nvars, varnames)

print*,''
print*,'Finished Conjoining Files.'
print*,''

end program conjoin_sib

