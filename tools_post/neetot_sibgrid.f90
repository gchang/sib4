!---------------------------------------------
program neetot_sibgrid
!---------------------------------------------
!Program to calculate the annual NEE
!   contribution from PFTs and biomes.
!
!Assumes the input files are 
!   monthly SiB4 diagnostic output files.
!
!Expecting 15 PFTs to match to biomes.
!
!Note: Assumes 30-day months for now,
!  always starting Jan 1.
!
!Program is included in Makefile, so to compile:
!>make neetot
!
!kdhaynes, 2018/11

implicit none

!------------------------------------------------------
!Enter In Filenames and Variables:

integer, parameter :: nfiles=12
logical, parameter :: isglobal=.true.
logical, parameter :: print_biome=.true.
logical, parameter :: print_pft=.true.

character(len=256), dimension(nfiles) :: &
   filenamesin

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
!------------------------------------------------------

call neetot_output(nfiles, filenamesin, &
     isglobal, print_biome, print_pft)

print*,''
print*,'Finished Calculating Total NEE.'
print*,''

end program neetot_sibgrid
