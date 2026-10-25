!---------------------------------------------
program cropdist_sibgrid
!---------------------------------------------
!Program to globally distribute crop yield to 
!  balance crop fluxes.
!
!Uses a crop distribution map for spatial
!  distribution of the crop flux.
!
!Uses a region definition map to keep
!  distributions domestic, rather
!  than all international.
!
!The crop carbon redistribution is calculated
!  over the entire time period.
!
!Notes :
!   -This program requires netcdf.
!    The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make cropdist
!
!kdhaynes, 10/17

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=12
character(len=256), dimension(nfiles) :: filenamesin
character(len=256) :: outfile, cropfile, regionfile

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

outfile = &
   '/Users/kdhaynes/Output/global/test/qcrop_200001_200012.cro.qp3.nc'
cropfile = &
   '/Users/kdhaynes/Research/sib4_data/tools_crop/fao_cropdist_0.5deg.nc'
regionfile = &
   '/Users/kdhaynes/Research/sib4_data/tools_crop/transcom_0.5deg.nc'

!------------------------------------------------------

call cropdist_output(nfiles, &
     filenamesin, outfile, cropfile, regionfile)


print*,''
print*, 'Finished Distributing Harvested Crops.'
print*,''

end program cropdist_sibgrid
