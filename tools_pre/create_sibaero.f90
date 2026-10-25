!Program to create the parameter file holding
! the aerodynamic constants
!
!The original table is saved at:
!   /archive/datasets/MapperData/AeroTab.nc
!
!The original table has values for 
!   LAI vs FVCOVER.
!
!The original SiB3 biomes:
!1=Broadleaf Evergreen Trees
!2=Broadleaf Deciduous Trees
!3=Mixed Forest
!4=Needleleaf Evergreen Trees
!5=Needleleaf Deciduous Trees
!6=C4 Short Vegetation
!7=C4 Short Vegetation (Maize optical properties)
!8=C4 Short Vegetation (Maize optical properties)
!9=Short Broadleaf Shrubs
!10=Tundra
!11=Desert
!12=C3 Agriculture and Grass
!13=Ice
!
!The new SiB4 PFTs:
!  PFT       Biome   Description
!1=dbg       11      Desert and Bare Ground
!2=enf       4       Evergreen Needleleaf Forest
!4=dnf       5       Deciduous Needleleaf Forest
!5=ebf       1       Evergreen Broadleaf Forest
!8=dbf       2       Deciduous Broadleaf Forest
!11=shb      9       Shrubs (Non-Arctic)
!12=sha      10      Tundra Shrubs
!13=c3a      10      Tundra Grassland (C3)
!14=c3g      12      C3 Grassland (non-Tundra)
!15=c4g      12      C4 Grassland
!17=c3c      12      Generic C3 Cropland
!18=c4c      12      Generic C4 Cropland
!20=mze      12      Maize
!22=soy      12      Soybean
!24=wwt      12      Winter Wheat
!
!To compile, use:
!>gfortran create_sibaero.f90 -L/usr/local/netcdf3-gcc/lib -lnetcdf -I/usr/local/netcdf3-gcc/include

!kdhaynes, 10/17

program create_sibaero

use netcdf
implicit none

integer, parameter :: npft=15, charlen=3
character*100, parameter :: filein='/Users/kdhaynes/Research/sib4_data/pre/AeroTab.nc'
character*100, parameter :: fileout='/Users/kdhaynes/Research/sib4_corral/input/params/sib_aero.nc'

!Constants required
character*3 :: pftname(npft)
integer :: pftnum(npft)
integer :: numvartopft(npft)
integer :: grid, numvar
real, dimension(:), allocatable :: laigrid, fvcovergrid
real, dimension(:,:,:), allocatable :: aero_zo, aero_zp, aero_rbc, aero_rdc

!New constants
integer :: nnumvar
real, dimension(:,:,:), allocatable :: naero_zo, naero_zp, naero_rbc, naero_rdc

!Variables to write out netcdf data
integer :: ncid
integer :: dimid, varid
integer :: nnumvarid, ngridid, ncharid
integer :: charpnid
integer :: laiid, vcovid, zoid, zpid, rbcid, rdcid

!Misc variables
integer :: i

data pftname/'dbg','enf','dnf','ebf','dbf', &
         'shb','sha','c3a','c3g','c4g', &
         'c3c','c4c','mze','soy','wwt'/

data pftnum/1,2,4,5,8,  &
          11,12,13,14,15, &
          17,18,20,22,24/

data numvartopft/11,4,5,1,2,    & !forests
                 9,10,10,12,12, & !shrubs and grasslands
                 12,12,12,12,12/  !crops

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Read in data
call check ( nf90_open( trim(filein), nf90_nowrite, ncid ) )
print*,'Opening file: ',trim(filein)

call check ( nf90_inq_dimid( ncid, 'grid', dimid ) )
call check ( nf90_inquire_dimension( ncid, dimid, len=grid ) )
call check ( nf90_inq_dimid( ncid, 'numvar', dimid ) )
call check ( nf90_inquire_dimension( ncid, dimid, len=numvar ) )
print('(a,2i6)'),'   Dimensions (ngrid,nbiome): ',grid,numvar

nnumvar = npft
allocate(laigrid(grid),fvcovergrid(grid))
allocate(aero_zo(grid, grid, numvar),naero_zo(nnumvar, grid, grid))
allocate(aero_zp(grid, grid, numvar),naero_zp(nnumvar, grid, grid))
allocate(aero_rbc(grid, grid, numvar),naero_rbc(nnumvar, grid, grid))
allocate(aero_rdc(grid, grid, numvar),naero_rdc(nnumvar, grid, grid))


call check ( nf90_inq_varid( ncid, 'LAIgrid', varid ) )
call check ( nf90_get_var( ncid, varid, laigrid ) )

call check ( nf90_inq_varid( ncid, 'fVCovergrid', varid ) )
call check ( nf90_get_var( ncid, varid, fvcovergrid ) )

call check ( nf90_inq_varid( ncid, 'Aero_zo', varid ) )
call check ( nf90_get_var( ncid, varid, aero_zo ) )

call check ( nf90_inq_varid( ncid, 'Aero_zp', varid ) )
call check ( nf90_get_var( ncid, varid, aero_zp ) )

call check ( nf90_inq_varid( ncid, 'Aero_RbC', varid ) )
call check ( nf90_get_var( ncid, varid, aero_rbc ) )
 
call check ( nf90_inq_varid( ncid, 'Aero_RdC', varid ) )
call check ( nf90_get_var( ncid, varid, aero_rdc ) )

call check ( nf90_close(ncid) )

!Process data
do i=1,nnumvar
   naero_zo(i,:,:)  = aero_zo(:,:,numvartopft(i))
   naero_zp(i,:,:)  = aero_zp(:,:,numvartopft(i))
   naero_rbc(i,:,:) = aero_rbc(:,:,numvartopft(i))
   naero_rdc(i,:,:) = aero_rdc(:,:,numvartopft(i))
enddo

!Write out data
  call check (nf90_create(trim(fileout), nf90_clobber, ncid ) )
  print*,'Writing file: ',trim(fileout)
  print('(a,2i6)'),'   Dimensions (ngrid,npft): ',grid,nnumvar

  call check (nf90_def_dim( ncid,'npft',nnumvar, nnumvarid ) )
  call check (nf90_def_dim( ncid,'ngrid',grid, ngridid ) )
  call check (nf90_def_dim( ncid,'clen',charlen,ncharid ) )

  call check (nf90_def_var( ncid,'pft_names',nf90_char, (/ncharid,nnumvarid/), charpnid))
  call check (nf90_def_var( ncid,'laigrid',nf90_float, ngridid, laiid ) )
  call check (nf90_def_var( ncid,'fvcovergrid',nf90_float, ngridid, vcovid ) )
  call check (nf90_def_var( ncid,'aero_zo',nf90_float, (/nnumvarid, ngridid, ngridid/), zoid ) )
  call check (nf90_def_var( ncid,'aero_zp',nf90_float, (/nnumvarid, ngridid, ngridid/), zpid ) )
  call check (nf90_def_var( ncid,'aero_rbc',nf90_float, (/nnumvarid, ngridid, ngridid/), rbcid ) )
  call check (nf90_def_var( ncid,'aero_rdc',nf90_float, (/nnumvarid, ngridid, ngridid/), rdcid ) )

  call check (nf90_enddef( ncid ) )

  call check (nf90_put_var( ncid, charpnid, pftname ) )
  call check (nf90_put_var( ncid, laiid, laigrid ) )
  call check (nf90_put_var( ncid, vcovid, fvcovergrid ) )
  call check (nf90_put_var( ncid, zoid, naero_zo ) )
  call check (nf90_put_var( ncid, zpid, naero_zp ) )
  call check (nf90_put_var( ncid, rbcid, naero_rbc ) )
  call check (nf90_put_var( ncid, rdcid, naero_rdc ) )

  call check ( nf90_close( ncid ) )

end program create_sibaero

!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine check(status)

use netcdf
use typeSizes
implicit none

integer, intent ( in ) :: status
 
     if (status /= nf90_noerr) then
        !print *, trim(nf90_strerror(status))
        stop "Error with netcdf.  Stopped."
      end if
end subroutine check   


