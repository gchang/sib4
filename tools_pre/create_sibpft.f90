!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Program to convert CLM PFT file with 35 PFTs
!!  to SiB4 with 24 PFTs.
!!
!!The original PFT map is 0.1-degree MODIS data (MOD12Q)
!!converted to CLM PFTs following:
!!P.J. Lawrence et al., 2007, Representing a new MODIS consistent
!!land surface in the Community Land Model (CLM 3.0).  J. Geophys. 
!!Res., 112, G01023.
!!
!!
!!To compile, use:
!!gfortran create_sibpft.f90 -L/usr/local/netcdf3-gcc/lib
!!    -lnetcdf -I/usr/local/netcdf3-gcc/include
!!
!!kdhaynes, 06/17
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program create_sibpft

use netcdf
implicit none

integer, parameter :: npftorig=35, npftnew=15
integer, parameter :: clenorig=07, clennew=03

!For 0.5-degree:
character*100, parameter :: &
     vegin='/Users/kdhaynes/Research/sib4_data/pre/pft35_global_0.5deg.nc'
character*100, parameter :: &
     landin='/Users/kdhaynes/Research/sib4_data/pre/land_mask_0.5deg.nc'
character*100, parameter :: &
     mapout='/Users/kdhaynes/Research/sib4_data/pre/pftsib_global_0.5deg.nc'

real, parameter :: &
     minpftval=0.05  !Minimum fractional coverage per PFT

character(len=clenorig), dimension(npftorig), parameter :: &
     pftnameorig=['bar_all','enf_tem','enf_bor','dnf_bor','ebf_tro','ebf_tem', &
        'dbf_tro','dbf_tem','dbf_bor','ebs_all','dbs_tem','dbs_bor',  &
        'c3g_arc','c3g_nar','c4g_all', &
        'cro_brl','cro_cas','cro_cot','cro_grn','cro_mze','cro_mil', &
        'cro_oil','cro_oth','cro_pot','cro_pul','cro_rap','cro_ric','cro_rye', &
        'cro_sor','cro_soy','cro_sgb','cro_sgc','cro_sun','cro_wht','wat_all']
integer, dimension(npftorig), parameter :: &
     pfttonew=[ 1,  2,  2,  3,  4,  4,  &
                5,  5,  5,  6,  6,  7, &
                8,  9, 10, &
               11, 11, 11, 11, 13, 12, &
               11, 11, 11, 11, 11, 11, 11, &
               12, 14, 11, 12, 11, 15, 1]

character(len=clennew), dimension(npftnew), parameter :: &
   pftnamenew=['dbg','enf','dnf','ebf','dbf',  &
               'shb','sha','c3a','c3g','c4g',  &
               'c3c','c4c','mze','soy','wwt']
integer(kind=1), dimension(npftnew), parameter :: &
     pftrefnew=[1,  2,  4,  5,  8,  &
               11, 12, 13, 14, 15,  &
               17, 18, 20, 22, 24]

integer, parameter :: dbg=1
integer, parameter :: shb=11
integer, parameter :: sha=12
real, parameter :: arclat=60.      !latitude barrier between temperate and arctic
real, parameter :: des_switch=0.0  !fraction of bare ground to switch to shrubs

real, parameter :: minmatch=0.99
real, parameter :: maxmatch=1.01

!Variables for MODIS data
integer :: nlon,nlat
real, allocatable, dimension(:) :: lat,lon
integer*1, allocatable, dimension(:,:) :: obfractemp
real,allocatable,dimension(:,:,:) :: obfrac

!Variables for land mask data
integer :: nlonl,nlatl
integer*1, allocatable, dimension(:,:) :: land

!Variables for netcdf
integer :: ncid
integer :: nlonid,nlatid,ncharid,npftid
integer :: dataid,lonid,latid
integer :: pftnameid,pftrefid,pftfracid

!Variables for SiB4 output
real, allocatable, dimension(:,:,:) :: sibfrac

!Misc variables
integer :: i,j
integer :: p, pref
real :: des_cur_frac, totarea

!!!!!!!!!!!!!!!!!!!!
!Open the land mask
print*,'Opening file: ',trim(landin)
call check ( nf90_open( trim(landin), nf90_nowrite, ncid ) )

call check ( nf90_inq_dimid( ncid,'nlon',nlonid ) )
call check ( nf90_inquire_dimension( ncid, nlonid, len=nlonl ) )
call check ( nf90_inq_dimid( ncid,'nlat',nlatid ) )
call check ( nf90_inquire_dimension( ncid, nlatid, len=nlatl ) )
call check ( nf90_inq_varid( ncid,'data',dataid ) )

allocate(land(nlonl,nlatl))
call check ( nf90_get_var( ncid, dataid, land ) )
call check ( nf90_close(ncid) )

!Open the PFT data
print*,'Opening file: ',trim(vegin)
call check ( nf90_open( trim(vegin), nf90_nowrite, ncid ) )

call check ( nf90_inq_dimid( ncid, 'lon', nlonid ) )
call check ( nf90_inquire_dimension( ncid, nlonid, len=nlon ) )
call check ( nf90_inq_dimid( ncid, 'lat', nlatid ) )
call check ( nf90_inquire_dimension( ncid,nlatid,len=nlat ) )

if ((nlon .ne. nlonl) .or. (nlat .ne. nlatl)) then
   print*,'Mismatching Grids.  Stopping.'
   print*,'   Land Mask Lon/Lat, PFT Lon/Lat: ', &
         nlonl, nlatl, nlon, nlat
   stop
endif

allocate(lat(nlat),lon(nlon))
allocate(obfractemp(nlon,nlat))
allocate(obfrac(nlon,nlat,npftorig))
obfrac(:,:,:) = 0.0

allocate(sibfrac(nlon,nlat,npftnew))
sibfrac(:,:,:) = 0.0

call check ( nf90_inq_varid( ncid,'lat',latid ) )
call check ( nf90_get_var( ncid,latid,lat ) )
call check ( nf90_inq_varid( ncid,'lon',lonid ) )
call check ( nf90_get_var( ncid,lonid,lon ) )

do p=1,npftorig
    !print*,'   Getting: ',pftnameorig(p)
    call check (nf90_inq_varid( ncid,pftnameorig(p),dataid ) )
    call check (nf90_get_var( ncid,dataid,obfractemp ) )
    obfrac(:,:,p) = real(obfractemp(:,:))/100.
enddo
call check (nf90_close( ncid ) )

print*,''
print*,'Creating the SiB4 PFT map.'

!Fix the map
do i=1,nlon
   do j=1,nlat
      do p=1,npftorig
         pref = pfttonew(p)
         sibfrac(i,j,pref) = sibfrac(i,j,pref) + obfrac(i,j,p)
         !Switch non-veg/bar ground to shrub/grass
         if (des_switch .gt. 0.0) then
             des_cur_frac = sibfrac(i,j,dbg)
             if (lat(j) .lt. arclat) then
                sibfrac(i,j,shb) = sibfrac(i,j,shb) + des_switch*des_cur_frac
             else
                sibfrac(i,j,sha) = sibfrac(i,j,sha) + des_switch*des_cur_frac
             endif
             sibfrac(i,j,dbg) = (1-des_switch)*des_cur_frac
         endif
       enddo  !p=1,npftorig
         
      !Remove contributions less than 5% and 
      !  any points that are not considered land in the SiB4 grid
      !Check to make sure all areas sum to 1.
      do p=1,npftnew
         if (sibfrac(i,j,p) .lt. minpftval) sibfrac(i,j,p)=0.
      enddo
      totarea = sum(sibfrac(i,j,:))
      if (land(i,j) .eq. 0) then
         sibfrac(i,j,:) = 0.0
      else
         if (totarea .lt. 0.1) then
             print*,''
             print*,'Error. Stopping'
             print*,'Lon/Lat: ',lon(i),lat(j)
             print*,'SiB Frac : ',sibfrac(i,j,:)
             print*,''
             print*,'Orig Frac: ',obfrac(i,j,:)
             print*,''
             stop
          else
             sibfrac(i,j,:) = sibfrac(i,j,:)/totarea
          endif
      endif 
          
      !Check all areas and number of land points
      totarea=sum(sibfrac(i,j,:))
      if (land(i,j) .ne. 0) then
         if ((totarea .lt. minmatch) .or. (totarea .gt. maxmatch)) then
             print*,'Land coverage incomplete!',totarea,lon(i),lat(j)
             stop
         endif
      else
         if (totarea .gt. 1E-6) then
            print*,'Extra land coverage!',totarea,lon(i),lat(j)
            stop
          endif
      endif

   enddo  !j
enddo  !i

! Write out gridded SiB4 PFT file
  call check (nf90_create(trim(mapout), nf90_clobber, ncid ) )
  print*,'Writing file: ',trim(mapout)

  call check (nf90_def_dim( ncid,'nlon', nlon, nlonid ) )
  call check (nf90_def_dim( ncid,'nlat',nlat, nlatid ) )
  call check (nf90_def_dim( ncid,'npft', npftnew, npftid ) )
  call check (nf90_def_dim( ncid,'clen',clennew,ncharid ) )

  call check (nf90_def_var( ncid,'lon',nf90_float, nlonid, lonid ) )
  call check (nf90_def_var( ncid,'lat',nf90_float, nlatid, latid ) )
  call check (nf90_def_var( ncid,'pft_names',nf90_char, (/ncharid,npftid/), pftnameid ) )
  call check (nf90_def_var( ncid,'pft_refs',nf90_byte, (/npftid/), pftrefid ) )
  call check (nf90_def_var( ncid,'pft_frac',nf90_float, (/nlonid,nlatid,npftid/), pftfracid ) )
  call check (nf90_enddef( ncid ) )

  call check ( nf90_put_var( ncid, lonid, lon ) )
  call check ( nf90_put_var( ncid, latid, lat ) )
  call check ( nf90_put_var( ncid, pftnameid, pftnamenew ) )
  call check ( nf90_put_var( ncid, pftrefid, pftrefnew ) )
  call check ( nf90_put_var( ncid, pftfracid, sibfrac ) )
  call check ( nf90_close( ncid ) )

end program create_sibpft



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


