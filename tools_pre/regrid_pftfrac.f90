!Program to regrid 0.1-degree PFT data from MODIS to
!  dimensions specified in land area map.
!
!Uses a land mask file, landin, to determine the new dimensions/points.
!
!kdhaynes, 10/17

program regrid_pftfrac

use netcdf
implicit none

!!Input Data
character*100, parameter :: &
   pftin='/Users/kdhaynes/Research/sib4_data/pre/pft35_global_0.1deg.nc'
character*100, parameter :: &
   landin='/Users/kdhaynes/Research/sib4_data/pre/land_mask_0.5deg.nc'

character*100, parameter :: &
   mapout='/Users/kdhaynes/Research/sib4_data/pre/pft35_global_0.5deg.nc'

integer, parameter :: pftseparate=1
integer, parameter :: numpft=35
integer, parameter :: charlen=7

!!Variables for PFT data
integer :: nlon,nlat
real*8, allocatable, dimension(:) :: lon,lat
integer*4, allocatable, dimension(:,:) :: pixpercell
integer*1, allocatable, dimension(:,:) :: pfttemp
integer*1, allocatable, dimension(:,:,:) :: pft
integer*1, allocatable, dimension(:,:) :: newpfttemp
real, allocatable, dimension(:,:,:) :: newpft
character*7 :: pftname(numpft)

!!Variables for land mask
integer :: newnlon,newnlat
real :: deltanewlon,deltanewlat
real, allocatable, dimension(:) :: newlon,newlat

!!Variables for netcdf
integer :: shit
integer :: ncid
integer :: nlonid,nlatid,npftid,ncharid
integer :: lonid,latid,pftid,dataid
integer, dimension(numpft) :: varid

!!Misc variables
integer :: newi,newj
integer :: i,j,p,ii,jj

!!pftname for 35 PFTs
data pftname/'bar_all','enf_tem','enf_bor','dnf_bor','ebf_tro','ebf_tem','dbf_tro', &
         'dbf_tem','dbf_bor','ebs_all','dbs_tem','dbs_bor','c3g_arc','c3g_nar', &
         'c4g_all','cro_brl','cro_cas','cro_cot','cro_grn','cro_mze','cro_mil', &
         'cro_oil','cro_oth','cro_pot','cro_pul','cro_rap','cro_ric','cro_rye', &
         'cro_sor','cro_soy','cro_sgb','cro_sgc','cro_sun','cro_wht','wat_all'/

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Open the PFT data
call check ( nf90_open(trim(pftin), nf90_nowrite, ncid ) )
print*,'Opening file: ',trim(pftin)

call check ( nf90_inq_dimid( ncid, 'lon', nlonid ) )
call check ( nf90_inquire_dimension( ncid, nlonid, len=nlon ) )
call check ( nf90_inq_dimid( ncid, 'lat', nlatid ) )
call check ( nf90_inquire_dimension( ncid, nlatid, len=nlat ) )
print*,'   Number of original lon/lat: ',nlon,nlat

allocate(lat(nlat),lon(nlon),pfttemp(nlon,nlat),pft(nlon,nlat,numpft))
call check ( nf90_inq_varid( ncid,'lon',lonid ) )
call check ( nf90_get_var( ncid,lonid,lon ) )
call check ( nf90_inq_varid( ncid,'lat',latid ) )
call check ( nf90_get_var( ncid,latid,lat ) )

do p=1,numpft
   call check ( nf90_inq_varid( ncid,pftname(p),dataid ) )
   call check ( nf90_get_var( ncid,dataid,pfttemp ) )
   pft(:,:,p) = pfttemp(:,:)
enddo

call check ( nf90_close( ncid ) )

!!Open the land mask data to get new lon/lat
call check ( nf90_open( trim(landin), nf90_nowrite, ncid ) )
print*,'Opening file: ',trim(landin)

call check ( nf90_inq_dimid( ncid, 'nlat', nlatid ) )
call check ( nf90_inquire_dimension( ncid, nlatid, len=newnlat ) )
call check ( nf90_inq_dimid( ncid, 'nlon', nlonid ) )
call check ( nf90_inquire_dimension( ncid,nlonid,len=newnlon ) )
print*,'   Number of output lon/lat: ',newnlon,newnlat

allocate(newlat(newnlat),newlon(newnlon))
call check ( nf90_inq_varid( ncid,'lat',latid ) )
call check ( nf90_get_var( ncid,latid,newlat ) )
call check ( nf90_inq_varid( ncid,'lon',lonid ) )
call check ( nf90_get_var( ncid,lonid,newlon ) )
call check (nf90_close( ncid ) )

!!Combine 0.1-degree PFTs into resolution of land mask
allocate(newpft(newnlon,newnlat,numpft))
allocate(pixpercell(newnlon,newnlat))
newpft=0.
pixpercell=0.
deltanewlon=abs(newlon(2)-newlon(1))/2.
deltanewlat=abs(newlat(2)-newlat(1))/2.
print*,'   Gridcell size (lon,lat): ',deltanewlon,deltanewlat

print*,'Calculating new map...'
do i=1,nlon
   do j=1,nlat
      newi=0
      newj=0
      do ii=1,newnlon
         if ((lon(i) .ge. newlon(ii)-deltanewlon) .and. &
             (lon(i) .le. newlon(ii)+deltanewlon)) then
             newi=ii
             exit
          endif
      enddo
      do jj=1,newnlat
         if ((lat(j) .ge. newlat(jj)-deltanewlat) .and. &
             (lat(j) .le. newlat(jj)+deltanewlat)) then
             newj=jj
             exit
         endif
       enddo

       if (newi .lt. 1 .or. newj .lt. 1) then
          print*,'Bad Lat/Lon Regrid Value.  Stopping.'
          print*,'Original Lat/Lon: ',lat(i),lon(j)
          stop
        endif
       newpft(newi,newj,:) = newpft(newi,newj,:) + pft(i,j,:)
       pixpercell(newi,newj) = pixpercell(newi,newj) + 1

   enddo !j=1,nlat
enddo  !i=1,nlon

do i=1,newnlon
   do j=1,newnlat
      if (pixpercell(i,j) .gt. 0) then
         newpft(i,j,:) = newpft(i,j,:)/(100.*pixpercell(i,j))
      else
         newpft(i,j,:) = 0.
      endif
   enddo  !j=1,newnlat
enddo  !i=1,newnlon


!!Write out new PFT data
if (pftseparate .eq. 0) then
   call check ( nf90_create(trim(mapout), nf90_clobber, ncid ) )
   print*,'Writing file: ',trim(mapout)

   call check (nf90_def_dim( ncid,'lon',newnlon,nlonid ) )
   call check (nf90_def_dim( ncid,'lat',newnlat,nlatid ) )
   call check (nf90_def_dim( ncid,'pft',numpft,npftid ) )
   call check (nf90_def_dim( ncid,'clen',charlen,ncharid ) )

   call check (nf90_def_var( ncid,'lon',nf90_float, nlonid, lonid ) )
   call check (nf90_def_var( ncid,'lat',nf90_float, nlatid, latid ) )
   call check (nf90_def_var( ncid,'pft_names',nf90_char, (/ncharid,npftid/), pftid ) )
   call check (nf90_def_var( ncid,'pft_frac',nf90_float, &
           (/nlonid,nlatid,npftid/),dataid ) )
   call check (nf90_enddef( ncid ) )

   call check ( nf90_put_var( ncid,lonid,newlon ) )
   call check ( nf90_put_var( ncid,latid,newlat ) )
   call check ( nf90_put_var( ncid,pftid,pftname ) )
   call check ( nf90_put_var( ncid,dataid,newpft ) )
   call check ( nf90_close( ncid ) )
else
   call check (nf90_create(trim(mapout), nf90_clobber, ncid ) )
   print*,'Writing file: ',trim(mapout)

   call check (nf90_def_dim( ncid,'lon',newnlon,nlonid ) )
   call check (nf90_def_dim( ncid,'lat',newnlat,nlatid ) )  

   call check (nf90_def_var( ncid,'lon',nf90_float, nlonid, lonid ) )
   call check (nf90_def_var( ncid,'lat',nf90_float, nlatid, latid ) )
   do p=1,numpft
      call check ( nf90_def_var( ncid,pftname(p),nf90_byte,(/nlonid,nlatid/), varid(p) ) )
   enddo

   call check (nf90_enddef(ncid))

   call check ( nf90_put_var( ncid, lonid, newlon ) )
   call check ( nf90_put_var( ncid, latid, newlat ) )

   allocate(newpfttemp(newnlon,newnlat))
   do p=1,numpft
      newpfttemp(:,:) = newpft(:,:,p)*100
      call check ( nf90_put_var( ncid, varid(p), newpfttemp ) )
   enddo

   call check ( nf90_close( ncid ) )
endif

end program regrid_pftfrac


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

