!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!To compile, use (all one line in application):
!!!gfortran create_sibvs.f90 -L/usr/local/netcdf3-gcc/lib -lnetcdf 
!!!       -I/usr/local/netcdf3-gcc/includef90nc
!!!
!!!Calculates a global sib_vs.nc file, with
!!!   the number of PFTs per land unit set to 1.
!!!
!!!The area of the land unit changes,
!!!    but since there is only one PFT per land
!!!    unit the area of PFT per land unit is always 1.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Program to combine all necessary surface data for SiB4.
!  --Assumes one PFT per Land Unit
!
!The original PFT map is the same as the NCAR CLM model,
!  with 35  PFTs.
!The reference publication:
!P.J. Lawrence and T.N. Chase, 2007, Representing a new MODIS consistent
!land surface in the Community Land Model (CLM 3.0).  J. Geophys. 
!Res., 112, G01023.
!
!The land mask comes from the Center for Sustainability and the Global Environment (SAGE)
!at the Gaylord Nelson Institute for Environmental Studies, University of Wisconsin-Madison
!On-line: http://www.sage.wisc.edu/iamdata/grid.php
!Contact: N. Ramankutty
!
!Soil reflectance was downloaded from ORNL DAAC.
!The data is from the ISLSCP Initiative II, originally on 1-degree.
!The citation information:
!Global Soil Data Task Group. 2000. Global Gridded Surfaces of Selected Soil 
!Characteristics (IGBP-DIS). Data set. Available on-line [http://daac.ornl.gov] 
!from Oak Ridge National Laboratory Distributed Active Archive Center, Oak Ridge, 
!Tennessee, U.S.A. doi:10.3334/ORNLDAAC/569
!
!Soil sand/clay fractions was from Ray Drapek.
!Obtained from ftp://ftp.fsl.orst.edu/pub/drapek/data_basin/Global/Inputs/
!    (and on Data Basin in ArcGIS format).
!The citation information:
!Global Gridded Surfaces of Selected Soil Characteristics. 2005.
!International Geosphere Biosphere Programme. 
!ftp://daac.ornl.gov/data/global_soil/IGBP-SurfaceProducts
!Initiative I. Volumes 1-5. Global Data Sets for Land-Atmosphere Models. 1996. 
!The International Satellite Land Surface Climatology Project.
!
!Acknowledgement of the following agencies in products derived from these data:
!  International Geosphere Biosphere Programme (IGBP),
!  the International Satellite Land Surface Climatology Project (ISLSCP), and
!  the Pacific Northwest Research Laboratory of the USDA Forest Service.
!
!Credits:FAO. Ray Drapek and Ron Neilson.
!         MAPSS team. USDA Forest Service. PNW Research station.
!
!Expects land mask, PFT, and soil maps all to match.
!
!kdcorbin, 10/17

program create_sibvs

use netcdf
implicit none

integer, parameter :: ntlu=10
integer, parameter :: clen=3, flen=160
real, parameter    :: minpft=0.01, badval=-9999.

character(len=flen), parameter :: &
   landin='/Users/kdhaynes/Dropbox/sib4v2/data/land/land_mask_0.5deg.nc'
character(len=flen), parameter :: &
   pftin='/Users/kdhaynes/Dropbox/sib4v2/data/land/pftsib_global_0.5deg.nc'
character(len=flen), parameter :: &
   soilin='/Users/kdhaynes/Dropbox/sib4v2/data/land/soil_global_0.5deg.nc'
character(len=flen), parameter :: &
   sorefin='/Users/kdhaynes/Dropbox/sib4v2/data/land/soref_global_0.5deg.nc'
character(len=flen), parameter :: &
   mapout='/Users/kdhaynes/Dropbox/sib4v2/data/sib_vs_0.5deg.nc'


!Variables for land mask
integer :: nlon,nlat
real, allocatable, dimension(:) :: lat,lon
integer*1, allocatable, dimension(:,:) :: land

!Variables for PFT data
integer :: nlonp,nlatp,npft
character(len=clen), allocatable, dimension(:) :: pft_names
integer(kind=1), dimension(:), allocatable :: pft_refs
real,allocatable,dimension(:,:,:) :: pft_frac

!Variables for soil data
integer :: nlons,nlats
real,allocatable,dimension(:,:) :: sand,clay
real,allocatable,dimension(:,:) :: vis,nir


!Variables for netcdf
integer :: ncid
integer :: nlonid,nlatid
integer :: varid

integer :: nsibid, npftid, nluid, ncharid
integer :: latsibid, lonsibid
integer :: pnameid, prefid, clayid, sandid
integer :: gnluid, lgareaid, ppftrefid
integer :: nirid, visid

!Variables for processing data
real,allocatable,dimension(:) :: temppft
integer,allocatable,dimension(:) :: tempref

!Variables for nsib/vector output
integer :: nsib, countsib
real, allocatable, dimension(:) :: lonsib,latsib
integer(kind=1), allocatable, dimension(:) :: gnlusib
real, allocatable, dimension(:,:) :: lgareasib
integer(kind=1), allocatable, dimension(:,:) :: ppftrefsib
real, allocatable, dimension(:,:) :: sfracsib,cfracsib
real, allocatable, dimension(:,:) :: vissib,nirsib

!Misc variables
integer :: i,j,p
integer(kind=1) :: numpt


!Open the Land data
print*,'Opening file: ',trim(landin)
call check ( nf90_open( trim(landin), nf90_nowrite, ncid ) )

call check ( nf90_inq_dimid( ncid, 'nlon', nlonid ) )
call check ( nf90_inquire_dimension( ncid, nlonid, len=nlon ) )
call check ( nf90_inq_dimid( ncid, 'nlat', nlatid ) )
call check ( nf90_inquire_dimension( ncid,nlatid,len=nlat ) )

allocate(lon(nlon),lat(nlat))
allocate(land(nlon,nlat))
call check ( nf90_inq_varid( ncid, 'lon', varid ) )
call check ( nf90_get_var( ncid,varid,lon ) )
call check ( nf90_inq_varid( ncid, 'lat',varid ) )
call check ( nf90_get_var( ncid,varid,lat))
call check ( nf90_inq_varid( ncid, 'data',varid ) )
call check ( nf90_get_var( ncid,varid, land ) )
call check ( nf90_close(ncid))


!Open the PFT data
print*,'Opening file: ',trim(pftin)
call check ( nf90_open( trim(pftin), nf90_nowrite, ncid ) )

call check ( nf90_inq_dimid( ncid, 'nlon', nlonid ) )
call check ( nf90_inquire_dimension( ncid, nlonid, len=nlonp ) )
call check ( nf90_inq_dimid( ncid, 'nlat', nlatid ) )
call check ( nf90_inquire_dimension( ncid,nlatid,len=nlatp ) )
if ((nlatp .ne. nlat) .or. (nlonp .ne. nlon)) then
   print*,'!!!  Error.  Stopping.  !!!'
   print*,'Expecting Land Mask and PFT map to match'
   print*,'Land lon/lat: ',nlon,nlat
   print*,'PFT lon/lat: ',nlonp,nlatp
   stop
endif
call check ( nf90_inq_dimid( ncid, 'npft', npftid ) )
call check ( nf90_inquire_dimension( ncid,npftid,len=npft ) )
print*,'   Number of lon/lat/PFTs: ',nlon,nlat,npft

allocate(pft_names(npft),pft_refs(npft))
allocate(pft_frac(nlon,nlat,npft))
call check ( nf90_inq_varid( ncid, 'pft_names',varid ) )
call check ( nf90_get_var( ncid,varid, pft_names ) )
call check ( nf90_inq_varid( ncid, 'pft_refs', varid ) )
call check ( nf90_get_var( ncid,varid,pft_refs ) )
call check ( nf90_inq_varid( ncid, 'pft_frac',varid ) )
call check ( nf90_get_var( ncid,varid,pft_frac ) )
call check ( nf90_close(ncid))


!Open the soil data
print*,'Opening file: ',trim(soilin)
call check ( nf90_open( trim(soilin), nf90_nowrite, ncid ) )

call check ( nf90_inq_dimid( ncid, 'nlat', nlatid ) )
call check ( nf90_inquire_dimension( ncid, nlatid, len=nlats ) )
call check ( nf90_inq_dimid( ncid, 'nlon', nlonid ) )
call check ( nf90_inquire_dimension( ncid,nlonid,len=nlons ) )
if ((nlats .ne. nlat) .or. (nlons .ne. nlon)) then
   print*,'!!!  Error.  Stopping.  !!!'
   print*,'Expecting PFT map and soil map to match'
   print*,'PFT lon/lat: ',nlon,nlat
   print*,'Soil lon/lat: ',nlons,nlats
   stop
endif

allocate(clay(nlon,nlat),sand(nlon,nlat))
call check (nf90_inq_varid( ncid,'sand_frac',varid ) )
call check (nf90_get_var( ncid,varid,sand ) )
call check (nf90_inq_varid( ncid,'clay_frac',varid ) )
call check (nf90_get_var( ncid,varid,clay ) )
call check (nf90_close( ncid ) )


!Open the soref data
print*,'Opening file: ',trim(sorefin)
call check ( nf90_open( trim(sorefin), nf90_nowrite, ncid ) )

call check ( nf90_inq_dimid( ncid, 'nlat', nlatid ) )
call check ( nf90_inquire_dimension( ncid, nlatid, len=nlats ) )
call check ( nf90_inq_dimid( ncid, 'nlon', nlonid ) )
call check ( nf90_inquire_dimension( ncid,nlonid,len=nlons ) )
if ((nlats .ne. nlat) .or. (nlons .ne. nlon)) then
   print*,'!!!  Error.  Stopping.  !!!'
   print*,'Expecting PFT and soref maps to match'
   print*,'PFT lon/lat: ',nlon,nlat
   print*,'Soref lon/lat: ',nlons,nlats
   stop
endif
   
allocate(vis(nlon,nlat),nir(nlon,nlat))
call check (nf90_inq_varid( ncid,'soref_vis',varid ) )
call check (nf90_get_var( ncid,varid,vis ) )
call check (nf90_inq_varid( ncid,'soref_nir',varid ) )
call check (nf90_get_var( ncid,varid,nir ) )
call check (nf90_close( ncid ) )

!Prepare the data
!...determine number of sib points
nsib = 0
do i=1,nlon
   do j=1,nlat
      if (land(i,j) .eq. 1) then
          nsib = nsib + 1
      endif
   enddo
enddo
print*,'Number of SiB4 Points: ',nsib

!...fill arrays
allocate(temppft(npft),tempref(npft))
allocate(lonsib(nsib),latsib(nsib))
allocate(gnlusib(nsib))
allocate(lgareasib(nsib,ntlu))
allocate(ppftrefsib(nsib,ntlu))
allocate(sfracsib(nsib,ntlu),cfracsib(nsib,ntlu))
allocate(vissib(nsib,ntlu),nirsib(nsib,ntlu))

countsib=1.
do i=1,nlon
   do j=1,nlat

      if (land(i,j) .eq. 1) then

          lonsib(countsib) = lon(i)
          latsib(countsib) = lat(j)

          temppft(:) = pft_frac(i,j,:)
          where(temppft .lt. minpft) temppft=badval

          call hpsort(npft,temppft,tempref)
          call hprev(npft,temppft,tempref)

           numpt = 0
           DO p=1,npft
              IF (temppft(p) .gt. badval) THEN
                 numpt = numpt + 1
              ENDIF
           ENDDO

           !Hardwire soil values for all land units
           !  to have same soil properties
           sfracsib(countsib,1:numpt) = sand(i,j)
           cfracsib(countsib,1:numpt) = clay(i,j)
           vissib(countsib,1:numpt) = vis(i,j)
           nirsib(countsib,1:numpt) = nir(i,j)

           gnlusib(countsib) = numpt
           do p=1,numpt
              lgareasib(countsib,p) = temppft(p)
              ppftrefsib(countsib,p) = pft_refs(tempref(p))
           enddo

           countsib=countsib+1
        endif  !land eq 1
    enddo  !j=1,nlat
 enddo !i=1,nlon

!Write out the file
call check (nf90_create(trim(mapout), nf90_clobber, ncid ) )
  print*,''
  print*,'Writing file: ',trim(mapout)

  call check (nf90_def_dim( ncid,'nsib', nsib, nsibid ) )
  call check (nf90_def_dim( ncid,'npft',npft, npftid ) )
  call check (nf90_def_dim( ncid,'nlu', ntlu, nluid ) )
  call check (nf90_def_dim( ncid,'clen',clen,ncharid ) )
  call check (nf90_def_var( ncid,'pft_names',nf90_char, (/ncharid,npftid/), pnameid ) )
  call check (nf90_def_var( ncid,'pft_refs',nf90_byte, (/npftid/), prefid ) )
  call check (nf90_def_var( ncid,'lonsib',nf90_float, nsibid, lonsibid ) )
  call check (nf90_def_var( ncid,'latsib',nf90_float, nsibid, latsibid ) )
  call check (nf90_def_var( ncid,'g_nlu',nf90_byte, nsibid, gnluid ) )
  call check (nf90_def_var( ncid,'larea',nf90_float, (/nsibid,nluid/), lgareaid ) )
  call check (nf90_def_var( ncid,'pftref',nf90_byte, (/nsibid,nluid/), ppftrefid ) )
  call check (nf90_def_var( ncid,'sand_frac',nf90_float,(/nsibid,nluid/),sandid ) )
  call check (nf90_def_var( ncid,'clay_frac',nf90_float,(/nsibid,nluid/),clayid ) )
  call check (nf90_def_var( ncid,'soref_vis',nf90_float,(/nsibid,nluid/),visid ) )
  call check (nf90_def_var( ncid,'soref_nir',nf90_float,(/nsibid,nluid/),nirid ) )
  call check (nf90_enddef( ncid ) )

  call check ( nf90_put_var( ncid, lonsibid, lonsib ) )
  call check ( nf90_put_var( ncid, latsibid, latsib ) )
  call check ( nf90_put_var( ncid, pnameid, pft_names ) )
  call check ( nf90_put_var( ncid, prefid, pft_refs ) )
  call check ( nf90_put_var( ncid, gnluid, gnlusib ) )
  call check ( nf90_put_var( ncid, lgareaid, lgareasib ) )
  call check ( nf90_put_var( ncid, ppftrefid, ppftrefsib ) )
  call check ( nf90_put_var( ncid, sandid, sfracsib ) )
  call check ( nf90_put_var( ncid, clayid, cfracsib ) )
  call check ( nf90_put_var( ncid, visid, vissib ) )
  call check ( nf90_put_var( ncid, nirid, nirsib ) )
  call check ( nf90_close( ncid ) )

end program create_sibvs

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*  Sorts an array RA of length N in ascending order *
!*                by the Heapsort method             *
!* ------------------------------------------------- *
!* INPUTS:                                           *
!*	    N	  size of table RA                   *
!*          RA	  table to be sorted                 *
!* OUTPUT:                                           *
!*	    RA    table sorted in ascending order    *
!*                                                   *
!* NOTE: The Heapsort method is a N Log2 N routine,  *
!*       and can be used for very large arrays.      *
!*****************************************************       
subroutine hpsort(npts,rarray,refarray)

implicit none

integer :: npts
real, dimension(npts) :: rarray
integer, dimension(npts) :: refarray

real :: RRA
integer :: RRAREF

integer :: I,J,L,IR

DO I=1,npts
   refarray(I) = I
ENDDO
L=npts/2+1
IR=npts

10 continue
  if (L > 1) then
     L=L-1
     RRA=rarray(L)
     RRAREF=refarray(L)
  else
     RRA=rarray(IR)
     RRAREF=refarray(IR)
     rarray(IR) = rarray(1)
     refarray(IR) = refarray(1)
     IR=IR-1
     if (IR .eq. 1) then
        rarray(1)=RRA
        refarray(1)=RRAREF
        return
      endif
   endif
   I=L
   J=L+L
20 if (j.le.IR) then
       if (J < IR) then
          if (rarray(J) < rarray(J+1)) J=J+1
       endif
       if (RRA < rarray(J)) then
           rarray(I)=rarray(J)
           refarray(I)=refarray(J)
           I=J; J=J+J
        else
           J=IR+1
        endif
        goto 20
     endif

     rarray(I)=RRA
     refarray(I)=RRAREF
     goto 10

end subroutine hpsort


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine hprev(npts,rarray,refarray)

implicit none

integer :: npts
real, dimension(npts) :: rarray
integer, dimension(npts) :: refarray

real,dimension(npts) :: copyrarray
integer, dimension(npts) :: copyrefarray
integer :: i,count

copyrarray=rarray
copyrefarray=refarray
count=npts
DO i=1,npts
   rarray(i) = copyrarray(count)
   refarray(i) = copyrefarray(count)
   count=count-1
ENDDO


end subroutine hprev
