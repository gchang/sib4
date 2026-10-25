subroutine cropdist_output(nfiles, &
     fnamesin, outfile, cropfile, regionfile)

  use netcdf

  implicit none

  !input variables
  integer, intent(in) :: nfiles
  character(len=256), dimension(nfiles), intent(in) :: &
       fnamesin
  character(len=256), intent(in) :: &
       outfile, cropfile, regionfile

  !netcdf variables
  integer :: ncid, status, dimid

  !open the first file
  status = nf90_open(trim(fnamesin(1)), nf90_nowrite, ncid)

  !get dimensions
  status = nf90_inq_dimid(ncid, 'nsib', dimid)
  if (status .ne. nf90_noerr) then
     status = nf90_inq_dimid(ncid, 'landpoints', dimid)
  endif

  if (status .eq. nf90_noerr) then
     call cropdist_outsib(nfiles, &
          fnamesin, outfile, cropfile, regionfile)
  else
     call cropdist_outgrid(nfiles, &
          fnamesin, outfile, cropfile, regionfile)
  endif

  status = nf90_close(ncid)

end subroutine cropdist_output



!=========================================
subroutine cropdist_outsib(nfiles, &
      fnamesin, outfile, cropfile, regionfile)
!=========================================
!Subroutine controlling the sequoence of 
!   calls to redistribute crop yields.
!
!kdhaynes, 06/16

use sibinfo_module
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    fnamesin
character(len=256), intent(in) :: &
    outfile, cropfile, regionfile

!crop variables
integer :: nlonc, nlatc
integer, dimension(:), allocatable :: lonrefc, latrefc
real, dimension(:), allocatable :: lonc, latc
real, dimension(:,:), allocatable :: cropmap
real, dimension(:), allocatable :: cropsib

real, dimension(:), allocatable :: cropsib_region
real, dimension(:), allocatable :: cropsib_regiontot

!region variables
integer :: nregions
integer :: nlonr, nlatr
integer, dimension(:), allocatable :: lonrefr, latrefr
real, dimension(:), allocatable :: lonr, latr

integer*1, dimension(:,:), allocatable :: regionmap
real, dimension(:,:), allocatable :: regionmapf
real, dimension(:), allocatable :: regionsibf
logical :: use_regions

!netcdf variables
integer :: ncid, status
integer :: dimid, varid, respid, gppid

!sib4 variables
integer :: nsib, nlu, ntime, ntimetot
real*8 :: tconvert

real, dimension(:), allocatable :: lonsib, latsib
real*8, dimension(:), allocatable :: areasib

integer, dimension(:,:), allocatable :: lu_pref
real, dimension(:,:), allocatable :: lu_area

real, dimension(:,:,:), allocatable :: gpptemp, resptemp
real*8, dimension(:,:), allocatable :: gpplu, resplu
real*8, dimension(:,:), allocatable :: gppcrops, respcrops
real*8, dimension(:), allocatable :: gpppft, resppft

real*8, dimension(:,:), allocatable :: gppcrops_region, respcrops_region
real*8, dimension(:,:), allocatable :: imbalcrops_region

!crop respiration variables
integer :: nregionsgood
real*8 :: myratio
real*8 :: lesscrop, morecrop
real*8 :: lesscropless, lessthresh
real*8, dimension(:), allocatable :: resptot_gfrac
real*8, dimension(:,:), allocatable :: resptot_rfrac
real*8, dimension(:,:), allocatable :: resp_crop
real*8, dimension(:), allocatable :: respctot
real*8 :: gppcrops_tot, respcrops_tot, respctot_tot

!local variables
integer :: f, l, n
integer :: pref, pnum, pnumc
integer :: rref
character(len=20) :: myrespname

!parameters
real*8, parameter :: aconvert=12./1.E21  !convert from umol C to GtC
logical, parameter :: print_cropmapcheck=.false.
logical, parameter :: print_croprespcheck=.false.
logical, parameter :: print_cropregion=.false.
logical, parameter :: print_croppft=.true.
logical, parameter :: print_gppcheck=.false.

!-------------------------------------------
!Open the region map
use_regions=.false.
nregions=1
if (regionfile .ne. '') use_regions=.true.
if (use_regions) then
   status = nf90_open(trim(regionfile), nf90_nowrite, ncid)
  
   status = nf90_inq_dimid(ncid,'lon',dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlonr)
   allocate(lonr(nlonr))
   status = nf90_inq_varid(ncid,'lon',varid)
   status = nf90_get_var(ncid,varid,lonr)

   status = nf90_inq_dimid(ncid,'lat',dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlatr)
   allocate(latr(nlatr))
   status = nf90_inq_varid(ncid,'lat',varid)
   status = nf90_get_var(ncid,varid,latr)

   allocate(regionmap(nlonr,nlatr))
   status = nf90_inq_varid(ncid,'data',varid)
   status = nf90_get_var(ncid,varid,regionmap)
   if (status .ne. nf90_noerr) then
      print(fa),'Error Getting Regional Map.'
      print(fa),'Stopping.'
      STOP
   endif
   status = nf90_close(ncid)

   !remove ocean regions
   where(regionmap .eq. 0)  regionmap=1
   where(regionmap .eq. 12) regionmap=8
   where(regionmap .eq. 13) regionmap=9
   where(regionmap .eq. 14) regionmap=3
   where(regionmap .eq. 15) regionmap=10
   where(regionmap .eq. 16) regionmap=1
   where(regionmap .eq. 17) regionmap=2
   where(regionmap .eq. 18) regionmap=5
   where(regionmap .eq. 19) regionmap=6
   where(regionmap .eq. 20) regionmap=10
   where(regionmap .eq. 21) regionmap=9
   where(regionmap .eq. 22) regionmap=6

   nregions=maxval(regionmap)
   if (nregions .ne. 11) then
      print(fa),'Expecting 11 Land Regions.  Stopping'
      stop
   endif
endif

!---------------------
!Open the crop map
status = nf90_open(trim(cropfile), nf90_nowrite, ncid)
  
status = nf90_inq_dimid(ncid,'nlon',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlonc)
allocate(lonc(nlonc))
status = nf90_inq_varid(ncid,'lon',varid)
status = nf90_get_var(ncid,varid,lonc)

status = nf90_inq_dimid(ncid,'nlat',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlatc)
allocate(latc(nlatc))
status = nf90_inq_varid(ncid,'lat',varid)
status = nf90_get_var(ncid,varid,latc)

allocate(cropmap(nlonc,nlatc))
status = nf90_inq_varid(ncid,'mean',varid)
status = nf90_get_var(ncid,varid,cropmap)
if (status .ne. nf90_noerr) then
   print(fa),'Error Getting Crop Map.'
   print(fa),'Stopping.'
   STOP
endif
status = nf90_close(ncid)

!--------------------
!Loop through processing files
do f=1,nfiles
   !open files
   status = nf90_open(trim(fnamesin(f)), nf90_nowrite, ncid)
   IF (status .ne. nf90_noerr) then
       print(fa),'Error Opening File: '
       print(fa),'  ',trim(fnamesin(f))
       print(fa),'Stopping.'
       STOP
   ENDIF

   IF (f .eq. 1) THEN
       !...get dimensions
       status = nf90_inq_dimid(ncid,trim(nsibnamed),dimid)
       status = nf90_inquire_dimension(ncid,dimid,len=nsib)

       status = nf90_inq_dimid(ncid,trim(nluname),dimid)
       status = nf90_inquire_dimension(ncid,dimid,len=nlu)

       status = nf90_inq_dimid(ncid,'time',dimid)
       status = nf90_inquire_dimension(ncid,dimid,len=ntime)
       call calc_tconvert(ntime,tconvert)
       print(fae),'   Time Conversion Factor: ',tconvert
       ntimetot = 0

       !...get lat/lon/area
       allocate(lonsib(nsib),latsib(nsib))
       status = nf90_inq_varid(ncid,trim(lonname),varid)
       status = nf90_get_var(ncid,varid,lonsib)
       status = nf90_inq_varid(ncid,trim(latname),varid)
       status = nf90_get_var(ncid,varid,latsib)

       allocate(areasib(nsib))
       call calc_sibarea(nsib,lonsib,latsib,.false.,areasib)

       !...get PFT information
       allocate(lu_pref(nsib,nlu))
       status = nf90_inq_varid(ncid,trim(lprefname),varid)
       status = nf90_get_var(ncid,varid,lu_pref)
       IF (status .NE. nf90_noerr) THEN
           print(fa),'Error getting PFT references.'
           print(fa),'Stopping.'
             STOP
       ENDIF

       allocate(lu_area(nsib,nlu))
       status = nf90_inq_varid(ncid,trim(lareaname),varid)
       status = nf90_get_var(ncid,varid,lu_area)
       IF (status .NE. nf90_noerr) THEN
          print(fa),'Error getting PFT areal contributions.'
          print(fa),'Stopping.'
          STOP
       ENDIF

       !...setup gpp/resp arrays
       allocate(gpplu(nsib,nlu),resplu(nsib,nlu))
       gpplu(:,:) = 0.
       resplu(:,:) = 0.

       !...get respiration type (total vs. balanced)
       status = nf90_inq_varid(ncid,trim(respbname),respid)
       IF (status .EQ. nf90_noerr) THEN
          print(fa),'   Using Balanced Respiration.'
          myrespname=respbname
       ELSE
          status = nf90_inq_varid(ncid,trim(respname),respid)
          IF (status .EQ. nf90_noerr) THEN
             print(fa),'   Using Total Respiration (Not Balanced).'
             myrespname=respbname
          ELSE
              print(fa),'Error Getting Respiration.'
              print(fa),'Stopping.'
              STOP
          ENDIF
       ENDIF

   ENDIF !first file

   !Get crop gpp and respiration
   status = nf90_inq_dimid(ncid,'time',dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=ntime)
   ntimetot = ntimetot + ntime

   allocate(gpptemp(nsib,nlu,ntime))
   status = nf90_inq_varid(ncid,trim(gppname),gppid)
   status = nf90_get_var(ncid,gppid,gpptemp)
   IF (status .NE. nf90_noerr) THEN
       print(fa),'Error Getting GPP.'
       print(fa),'Stopping.'
       STOP
   ENDIF

   allocate(resptemp(nsib,nlu,ntime))
   status = nf90_inq_varid(ncid,trim(myrespname),respid)
   status = nf90_get_var(ncid,respid,resptemp)
   IF (status .NE. nf90_noerr) THEN
       print(fa),'Error Getting Respiration.'
       print(fa),'Stopping.'
       STOP
   ENDIF

    DO l=1,nlu
       DO n=1,nsib
         gpplu(n,l) = gpplu(n,l) + dble(SUM(gpptemp(n,l,:)))
         resplu(n,l) = resplu(n,l) + dble(SUM(resptemp(n,l,:)))
        ENDDO
    ENDDO

    status = nf90_close(ncid)
    deallocate(gpptemp,resptemp)

ENDDO !f=1,nfiles

!calculate total fluxes
allocate(gppcrops(nsib,ncrop),respcrops(nsib,ncrop))
allocate(gpppft(npft),resppft(npft))
gppcrops(:,:) = 0.
respcrops(:,:) = 0.
gpppft(:) = 0.
resppft(:) = 0.

pnumc = pftnum(npftcropstart) - 1
DO n=1,nsib
   DO l=1,nlu
      pref = lu_pref(n,l)
      IF (pref .GT. 0) THEN
          pnum = pftnum(pref)
          gpppft(pnum) = gpppft(pnum) &
              + gpplu(n,l)*lu_area(n,l) &
              * areasib(n)*tconvert

          resppft(pnum) = resppft(pnum) &
              + resplu(n,l)*lu_area(n,l) &
              * areasib(n)*tconvert
      ENDIF
      IF (pref .GE. npftcropstart) THEN
          pnum = pftnum(pref) - pnumc
          gppcrops(n,pnum) = gppcrops(n,pnum) &
             + gpplu(n,l)*lu_area(n,l) &
             * areasib(n)*tconvert
          respcrops(n,pnum) = respcrops(n,pnum) &
             + resplu(n,l)*lu_area(n,l) &
             * areasib(n)*tconvert
      ENDIF
   ENDDO !l=1,nlu
ENDDO !n=1,nsib

print(fai8),'   Total Timesteps : ',ntimetot
print(fa),''
print(fa),  '   Carbon Fluxes (Gt C):'
print(fa2f),'   GPP TOT  : ', &
      SUM(gpppft)*aconvert
print(fa2f),'   RESP TOT : ', &
      SUM(resppft)*aconvert
print(fa2f),'   GPP CROP : ', &
      SUM(gppcrops)*aconvert
print(fa2f),'   RESP CROP: ', &
      SUM(respcrops)*aconvert

IF (print_gppcheck) THEN
    print(fa),''
    print(fa2f),'   GPP CROP MIN/MAX : ', &
          minval(gppcrops)*aconvert,maxval(gppcrops)*aconvert
    print(fa2f),'   RESP CROP MIN/MAX: ', &
          minval(respcrops)*aconvert,maxval(respcrops)*aconvert
ENDIF

!setup the regional map
allocate(regionsibf(nsib))
regionsibf(:) = 1.
if (use_regions) then
    !...ungrid the regional map, 
    !.....putting it onto sib vector
    allocate(lonrefr(nsib),latrefr(nsib))
    allocate(regionmapf(nlonr,nlatr))
    lonrefr(:) = 0
    latrefr(:) = 0
    regionmapf = real(regionmap)
    call ungrid_dataf( &
         nlonr, nlatr, lonr, latr, regionmapf, &
         nsib, lonsib, latsib, &
         lonrefr, latrefr, regionsibf)
    IF (print_cropmapcheck) THEN
        print(fa),''
        print(fa),'   Check Region Map:'
        print(f2af),'      In Min/Max Region: ', &
               minval(regionmapf),'   ',maxval(regionmapf)
        print(f2af),'      Out Min/Max Region:', &
               minval(regionsibf),'   ',maxval(regionsibf)
    ENDIF
endif

!setup the crop map
!..ungrid, putting it onto sib vector
allocate(lonrefc(nsib),latrefc(nsib))
lonrefc(:) = 0
latrefc(:) = 0
allocate(cropsib(nsib))
cropsib(:) = 0.
call ungrid_dataf( &
     nlonc, nlatc, lonc, latc, cropmap, &
     nsib, lonsib, latsib, &
     lonrefc, latrefc, cropsib)
IF (print_cropmapcheck) THEN
   print(fa),''
   print(fa),'   Check Crop Map:'
   print(f2af),'      In Min/Max Crop : ', &
            minval(cropmap),'   ',maxval(cropmap)
   print(f2af),'      Out Min/Max Crop: ', &
            minval(cropsib),'   ',maxval(cropsib)
ENDIF

!...calculate crop map regional totals
allocate(cropsib_region(nsib))
allocate(cropsib_regiontot(nregions))
cropsib_regiontot(:) = 0.
DO n=1,nsib
   rref = nint(regionsibf(n))
   cropsib_regiontot(rref) = &
        cropsib_regiontot(rref) + &
        cropsib(n)
ENDDO

!calculate the crop imbalance by regions,
!....and the crop map by regions
allocate(gppcrops_region(nregions,ncrop))
gppcrops_region(:,:) = 0.0
allocate(respcrops_region(nregions,ncrop))
respcrops_region(:,:) = 0.0
allocate(imbalcrops_region(nregions,ncrop))
DO n=1,nsib
   rref = nint(regionsibf(n))
   gppcrops_region(rref,:) = gppcrops_region(rref,:) + gppcrops(n,:) 
   respcrops_region(rref,:) = respcrops_region(rref,:) + respcrops(n,:)

   cropsib_region(n) = cropsib(n)/cropsib_regiontot(rref)
ENDDO
imbalcrops_region = gppcrops_region - respcrops_region

IF (print_cropregion) THEN
   print(fa),''
   print(fa),'   Crops By Region (Gt C):'
   print(fa),'                   GPP        RESP     IMBAL'
   DO f=1,nregions
      print('(a,i2,3f10.4)'),'      Region ',f, &
           SUM(gppcrops_region(f,:))*aconvert, &
           SUM(respcrops_region(f,:))*aconvert, &
           SUM(imbalcrops_region(f,:))*aconvert
   ENDDO
   print('(a,3f10.4)'),    '      Total    ', &
           SUM(gppcrops_region)*aconvert,  &
           SUM(respcrops_region)*aconvert, &
           SUM(imbalcrops_region)*aconvert
ENDIF

IF (print_croppft) THEN
   print(fa),''
   print(fa),'   Crops By Type (Gt C):'
   print(fa),'                GPP      RESP       IMBAL'
   DO n=1,ncrop
      print('(3a,3f10.4)'),'      ',&
          pftnames(pftnum(npftcropstart)+n-1), &
          '   ', &
          SUM(gppcrops_region(:,n))*aconvert, &
          SUM(respcrops_region(:,n))*aconvert, &
          SUM(imbalcrops_region(:,n))*aconvert
   ENDDO
   print('(a,3f10.4)'),    '      Total ', &
          SUM(gppcrops_region)*aconvert, &
          SUM(respcrops_region)*aconvert, &
          SUM(imbalcrops_region)*aconvert
ENDIF

!check the imbalance, remove unproductive regions
!  where the imbalance is negative
IF (SUM(imbalcrops_region) .LE. 0.0) THEN
   print(fa),''
   print(fa),'   Net Crop GPP Lower Than RE!'
   print(fa),'   Stopping The Calculation!'
   STOP
ELSEIF (minval(imbalcrops_region) .LT. 0.0) THEN
   print(fa),''
   print(fa),'    NOTE: In Some Regions RESP > GPP'
   lesscrop = 0.0
   morecrop = 0.0
   DO f=1,nregions
      DO n=1,ncrop
         IF (imbalcrops_region(f,n) .LT. 0.0) THEN
            lesscrop = lesscrop + abs(imbalcrops_region(f,n))
            imbalcrops_region(f,n) = 0.0
         ELSE
            morecrop = morecrop + imbalcrops_region(f,n)
         ENDIF
      ENDDO
   ENDDO

   lessthresh=lesscrop*0.25
   nregionsgood = 0
   DO f=1,nregions
      DO n=1,ncrop
         IF (imbalcrops_region(f,n) .GT. lessthresh) THEN
             nregionsgood = nregionsgood + 1
         ENDIF
     ENDDO
   ENDDO

   IF (nregionsgood .LT. 1) THEN
      print(fa),'   All Regions Have Too Much Respiration!'
      print(fae),'   Less Crop Needed: ',lesscrop
      print(fae),'   More Crop Have  : ',morecrop
      print(fa),'   Stopping The Calculation!'
      RETURN
   ENDIF

   lesscropless = lesscrop / dble(nregionsgood)
   where(imbalcrops_region .GT. lessthresh) &
         imbalcrops_region = imbalcrops_region - lesscropless
   IF (minval(imbalcrops_region) .LT. 0.0) THEN
       print*,'Still Negative Regions!'
       print*,'Stopping.'
       STOP
   ENDIF
ENDIF

!calculate the respiration from harvested crops
allocate(resptot_gfrac(ncrop))
DO n=1,ncrop
   resptot_gfrac(n)=SUM(imbalcrops_region(:,n))*(1.-regionfrac)
ENDDO

allocate(resptot_rfrac(nregions,ncrop))
resptot_rfrac(:,:) = imbalcrops_region*regionfrac

allocate(resp_crop(nsib,ncrop))
resp_crop = 0.
DO n=1,nsib
   rref=nint(regionsibf(n))

   !regional contributions (same region)
   resp_crop(n,:) = cropsib_region(n)*resptot_rfrac(rref,:) &
                    /areasib(n)/tconvert/dble(ntimetot)

   !global contributions (i.e. export)
   resp_crop(n,:) = resp_crop(n,:) + &
                    cropsib(n)*resptot_gfrac(:) &
                    /areasib(n)/tconvert/dble(ntimetot)
ENDDO

IF (print_croprespcheck) THEN
    print(fa),''
    print(fa),  '   Crop Respiration Check (min/max): '
    print(fa),  '      Respiration Min/Max:'
    print(f2ae),'       ',minval(resp_crop),'   ',maxval(resp_crop)
    print(fa),  '      RespTot Region Min/Max:'
    print(f2ae),'       ',minval(resptot_rfrac),'   ',maxval(resptot_rfrac)
    print(fa),  '      CropSib Region Min/Max:'
    print(f2ae),'       ',minval(cropsib_region),'   ',maxval(cropsib_region)
    print(fa),  '      RespTot Global Min/Max:'
    print(f2ae),'       ',minval(resptot_gfrac),'   ',maxval(resptot_gfrac)
    print(fa),  '      CropSib Global Min/Max:'
    print(f2ae),'       ',minval(cropsib),'   ',maxval(cropsib)
    print(fa),  '      Area Min/Max'
    print(f2ae),'       ',minval(areasib),'   ',maxval(areasib)
ENDIF

!check the newly calculated crop respiration
allocate(respctot(ncrop))
respctot(:) = 0.
DO n=1,nsib
   respctot(:) = respctot(:) + resp_crop(n,:) &
                 *areasib(n)*tconvert*dble(ntimetot)
ENDDO

myratio = (SUM(gppcrops) - SUM(respcrops))/SUM(respctot)
resp_crop = resp_crop*myratio
respctot(:) = 0.
DO n=1,nsib
   respctot(:) = respctot(:) + resp_crop(n,:) &
                 *areasib(n)*tconvert*dble(ntimetot)
ENDDO

print(fa),''
print(fa),'   Redistributed Respiration (Gt C):'
print(fa),'        RESP          DIST          TOT'
print(fa3f),'  ',SUM(respcrops)*aconvert, &
      SUM(respctot)*aconvert, (SUM(respcrops)+SUM(respctot))*aconvert
print(fa),''
print(fa),'   Updated Fluxes (Gt C):'
print(fa),'       GPP_TOT       RESP_TOT     RESP_TOT_WDIST'
print(fa3f),'  ',SUM(gpppft)*aconvert, &
      SUM(resppft)*aconvert, &
      (SUM(resppft) + SUM(respctot))*aconvert
print(fa),''
print(fa),'    Crop Carbon Balance (Gt C):'
print(fa),'       GPP_CROP      RESP_CROP     %DIFF'
gppcrops_tot = SUM(gppcrops)
respcrops_tot = SUM(respcrops)
respctot_tot = SUM(respctot)
print(fa3f),' ',gppcrops_tot*aconvert, &
      (respcrops_tot + respctot_tot)*aconvert, &
      abs(gppcrops_tot - (respcrops_tot + respctot_tot)) &
       /gppcrops_tot*100.

!write out the respiration to the output file
call cropdist_writesib(nsib, ncrop, lonsib, latsib, &
                       resp_crop, outfile)


end subroutine cropdist_outsib


!----------------------------
subroutine cropdist_writesib( &
   nsibin, ncropin, lonsib, latsib, &
   resp_crop, outfile)

use sibinfo_module
use netcdf
implicit none

!input variables
integer, intent(in) :: nsibin, ncropin
real, dimension(nsibin), intent(in) :: lonsib, latsib
real*8, dimension(nsibin,ncropin), intent(in) :: resp_crop
character(len=256), intent(in) :: outfile

!netcdf variables
integer :: status, ncid
integer :: lonid, latid, cnameid, varid
integer :: nsibdid, ncropdid, clendid

!info variables
character(len=3), dimension(ncropin) :: crop_names

!misc variables
integer :: n

!----------------------------
print(fa),''
print(fa), '  Creating File: '
print(f2a),'   ',trim(outfile)

status = nf90_create(trim(outfile),nf90_64bit_offset,ncid)
IF (status .ne. nf90_noerr) THEN
    print(fa),'Error Opening Crop Output File.'
    print(fa),'Stopping.'
    STOP
ENDIF

status = nf90_def_dim(ncid,trim(nsibnamed),nsibin,nsibdid)
status = nf90_def_dim(ncid,trim(ncropname),ncrop,ncropdid)
status = nf90_def_dim(ncid,'clen',3,clendid)

status = nf90_def_var(ncid,trim(lonname),nf90_float, &
         (/nsibdid/), lonid)
status = nf90_put_att(ncid,lonid,'long_name','SiB Point Longitude')
status = nf90_put_att(ncid,lonid,'units','degrees_east')

status = nf90_def_var(ncid,trim(latname),nf90_float, &
         (/nsibdid/), latid)
status = nf90_put_att(ncid,latid,'long_name','SiB Point Latitude')
status = nf90_put_att(ncid,latid,'units','degrees_north')

status = nf90_def_var(ncid,'crop_names',nf90_char, &
         (/clendid,ncropdid/), cnameid)

status = nf90_def_var(ncid,trim(respcname),nf90_float, &
          (/nsibdid,ncropdid/), varid)
IF (status .ne. nf90_noerr) THEN
     print(fa),'Error Defining Crop Respiration In Output File.'
     print(fa),'Stopping.'
     STOP
ENDIF
status = nf90_put_att(ncid,varid,'long_name',trim(respclong))
status = nf90_put_att(ncid,varid,'units',trim(respcunits))
status = nf90_enddef(ncid)

status = nf90_put_var(ncid,lonid,lonsib)
status = nf90_put_var(ncid,latid,latsib)

DO n=1,ncropin
   crop_names(n) = pftnames(pftnum(npftcropstart)+n-1)
ENDDO

status = nf90_put_var(ncid,cnameid,crop_names)
IF (status .ne. nf90_noerr) THEN
   print(fa),'Error Writing Out Crop Names.'
   print(fa),'Stopping.'
   STOP
ENDIF

status = nf90_put_var(ncid,varid,resp_crop)
IF (status .ne. nf90_noerr) THEN
   print(fa),'Error Writing Out Crop Respiration.'
   print(fa),'Stopping.'
   STOP
ENDIF
status = nf90_close(ncid)


end subroutine cropdist_writesib



!=========================================
subroutine cropdist_outgrid(nfiles, &
      fnamesin, outfile, cropfile, regionfile)
!=========================================
!Subroutine controlling the sequoence of 
!   calls to redistribute crop yields.
!
!kdhaynes, 06/16

  use netcdf
  use sibinfo_module
  implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
    fnamesin
character(len=256), intent(in) :: &
    outfile, cropfile, regionfile

!crop variables
integer :: nlonc, nlatc
real, dimension(:,:), allocatable :: cropmap
real, dimension(:,:), allocatable :: cropmap_region
real, dimension(:), allocatable :: cropmap_regiontot

!region variables
integer :: nregions
integer :: nlonr, nlatr

integer*1, dimension(:,:), allocatable :: regionmap
logical :: use_regions

!netcdf variables
integer :: ncid, status
integer :: dimid, varid, respid, gppid

!sib4 variables
integer :: nlon, nlat, nlu, mynpft, ntime, ntimetot
real*8 :: tconvert

real, dimension(:), allocatable :: lon, lat
real*8, dimension(:,:), allocatable :: area

integer, dimension(:,:,:), allocatable :: lu_pref
real, dimension(:,:,:), allocatable :: lu_area

real, dimension(:,:,:,:), allocatable :: gpptemp, resptemp
real*8, dimension(:,:,:), allocatable :: gpplu, resplu
real*8, dimension(:,:,:), allocatable :: gppcrops, respcrops
real*8, dimension(:), allocatable :: gpppft, resppft

real*8, dimension(:,:), allocatable :: gppcrops_region, respcrops_region
real*8, dimension(:,:), allocatable :: imbalcrops_region

!crop respiration variables
integer :: nregionsgood
real*8 :: myratio
real*8 :: lesscrop, morecrop
real*8 :: lesscropless, lessthresh
real*8, dimension(:), allocatable :: resptot_gfrac
real*8, dimension(:,:), allocatable :: resptot_rfrac
real*8, dimension(:,:,:), allocatable :: resp_crop
real*8, dimension(:), allocatable :: respctot
real*8 :: gppcrops_tot, respcrops_tot, respctot_tot

!local variables
integer :: f, l, i, j, n
integer :: pref, pnum, pnumc
integer :: rref
character(len=20) :: myrespname

!parameters
real*8, parameter :: aconvert=12./1.E21  !convert from umol C to GtC
logical, parameter :: print_cropmapcheck=.false.
logical, parameter :: print_croprespcheck=.false.
logical, parameter :: print_cropregion=.false.
logical, parameter :: print_croppft=.true.
logical, parameter :: print_gppcheck=.false.

!-------------------------------------------
!Open the region map
use_regions=.false.
nregions=1
if (regionfile .ne. '') use_regions=.true.
if (use_regions) then
   status = nf90_open(trim(regionfile), nf90_nowrite, ncid)
  
   status = nf90_inq_dimid(ncid,'lon',dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlonr)

   status = nf90_inq_dimid(ncid,'lat',dimid)
   status = nf90_inquire_dimension(ncid,dimid,len=nlatr)

   allocate(regionmap(nlonr,nlatr))
   status = nf90_inq_varid(ncid,'data',varid)
   status = nf90_get_var(ncid,varid,regionmap)
   if (status .ne. nf90_noerr) then
      print(fa),'Error Getting Regional Map.'
      print(fa),'Stopping.'
      STOP
   endif
   status = nf90_close(ncid)

   !remove ocean regions
   where(regionmap .eq. 0)  regionmap=1
   where(regionmap .eq. 12) regionmap=8
   where(regionmap .eq. 13) regionmap=9
   where(regionmap .eq. 14) regionmap=3
   where(regionmap .eq. 15) regionmap=10
   where(regionmap .eq. 16) regionmap=1
   where(regionmap .eq. 17) regionmap=2
   where(regionmap .eq. 18) regionmap=5
   where(regionmap .eq. 19) regionmap=6
   where(regionmap .eq. 20) regionmap=10
   where(regionmap .eq. 21) regionmap=9
   where(regionmap .eq. 22) regionmap=6

   nregions=maxval(regionmap)
   if (nregions .ne. 11) then
      print(fa),'Expecting 11 Land Regions.  Stopping'
      stop
   endif
endif

!---------------------
!Open the crop map
status = nf90_open(trim(cropfile), nf90_nowrite, ncid)
  
status = nf90_inq_dimid(ncid,'nlon',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlonc)

status = nf90_inq_dimid(ncid,'nlat',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlatc)

allocate(cropmap(nlonc,nlatc))
status = nf90_inq_varid(ncid,'mean',varid)
status = nf90_get_var(ncid,varid,cropmap)
if (status .ne. nf90_noerr) then
   print(fa),'Error Getting Crop Map.'
   print(fa),'Stopping.'
   STOP
endif
status = nf90_close(ncid)

!--------------------
!Loop through processing files
   do f=1,nfiles
      !open files
      status = nf90_open(trim(fnamesin(f)), nf90_nowrite, ncid)
      IF (status .ne. nf90_noerr) then
         print(fa),'Error Opening File: '
         print(fa),'  ',trim(fnamesin(f))
         print(fa),'Stopping.'
         STOP
      ENDIF

      IF (f .eq. 1) THEN
          !...get dimensions
          status = nf90_inq_dimid(ncid,'lon',dimid)
          status = nf90_inquire_dimension(ncid,dimid,len=nlon)
          if ((nlon .ne. nlonc) .or. (nlon .ne. nlonr)) then
             print*,'Mismatching SiB4 and Crop Maps.'
             print*,'Stopping.'
             STOP
          ENDIF
          
          status = nf90_inq_dimid(ncid,'lat',dimid)
          status = nf90_inquire_dimension(ncid,dimid,len=nlat)
          IF ((nlat .ne. nlatc) .or. (nlat .ne. nlatr)) THEN
             print*,'Mismatching SiB4 and Crop Maps.'
             print*,'Stopping.'
             STOP
          ENDIF
          
          status = nf90_inq_dimid(ncid,'nlu',dimid)
          status = nf90_inquire_dimension(ncid,dimid,len=nlu)

          status = nf90_inq_dimid(ncid,'npft',dimid)
          status = nf90_inquire_dimension(ncid,dimid,len=mynpft)
          IF (mynpft .ne. npft) THEN
             print*,'Mismatching SiB4 and Crop Maps.'
             print*,'   SiB4/Crop PFTs: ',npft,mynpft
          ENDIF

          status = nf90_inq_dimid(ncid,'time',dimid)
          status = nf90_inquire_dimension(ncid,dimid,len=ntime)
          call calc_tconvert(ntime,tconvert)
          print(fae),'   Time Conversion Factor: ',tconvert
          ntimetot = 0

          !...get lat/lon/area
          allocate(lon(nlon),lat(nlat))
          status = nf90_inq_varid(ncid,'lon',varid)
          status = nf90_get_var(ncid,varid,lon)
          status = nf90_inq_varid(ncid,'lat',varid)
          status = nf90_get_var(ncid,varid,lat)

          allocate(area(nlon,nlat))
          call calc_gridarea(nlon, nlat, lon, lat, area)

          !...get PFT information
          allocate(lu_pref(nlon, nlat, nlu))
          status = nf90_inq_varid(ncid,'lu_pref',varid)
          status = nf90_get_var(ncid,varid,lu_pref)
          IF (status .NE. nf90_noerr) THEN
             print(fa),'Error getting PFT references.'
             print(fa),'Stopping.'
             STOP
          ENDIF

          allocate(lu_area(nlon, nlat, nlu))
          status = nf90_inq_varid(ncid,'lu_area',varid)
          status = nf90_get_var(ncid,varid,lu_area)
          IF (status .NE. nf90_noerr) THEN
             print(fa),'Error getting PFT areal contributions.'
             print(fa),'Stopping.'
             STOP
          ENDIF

          !...setup gpp/resp arrays
          allocate(gpplu(nlon,nlat,nlu),resplu(nlon,nlat,nlu))
          gpplu(:,:,:) = 0.
          resplu(:,:,:) = 0.

          !...get respiration name
          status = nf90_inq_varid(ncid,trim(respbname),respid)
          IF (status .EQ. nf90_noerr) THEN
             print(fa),'   Using Balanced Respiration.'
             myrespname=respbname
          ELSE
             status = nf90_inq_varid(ncid,trim(respname),respid)
             IF (status .EQ. nf90_noerr) THEN
                print(fa),'   Using Total Respiration (Not Balanced).'
                myrespname=respbname
             ELSE
                print(fa),'Error Getting Respiration.'
                print(fa),'Stopping.'
                STOP
             ENDIF
          ENDIF
          
      ENDIF !first file

      !get crop gpp and respiration
      status = nf90_inq_dimid(ncid,'time',dimid)
      status = nf90_inquire_dimension(ncid,dimid,len=ntime)
      ntimetot = ntimetot + ntime

      allocate(gpptemp(nlon,nlat,nlu,ntime))
      status = nf90_inq_varid(ncid,trim(gppname),gppid)
      status = nf90_get_var(ncid,gppid,gpptemp)
      IF (status .NE. nf90_noerr) THEN
          print(fa),'Error Getting Asismilation.'
          print(fa),'Stopping.'
          STOP
      ENDIF

      allocate(resptemp(nlon,nlat,nlu,ntime))
      status = nf90_inq_varid(ncid,trim(myrespname),respid)
      status = nf90_get_var(ncid,respid,resptemp)
      IF (status .NE. nf90_noerr) THEN
           print(fa),'Error Getting Resp.'
           print(fa),'Stopping.'
           STOP
      ENDIF

      DO l=1,nlu
         DO j=1,nlat
            DO i=1,nlon
               gpplu(i,j,l) = gpplu(i,j,l) + dble(SUM(gpptemp(i,j,l,:)))
               resplu(i,j,l) = resplu(i,j,l) + dble(SUM(resptemp(i,j,l,:)))

            ENDDO
         ENDDO
      ENDDO

      !close files
      status = nf90_close(ncid)
      deallocate(gpptemp,resptemp)

    enddo !f=1,nfiles

    !calculate total fluxes
    allocate(gppcrops(nlon,nlat,ncrop),respcrops(nlon,nlat,ncrop))
    allocate(gpppft(npft),resppft(npft))
    gppcrops(:,:,:) = 0.
    respcrops(:,:,:) = 0.
    gpppft(:) = 0.
    resppft(:) = 0.

    pnumc = pftnum(npftcropstart) - 1
    DO l=1,nlu
       DO j=1,nlat
          DO i=1,nlon
             pref = lu_pref(i,j,l)
             IF (pref .GT. 0) THEN
                 pnum = pftnum(pref)
                 gpppft(pnum) = gpppft(pnum) &
                     + gpplu(i,j,l)*lu_area(i,j,l) &
                     * area(i,j)*tconvert

                 resppft(pnum) = resppft(pnum) &
                     + resplu(i,j,l)*lu_area(i,j,l) &
                     * area(i,j)*tconvert
             ENDIF
             IF (pref .GE. npftcropstart) THEN
                 pnum = pftnum(pref) - pnumc
                 gppcrops(i,j,pnum) = gppcrops(i,j,pnum) &
                    + gpplu(i,j,l)*lu_area(i,j,l) &
                    * area(i,j)*tconvert

                 respcrops(i,j,pnum) = respcrops(i,j,pnum) &
                    + resplu(i,j,l)*lu_area(i,j,l) &
                    * area(i,j)*tconvert
             ENDIF
          ENDDO !i=1,nlon
       ENDDO !j=1,nlat
    ENDDO !l=1,nlu
    
    print(fai8),'   Total Timesteps : ',ntimetot
    print(fa),''
    print(fa),  '   Carbon Fluxes (Gt C):'
    print(fa2f),'   GPP TOT  : ', &
          SUM(gpppft)*aconvert
    print(fa2f),'   RESP TOT : ', &
          SUM(resppft)*aconvert
    print(fa2f),'   GPP CROP : ', &
          SUM(gppcrops)*aconvert
    print(fa2f),'   RESP CROP: ', &
          SUM(respcrops)*aconvert

    IF (print_gppcheck) THEN
       print(fa),''
       print(fa2f),'   GPP CROP MIN/MAX : ', &
             minval(gppcrops)*aconvert,maxval(gppcrops)*aconvert
       print(fa2f),'   RESP CROP MIN/MAX: ', &
             minval(respcrops)*aconvert,maxval(respcrops)*aconvert
    ENDIF

    !...calculate crop map regional totals
    allocate(cropmap_regiontot(nregions))
    cropmap_regiontot(:) = 0.
    DO j=1,nlat
       DO i=1,nlon
          rref = regionmap(i,j)
          cropmap_regiontot(rref) = &
            cropmap_regiontot(rref) + &
            cropmap(i,j)
       ENDDO
    ENDDO

    !calculate the crop imbalance by regions,
    !....and the crop map by regions
    allocate(cropmap_region(nlon,nlat))
    allocate(gppcrops_region(nregions,ncrop))
    gppcrops_region(:,:) = 0.0
    allocate(respcrops_region(nregions,ncrop))
    respcrops_region(:,:) = 0.0
    allocate(imbalcrops_region(nregions,ncrop))

    DO j=1,nlat
       DO i=1,nlon
          rref = regionmap(i,j)
          gppcrops_region(rref,:) = gppcrops_region(rref,:) + gppcrops(i,j,:) 
          respcrops_region(rref,:) = respcrops_region(rref,:) + respcrops(i,j,:)

          cropmap_region(i,j) = cropmap(i,j)/cropmap_regiontot(rref)
       ENDDO
    ENDDO
    imbalcrops_region = gppcrops_region - respcrops_region

    IF (print_cropregion) THEN
       print(fa),''
       print(fa),'   Crops By Region (Gt C):'
       print(fa),'                   GPP        RESP     IMBAL'
       DO f=1,nregions
          print('(a,i2,3f10.4)'),'      Region ',f, &
               SUM(gppcrops_region(f,:))*aconvert, &
               SUM(respcrops_region(f,:))*aconvert, &
               SUM(imbalcrops_region(f,:))*aconvert
       ENDDO
       print('(a,3f10.4)'),    '      Total    ', &
               SUM(gppcrops_region)*aconvert,  &
               SUM(respcrops_region)*aconvert, &
               SUM(imbalcrops_region)*aconvert
    ENDIF

    IF (print_croppft) THEN
       print(fa),''
       print(fa),'   Crops By Type (Gt C):'
       print(fa),'                GPP      RESP       IMBAL'
       DO n=1,ncrop
          print('(3a,3f10.4)'),'      ',&
              pftnames(pftnum(npftcropstart)+n-1), &
              '   ', &
              SUM(gppcrops_region(:,n))*aconvert, &
              SUM(respcrops_region(:,n))*aconvert, &
              SUM(imbalcrops_region(:,n))*aconvert
       ENDDO
       print('(a,3f10.4)'),    '      Total ', &
              SUM(gppcrops_region)*aconvert, &
              SUM(respcrops_region)*aconvert, &
              SUM(imbalcrops_region)*aconvert
    ENDIF

    !check the imbalance, remove unproductive regions
    !  where the imbalance is negative
    IF (SUM(imbalcrops_region) .LE. 0.0) THEN
       print(fa),''
       print(fa),'   Net Crop GPP Lower Than RE!'
       print(fa),'    Stopping The Calculation!'
       RETURN
    ELSEIF (minval(imbalcrops_region) .LT. 0.0) THEN
       print(fa),''
       print(fa),'    NOTE: In Some Regions RESP > GPP'

       lesscrop = 0.0
       morecrop = 0.0
       DO f=1,nregions
          DO n=1,ncrop
             IF (imbalcrops_region(f,n) .LT. 0.0) THEN
                lesscrop = lesscrop + abs(imbalcrops_region(f,n))
                imbalcrops_region(f,n) = 0.0
             ELSE
                morecrop = morecrop + imbalcrops_region(f,n)
             ENDIF
          ENDDO
       ENDDO

       lessthresh=lesscrop*0.25
       nregionsgood = 0
       DO f=1,nregions
          DO n=1,ncrop
             IF (imbalcrops_region(f,n) .GT. lessthresh) THEN
                 nregionsgood = nregionsgood + 1
             ENDIF
         ENDDO
       ENDDO

       IF (nregionsgood .LT. 1) THEN
          print(fa),'   All Regions Have Too Much Respiration!'
          print(fae),'   Less Crop Needed: ',lesscrop
          print(fae),'   More Crop Have  : ',morecrop
          print(fa),'   Stopping The Calculation!'
          RETURN
       ENDIF

       lesscropless = lesscrop / dble(nregionsgood)
       where(imbalcrops_region .GT. lessthresh) &
            imbalcrops_region = imbalcrops_region - lesscropless
       IF (minval(imbalcrops_region) .LT. 0.0) THEN
           print*,'Still Negative Regions!'
           print*,'Stopping.'
           STOP
       ENDIF
    ENDIF

    !calculate the respiration from harvested crops
    allocate(resptot_gfrac(ncrop))
    DO n=1,ncrop
       resptot_gfrac(n)=SUM(imbalcrops_region(:,n))*(1.-regionfrac)
    ENDDO

    allocate(resptot_rfrac(nregions,ncrop))
    resptot_rfrac(:,:) = imbalcrops_region*regionfrac

    allocate(resp_crop(nlon,nlat,ncrop))
    resp_crop = 0.
    DO j=1,nlat
       DO i=1,nlon
          rref=regionmap(i,j)

          !regional contributions (same region)
          resp_crop(i,j,:) = cropmap_region(i,j)*resptot_rfrac(rref,:) &
                        /area(i,j)/tconvert/dble(ntimetot)

          !global contributions (i.e. export)
          resp_crop(i,j,:) = resp_crop(i,j,:) + &
                        cropmap(i,j)*resptot_gfrac(:) &
                        /area(i,j)/tconvert/dble(ntimetot)
       ENDDO
    ENDDO
    

    IF (print_croprespcheck) THEN
        print(fa),''
        print(fa),  '   Crop Respiration Check (min/max): '
        print(fa),  '      Respiration Min/Max:'
        print(f2ae),'       ',minval(resp_crop),'   ',maxval(resp_crop)
        print(fa),  '      RespTot Region Min/Max:'
        print(f2ae),'       ',minval(resptot_rfrac),'   ',maxval(resptot_rfrac)
        print(fa),  '      Crop Region Min/Max:'
        print(f2ae),'       ',minval(cropmap_region),'   ',maxval(cropmap_region)
        print(fa),  '      RespTot Global Min/Max:'
        print(f2ae),'       ',minval(resptot_gfrac),'   ',maxval(resptot_gfrac)
        print(fa),  '      Crop Global Min/Max:'
        print(f2ae),'       ',minval(cropmap),'   ',maxval(cropmap)
        print(fa),  '      Area Min/Max'
        print(f2ae),'       ',minval(area),'   ',maxval(area)
    ENDIF

    !check the newly calculated crop respiration
    allocate(respctot(ncrop))
    respctot(:) = 0.
    DO j=1,nlat
       DO i=1,nlon
          respctot(:) = respctot(:) + resp_crop(i,j,:) &
                     *area(i,j)*tconvert*dble(ntimetot)
       ENDDO
    ENDDO
       
    myratio = (SUM(gppcrops) - SUM(respcrops))/SUM(respctot)
    resp_crop = resp_crop*myratio
    respctot(:) = 0.
    DO j=1,nlat
       DO i=1,nlon
          respctot(:) = respctot(:) + resp_crop(i,j,:) &
                     *area(i,j)*tconvert*dble(ntimetot)
       ENDDO
    ENDDO
       
    print(fa),''
    print(fa),'   Redistributed Respiration (Gt C):'
    print(fa),'        RESP          DIST          TOT'
    print(fa3f),'  ',SUM(respcrops)*aconvert, &
          SUM(respctot)*aconvert, (SUM(respcrops)+SUM(respctot))*aconvert

    print(fa),''
    print(fa),'   Updated Fluxes (Gt C):'
    print(fa),'       GPP_TOT       RESP_TOT     RESP_TOT_WDIST'
    print(fa3f),'  ',SUM(gpppft)*aconvert, &
          SUM(resppft)*aconvert, &
          (SUM(resppft) + SUM(respctot))*aconvert
    print(fa),''
    print(fa),'    Crop Carbon Balance (Gt C):'
    print(fa),'       GPP_CROP      RESP_CROP     %DIFF'
    gppcrops_tot = SUM(gppcrops)
    respcrops_tot = SUM(respcrops)
    respctot_tot = SUM(respctot)
    print(fa3f),' ',gppcrops_tot*aconvert, &
          (respcrops_tot + respctot_tot)*aconvert, &
          abs(gppcrops_tot - (respcrops_tot + respctot_tot)) &
           /gppcrops_tot*100.

   !write out the respiration to the output file
   call cropdist_writemap(nlon, nlat, ncrop, lon, lat, &
                          resp_crop, outfile)


end subroutine cropdist_outgrid


!----------------------------
subroutine cropdist_writemap( &
   nlon, nlat, ncrop, lon, lat, &
   resp_crop, outfile)

use netcdf
use sibinfo_module, only: &
     fa, f2a, &
     ncropname, npftcropstart, &
     pftnames, pftnum, &
     respclong, respcname, respcunits

implicit none

!input variables
integer, intent(in) :: nlon, nlat, ncrop
real, dimension(nlon), intent(in) :: lon
real, dimension(nlat), intent(in) :: lat
real*8, dimension(nlon,nlat,ncrop), intent(in) :: resp_crop
character(len=256), intent(in) :: outfile

!netcdf variables
integer :: status, ncid
integer :: lonid, latid, cnameid, varid
integer :: nlondid, nlatdid, ncropdid, clendid

!info variables
character(len=3), dimension(ncrop) :: crop_names

!misc variables
integer :: n

!----------------------------
print(fa),''
print(fa), '  Creating File: '
print(f2a),'   ',trim(outfile)

status = nf90_create(trim(outfile),nf90_64bit_offset,ncid)
IF (status .ne. nf90_noerr) THEN
    print(fa),'Error Opening Crop Output File.'
    print(fa),'Stopping.'
    STOP
ENDIF

status = nf90_def_dim(ncid,'lon',nlon,nlondid)
status = nf90_def_dim(ncid,'lat',nlat,nlatdid)
status = nf90_def_dim(ncid,trim(ncropname),ncrop,ncropdid)
status = nf90_def_dim(ncid,'clen',3,clendid)

status = nf90_def_var(ncid,'lon',nf90_float, &
         (/nlondid/), lonid)
status = nf90_put_att(ncid,lonid,'long_name','Longitude')
status = nf90_put_att(ncid,lonid,'units','degrees_east')

status = nf90_def_var(ncid,'lat',nf90_float, &
         (/nlatdid/), latid)
status = nf90_put_att(ncid,latid,'long_name','Latitude')
status = nf90_put_att(ncid,latid,'units','degrees_north')

status = nf90_def_var(ncid,'crop_names',nf90_char, &
         (/clendid,ncropdid/), cnameid)

status = nf90_def_var(ncid,trim(respcname),nf90_float, &
          (/nlondid,nlatdid,ncropdid/), varid)
IF (status .ne. nf90_noerr) THEN
     print(fa),'Error Defining Crop Respiration In Output File.'
     print(fa),'Stopping.'
     STOP
ENDIF
status = nf90_put_att(ncid,varid,'long_name',trim(respclong))
status = nf90_put_att(ncid,varid,'units',trim(respcunits))
status = nf90_enddef(ncid)

status = nf90_put_var(ncid,lonid,lon)
status = nf90_put_var(ncid,latid,lat)

DO n=1,ncrop
   crop_names(n) = pftnames(pftnum(npftcropstart)+n-1)
ENDDO

status = nf90_put_var(ncid,cnameid,crop_names)
IF (status .ne. nf90_noerr) THEN
   print(fa),'Error Writing Out Crop Names.'
   print(fa),'Stopping.'
   STOP
ENDIF

status = nf90_put_var(ncid,varid,resp_crop)
IF (status .ne. nf90_noerr) THEN
   print(fa),'Error Writing Out Crop Respiration.'
   print(fa),'Stopping.'
   STOP
ENDIF
status = nf90_close(ncid)


end subroutine cropdist_writemap
