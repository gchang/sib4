!==================================================
module ncmodule
!==================================================

implicit none

!parameters
real :: mminval = -900.
real :: badval=-9999.99
integer, dimension(12) :: &
     dayspermon = (/31,28,31,30,31,30, &
                    31,31,30,31,30,31/)

!netcdf information
integer :: status
integer :: inid, outid
integer :: dimid, varid

!dimension variables
integer :: nsib, nsibdr
integer :: cndid, csdid, cldid
integer :: npftdid, nsdid, ntdid
integer :: pnameid, ptitleid
integer :: snameid, spftid, slatid, slonid 
integer :: doyid, timeid
integer :: sgppid, sreid, sneeid
integer :: sshid, slhid, ssifid
integer :: srst1id, srst2id, srst3id, srst4id
integer :: sprid, sswid, slaiid
integer :: gppid, reid, neeid
integer :: shid, lhid
integer :: prid, swid, laiid

!flags
logical :: dohourly, dodaily

end module ncmodule

!==================================================
subroutine get_site_refs( &
     infile, nsites, nmaxpfts, &
     sitename, sitelat, sitelon, &
     siteprefw, siteref, sitepnum)
!==================================================

use netcdf
implicit none

!input variables
character(len=256), intent(in) :: infile
integer, intent(in) :: nsites, nmaxpfts
character(len=6), dimension(nsites), intent(in) :: sitename
real, dimension(nsites), intent(in) :: sitelat, sitelon
integer, dimension(nsites,nmaxpfts), intent(in) :: siteprefw
integer, dimension(nsites), intent(inout) :: siteref
integer, dimension(nsites,nmaxpfts), intent(inout) :: sitepnum

!netcdf variables
integer :: ncid, status
integer :: dimid, varid
integer :: nsib, nlu
integer, dimension(:,:), allocatable :: lu_pref
real, dimension(:), allocatable :: lonsib, latsib

!misc variables
integer :: i, j, ns
real :: diff, mindist


!---------------------------------------
!open SiB4 file and get lon/lat arrays
status = nf90_open(trim(infile),nf90_nowrite,ncid)
if (status .ne. nf90_noerr) then
   print*,'Error Opening SiB4 File: '
   print*,'  ',trim(infile)
   print*,'Stopping.'
   STOP
endif

status = nf90_inq_dimid(ncid,'landpoints',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nsib)
if (status .ne. nf90_noerr) then
   print*,'Error Finding Number Of SiB4 Points.'
   print*,'Stopping.'
   STOP
endif

status = nf90_inq_dimid(ncid,'nlu',dimid)
status = nf90_inquire_dimension(ncid,dimid,len=nlu)
if (status .ne. nf90_noerr) then
   print*,'Error Finding Number Of Land Units.'
   print*,'Stopping.'
   STOP
endif

!find sites
allocate(lonsib(nsib),latsib(nsib))
status = nf90_inq_varid(ncid,'lonsib',varid)
status = nf90_get_var(ncid,varid,lonsib)
status = nf90_inq_varid(ncid,'latsib',varid)
status = nf90_get_var(ncid,varid,latsib)

siteref(:) = 0
do ns=1,nsites
   mindist=999.
   do i=1,nsib
      diff = abs(lonsib(i)-sitelon(ns)) + abs(latsib(i)-sitelat(ns))
      if (diff .lt. mindist) then
         siteref(ns) = i
         mindist=diff
      endif
   enddo

   if (siteref(ns) .le. 0) then
      print*,'Error Finding Site in SiB4.'
      print*,'   Site Lon/Lat: ',sitelon(ns),sitelat(ns)
      print*,'   Stopping.'
      STOP
   endif
enddo

!find pfts
allocate(lu_pref(nsib,nlu))
status = nf90_inq_varid(ncid,'lu_pref',varid)
status = nf90_get_var(ncid,varid,lu_pref)

sitepnum(:,:) = 0
do ns=1,nsites
   do i=1,nmaxpfts
      if (siteprefw(ns,i) .gt. 0) then
          do j=1,nlu
             if (siteprefw(ns,i) .eq. lu_pref(siteref(ns),j)) then
                sitepnum(ns,i) = j
             endif
          enddo

          if (sitepnum(ns,i) .le. 0) then
              print('(a,a,a,i6,a)'),'Error Finding Matching PFT: '&
                         ,sitename(ns)
              print('(a,i6)'),    '   Site Point  : ',siteref(ns)
              print('(a,2f10.2)'),'   Site Lon/Lat: ' &
                         ,sitelon(ns),sitelat(ns)
              print('(a,2f10.2)'),'   SiB4 Lon/Lat: ' &
                         ,lonsib(siteref(ns)),latsib(siteref(ns)) 
              print('(a,i3)'), '   Site PFT: ',siteprefw(ns,i)
              print('(a,10i4)'),'   Simulated PFTs: ',lu_pref(siteref(ns),:)
              print('(a)'),'   Stopping.'
              STOP
          endif
       endif !sitepft > 0
   enddo !i=1,sitenpfts
enddo !ns=1,nsites

status = nf90_close(ncid)

RETURN
end subroutine get_site_refs

!==================================================
subroutine close_outfile()
!==================================================

use ncmodule
use netcdf
implicit none

status=nf90_close(outid)

RETURN
end subroutine close_outfile

!==================================================
subroutine create_outfile( &
     outfile, yrstart, &
     nsites, sitename, sitepft, &
     sitelat, sitelon, &
     npfts, pftnames,  &
     pfttitles)
!==================================================

use ncmodule
use netcdf
implicit none

!input variables
character(len=256), intent(in) :: outfile
integer, intent(in) :: yrstart, nsites
character(len=6), dimension(nsites), intent(in) :: sitename
character(len=3), dimension(nsites), intent(in) :: sitepft
real, dimension(nsites), intent(in) :: sitelat, sitelon

integer, intent(in) :: npfts
character(len=3), dimension(npfts), intent(in) :: pftnames
character(len=27), dimension(npfts), intent(in) :: pfttitles

!local variables
character(len=30) :: strtunits

!----------------------------------------
status = nf90_create(trim(outfile),nf90_64bit_offset,outid)
if (status .ne. nf90_noerr) then 
   print*,'Error Creating File: '
   print*,'  ',trim(outfile)
   print*,'Stopping.'
   STOP
endif

status=nf90_def_dim(outid,'cname',6,cndid)
status=nf90_def_dim(outid,'cshort',3,csdid)
status=nf90_def_dim(outid,'clong',27,cldid)
status=nf90_def_dim(outid,'npfts',npfts,npftdid)
status=nf90_def_dim(outid,'nsites',nsites,nsdid)
status=nf90_def_dim(outid,'time',nf90_unlimited,ntdid)

status=nf90_def_var(outid,'pft_names',nf90_char,(/csdid,npftdid/),pnameid)
status=nf90_put_att(outid,pnameid,'long_name','Vegetation Type Name')

status=nf90_def_var(outid,'pft_titles',nf90_char,(/cldid,npftdid/),ptitleid)
status=nf90_put_att(outid,ptitleid,'long_name','Vegetation Type Title')

status=nf90_def_var(outid,'site_names',nf90_char,(/cndid,nsdid/),snameid)
status=nf90_put_att(outid,snameid,'long_name','Site Name')

status=nf90_def_var(outid,'site_pft',nf90_char,(/csdid,nsdid/),spftid)
status=nf90_put_att(outid,spftid,'long_name','Site Vegetation Type')

status=nf90_def_var(outid,'lat',nf90_float,(/nsdid/),slatid)
status=nf90_put_att(outid,slatid,'long_name','Site Latitude')
status=nf90_put_att(outid,slatid,'units','degrees_north') 

status=nf90_def_var(outid,'lon',nf90_float,(/nsdid/),slonid)
status=nf90_put_att(outid,slonid,'long_name','Site Longitude')
status=nf90_put_att(outid,slonid,'units','degrees_east')

status=nf90_def_var(outid,'doy',nf90_float,(/ntdid/),doyid)
status=nf90_put_att(outid,doyid,'long_name','Day of Year')
write(strtunits,'(a,i4.4,a)') &
    'days since ', yrstart, '-00-00'
status=nf90_put_att(outid,doyid,'units',trim(strtunits))

status=nf90_def_var(outid,'time',nf90_double,(/ntdid/),timeid)
status=nf90_put_att(outid,timeid,'long_name','Time')
write(strtunits,'(a)') 'years since 0000-00-00'
status=nf90_put_att(outid,timeid,'units',trim(strtunits))

status=nf90_def_var(outid,'gpp_sib4',nf90_float,(/nsdid,ntdid/),sgppid)
status=nf90_put_att(outid,sgppid,'long_name','SiB4 Gross Primary Production')
status=nf90_put_att(outid,sgppid,'units','micromoles C/m2/s')

status=nf90_def_var(outid,'resp_sib4',nf90_float,(/nsdid,ntdid/),sreid)
status=nf90_put_att(outid,sreid,'long_name','SiB4 Ecosystem Respiration')
status=nf90_put_att(outid,sreid,'units','micromoles C/m2/s')

status=nf90_def_var(outid,'nee_sib4',nf90_float,(/nsdid,ntdid/),sneeid)
status=nf90_put_att(outid,sneeid,'long_name','SiB4 Net Ecosystem Exchange')
status=nf90_put_att(outid,sneeid,'units','micromoles C/m2/s')

status=nf90_def_var(outid,'sh_sib4',nf90_float,(/nsdid,ntdid/),sshid)
status=nf90_put_att(outid,sshid,'long_name','SiB4 Sensible Heat Flux')
status=nf90_put_att(outid,sshid,'units','W/m2')

status=nf90_def_var(outid,'lh_sib4',nf90_float,(/nsdid,ntdid/),slhid)
status=nf90_put_att(outid,slhid,'long_name','SiB4 Latent Heat Flux')
status=nf90_put_att(outid,slhid,'units','W/m2')

status=nf90_def_var(outid,'precip_sib4',nf90_float,(/nsdid,ntdid/),sprid)
status=nf90_put_att(outid,sprid,'long_name','Fluxnet Precipitation')
status=nf90_put_att(outid,sprid,'units','mm')

status=nf90_def_var(outid,'sw_sib4',nf90_float,(/nsdid,ntdid/),sswid)
status=nf90_put_att(outid,sswid,'long_name','Fluxnet Downwelling Solar Radiation')
status=nf90_put_att(outid,sswid,'units','W/m2')

status=nf90_def_var(outid,'sif_sib4',nf90_float,(/nsdid,ntdid/),ssifid)
status=nf90_put_att(outid,ssifid,'long_name','SiB4 Solar-Induced Fluorescence')
status=nf90_put_att(outid,ssifid,'units','W/m2/nm/sr')

status=nf90_def_var(outid,'rstfac4_sib4',nf90_float,(/nsdid,ntdid/),srst4id)
status=nf90_put_att(outid,srst4id,'long_name','SiB4 Combined Stress Factor')
status=nf90_put_att(outid,srst4id,'units','')

if (.not. dohourly) then
   status=nf90_def_var(outid,'rstfac1_sib4',nf90_float,(/nsdid,ntdid/),srst1id)
   status=nf90_put_att(outid,srst1id,'long_name','SiB4 Humditiy Sress')
   status=nf90_put_att(outid,srst1id,'units','')

   status=nf90_def_var(outid,'rstfac2_sib4',nf90_float,(/nsdid,ntdid/),srst2id)
   status=nf90_put_att(outid,srst2id,'long_name','SiB4 Soil Moisture Stress')
   status=nf90_put_att(outid,srst2id,'units','')

   status=nf90_def_var(outid,'rstfac3_sib4',nf90_float,(/nsdid,ntdid/),srst3id)
   status=nf90_put_att(outid,srst3id,'long_name','SiB4 Temperature Stress')
   status=nf90_put_att(outid,srst3id,'units','')

   status=nf90_def_var(outid,'lai_sib4',nf90_float,(/nsdid,ntdid/),slaiid)
   status=nf90_put_att(outid,slaiid,'long_name','SiB4 Leaf Area Index')
   status=nf90_put_att(outid,slaiid,'units','m2/m2')
endif

status=nf90_def_var(outid,'gpp_fluxnet',nf90_float,(/nsdid,ntdid/),gppid)
status=nf90_put_att(outid,gppid,'long_name','Fluxnet Gross Primary Production')
status=nf90_put_att(outid,gppid,'units','micromoles C/m2/s')
status=nf90_put_att(outid,gppid,'missing_value',badval)

status=nf90_def_var(outid,'resp_fluxnet',nf90_float,(/nsdid,ntdid/),reid)
status=nf90_put_att(outid,reid,'long_name','Fluxnet Ecosystem Respiration')
status=nf90_put_att(outid,reid,'units','micromoles C/m2/s')
status=nf90_put_att(outid,reid,'missing_value',badval)

status=nf90_def_var(outid,'nee_fluxnet',nf90_float,(/nsdid,ntdid/),neeid)
status=nf90_put_att(outid,neeid,'long_name','Fluxnet Net Ecosystem Exchange')
status=nf90_put_att(outid,neeid,'units','micromoles C/m2/s')
status=nf90_put_att(outid,neeid,'missing_value',badval)

status=nf90_def_var(outid,'sh_fluxnet',nf90_float,(/nsdid,ntdid/),shid)
status=nf90_put_att(outid,shid,'long_name','Fluxnet Sensible Heat Flux')
status=nf90_put_att(outid,shid,'units','W/m2')
status=nf90_put_att(outid,shid,'missing_value',badval)

status=nf90_def_var(outid,'lh_fluxnet',nf90_float,(/nsdid,ntdid/),lhid)
status=nf90_put_att(outid,lhid,'long_name','Fluxnet Latent Heat Flux')
status=nf90_put_att(outid,lhid,'units','W/m2')
status=nf90_put_att(outid,lhid,'missing_value',badval)

status=nf90_def_var(outid,'precip_fluxnet',nf90_float,(/nsdid,ntdid/),prid)
status=nf90_put_att(outid,prid,'long_name','Fluxnet Precipitation')
status=nf90_put_att(outid,prid,'units','mm')
status=nf90_put_att(outid,prid,'missing_value',badval)

status=nf90_def_var(outid,'sw_fluxnet',nf90_float,(/nsdid,ntdid/),swid)
status=nf90_put_att(outid,swid,'long_name','Fluxnet Downwelling Solar Radiation')
status=nf90_put_att(outid,swid,'units','W/m2')
status=nf90_put_att(outid,swid,'missing_value',badval)

if (.not. dohourly) then
   status=nf90_def_var(outid,'lai_modis',nf90_float,(/nsdid,ntdid/),laiid)
   status=nf90_put_att(outid,laiid,'long_name','MODIS Leaf Area Index')
   status=nf90_put_att(outid,laiid,'units','m2/m2')
   status=nf90_put_att(outid,laiid,'missing_value',badval)
endif
status=nf90_enddef(outid)

status=nf90_put_var(outid,pnameid,pftnames)
status=nf90_put_var(outid,ptitleid,pfttitles)
status=nf90_put_var(outid,spftid,sitepft)
status=nf90_put_var(outid,snameid,sitename)
status=nf90_put_var(outid,slonid,sitelon)
status=nf90_put_var(outid,slatid,sitelat)

RETURN
end subroutine create_outfile


!==================================================
subroutine write_sib4_outfile(infile, &
   nsites, nmaxpft, &
   siteref, sitepnum, siteparea, &
   year, doyper, doystart, ntstart, ntime)
!==================================================

use ncmodule
use netcdf
implicit none

!input variables
character(len=256), intent(in) :: infile
integer, intent(in) :: nsites, nmaxpft
integer, dimension(nsites), intent(in) :: siteref
integer, dimension(nsites,nmaxpft), intent(in) :: sitepnum
real, dimension(nsites,nmaxpft), intent(in) :: siteparea
integer, intent(in) :: year, doyper, doystart, ntstart
integer, intent(inout) :: ntime

!file variables
integer :: nlu
real, dimension(:,:,:), allocatable :: &
   assimall, respall, lhall, shall, sifall, laiall
real, dimension(:,:,:), allocatable :: &
     rstfac1all, rstfac2all, rstfac3all, rstfac4all

!local variables
integer :: np, ns
integer :: pref, sref
real :: parea
real, dimension(:), allocatable :: doytemp
real, dimension(:), allocatable :: doy
real, dimension(:), allocatable :: gpp, resp, nee
real, dimension(:), allocatable :: lh, sh, sif, lai
real, dimension(:), allocatable :: rstfac1, rstfac2, rstfac3, rstfac4
real*8, dimension(:), allocatable :: time

!------------------------------------
status=nf90_open(trim(infile),nf90_nowrite,inid)
if (status .ne. nf90_noerr) then
   print*,'Error Opening File: '
   print*,'  ',trim(infile)
   print*,'Stopping.'
   STOP
endif

status=nf90_inq_dimid(inid,'landpoints',dimid)
status=nf90_inquire_dimension(inid,dimid,len=nsib)
status=nf90_inq_dimid(inid,'nlu',dimid)
status=nf90_inquire_dimension(inid,dimid,len=nlu)
status=nf90_inq_dimid(inid,'time',dimid)
status=nf90_inquire_dimension(inid,dimid,len=ntime)

allocate(doytemp(ntime),doy(ntime))
status = nf90_inq_varid(inid,'time',varid)
status = nf90_get_var(inid,varid,doytemp)
doy(:) = doytemp(:) + doystart
status = nf90_put_var(outid,doyid,doy,start=(/ntstart/),count=(/ntime/))
if (status .ne. nf90_noerr) then
   print*,'Error Writing DOY.'
   print*,'Stopping.'
   STOP
endif

allocate(time(ntime))
time = doytemp/real(doyper) + year
status = nf90_put_var(outid,timeid,time,start=(/ntstart/),count=(/ntime/))
if (status .ne. nf90_noerr) then
   print*,'Error Writing Time.'
   print*,'Stopping.'
   STOP
endif

!get all data, then pull from saved arrays
allocate(assimall(nsib,nlu,ntime))
status=nf90_inq_varid(inid,'assim',varid)
if (status .ne. nf90_noerr) then
   print*,'SiB4 File Missing Assim!'
   print*,'Stopping.'
   STOP
endif
status=nf90_get_var(inid,varid,assimall)
if (status .ne. nf90_noerr) then
   print*,'Error Getting Assim! Stopping.'
   STOP
endif

allocate(respall(nsib,nlu,ntime))
status=nf90_inq_varid(inid,'resp_bal',varid)
if (status .ne. nf90_noerr) then
    if (ntstart .eq. 1) then 
       print*,''
       print*,'  SiB4 File Missing Resp_Bal!'
    endif
    status = nf90_inq_varid(inid,'resp_tot',varid)
    if (status .ne. nf90_noerr) then
       print*,'SiB4 File Missing Resp_Tot!'
       print*,'Stopping.'
       STOP
    else
       if (ntstart .eq. 1) then
          print*,'    --Using Native Respiration--'
       endif
    endif
endif
status=nf90_get_var(inid,varid,respall)
if (status .ne. nf90_noerr) then
   print*,'Error Getting Resp! Stopping.'
endif

allocate(lhall(nsib,nlu,ntime))
status=nf90_inq_varid(inid,'fws',varid)
if (status .ne. nf90_noerr) then
   if (ntstart .eq. 1) then
       print*,'  SiB4 File Missing LH (fws)!'
   endif
   lhall(:,:,:) = badval
else
   status=nf90_get_var(inid,varid,lhall)
   if (status .ne. nf90_noerr) then
      print*,'Error Getting LH! Stopping.'
      STOP
   endif
endif

allocate(shall(nsib,nlu,ntime))
status=nf90_inq_varid(inid,'fss',varid)
if (status .ne. nf90_noerr) then
   if (ntstart .eq. 1) then
       print*,'  SiB4 File Missing SH (fss)!'
   endif
   shall(:,:,:) = badval
else
   status=nf90_get_var(inid,varid,shall)
   if (status .ne. nf90_noerr) then
      print*,'Error Getting SH! Stopping.'
      STOP
   endif
endif

allocate(sifall(nsib,nlu,ntime))
status=nf90_inq_varid(inid,'sif',varid)
if (status .ne. nf90_noerr) then
   print*,'SiB4 File Missing SIF!'
   print*,'Stopping.'
   STOP
endif
status=nf90_get_var(inid,varid,sifall)
if (status .ne. nf90_noerr) then
   print*,'Error Getting SIF! Stopping.'
   STOP
endif

allocate(rstfac4all(nsib,nlu,ntime))
status=nf90_inq_varid(inid,'rstfac4',varid)
if (status .ne. nf90_noerr) then
   print*,'SiB4 File Missing rstfac4!'
   print*,'Stopping.'
   STOP
endif
status=nf90_get_var(inid,varid,rstfac4all)
if (status .ne. nf90_noerr) then
   print*,'Error Getting rstfac4! Stopping.'
   STOP
endif

if (.not. dohourly) then
   allocate(laiall(nsib,nlu,ntime))
   status=nf90_inq_varid(inid,'lai',varid)
   if (status .eq. nf90_noerr) then
      status=nf90_get_var(inid,varid,laiall)
      if (status .ne. nf90_noerr) then
         print*,'Error Getting LAI! Stopping.'
         STOP
      endif
   else
      print*,'SiB4 File Missing LAI!'
      laiall = badval
   endif

   allocate(rstfac1all(nsib,nlu,ntime))
   status=nf90_inq_varid(inid,'rstfac1',varid)
   if (status .ne. nf90_noerr) then
      print*,'SiB4 File Missing rstfac1!'
      print*,'Stopping.'
      STOP
   endif
   status=nf90_get_var(inid,varid,rstfac1all)
   if (status .ne. nf90_noerr) then
      print*,'Error Getting rstfac1! Stopping.'
      STOP
   endif

   allocate(rstfac2all(nsib,nlu,ntime))
   status=nf90_inq_varid(inid,'rstfac2',varid)
   if (status .ne. nf90_noerr) then
      print*,'SiB4 File Missing rstfac2!'
      print*,'Stopping.'
      STOP
   endif
   status=nf90_get_var(inid,varid,rstfac2all)
   if (status .ne. nf90_noerr) then
      print*,'Error Getting rstfac2! Stopping.'
      STOP
   endif

   allocate(rstfac3all(nsib,nlu,ntime))
   status=nf90_inq_varid(inid,'rstfac3',varid)
   if (status .ne. nf90_noerr) then
      print*,'SiB4 File Missing rstfac3!'
      print*,'Stopping.'
      STOP
   endif
   status=nf90_get_var(inid,varid,rstfac3all)
   if (status .ne. nf90_noerr) then
      print*,'Error Getting rstfac3! Stopping.'
      STOP
   endif
   
endif

!loop through the sites and pull out data
allocate(gpp(ntime),resp(ntime),nee(ntime))
allocate(lh(ntime),sh(ntime),sif(ntime),rstfac4(ntime))
if (.not. dohourly) allocate(lai(ntime))
if (.not. dohourly) allocate(rstfac1(ntime),rstfac2(ntime),rstfac3(ntime))
do ns=1,nsites
   gpp(:) = 0.
   resp(:) = 0.
   nee(:) = 0.
   lh(:) = 0.
   sh(:) = 0.
   sif(:) = 0.
   rstfac4(:) = 0.
   if (.not. dohourly) then
      lai(:) = 0.
      rstfac1(:) = 0.
      rstfac2(:) = 0.
      rstfac3(:) = 0.
    endif
      
   sref = siteref(ns)
   do np=1,nmaxpft
      pref=sitepnum(ns,np)
      parea=siteparea(ns,np)
      if (pref .gt. 0) then
          gpp(:) = gpp(:) + assimall(sref,pref,:)*parea
          resp(:) = resp(:) + respall(sref,pref,:)*parea
          nee(:) = nee(:) + &
                   (respall(sref,pref,:)-assimall(sref,pref,:)) &
                   *parea
          sh(:) = sh(:) + shall(sref,pref,:)*parea
          lh(:) = lh(:) + lhall(sref,pref,:)*parea
          sif(:) = sif(:) + sifall(sref,pref,:)*parea
          rstfac4(:) = rstfac4(:) + rstfac4all(sref,pref,:)*parea
          
          if (.not. dohourly) then
             if (maxval(laiall) .ge. 0.) then
                lai(:) = lai(:) + laiall(sref,pref,:)*parea
             else
                lai(:) = badval
             endif

             if (maxval(rstfac1all) .ge. 0.) then
                rstfac1(:) = rstfac1(:) + rstfac1all(sref,pref,:)*parea
             else
                rstfac1(:) = badval
             endif
             if (maxval(rstfac2all) .ge. 0.) then
                rstfac2(:) = rstfac2(:) + rstfac2all(sref,pref,:)*parea
             else
                rstfac2(:) = badval
             endif
             if (maxval(rstfac3all) .ge. 0.) then
                rstfac3(:) = rstfac3(:) + rstfac3all(sref,pref,:)*parea
             else
                rstfac3(:) = badval
             endif
             
          endif
       endif
   enddo !np=1,nmaxpft

   status=nf90_put_var(outid,sgppid,gpp,   &
          start=(/ns,ntstart/), count=(/1,ntime/))
   status=nf90_put_var(outid,sreid,resp, &
          start=(/ns,ntstart/), count=(/1,ntime/))
   nee = resp - gpp
   status=nf90_put_var(outid,sneeid,nee, &
          start=(/ns,ntstart/), count=(/1,ntime/))

   status=nf90_put_var(outid,sshid,sh, &
          start=(/ns,ntstart/), count=(/1,ntime/))
   status=nf90_put_var(outid,slhid,lh, &
          start=(/ns,ntstart/), count=(/1,ntime/))
   status=nf90_put_var(outid,ssifid,sif, &
          start=(/ns,ntstart/), count=(/1,ntime/))
   status=nf90_put_var(outid,srst4id,rstfac4, &
          start=(/ns,ntstart/), count=(/1,ntime/))
   if (.not. dohourly) then
      status=nf90_put_var(outid,slaiid,lai, &
           start=(/ns,ntstart/), count=(/1,ntime/))
      status=nf90_put_var(outid,srst1id,rstfac1, &
           start=(/ns,ntstart/), count=(/1,ntime/))
      status=nf90_put_var(outid,srst2id,rstfac2, &
           start=(/ns,ntstart/), count=(/1,ntime/))
      status=nf90_put_var(outid,srst3id,rstfac3, &
           start=(/ns,ntstart/), count=(/1,ntime/))
   endif
enddo !ns=1,nsites

status = nf90_close(inid)
deallocate(assimall,respall,shall,lhall,sifall,rstfac4all)
if (.not. dohourly) deallocate(laiall,rstfac1all,rstfac2all,rstfac3all)

RETURN

end subroutine write_sib4_outfile



