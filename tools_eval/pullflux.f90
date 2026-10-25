!---------------------------------------------
program pullflux
!---------------------------------------------
!Program to pull specific flux variables
!  from SiB4 and Fluxnet data.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make pullflux
!
!kdhaynes, 2018/02

use ncmodule, only: &
    dayspermon, dohourly, dodaily
implicit none

!input variables
character(len=100), parameter :: specfile='pullflux.txt'
integer :: startmon, stopmon, startyr, stopyr
integer :: nfiles, nsites, nmaxpft
character(len=256) :: fluxdir, modisdir, merradir, sibdir
character(len=256) :: prefix, suffix, trash
character(len=256), dimension(:), allocatable :: &
     infiles, merrafiles
characteR(len=256), dimension(:,:), allocatable :: &
     modisfiles
character(len=30) :: outsuffix
character(len=256) :: outdir, outfile
character(len=6), dimension(:), allocatable :: sitename
character(len=3), dimension(:), allocatable :: sitepft
real, dimension(:), allocatable :: sitelat, sitelon
integer, dimension(:,:), allocatable :: sitepref
real, dimension(:,:), allocatable :: siteparea

!sib4 site information
integer, dimension(:), allocatable :: siteref
integer, dimension(:,:), allocatable :: sitepnum

!fluxnet site information
character(len=8), dimension(:), allocatable :: fprefix
character(len=20), dimension(:), allocatable :: &
     fneename, fgppname, frename, &
     fprname, flename, fshname, fswname
logical, dimension(:), allocatable :: flstflag
integer, dimension(:), allocatable :: fhrshift

!misc variables
integer :: count, ndpermon, ns
integer :: day, mon, yr
integer :: doyper, doystart, dpermon
integer :: ntstart, ntime
integer :: nmon, mymonstart, mymonstop
integer :: spos1,spos2

!parameters
character(len=12), parameter :: outname='sib4_eval'
integer, parameter :: npfts=19
character(len=3), dimension(npfts), parameter :: &
    pftnames = (/'ENF', 'DNF', 'EBF', 'DBF', 'SHB', &
      'SHA', 'C3A', 'C3G', 'C4G', 'C3C','C4C', & 
      'MZE', 'SOY', 'WWT', 'MXF', 'TUN', &
      'SAV', 'CXG','CXC'/)
character(len=27), dimension(npfts), parameter :: &
    pfttitles = (/ 'Evergreen Needleleaf Forest', &
                   'Deciduous Needleaf Forest  ', &
                   'Evergreen Broadleaf Forest ', &
                   'Deciduous Broadleaf Forest ', &
                   'Shrubs (non-Arctic)        ', &
                   'Arctic Shrubs              ', &
                   'Arctic C3 Grassland        ', &
                   'C3 Grassland               ', &
                   'C4 Grassland               ', &
                   'C3 Crops                   ', &
                   'C4 Crops                   ', &
                   'Maize                      ', &
                   'Soybeans                   ', &
                   'Winter Wheat               ', &
                   'Mixed Forest               ', &
                   'Tundra                     ', &
                   'Savannah                   ', &
                   'Mixed Grassland            ', &
                   'Mixed Cropland             '/)


!------------------------------------------------------

!read spec file
open(unit=33,file=trim(specfile),form='formatted')
read(33,*) startmon
read(33,*) startyr
read(33,*) stopmon
read(33,*) stopyr

nmon=0
do yr=startyr, stopyr
   mymonstart=1
   if (yr .eq. startyr) mymonstart=startmon
   mymonstop=12
   if (yr .eq. stopyr) mymonstop=stopmon

   do mon=mymonstart,mymonstop
      nmon=nmon+1
   enddo
enddo
print('(a,i5,a)'), ' Processing ',nmon, ' Months'

read(33,('(a256)')) trash
read(33,('(a256)')) merradir
spos1=index(merradir,'xxx')
prefix=merradir(1:spos1-1)
spos2=len_trim(merradir)
suffix=merradir(spos1+3:spos2)
allocate(merrafiles(nmon))
count=0
do yr=startyr, stopyr
   mymonstart=1
   if (yr .eq. startyr) mymonstart=startmon
   mymonstop=12
   if (yr .eq. stopyr) mymonstop=stopmon

   do mon=mymonstart,mymonstop
      count=count+1
      write(merrafiles(count),'(a,i4.4,i2.2,a)') &
          trim(prefix), yr, mon, trim(suffix)
   enddo
enddo

read(33,('(a256)')) trash
read(33,('(a256)')) sibdir
spos1=index(sibdir,'xxx')
prefix=sibdir(1:spos1-1)
spos2=len_trim(sibdir)
suffix=sibdir(spos1+3:spos2)
if (index(sibdir,'hsib') .ge. 1) then
   dohourly=.TRUE.
else
   dohourly=.FALSE.
endif
if (index(sibdir,'psib') .ge. 1) then
   dodaily=.TRUE.
else
   dodaily=.FALSE.
endif

IF (.not. dohourly) THEN
   nfiles=nmon
   allocate(infiles(nmon))
   count=0
   do yr=startyr, stopyr
      mymonstart=1
      if (yr .eq. startyr) mymonstart=startmon
      mymonstop=12
      if (yr .eq. stopyr) mymonstop=stopmon

      do mon=mymonstart,mymonstop
         count=count+1
         write(infiles(count),'(a,i4.4,i2.2,a)') &
             trim(prefix), yr, mon, trim(suffix)
      enddo
   enddo
ELSE
   nfiles=0
   do yr=startyr, stopyr
      if (mod(yr,4) .eq. 0) then
         nfiles = nfiles + 366
      else
         nfiles = nfiles + 365
      endif
   enddo

   allocate(infiles(nfiles))
   count=1
   do yr=startyr, stopyr
      mymonstart=1
      if (yr .eq. startyr) mymonstart=startmon
      mymonstop=12
      if (yr .eq. stopyr) mymonstop=stopmon

      do mon=mymonstart,mymonstop
         dpermon = dayspermon(mon)
         if ((mod(yr,4) .eq. 0) .and. (mon .eq. 2)) then
            dpermon = dpermon + 1
         endif
         
         do day=1,dpermon
            write(infiles(count),'(a,i4.4,i2.2,i2.2,a)') &
                 trim(prefix),yr,mon,day,trim(suffix)
            count=count+1
         enddo
      enddo
    enddo
ENDIF

read(33,'(a256)') trash
read(33,'(a256)') fluxdir
read(33,'(a256)') trash
read(33,'(a256)') modisdir
read(33,'(a256)') trash
read(33,'(a256)') outdir
read(33,'(a256)') trash
read(33,*) nsites, nmaxpft
print('(a,i5,a)'), ' Processing ',nsites, ' Sites'

allocate(sitename(nsites),sitepft(nsites))
allocate(sitelat(nsites),sitelon(nsites))
allocate(sitepref(nsites,nmaxpft),siteparea(nsites,nmaxpft))
do ns=1,nsites
   read(33,*) sitename(ns), sitelat(ns), sitelon(ns), &
              sitepft(ns), sitepref(ns,:), siteparea(ns,:)
   if ((sum(siteparea(ns,:)) .gt. 1.01) .or. &
       (sum(siteparea(ns,:)) .lt. 0.99)) then
      print*,'Error With PFT Fractions: ',trim(sitename(ns))
      print*,'Stopping.'
      STOP
   endif
enddo
close(33)

!Get SiB4 references
allocate(siteref(nsites),sitepnum(nsites,nmaxpft))
call get_site_refs(infiles(1), nsites, nmaxpft, &
      sitename, sitelat, sitelon, sitepref, siteref, sitepnum)
!print('(a,i2,a)'),'   Using ',nmaxpft,' LUs/PFTs per site.'
print('(a)'),     '   Site      Ref    PFT_Refs    LU_Nums'
do ns=1,nsites
   print('(3a,i6,a,3i3,a,3i3)'), '   ', &
        sitename(ns), '  ', siteref(ns), '  ', sitepref(ns,:), &
                      '  ', sitepnum(ns,:)
enddo

!Open output file
outsuffix=''
spos1 = index(sibdir,'hsib')
if (spos1 .gt. 0) outsuffix='_hourly.nc'
spos1 = index(sibdir,'psib')
if (spos1 .gt. 0) outsuffix='_daily.nc'
spos1 = index(sibdir,'qsib')
if (spos1 .gt. 0) outsuffix='_monthly.nc'
outfile = trim(outdir) // trim(outname) // trim(outsuffix)

call create_outfile(outfile, startyr, &
   nsites, sitename, sitepft, &
   sitelat, sitelon, &
   npfts, pftnames, pfttitles)

!Write SiB4 output to file
count=0
doystart=0
doyper=0
ntstart=1
print('(a)'),     ' Processing SiB4'
do yr=startyr, stopyr
   mymonstart=1
   if (yr .eq. startyr) mymonstart=startmon
   mymonstop=12
   if (yr .eq. stopyr) mymonstop=stopmon

   doyper=365
   if (mod(yr,4) .eq. 0) doyper=366

   do mon=mymonstart,mymonstop

      if (.not. dohourly) then
         ndpermon = 1
      else
         ndpermon = dayspermon(mon)
         if ((mod(yr,4) .eq. 0) .and. (mon .eq. 2)) then
            ndpermon = ndpermon + 1
         endif
      endif

      do day=1,ndpermon
          count=count+1
          call write_sib4_outfile(infiles(count), &
               nsites, nmaxpft, &
               siteref, sitepnum, siteparea, &
               yr, doyper, doystart, ntstart, ntime)
          ntstart=ntstart+ntime
      enddo !day
   enddo !mon
   
   doystart = doystart+doyper
enddo


!Write MERRA output to file
count=0
doystart=0
doyper=0
ntstart=1
print('(a)'),     ' Processing MERRA'
do yr=startyr, stopyr
   mymonstart=1
   if (yr .eq. startyr) mymonstart=startmon
   mymonstop=12
   if (yr .eq. stopyr) mymonstop=stopmon
   doyper=365
   if (mod(yr,4) .eq. 0) doyper=366

   do mon=mymonstart,mymonstop
      ntime=1
      if ((dodaily) .or. (dohourly)) then
         ntime=dayspermon(mon)
         if ((mod(yr,4) .eq. 0) .and. (mon .eq. 2)) then
             ntime=ntime+1
         endif
      endif
      if (dohourly) ntime=ntime*24
      count=count+1
      
      call write_merra_outfile(merrafiles(count), &
           nsites, siteref, ntstart, ntime)
      ntstart=ntstart+ntime
   enddo !mon
   
   doystart = doystart+doyper
enddo


!Write Fluxnet data to file
print*,''
print*,'Processing Fluxnet Data'
allocate(fprefix(nsites))
allocate(fneename(nsites),fgppname(nsites),frename(nsites))
allocate(flename(nsites),fshname(nsites))
allocate(fprname(nsites),fswname(nsites))
allocate(flstflag(nsites),fhrshift(nsites))

call get_site_info(nsites,sitename, &
     fprefix, fneename, fgppname, frename, &
     fprname, flename, fshname, fswname, flstflag)
call get_site_toffset(nsites, sitelon, &
     flstflag, fhrshift)
call write_fluxnet_outfile( fluxdir, &
     nsites, sitename, fprefix, &
     fneename, fgppname, frename, &
     fprname, flename, fshname, fswname, fhrshift)

!Write MODIS LAI data to file
IF (.not. dohourly) then
   print*,''
   print*,'Processing MODIS LAI Data'
   allocate(modisfiles(2,nsites))
   call get_modis_fnames(nsites,sitename, &
        modisdir, modisfiles)
   call write_modis_outfile( nsites,sitename, &
        modisfiles)
ENDIF

!Close output file
call close_outfile()

print*,''
print*,'Finished Pulling Site Fluxes: ',trim(outname),trim(outsuffix)
print*,''

end program pullflux

