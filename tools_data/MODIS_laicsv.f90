!Program to convert MODIS LAI data in csv files
!to netcdf.
!
!Please note, the date in the file must be changed!!
!Replace all date "/" with "," before using this program.
!
!Kathy Haynes, Aug 2019

Program MODIS_laicsv

use netcdf
implicit none

! parameters
character(len=100), parameter :: &
       specfile= 'MODIS_laicsv.txt'
integer, parameter :: yrlen=365
integer, dimension(12), parameter :: &
     doymonth = [0,31,59,90,120,151,181,212,243,273,304,334]

! file variables
character(len=6) :: sitename
character(len=8) :: sat, band
character(len=7) :: product
character(len=100) :: indir, outdir, trash
character(len=180), dimension(2) :: infile, outfile
integer :: iday, imonth, iyear
real :: val_min, val_max, val_sum, val_range, val_mean, val_var, val_std

! netcdf variables
integer :: ncid, status, tdid
integer :: dayid, monid, yrid, timeid, laiid

! misc variables
integer :: fid, iindex
real :: ftime


!File the files names to read/create
open(unit=48,file=trim(specfile),form='formatted',status='old',iostat=status)
read(48,*) trash
read(48,*) sitename
read(48,*) trash
read(48,*) indir
read(48,*) trash
read(48,*) outdir
close(48)

infile(1) = trim(indir) // sitename // '_MODLai.csv'
infile(2) = trim(indir) // sitename // '_MYDLai.csv'
outfile(1) = trim(outdir) // 'MOD15A2_' // sitename // '.nc'
outfile(2) = trim(outdir) // 'MYD15A2_' // sitename // '.nc'


!Open the original files and process
do fid = 1, 2
   !...Open input file
   print*,'Reading File: ',trim(infile(fid))
   open(unit=36,file=trim(infile(fid)),form='formatted',status='old',iostat=status)
   if(status /= 0) stop 'bad input filename'
   read(36,*) trash

   !...Open output file
   status = nf90_create(trim(outfile(fid)),nf90_64bit_offset,ncid )
   if (status .ne. nf90_noerr) then
       print*,'Error Opening Output File.'
       return
    else
       print*,'Writing File: ',trim(outfile(fid))
    endif

    status = nf90_def_dim(ncid,'time',nf90_unlimited,tdid)
    status = nf90_def_var(ncid,'time',nf90_float,(/tdid/),timeid)
    status = nf90_def_var(ncid,'year',nf90_int,(/tdid/),yrid)
    status = nf90_def_var(ncid,'month',nf90_int,(/tdid/),monid)
    status = nf90_def_var(ncid,'day',nf90_int,(/tdid/),dayid)
    status = nf90_def_var(ncid,'lai',nf90_float,(/tdid/),laiid)
    status = nf90_enddef(ncid)

    iindex = 1
    do
       read(36,*,iostat=status) sat, product, band, imonth, iday, iyear, &
            val_min, val_max, val_sum, val_range, val_mean, val_var, val_std
       if(status /= 0) exit

        iyear=iyear+2000
        ftime=iyear + real(doymonth(imonth) + iday)/real(yrlen)
        status = nf90_put_var(ncid,timeid,ftime,start=(/iindex/))
        status = nf90_put_var(ncid,yrid,iyear,start=(/iindex/))
        status = nf90_put_var(ncid,monid,imonth,start=(/iindex/))
        status = nf90_put_var(ncid,dayid,iday,start=(/iindex/))
        status = nf90_put_var(ncid,laiid,val_mean,start=(/iindex/))
        iindex = iindex + 1
    enddo

    status = nf90_close(ncid)

enddo !fid

end program MODIS_laicsv

   
