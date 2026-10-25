!itb...program to process MOD15A2 MODIS 7x7 composite 
!itb...LAI/fPAR data for individual tower sites.

!Created by: Ian Baker, October 2010
!Modified by: Kathy Haynes, June 2015

!f90nc MODIS_laifpar.f90

   Program MODIS_laifpar

   use netcdf
   implicit none


!......................

   character(len=100), parameter :: &
       specfile= 'MODIS_laifpar.txt'

   integer, parameter :: &
       ioutputnc=1  ! =0 for text output  =1 for netcdf output
   real, parameter :: laimax=-999.    ! set this to a maximum LAI, 
                                      !   for sites with unrealistic jumps
   integer, parameter :: uselaimax=0  ! 0=do not use laimax 1=use laimax for individual pixels 
                                      ! 2=use for end total value

   integer, parameter :: qflag = 2    ! determines amount of data screening
                                      ! qflag=0 means minimal screening, increasing
                                      ! qflag (up to 3) imposes more restrictions
                                      ! on pixel use. exact screening criteria 
                                      ! explained below
   logical, parameter :: print_finfo = .false.  ! print filename information, stopping

   character(len=100) :: hdfname,product,date,site,procdate,band
   character(len=100) :: trash, infiletemp, outfiletemp
   character(len=100), dimension(2) :: infile, outfile

   integer :: status
   integer :: idate
   integer :: iyear
   integer :: ijday
   integer :: imonth
   integer :: iday
   integer :: numobs

   integer :: iindex,iindex1,iindex2
   integer :: oindex1,oindex2

   integer :: startyear,endyear,numyear

   integer,dimension(49) :: data

   real,dimension(49)    :: lai,fpar,lai_std,fpar_std
   integer,dimension(49) :: qc,eqc

   integer,dimension(49,4)  :: qc_comp
   integer,dimension(49,7)  :: eqc_comp

   real,dimension(:,:),allocatable :: dat_all_in

   real,dimension(46,3) :: modmean
   real,dimension(46)   :: datmean

   integer :: i0,i1,i2,i3,i4

   real :: fcount
   real :: lai_out,fpar_out
   integer, dimension(2) :: ivar

   real :: ftime
   integer :: yrlen

   !...netcdf variables
   integer :: ncstatus, ncid
   integer :: tdid
   integer :: tid, yid, jdayid, monid, dayid, laiid, fparid, nobsid

   DATA datmean/1.,9.,17.,25.,33.,41.,49.,57.,65.,73.,81.,89.,97.,105.,   &
                113.,121.,129.,137.,145.,153.,161.,169.,177.,185.,193.,     &
                201.,209.,217.,225.,233.,241.,249.,257.,265.,273.,281.,     &
                289.,297.,305.,313.,321.,329.,337.,345.,353.,361./

   !...loop variables
   integer :: fid

!File the files names to read/create
open(unit=48,file=trim(specfile),form='formatted',status='old',iostat=status)
read(48,*) trash
read(48,*) infiletemp
read(48,*) trash
read(48,*) outfiletemp

!From the file name, determine if MOD or MYD
iindex1 = index(trim(infiletemp),'MOD')
iindex2 = index(trim(infiletemp),'MYD')
oindex1 = index(trim(outfiletemp),'MOD')
oindex2 = index(trim(outfiletemp),'MYD')

infile(1:2) = infiletemp
outfile(1:2) = outfiletemp

if (iindex1 > 0) then
    ivar(1) = 10
    ivar(2) = 11

    infile(2)(iindex1:iindex1+2) = 'MYD'
    outfile(2)(oindex1:oindex1+2) = 'MYD'

elseif (iindex2 > 0) then
    ivar(1) = 11
    ivar(2) = 10

    infile(2)(iindex2:iindex2+2) = 'MOD'
    outfile(2)(oindex2:oindex2+2) = 'MOD'
else
    print*,'Expecting MOD or MYD file!'
    stop
endif

if (print_finfo) then
    print*,'Input File 1: '
    print*,' ',trim(infile(1))
    print*,'Input File 2: '
    print*,' ',trim(infile(2))
    print*,'Output File 1: '
    print*,' ',trim(outfile(1))
    print*,'Output File 2: '
    print*,' ',trim(outfile(2))
    stop
endif

!Open the original files and process
do fid = 1, 2
   print*,'Reading File: ',trim(infile(fid))
   open(unit=36,file=trim(infile(fid)),form='formatted',status='old',iostat=status)
   if(status /= 0) stop 'bad input filename'


   !itb...count the number of obs in the file
   numobs = 0

   !itb...read past header line
   read(36,*)hdfname
   !print*,trim(hdfname)

   do
     numobs = numobs + 1
     do i0 = 1,6
       read(36,*,iostat=status)hdfname
       if(status /= 0) exit
     enddo
     if(status /= 0) exit
   enddo
   rewind(36)
   print*,'  Number of MODIS records:',numobs
   allocate(dat_all_in(numobs,6))

   if (ioutputnc == 0) then
      open(unit=37,file=trim(outfile(fid)),form='formatted',status='unknown')
      write(37,*)numobs
      write(37,'(a)')' index   year  jday   month   day    LAI           fPAR           # obs' 
   else
      ncstatus = nf90_create(trim(outfile(fid)),nf90_64bit_offset,ncid )
      if (ncstatus .ne. nf90_noerr) then
          print*,'Error Opening Output File.'
          return
      else
          print*,'Writing File: ',trim(outfile(fid))
      endif

      ncstatus = nf90_def_dim(ncid,'time',nf90_unlimited,tdid)
      ncstatus = nf90_def_var(ncid,'time',nf90_float,(/tdid/),tid)
      ncstatus = nf90_def_var(ncid,'year',nf90_int,(/tdid/),yid)
      ncstatus = nf90_def_var(ncid,'jday',nf90_int,(/tdid/),jdayid)
      ncstatus = nf90_def_var(ncid,'month',nf90_int,(/tdid/),monid)
      ncstatus = nf90_def_var(ncid,'day',nf90_int,(/tdid/),dayid)
      ncstatus = nf90_def_var(ncid,'lai',nf90_float,(/tdid/),laiid)
      ncstatus = nf90_def_var(ncid,'fpar',nf90_float,(/tdid/),fparid)
      ncstatus = nf90_def_var(ncid,'nobs',nf90_int,(/tdid/),nobsid)

      ncstatus = nf90_enddef(ncid)
   endif

   !itb...read past header line
   read(36,*)hdfname
   iindex = 0

   do 
     do i0 = 1,6
        read(36,*,iostat=status)hdfname,product,date,site,procdate,band  ,     &
                             (data(i1),i1=1,49)

        if(status /= 0) exit

        read(date(2:8),'(i7.7)')idate
        iyear = idate/1000
        ijday = mod(idate,1000)

        call jd_proc

       !itb...reassign the variables
        if(trim(band) == 'FparExtra_QC') then
          do i1 = 1,49
            eqc(i1) = data(i1)
          enddo
        endif

        !itb...parse out the components
        do i1 = 1,49
          eqc_comp(i1,1) = mod(eqc(i1),100)
          eqc_comp(i1,2) = mod(eqc(i1)/100,10)
          eqc_comp(i1,3) = mod(eqc(i1)/1000,10)
          eqc_comp(i1,4) = mod(eqc(i1)/10000,10)
          eqc_comp(i1,5) = mod(eqc(i1)/100000,10)
          eqc_comp(i1,6) = mod(eqc(i1)/1000000,10)
          eqc_comp(i1,7) = eqc(i1)/10000000
        enddo

        if(trim(band) == 'FparLai_QC') then
          do i1 = 1,49
            qc(i1) = data(i1)
          enddo
        endif

        !itb...parse out the components
        do i1 = 1,49
          qc_comp(i1,1) = mod(qc(i1),100)
          qc_comp(i1,2) = mod(qc(i1)/100,10)
          qc_comp(i1,3) = mod(qc(i1)/1000,100)
          qc_comp(i1,4) = qc(i1)/100000
        enddo

        if(trim(band) == 'FparStdDev_1km') then
          do i1 = 1,49
            fpar_std(i1) = float(data(i1)) 
          enddo
        endif

        if(trim(band) == 'Fpar_1km') then
          do i1 = 1,49
            fpar(i1) = float(data(i1)) * 0.01
          enddo
        endif

        if(trim(band) == 'LaiStdDev_1km') then
          do i1 = 1,49
            lai_std(i1) = float(data(i1)) 
          enddo
        endif

        if(trim(band) == 'Lai_1km') then
          do i1 = 1,49
            lai(i1) = float(data(i1)) * 0.1
          enddo

        endif

     enddo   ! i0; 6 component parts of a record


   !itb...component information in! now calculate fPAR and LAI using relevant info...
   !    QC flags:   flag        name   value       definition
   !                 1        MODLAND   00          best possible
   !                                    01          OK, but not the best (?)
   !                                    10          not produced, due to cloud        
   !                                    11          not produced, due to other reasons
   !
   !                 2   DEAD-DETECTOR   0          detectors apparently fine for up to 
   !                                                   50% of channels 1, 2
   !                                     1          dead detectors caused >50% adjacent
   !                                                   detector retrievals
   !
   !                 3   CLOUDSTATE     00          significant clouds NOT present (clear)
   !                                    01          significant clouds WERE present
   !                                    10          mixed cloud present on pixel
   !                                    11          cloud state not defined, assumed clear
   !
   !                 4   SCF_QC        000          main (RT) method used with best possible results
   !                                   001          main (RT) method used with saturation
   !                                   010          main (RT) method failed due to geometry problems,
   !                                                   empirical method used
   !                                   011          main (RT) method failed due to problems other than
   !                                                   geometry, empirical method used
   !                                   100          couldn't retrieve pixel

   !   EQC flags:   flag        name   value       definition
   !                  1        LANDSEA   00          land
   !                                     01          shore
   !                                     10          freshwater
   !                                     11          ocean
   !
   !                  2       SNOW_ICE    0          no snow or ice detected
   !                                      1          snow, ice were detected
   !        
   !                  3       AEROSOL     0          no low or atmospheric aerosol levels detected
   !                                      1          average or high aerosol levels detected 
   !
   !                  4        CIRRUS     0          no cirrus detected
   !                                      1          yup, cirrus detected
   !
   !            5   INTERNAL_CLOUD_MASK   0          no clouds detected
   !                                      1          clouds detected
   !
   !                  6  CLOUD_SHADOW     0          no cloud shadow detected
   !                                      1          cloud shadow detected
   !
   !                  7    SCF_MASK       0          custom SCF mask, EXCLUDE this pixel
   !                                      1          custom SCF mask, INCLUDE this pixel

   !itb...SCREENING CRITERIA
   !            qflag = 0             value used if MODLAND < 10, no other screening performed
   !
   !            qflag = 1               "     "   " MODLAND < 10
   !                                                CLOUDSTATE = 0 or CLOUDSTATE = 11
   !
   !            qflag = 2               "     "   " MODLAND < 10
   !                                                DEAD_DETECTOR = 0
   !                                                CLOUDSTATE = 0 or CLOUDSTATE = 11
   !                                                SCF_QC < 10
   !
   !itb...selecting 3, or 'everything', has the effect of excluding all data...
   !            qflag = 3 (everything)  "     "   " MODLAND < 10
   !                                                DEAD_DETECTOR = 0
   !                                                CLOUDSTATE = 0 or CLOUDSTATE = 11
   !                                                SCF_QC < 100
   !                                                LANDSEA = 0
   !                                                SNOW_ICE = 0
   !                                                AEROSOL = 0
   !                                                CIRRUS = 0
   !                                                INTERNAL_CLOUD_MASK = 0
   !                                                CLOUD_SHADOW = 0
   !                                                SCF_MASK = 1
   !
   !           qflag = 4 (custom) : user will need to enter selection criteria


     fcount   = 0.0
     lai_out  = 0.0
     fpar_out = 0.0

     do i1 = 1,49

       if(qflag == 0) then   

         if(qc_comp(i1,1) < ivar(fid)) then     ! MODLAND; most basic    

           fcount = fcount + 1.0
           lai_out = lai_out + lai(i1)
           fpar_out = fpar_out + fpar(i1)
   
         endif
   
       elseif(qflag == 1) then

         if(qc_comp(i1,1) < ivar(fid)    .AND.     &                        ! MODLAND
             ( qc_comp(i1,3) == 0 .OR. qc_comp(i1,3) == 11) )then    ! CLOUDSTATE

           fcount = fcount + 1.0
           lai_out = lai_out + lai(i1)
           fpar_out = fpar_out + fpar(i1)
   
         endif


       elseif(qflag == 2) then

         if(qc_comp(i1,1) < ivar(fid)    .AND.     &       ! MODLAND
            qc_comp(i1,2) == 0    .AND.     &         ! DEAD_DETECTOR
             ( qc_comp(i1,3) == 0 .OR. qc_comp(i1,3) == 11) .AND. &  ! CLOUDSTATE
            qc_comp(i1,4) < 100 ) then                ! SCF_QC

           if ((lai(i1) .gt. laimax) .and. &
               ( uselaimax .eq. 1)) then
               print*,'Bad LAI Value: ',lai(i1)
           else
              fcount = fcount + 1.0
              lai_out = lai_out + lai(i1)
              fpar_out = fpar_out + fpar(i1)
           endif
         endif
       
       elseif(qflag == 3) then         

         if(qc_comp(i1,1) < ivar(fid)  .AND.     &                        ! MODLAND
            qc_comp(i1,2) == 0    .AND.     &                        ! DEAD_DETECTOR
             ( qc_comp(i1,3) == 0 .OR. qc_comp(i1,3) == 11) .AND. &  ! CLOUDSTATE
            qc_comp(i1,4) < 100   .AND.                           &  ! SCF_QC
            eqc_comp(i1,1) == 0   .AND.                           &  ! LANDSEA
            eqc_comp(i1,2) == 0   .AND.                           &  ! SNOW_ICE
            eqc_comp(i1,3) == 0   .AND.                           &  ! AEROSOL
            eqc_comp(i1,4) == 0   .AND.                           &  ! CIRRUS
            eqc_comp(i1,5) == 0   .AND.                           &  ! CLOUD_MASK
            eqc_comp(i1,6) == 0   .AND.                           &  ! CLOUD_SHADOW
            eqc_comp(i1,7) == 1 ) then                               ! SCF_MASK

           fcount = fcount + 1.0
           lai_out = lai_out + lai(i1)
           fpar_out = fpar_out + fpar(i1)
   
         endif

       elseif(qflag == 4) then   ! custom

          stop'USER MUST ENTER CRITERIA'
 
       endif        ! custom/not custom 

      enddo   !  i1; 49 pixels

     if(fcount > 1.0 ) then
       lai_out = lai_out / fcount
       if (uselaimax .eq. 2) lai_out = MIN(laimax, lai_out)

       fpar_out = fpar_out / fcount
     else
       lai_out = -9999.9
       fpar_out = -9999.9
     endif

     iindex = iindex + 1
 
     if (ioutputnc .eq. 0) then
         write(37,'(5i6,3g15.5)'),iindex,iyear,ijday,imonth,iday,lai_out,fpar_out,fcount
     else
         if (mod(iyear,4) == 0) then
            yrlen=366
         else
            yrlen=365
         endif
         ftime=iyear + real(ijday)/real(yrlen)

         ncstatus = nf90_put_var(ncid,tid,ftime,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,yid,iyear,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,jdayid,ijday,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,monid,imonth,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,dayid,iday,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,laiid,lai_out,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,fparid,fpar_out,start=(/iindex/))
         ncstatus = nf90_put_var(ncid,nobsid,fcount,start=(/iindex/))

     endif

     !itb...assign data into the dat_all_in array
     dat_all_in(iindex,1) = float(iyear)
     dat_all_in(iindex,2) = float(ijday)
     dat_all_in(iindex,3) = float(imonth)
     dat_all_in(iindex,4) = float(iday)
     dat_all_in(iindex,5) = fpar_out
     dat_all_in(iindex,6) = lai_out

     if(status /= 0) exit

   enddo

   if (ioutputnc .eq. 0) then
       close(37)
   else
       ncstatus = nf90_close(ncid)
   endif
   !itb...data read in! now we can play games...

   !itb...first: determine starting and ending years, number of obs per month per year
   startyear =  100000
   endyear   = -100000
   do i0 = 1,numobs
     if(dat_all_in(i0,1) < startyear) startyear = int(dat_all_in(i0,1))
     if(dat_all_in(i0,1) > endyear)   endyear   = int(dat_all_in(i0,1))
   enddo

   print*,'  Start year=',startyear,'   End year=',endyear

   deallocate(dat_all_in)
enddo !fid

   contains

!-------------------------------
   subroutine jd_proc

     integer, dimension(12) :: jmonth

     DATA jmonth/31,28,31,30,31,30,31,31,30,31,30,31/

     imonth = 1
     i1     = 0
     i2     = 0
     do 
       i1 = i1 + 1
       i2 = i2 + 1
       if (i1 > jmonth(imonth)) then
        imonth = imonth + 1
        i1     = 1
       endif
       if(i2 == ijday) exit
     enddo

     iday = i1

     return
         
       

   end subroutine jd_proc

   end program MODIS_laifpar

   
