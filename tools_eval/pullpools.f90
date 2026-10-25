!----------------------------------------------------
program pullpools
!----------------------------------------------------
!Program to pull global mean pools by PFT from SiB4.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!   The input is expected to be monthly files.
!
!To compile:
!>make pullpools
!
!kdhaynes, 2018/06

use netcdf
implicit none

!input variables
character(len=100), parameter :: specfile='pullpools.txt'
integer :: startmon, stopmon, startyr, stopyr
character(len=256) :: sibdir, outfile
character(len=256), dimension(:), allocatable :: &
   infiles
integer :: nvars
character(len=18), dimension(:), allocatable :: &
   varnames
character(len=20), dimension(:), allocatable :: &
   vartitles
character(len=38) :: trash

!netcdf variables
character(len=256) :: prefix
character(len=30) :: suffix
integer :: status, ncid
integer :: dimid, varid

!data variables
integer :: nsib, nlu, npft, ntime, maxpftref
character(len=3), dimension(:), allocatable :: pft_names
integer*1, dimension(:), allocatable :: pft_refnums
integer, dimension(:), allocatable :: pftrns
integer, dimension(:,:), allocatable :: lu_pref
real, dimension(:,:,:,:), allocatable :: data, datatemp

!stat variables
integer :: cntdata
integer, dimension(:), allocatable :: cntpft
real, dimension(:,:,:), allocatable :: meandata
real, dimension(:,:), allocatable :: meanpft, stdpft

!misc variables
integer :: ns, nl, mon, yr
integer :: t, v
integer :: mypft, myref
integer :: nmon, mymonstart, mymonstop
integer :: spos1, spos2

!parameters
character(len=3), dimension(12), parameter :: &
    monname = ['Jan','Feb','Mar','Apr','May','Jun', &
             'Jul','Aug','Sep','Oct','Nov','Dec']

character(len=27), dimension(15), parameter :: &
    pft_titles = (/  &
           'Bare Ground                ', &
           'Evergreen Needleleaf Forest', &
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
           'Winter Wheat               '/)

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
print*,''
print('(a,i5,a)'), ' Processing ',nmon, ' Months'

read(33,('(a38)')) trash
read(33,('(a256)')) sibdir

spos1=index(sibdir,'xxx')
prefix=sibdir(1:spos1-1)
spos2=len_trim(sibdir)
suffix=sibdir(spos1+3:spos2)
allocate(infiles(nmon))
nmon=0
do yr=startyr, stopyr
   mymonstart=1
   if (yr .eq. startyr) mymonstart=startmon
   mymonstop=12
   if (yr .eq. stopyr) mymonstop=stopmon

   do mon=mymonstart,mymonstop
      nmon=nmon+1
      write(infiles(nmon),'(a,i4.4,i2.2,a)') &
          trim(prefix), yr, mon, trim(suffix)
   enddo
enddo

read(33,'(a38)') trash
read(33,'(a256)') outfile
read(33,'(a38)') trash
read(33,'(i3)') nvars
allocate(varnames(nvars))
allocate(vartitles(nvars))
do v=1, nvars
   read(33,'(a38)') trash
   spos1 = index(trash,',')
   spos2 = len_trim(trash)
   varnames(v) = trash(1:spos1-1)
   vartitles(v) = trash(spos1+1:spos2)
enddo
close(33)


!Save Information
do mon=1,nmon
   status=nf90_open(trim(infiles(mon)),nf90_nowrite,ncid)
   if (status .ne. nf90_noerr) then
      print*,'Error Opening File: '
      print*,'  ',trim(infiles(mon))
      print*,'Error Message: '
      print*,'  ',trim(nf90_strerror(status))
      print*,'Stopping.'
      STOP
   !else
   !   print*,'Opening File: '
   !   print*,trim(infiles(mon))
   endif

   if (mon .eq. 1) then
      status=nf90_inq_dimid(ncid,'landpoints',dimid)
      status=nf90_inquire_dimension(ncid,dimid,len=nsib)
      status=nf90_inq_dimid(ncid,'nlu',dimid)
      status=nf90_inquire_dimension(ncid,dimid,len=nlu)
      status=nf90_inq_dimid(ncid,'npft',dimid)
      status=nf90_inquire_dimension(ncid,dimid,len=npft)
      IF (npft .ne. 15) THEN
         print*,'Please Change PFT Titles.'
         STOP
      ENDIF
      print('(a,i8,a,i2,a,i2)'),' Dimensions (nsib/nlu/npft): ', &
              nsib,'  ',nlu,'  ',npft

      allocate(pft_names(npft))
      status=nf90_inq_varid(ncid,'pft_names',varid)
      status=nf90_get_var(ncid,varid,pft_names)
      IF (status .ne. nf90_noerr) THEN
         print*,'Problem Getting PFT Names.'
         STOP
      ENDIF

      allocate(pft_refnums(npft))
      status=nf90_inq_varid(ncid,'pft_refnums',varid)
      status=nf90_get_var(ncid,varid,pft_refnums)
      IF (status .ne. nf90_noerr) THEN
         print*,'Problem Getting PFT Ref Numbers.'
      ENDIF

      maxpftref = maxval(pft_refnums)
      allocate(pftrns(maxpftref))
      do nl=1, npft
         pftrns(pft_refnums(nl)) = nl
      enddo

      allocate(lu_pref(nsib,nlu))
      status=nf90_inq_varid(ncid,'lu_pref',varid)
      status=nf90_get_var(ncid,varid,lu_pref)
      IF (status .ne. nf90_noerr) THEN
         print*,'Problem Getting PFT References.'
         STOP
      ENDIF

      allocate(cntpft(npft))
      allocate(data(nsib,nlu,nmon,nvars))
      allocate(meandata(nsib,nlu,nvars))
      allocate(meanpft(npft,nvars))
      allocate(stdpft(npft,nvars))

      cntpft(:) = 0
      data(:,:,:,:) = 0.
      meandata(:,:,:) = 0.
      meanpft(:,:) = 0.
      stdpft(:,:) = 0.

   endif

   status=nf90_inq_dimid(ncid,'time',dimid)
   status=nf90_inquire_dimension(ncid,dimid,len=ntime)
   allocate(datatemp(nsib,nlu,ntime,nvars))
   datatemp(:,:,:,:) = 0.

   do v=1,nvars
      IF ((trim(varnames(v)) .ne. 'agb') .and. &
          (trim(varnames(v)) .ne. 'bgb')) THEN
          status=nf90_inq_varid(ncid,trim(varnames(v)),varid)
          IF (status .ne. nf90_noerr) THEN
             print*,'Missing Variable: ', trim(varnames(v))
             STOP
          ENDIF
          status=nf90_get_var(ncid,varid,datatemp(:,:,:,v))
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Getting Variable: ',trim(varnames(v))
             STOP
          ENDIF
      ENDIF
   enddo

   if (ntime .ge. 1) then
      do v=1,nvars
         do nl=1,nlu
            do ns=1,nsib
               data(ns,nl,mon,v) = data(ns,nl,mon,v) + sum(datatemp(ns,nl,:,v))/ntime
            enddo
         enddo
      enddo

      do v=1,nvars
         if (trim(varnames(v)) .eq. 'agb') THEN
             data(:,:,:,v) = data(:,:,:,2) + &
                  data(:,:,:,5) + &
                  data(:,:,:,6) + data(:,:,:,7) + &
                  data(:,:,:,8) + data(:,:,:,9)
         elseif (trim(varnames(v)) .eq. 'bgb') THEN
             data(:,:,:,v) = data(:,:,:,3) + data(:,:,:,4) + &
                  data(:,:,:,10) + &
                  data(:,:,:,11) + data(:,:,:,12) 
         endif
      enddo
   endif

   status = nf90_close(ncid)
   deallocate(datatemp)
enddo

!Calculate the mean
do nl=1,nlu
   do ns=1,nsib
      mypft = lu_pref(ns,nl)
      IF (mypft .gt. 0) THEN
          cntdata = 0
          myref = pftrns(mypft)
         
          do t=1,nmon
             cntdata = cntdata + 1
             cntpft(myref) = cntpft(myref) + 1

             do v=1,nvars
                meandata(ns,nl,v) = meandata(ns,nl,v) + data(ns,nl,t,v)
                meanpft(myref,v) = meanpft(myref,v) + data(ns,nl,t,v)
             enddo
          enddo !t=1,nmon
          meandata(ns,nl,:) = meandata(ns,nl,:) / cntdata
      ENDIF!mypft > 0
   enddo !ns=1,nsib
enddo !nl=1,nlu

do nl=1,npft
   if (cntpft(nl) .gt. 0) then
       meanpft(nl,:) = meanpft(nl,:)/cntpft(nl)
   endif
enddo

!Calculate stddev, in space only (ignoring time)
cntpft(:) = 0
do nl=1,nlu
   do ns=1,nsib
      mypft = lu_pref(ns,nl)
      IF (mypft .gt. 0) then
         cntpft(myref) = cntpft(myref) + 1
         myref = pftrns(mypft)
         do v=1,nvars
            stdpft(myref,v) = stdpft(myref,v) + &
                  (meandata(ns,nl,v) - meanpft(myref,v))**2.
         enddo !v=1,nvars
      ENDIF
   enddo !ns=1,nsib
enddo !nl=1,nlu

do nl=1,npft
   if (cntpft(nl) .gt. 1) then
       stdpft(nl,:) = sqrt(stdpft(nl,:)/(cntpft(nl)-1))
   else
       stdpft(nl,:) = 0.
   endif
enddo


!Write the results to a file
print*,'Writing Output To File: '
print*,'   ',trim(outfile)
open(unit=10,file=trim(outfile),form='formatted')
write(10,'(a,a,a,i4,a,a,a,i4)') &
     'SiB4 Information For ', &
     monname(startmon), ' ', startyr, &
     ' to ', monname(stopmon),' ', stopyr
write(10,'(a)') 'MEAN: Mean Over Time and Space'
write(10,'(a)') 'STDDEV: Standard Deviation Over Space Only'
write(10,'(a)') 'LAI units are m2/m2'
write(10,'(a)') 'Pool units are Mg C/ha'
write(10,*) ''
do nl=2,npft
    write(10,'(2a)') pft_titles(nl), '   MEAN      STDDEV'
    do v=1,nvars
       if (varnames(v) .eq. 'agb') THEN
          write(10,'(a)') '     ----------------------------------------'
       endif
       write(10,'(2a,2f10.3)')  '     ',vartitles(v), meanpft(nl,v), stdpft(nl,v)

    enddo
    write(10,'(a)') ''
enddo
close(10)


print*,'Finished Pulling Global Pool Information.'
print*,''

end program pullpools

