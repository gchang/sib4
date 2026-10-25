!==================================================
subroutine get_modis_fnames( &
     nsites, sitenames, modisdir, modisfiles)
!==================================================

implicit none

!input variables
integer, intent(in) :: nsites
character(len=6), dimension(nsites), intent(in) :: sitenames
character(len=256), intent(inout) :: modisdir
character(len=256), dimension(2,nsites), intent(inout) :: modisfiles

!filename variables
character(len=8) :: mprefix

!local variables
integer :: mf, ns

do mf = 1,2
   if (mf .eq. 1) then
      mprefix = 'MOD15A2_'
   else
      mprefix = 'MYD15A2_'
   endif

   do ns=1,nsites
      write(modisfiles(mf,ns),'(4a)') &
           trim(modisdir), mprefix, sitenames(ns), '.nc'
   enddo
enddo

RETURN

end subroutine get_modis_fnames



!==================================================
subroutine write_modis_outfile( &
     nsites, sitenames, modisfiles)
!==================================================

use ncmodule
use netcdf
implicit none

!input variables
integer, intent(in) :: nsites
character(len=*), dimension(nsites), intent(in) :: sitenames
character(len=*), dimension(2,nsites), intent(in) :: modisfiles

!file variables
integer, parameter :: nlaimax = 1040
integer :: ntimem
real*8 :: dtime
real   :: dtimem
real, dimension(nlaimax) :: timem
real, dimension(nlaimax) :: laim

!sib4 time variables
integer :: ntime
real*8, dimension(:), allocatable :: time
real, dimension(:), allocatable :: lai

!local variables
integer :: mf, ns, t
integer :: tcountm, tstart, tstartm
integer, dimension(:), allocatable :: tcountg
logical :: exist


!Get SiB4 Output Time
status=nf90_inq_dimid(outid,'time',dimid)
status=nf90_inquire_dimension(outid,dimid,len=ntime)
allocate(time(ntime))
status=nf90_inq_varid(outid,'time',varid)
status=nf90_get_var(outid,varid,time)
if (status .ne. nf90_noerr) then
     print*,'Error Getting Output Times in MODIS LAI Routine.'
     print*,'Stopping.'
     STOP
endif
if (ntime .eq. 1) then
   dtime = 1./12.*0.5
else
   dtime = time(2)-time(1)
endif

!Loop over sites
allocate(lai(ntime))
allocate(tcountg(ntime))
do ns=1,nsites
   lai(:) = badval
   tcountg(:) = 0
   
   !Get MODIS LAI data
   do mf=1,2
      timem(:) = badval
      laim(:) = badval
      inquire(file=trim(modisfiles(mf,ns)), exist=exist)

      if (exist) then
         status=nf90_open(trim(modisfiles(mf,ns)),nf90_nowrite,inid)
         !print*,'    Opening File: '
         !print*,'       ',trim(modisfiles(mf,ns))
         
         status=nf90_inq_dimid(inid,'time',dimid)
         status=nf90_inquire_dimension(inid,dimid,len=ntimem)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting MODIS Time-Steps.'
            print*,'   Site: ',sitenames(ns)
            print*,'Stopping.'
            STOP    
         endif

         status=nf90_inq_varid(inid,'time',varid)
         status=nf90_get_var(inid,varid,timem(1:ntimem))
         if (status .ne. nf90_noerr) then
            print*,'Error Getting MODIS Time.'
            print*,'   Site: ',sitenames(ns)
            print*,'Stopping.'
            STOP
         endif

         status=nf90_inq_varid(inid,'lai',varid)
         status=nf90_get_var(inid,varid,laim(1:ntimem))
         if (status .ne. nf90_noerr) then
            print*,'Error Getting MODIS LAI.'
            print*,'   Site: ',sitenames(ns)
            print*,'Stopping.'
            STOP
         endif
         
         status=nf90_close(inid)

      endif !file exists

      !See if any data/sib4 times match up
      if ((maxval(timem) .gt. time(1)) .and. &
           (timem(1) .lt. time(ntime))) then

         !Find matching starting times
         tstart=1
         tstartm=1
         dtimem = timem(2)-timem(1)
         if (timem(tstartm) .lt. time(tstart)-dtime) then
             do while (timem(tstartm) .lt. time(tstart)-dtime)
                  tstartm=tstartm+1
             enddo
         else
             do while (timem(tstartm) .gt. time(tstart)+dtime)
                  tstart=tstart+1
             enddo
         endif

         if (mf .eq. 1) then
              print('(2a)'),'   ',sitenames(ns)
              print('(2(a,f12.6,i6))'), &
                   '     SiB4: ', time(tstart), tstart, &
                   '     AQUA: ', timem(tstartm),tstartm
          else
              print('(2(a,f12.6,i6))'), &
                   '     SiB4: ', time(tstart), tstart, &
                   '    TERRA: ', timem(tstartm),tstartm
         endif

         !Loop over times to get matching points
         tcountm=tstartm
         do t=tstart,ntime
            if ((timem(tcountm) .ge. time(t)-dtime) .and. &
                 (timem(tcountm) .lt. time(t)+dtime)) then
               do while ((timem(tcountm) .lt. time(t)+dtime) .and. &
                         (timem(tcountm) .gt. 0.))
                        if (laim(tcountm) .gt. mminval) then
                           if (lai(t) .gt. mminval) then
                              lai(t) = lai(t) + laim(tcountm)
                              tcountg(t) = tcountg(t) + 1
                           else
                              lai(t) = laim(tcountm)
                              tcountg(t) = 1
                           endif
                        endif
                        tcountm=tcountm+1
                  enddo
             endif !matching times exist
          enddo
      endif !matching times exist
   enddo !MODIS AQUA and TERRA

   !Take the average between two valid MODIS values
   do t=1,ntime
      if (tcountg(t) .gt. 0) lai(t)=lai(t)/real(tcountg(t))
   enddo

   !Add site LAI to output file
   IF (maxval(lai) .gt. mminval) THEN
      print('(a,f10.3)'),'     MODIS MAX LAI: ', maxval(lai)
   ENDIF
   status=nf90_put_var(outid,laiid,lai, &
        start=(/ns,1/), count=(/1,ntime/))
   
enddo


RETURN

end subroutine write_modis_outfile
