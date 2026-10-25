!=============================================================
subroutine combine_output(nfiles, filenamesin, filenamesout)
!=============================================================

use sibinfo_module
use netcdf

implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
   filenamesin, filenamesout

!netcdf variables
integer :: status, inid, outid
integer :: nsibid, nluid, ntimeid
integer :: nsibido, ntimeido
integer :: lonid, latid, timeid
integer, dimension(2) :: dimids
character(len=20) :: name, attname
integer :: xtype, varinid, varoutid
integer :: numvars, numatts
integer :: numdims, numdimsg, dimlen

!data variables
integer :: nsib, nlu, ntime
real, dimension(:), allocatable :: lonsib, latsib
real*8, dimension(:), allocatable :: time

real, dimension(:,:), allocatable :: lu_area
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3

!misc variables
integer :: f, n,l
integer :: att, dim, var
real    :: parea

!---------------------
do f=1,nfiles
   ! open input files
   status = nf90_open(trim(filenamesin(f)), nf90_nowrite, inid)
   IF (status .NE. nf90_noerr) THEN
       print*,'Error Opening Input File: '
       print*,' ',trim(filenamesin(f))
       print*,'Stopping.'
       stop
   ENDIF

   ! get dimension ids and sizes
   status = nf90_inquire(inid, nDimensions=numdims, &
            nVariables=numvars, nAttributes=numatts)
   status = nf90_inq_dimid(inid, nsibnamed, nsibid)
   status = nf90_inquire_dimension(inid, nsibid, len=nsib)
   status = nf90_inq_dimid(inid, nluname, nluid)
   status = nf90_inquire_dimension(inid, nluid, len=nlu)
   status = nf90_inq_dimid(inid, 'time', ntimeid)
   status = nf90_inquire_dimension(inid, ntimeid, len=ntime)

   dimids(1)=nsibid
   dimids(2)=ntimeid

   ! open output files
   status = nf90_open(trim(filenamesout(f)), nf90_write, outid)

   !....create new output file if does not exist
   IF (status .NE. nf90_noerr) THEN
       print*,'Creating Output File: '
       print*,' ',trim(filenamesout(f))

       if (use_deflate) then
           status = nf90_create(trim(filenamesout(f)), &
                     nf90_hdf5, outid)
       else
           status = nf90_create(trim(filenamesout(f)), &
                     nf90_64bit_offset, outid)
       endif
       if (status .ne. nf90_noerr) THEN
          print*,'Error Creating Output File!'
          STOP
       endif

       ! copy global attributes
       do att = 1, numatts
          status = nf90_inq_attname(inid, nf90_global, att, name)
          status = nf90_copy_att(inid, nf90_global, trim(name), &
                    outid, nf90_global)
       enddo
       
       ! copy dimensions
       numdimsg = 0 
       do dim = 1, numdims
          status = nf90_inquire_dimension(inid, dim, name=name, len=dimlen)
          IF ((trim(name) .ne. nluname) .and. &
              (trim(name) .ne. npftname) .and. &
              (trim(name) .ne. ncharname)) THEN
              numdimsg = numdimsg + 1
              status = nf90_def_dim( outid, trim(name), dimlen, numdimsg)
          ENDIF
          IF ((trim(name) .eq. trim(nsibnamed)) .or. &
              (trim(name) .eq. trim(nsibnamer))) THEN
              nsibido = numdimsg
          ENDIF
          IF (trim(name) .eq. trim(timename)) THEN
              ntimeido = numdimsg
          ENDIF

       enddo

       ! copy lon, lat, and time
       status = nf90_def_var(outid, trim(lonname), nf90_float, &
                  nsibido, lonid)
       status = nf90_def_var(outid, trim(latname), nf90_float, &
                  nsibido, latid)
       status = nf90_def_var(outid, trim(timename), nf90_double, &
                  ntimeido, timeid)

       if (use_deflate) then
           status = nf90_def_var_deflate(outid, lonid, 1, 1, deflev)
           status = nf90_def_var_deflate(outid, latid, 1, 1, deflev)
           status = nf90_def_var_deflate(outid, timeid, 1, 1, deflev)
       endif

       status = nf90_enddef( outid )

       allocate(lonsib(nsib), latsib(nsib))
       allocate(time(ntime))
       status = nf90_inq_varid(inid,trim(lonname), varinid)
       status = nf90_get_var(inid,varinid,lonsib)
       status = nf90_put_var(outid, lonid, lonsib)
       IF (status .ne. nf90_noerr) THEN
          print*,'Error Putting Variable: ',trim(lonname)
          STOP
       ENDIF
       
       status = nf90_inq_varid(inid,trim(latname), varinid)
       status = nf90_get_var(inid,varinid,latsib)
       status = nf90_put_var(outid, latid, latsib)
       IF (status .ne. nf90_noerr) THEN
          print*,'Error Putting Variable: ',trim(latname)
          STOP
       ENDIF

       status = nf90_inq_varid(inid,trim(timename),varinid)
       status = nf90_get_var(inid,varinid,time)
       status = nf90_put_var(outid, timeid, time)
       IF (status .ne. nf90_noerr) THEN
          print*,'Error Putting Variable: ',trim(timename)
          STOP
       ENDIF
    
   ELSE
       status = nf90_inq_dimid(outid, trim(nsibnamed), nsibido)
       status = nf90_inq_dimid(outid, trim(timename), ntimeido)
   ENDIF

   ! allocate variables
   allocate(fvalues2(nsib,ntime))
   allocate(fvalues3(nsib,nlu,ntime))
   dimids(1)=nsibido
   dimids(2)=ntimeido

   ! get pft areas for combining
   allocate(lu_area(nsib,nlu))
   status = nf90_inq_varid(inid,lareaname,varinid)
   if (status /= nf90_noerr) then
       print*,'No PFT area saved for combining PFTs'
       print*,'Stopping.'
       stop
   endif
   status = nf90_get_var(inid,varinid,lu_area)

   ! process variables
   status = nf90_inquire(inid, nDimensions=numdims, &
             nVariables=numvars)

   do var = 1, numvars
      status = nf90_inquire_variable(inid, var, name=name, &
           xtype=xtype, ndims=numdims, natts=numatts )
      !print*,'     Variable: ',trim(name)

      !Process diagnostics
      if ((xtype .eq. nf90_float) .and. &
          (numdims .eq. 3)) then
          status = nf90_inq_varid(outid,trim(name),varoutid)
          IF (status .NE. nf90_noerr) THEN
              status = nf90_redef(outid)
              status = nf90_def_var(outid,trim(name),xtype, &
                        dimids, varoutid)
              if (use_deflate) then
                  status = nf90_def_var_deflate(outid,varoutid,1,1,deflev)
              endif
              IF (status .NE. nf90_noerr) THEN
                 print*,'Error Defining Variable: ',trim(name)
                 print*,'xtype: ',xtype
                 print*,'dimids: ',dimids
                 print*,'varoutid: ',varoutid
                 print*,'Stopping.'
                 stop
              ENDIF      

              do att=1,numatts
                 status = nf90_inq_attname(inid,var,att,attname)
                 status = nf90_copy_att(inid,var,trim(attname),outid,varoutid)
              enddo

              status = nf90_enddef(outid)
          ENDIF

          ! copy variables to output file
          status = nf90_get_var(inid, var, fvalues3)
 
          fvalues2(:,:) = 0.0
          do n=1,nsib
              do l=1,nlu
                  parea=lu_area(n,l)
                  if (parea .gt. 0.001) then
                     if (trim(name) .eq. 'resp_fire') then
                        fvalues2(n,:) = fvalues2(n,:) + fvalues3(n,l,:)
                     else
                        fvalues2(n,:) = fvalues2(n,:) + &
                             fvalues3(n,l,:)*parea
                     endif
                  endif
               enddo
          enddo

          status = nf90_put_var( outid, varoutid, fvalues2 )
          IF (status .NE. nf90_noerr) THEN
             print*,'Error Putting Data Into File: '
             print*,' ',trim(filenamesout(f))
             print*,'Stopping.'
        
          ENDIF
      endif
   enddo !var=1,numvars

   ! close all files
   status = nf90_close(inid)
   status = nf90_close(outid)

   ! deallocate variables
   deallocate(fvalues2,fvalues3)
   deallocate(lu_area)

enddo !f=1,nfiles

end subroutine combine_output








