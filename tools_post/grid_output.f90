!==================================================
subroutine grid_output(nfiles, filenamesin, &
           filenamesout, isglobal)
!==================================================

use sibinfo_module
use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
     filenamesin, filenamesout
logical, intent(in) :: isglobal

!netcdf variables
integer :: status, inid, outid
integer :: nsibdid, londid, latdid
integer :: varid
integer, dimension(:), allocatable :: dimidsin, dimidsout
integer, dimension(:), allocatable :: vardimsin, vardimsout

integer :: numdims, numdimsall
integer :: numvdimsin, numvdimsout
integer :: numvars, numatts
integer :: dimlen
integer, dimension(:), allocatable :: dimlensin, dimlensout

integer :: xtype
character(len=20) :: dimname, varname, attname
character(len=20), dimension(:), allocatable :: dimnamesin, dimnamesout

!grid variables
integer :: nsib
integer :: nlon, nlat
real :: deltalat, deltalon, truelatmin, truelonmin
real, dimension(:), allocatable :: lonsib, latsib
real, dimension(:), allocatable :: lon, lat
integer, dimension(:), allocatable :: lonref, latref

!data variables
integer*1, dimension(:), allocatable :: bvalues1
integer, dimension(:), allocatable :: ivalues1
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4
real*8, dimension(:), allocatable :: dvalues
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7), dimension(:), allocatable :: cvalues7 
character(len=10), dimension(:), allocatable :: cvalues10
character(len=6), dimension(:,:), allocatable :: cvalues62d

!misc variables
integer :: f, n
integer :: ival1in, ival2in, ival3in
integer :: ival1o, ival2o, ival3o, ival4o
integer :: att, dim, ddim, var
integer :: countd
integer :: strindex, strlen
character(len=256) :: shortname

!----------------------
DO f=1,nfiles
   !Open the input file
   status = nf90_open(trim(filenamesin(f)),nf90_nowrite,inid)
   IF (status .ne. nf90_noerr) THEN
      print*,'  Error Opening File: '
      print*,'   ',trim(filenamesin(f))
      RETURN
   ELSE
      print('(a,i5)'),'  File ',f
      strindex = index(trim(filenamesin(f)),'/', back=.true.)
      strlen = len_trim(filenamesin(f))
      shortname = filenamesin(f)(strindex:strlen)
      print*,'     In: ', trim(shortname)

      strindex = index(trim(filenamesout(f)),'/', back=.true.)
      strlen = len_trim(filenamesout(f))
      shortname = filenamesout(f)(strindex:strlen)
      print*,'     Out: ',trim(shortname)
   ENDIF

   IF (f .EQ. 1) THEN
       !Get dimensions and attributes
       status = nf90_inquire(inid, nDimensions=numdims, &
                 nVariables=numvars, &
                 nAttributes=numatts)

       allocate(dimlensin(numdims))
       allocate(dimnamesin(numdims))
       allocate(dimidsin(numdims))

       numdimsall=numdims+1
       allocate(dimlensout(numdimsall))
       allocate(dimnamesout(numdimsall))
       allocate(dimidsout(numdimsall))

       allocate(vardimsin(numdims))
       allocate(vardimsout(numdimsall))

       !Get nsib and original lat/lon
       status = nf90_inq_dimid(inid, nsibnamed, nsibdid)
       IF (status .ne. nf90_noerr) THEN
          status = nf90_inq_dimid(inid, 'nsib', nsibdid)
       ENDIF
       IF (status .ne. nf90_noerr) THEN
          print*,'Error Finding Number Of Points.'
          STOP
       ENDIF
       status = nf90_inquire_dimension(inid, nsibdid, len=nsib)

       allocate(lonsib(nsib),latsib(nsib))
       status = nf90_inq_varid(inid,trim(lonname),varid) 
       status = nf90_get_var(inid,varid,lonsib)
       status = nf90_inq_varid(inid,trim(latname),varid)
       status = nf90_get_var(inid,varid,latsib)

       !Calculate the grid information
       if (isglobal) then
           call grid_sib_nlonlat(nsib,lonsib,latsib, &
                nlon, nlat, deltalon, deltalat,   &
                truelonmin, truelatmin)
       else
          call grid_sib_nllloc(nsib, lonsib, latsib, &
               nlon, nlat, deltalon, deltalat, &
               truelonmin, truelatmin)
       endif

       allocate(lon(nlon),lat(nlat))
       allocate(lonref(nsib),latref(nsib))
       call grid_sib_ref(nsib,lonsib,latsib, &
             nlon,nlat,deltalon,deltalat, &
             truelonmin, truelatmin, &
             lon, lat, lonref, latref)
   ENDIF

   !Open the output file
   if (use_deflate) then
      status = nf90_create(trim(filenamesout(f)),nf90_netcdf4,outid)
      !status = nf90_create(trim(filenamesout(f)), nf90_hdf5, outid)
   else
      status = nf90_create(trim(filenamesout(f)),nf90_64bit_offset, outid)
   endif

   IF (status .ne. nf90_noerr) THEN
      print*,'Error Creating Output File: '
      print*,'  ',trim(filenamesout(f))
      print*,'Stopping.'
      STOP
   ENDIF

   !Copy global attributes over to output file
   do att=1,numatts
      status = nf90_inq_attname(inid,nf90_global,att,attname)
      status = nf90_copy_att(inid,nf90_global,trim(attname), &
                   outid,nf90_global)
   enddo

   !Define output file dimensions
   !print*,'   Defining dimensions'
   countd=1
   do dim=1,numdims
      status=nf90_inquire_dimension(inid,dim,name=dimname,len=dimlen)
      dimlensin(dim)=dimlen
      dimnamesin(dim)=dimname
      dimidsin(dim)=dim

      if (dimlensin(dim) .eq. nsib) then
          nsibdid = dim

          londid=countd
          dimlensout(countd) = nlon
          dimnamesout(countd) = 'lon'
          status=nf90_def_dim(outid,trim('lon'),dimlensout(countd), &
                              dimidsout(countd))
          countd=countd+1

          latdid=countd
          dimlensout(countd) = nlat
          dimnamesout(countd) = 'lat'
          status=nf90_def_dim(outid,trim('lat'),dimlensout(countd), &
                             dimidsout(countd))
          countd=countd+1
      else
          dimlensout(countd) = dimlensin(dim)
          dimnamesout(countd) = trim(dimname)
          status=nf90_def_dim(outid,trim(dimname),dimlensout(countd), &
                             dimidsout(countd))
          countd=countd+1
      endif
   enddo

   !Define the variables
   !print*,'   Processing variables'
   do var=1,numvars
      status = nf90_inquire_variable(inid,var,name=varname,   &
               xtype=xtype, ndims=numvdimsin, dimids=vardimsin, &
               natts=numatts)

      if (varname .eq. 'lonsib') then
         numvdimsout=1
         vardimsout(1)=londid
         varname='lon'
      elseif (varname .eq. 'latsib') then
         numvdimsout=1
         vardimsout(1)=latdid
         varname='lat'
      else  !find the new dimensions
          countd=1
          numvdimsout=numvdimsin
          do dim=1,numvdimsin   
             if (vardimsin(dim) .eq. nsibdid) then
                 vardimsout(countd)=londid
                 vardimsout(countd+1)=latdid

                 countd=countd+2
                 numvdimsout=numvdimsout+1
             else
                 do ddim=1,numdimsall
                     if (trim(dimnamesin(vardimsin(dim))) .eq. &
                         dimnamesout(ddim)) then
                              vardimsout(countd)=ddim
                     endif
                 enddo
                 countd=countd+1
             endif
          enddo !dim=1,numdims
      endif

      !define the variable
      status = nf90_redef(outid)
      status = nf90_def_var(outid,trim(varname),xtype, &
               vardimsout(1:numvdimsout),varid)
      if (use_deflate) then
          status = nf90_def_var_deflate(outid,varid,1,1,deflev)
      endif
      IF (status .ne. nf90_noerr) THEN
         print*,'Error Defining Variable: ',trim(varname)
         print*,'   Error: ',trim(nf90_strerror(status))
         print*,'Stopping.'
         STOP
      ENDIF

      !copy attributes
      do att=1,numatts
         status=nf90_inq_attname(inid,var,att,name=attname)
         status=nf90_copy_att(inid,var,trim(attname),outid,varid)
      enddo

      !end define mode
      status = nf90_enddef(outid)

      !grid the variable and write to output
      if ((xtype .eq. 6) .and. (numvdimsout .eq. 1)) then
         allocate(dvalues(dimlensout(vardimsout(1))))
         status=nf90_get_var(inid,var,dvalues)
         status=nf90_put_var(outid,var,dvalues)
         deallocate(dvalues)
  
      elseif ((xtype .eq. 5) .and. (numvdimsout .eq. 1)) then
         if (dimnamesout(vardimsout(1)) .eq. 'lon') then
             status=nf90_put_var(outid,var,lon)
         elseif (dimnamesout(vardimsout(1)) .eq. 'lat') then
             status=nf90_put_var(outid,var,lat)
         else
             allocate(fvalues(dimlensout(vardimsout(1))))
             status=nf90_get_var(inid,var,fvalues)

             status=nf90_put_var(outid,var,fvalues)
             deallocate(fvalues)
         endif

      elseif ((xtype .eq. 5) .and. (numvdimsout .eq. 3)) then
         if (numvdimsin .eq. 3) then
             ival1in=dimlensin(vardimsin(1))
             ival2in=dimlensin(vardimsin(2))
             ival3in=dimlensin(vardimsin(3))
             allocate(fvalues3(ival1in,ival2in,ival3in))
             status = nf90_get_var(inid,var,fvalues3)

             ival1o=dimlensout(vardimsout(1))
             ival2o=dimlensout(vardimsout(2))
             ival3o=dimlensout(vardimsout(3))
             if ((ival1in .ne. ival1o) .or. &
                 (ival2in .ne. ival2o) .or. &
                 (ival3in .ne. ival3o)) then
                  print*,'Problem With 3-D Array.'
                  print*,'Stopping.'
                  STOP
             else
                  status = nf90_put_var(outid,var,fvalues3)
            endif
         elseif (numvdimsin .eq. 2) then
             ival1in=dimlensin(vardimsin(1))
             ival2in=dimlensin(vardimsin(2))
             allocate(fvalues2(ival1in,ival2in))
             status = nf90_get_var(inid,var,fvalues2)

             ival1o=dimlensout(vardimsout(1))
             ival2o=dimlensout(vardimsout(2))
             ival3o=dimlensout(vardimsout(3))
             allocate(fvalues3(ival1o,ival2o,ival3o))
             fvalues3(:,:,:) = 0.
             if (vardimsin(1) .eq. nsibdid) then
                do n=1,ival1in
                   fvalues3(lonref(n),latref(n),:) = &
                         fvalues2(n,:)
                enddo
             else
                print*,'Expecting Points In First Dimension.'
                print*,'Stopping.'
                STOP
             endif
 
             status = nf90_put_var(outid,var,fvalues3)
       
             deallocate(fvalues2)
             deallocate(fvalues3)
         else
             print(fa2i),'Unexpected Dims For Real Array (in/out): ', &
                      numvdimsin, numvdimsout
             print*,'Stopping.'
             STOP
         endif

      elseif ((xtype .eq. 5) .and. (numvdimsout .eq. 4)) then
         ival1in=dimlensin(vardimsin(1))
         ival2in=dimlensin(vardimsin(2))
         ival3in=dimlensin(vardimsin(3))
         allocate(fvalues3(ival1in,ival2in,ival3in))
         status = nf90_get_var(inid,var,fvalues3)

         ival1o=dimlensout(vardimsout(1))
         ival2o=dimlensout(vardimsout(2))
         ival3o=dimlensout(vardimsout(3))
         ival4o=dimlensout(vardimsout(4))
         allocate(fvalues4(ival1o,ival2o,ival3o,ival4o))
         fvalues4(:,:,:,:) = 0.
         
         !assume nsib is in first spot
         if (vardimsin(1) .eq. nsibdid) then
            do n=1,ival1in
               fvalues4(lonref(n),latref(n),:,:) = &
                   fvalues3(n,:,:)
            enddo
         else
              print*,'Expecting Points In First Dimension.'
              print*,'Stopping.'
              STOP
         endif

         status = nf90_put_var(outid,var,fvalues4)
       
         deallocate(fvalues3)
         deallocate(fvalues4)

      elseif ((xtype .eq. 4) .and. (numvdimsout .eq. 1)) then
         ival1in=dimlensin(vardimsin(1))
         allocate(ivalues1(ival1in))
         status = nf90_get_var(inid,var,ivalues1)
         if (status .ne. nf90_noerr) then
            print*,'Error Getting Variable: ',trim(varname)
            STOP
         endif
         
         if (vardimsin(1) .eq. nsibdid) then
            ival1o=dimlensout(vardimsout(1))
            ival2o=dimlensout(vardimsout(2))
            allocate(ivalues2(ival1o,ival2o))
            do n=1,ival1in
               ivalues2(lonref(n),latref(n)) = ivalues1(n)
            enddo
            status = nf90_put_var(outid,var,ivalues2)
            deallocate(ivalues2)
         else
            status = nf90_put_var(outid,var,ivalues1)
         endif
         if (status .ne. nf90_noerr) then
            print*,'Error Writing Variable: ',trim(varname)
         endif

         deallocate(ivalues1)
         
      elseif ((xtype .eq. 4) .and. (numvdimsout .eq. 3)) then
         if (numvdimsin .eq. 3) then
             ival1in=dimlensin(vardimsin(1))
             ival2in=dimlensin(vardimsin(2))
             ival3in=dimlensin(vardimsin(3))
             allocate(ivalues3(ival1in,ival2in,ival3in))
             status = nf90_get_var(inid,var,ivalues3)

             ival1o=dimlensout(vardimsout(1))
             ival2o=dimlensout(vardimsout(2))
             ival3o=dimlensout(vardimsout(3))
             if ((ival1in .ne. ival1o) .or. &
                 (ival2in .ne. ival2o) .or. &
                 (ival3in .ne. ival3o)) then
                 print*,'Problem With 3-D Array.'
                 print*,'Stopping.'
                 STOP
             else
                status = nf90_put_var(outid,var,ivalues3)
             endif
         elseif (numvdimsin .eq. 2) then
             ival1in=dimlensin(vardimsin(1))
             ival2in=dimlensin(vardimsin(2))
             allocate(ivalues2(ival1in,ival2in))
             status = nf90_get_var(inid,var,ivalues2)

             ival1o=dimlensout(vardimsout(1))
             ival2o=dimlensout(vardimsout(2))
             ival3o=dimlensout(vardimsout(3))
             allocate(ivalues3(ival1o,ival2o,ival3o))
             ivalues3(:,:,:) = 0
             if (vardimsin(1) .eq. nsibdid) then
                do n=1,ival1in
                   ivalues3(lonref(n),latref(n),:) = &
                         ivalues2(n,:)
                enddo
             else
                print*,'Expecting Points In First Dimension.'
                print*,'Stopping.'
                STOP
             endif
 
             status = nf90_put_var(outid,var,ivalues3)
       
             deallocate(ivalues2)
             deallocate(ivalues3)
         else
             print(fa2i),'Unexpected Dims For Int Array (in/out): ', &
                      numvdimsin, numvdimsout
             print*,'Stopping.'
             STOP
         endif

      elseif ((xtype .eq. 2) .and. (numvdimsout .eq. 3)) then
          if (dimlensout(vardimsout(1)) .eq. 6) then
              ival2in=dimlensin(vardimsin(2))
              allocate(cvalues6(ival2in))
              status = nf90_get_var(inid,var,cvalues6)

              allocate(cvalues62d(dimlensout(vardimsout(2)), &
                                  dimlensout(vardimsout(3))))
              if (vardimsin(2) .eq. nsibdid) then
                 do n=1,ival2in
                    cvalues62d(lonref(n),latref(n)) = &
                         cvalues6(n)
                 enddo
              else
                 print*,'Expecting Points In First Dimension.'
                 print*,'Stopping.'
                 STOP
              endif
              
              status=nf90_put_var(outid,var,cvalues62d)
              deallocate(cvalues6,cvalues62d)
          else
              print*,'Unexpected Character Length Array.'
              print*,'Number of Characters: ',dimlensout(vardimsout(1))
              print*,'Stopping.'
              STOP
          endif

      elseif ((xtype .eq. 2) .and. (numvdimsout .eq. 2)) then
         if ((dimnamesout(vardimsout(2)) .ne. 'lon') .and. &
             (dimnamesout(vardimsout(2)) .ne. 'lat')) then
    
             if (dimlensout(vardimsout(1)) .eq. 10) then
                 allocate(cvalues10(dimlensout(vardimsout(2))))
                 status=nf90_get_var(inid,var,cvalues10)
                 status=nf90_put_var(outid,var,cvalues10)
                 deallocate(cvalues10)
             elseif (dimlensout(vardimsout(1)) .eq. 7) then
                 allocate(cvalues7(dimlensout(vardimsout(2))))
                 status=nf90_get_var(inid,var,cvalues7)
                 status=nf90_put_var(outid,var,cvalues7)
                 deallocate(cvalues7)
             elseif (dimlensout(vardimsout(1)) .eq. 3) then
                 allocate(cvalues3(dimlensout(vardimsout(2))))
                 status=nf90_get_var(inid,var,cvalues3)
                 status=nf90_put_var(outid,var,cvalues3)
                 deallocate(cvalues3)
             else
                 print*,'Unexpected Character Length Array.'
                 print*,'Number of Characters: ',dimlensout(vardimsout(1))
                 print*,'Stopping.'
                 STOP
             endif
        else
             print(f2a),'Unable To Handle Character Array Dimension: ', &
                     trim(dimnamesout(vardimsout(2)))
             print*,'Stopping.'
             STOP
        endif

      elseif ((xtype .eq. 1) .and. (numvdimsout .eq. 1)) then
         allocate(bvalues1(dimlensout(vardimsout(1))))
         status = nf90_get_var(inid,var,bvalues1)
         status = nf90_put_var(outid,var,bvalues1)
         deallocate(bvalues1)
      else
         print(fa),'Grid_SiB4: Unexpected Output Type and/or Dimension.'
         print(f3ai),'  Var name and type: ',trim(varname),' ',xtype
         print(fai),'  Number of dimensions: ',numvdimsout
         print(fa), '  Dimension Names:' 
         do n=1,numvdimsout 
             print(f2a),'      ',trim(dimnamesout(vardimsout(n)))
         enddo
     endif
   enddo !var=1,numvars


   !Close the files
   status = nf90_close(inid)
   status = nf90_close(outid)
ENDDO !nfiles


end subroutine grid_output
