!=========================================================
subroutine meanclim_output( nyrs, nfiles, &
           startyr, stopyr, infiles, outfiles, &
           nselvars, selvarnamesin)
!=========================================================

use sibinfo_module
use netcdf
implicit none

!input variables
integer, intent(in) :: nyrs, nfiles, startyr, stopyr, nselvars
character(len=20), dimension(nselvars), intent(in) :: selvarnamesin
character(len=256), dimension(nyrs,nfiles), intent(in) :: &
    infiles
character(len=256), dimension(nfiles), intent(in) :: &
    outfiles

!selected variable information
character(len=20), dimension(:), allocatable :: &
    selvarnames

!ncdf information
integer :: status, inid, outid, ncid
integer :: varid, vartid, ntid
integer :: numatts, numdims, numvars, nvarso
integer :: xtype
integer, dimension(:), allocatable :: &
    dimids, dimlens, vardims
character(len=20) :: name
character(len=60) :: attyears

!data variables
integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7), dimension(:), allocatable :: cvalues7
real*8, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2, fvalues2all
real, dimension(:,:,:), allocatable :: fvalues3, fvalues3all
real, dimension(:,:,:,:), allocatable :: fvalues4, fvalues4all
integer, dimension(:,:), allocatable :: ivalues2, ivalues2all
integer, dimension(:,:,:), allocatable :: ivalues3all

!misc variables
integer :: f, yr
integer :: att, dim, v
logical :: allvars

!--------------------------------------------------
DO f=1, nfiles

   ! Open input file
   status = nf90_open(trim(infiles(1,f)), nf90_nowrite, inid)
   IF (status .NE. nf90_noerr) THEN
      print*,'    Error Opening Input File: '
      print*,'     ',trim(infiles(1,f))
      print*,'    Stopping.'
      STOP
   ENDIF

   ! Open output file
   if (use_deflate) then
       status = nf90_create(trim(outfiles(f)), nf90_hdf5, outid)
   else
       status = nf90_create(trim(outfiles(f)),nf90_64bit_offset,outid)
   endif

   IF (status .NE. nf90_noerr) THEN
       print*,'    Error Opening Output File: '
       print*,'     ',trim(outfiles(f))
       print*,'    Stopping.'
       STOP
   ENDIF

   ! Get dimensions and attributes
   status = nf90_inquire(inid, nDimensions=numdims, &
            nVariables=numvars, nAttributes=numatts)
   status = nf90_inq_dimid(inid,'time',ntid)

   ! Copy global attributes to output file
   DO att = 1, numatts
      status = nf90_inq_attname( inid, nf90_global, att, name)
      status = nf90_copy_att( inid, nf90_global, trim(name), &
               outid, nf90_global)
   ENDDO
   write(attyears,('(a,i4,a,i4)')) 'Mean Output From ', &
         startyr, ' To ', stopyr
   status = nf90_put_att(outid,nf90_global,'Info',trim(attyears))

   ! Copy dimensions to output file
   allocate( dimids(numdims) )
   allocate( dimlens(numdims))
   DO dim = 1, numdims
      status = nf90_inquire_dimension(inid, &
           dim, name=name, len=dimlens(dim))
      IF ((dim .eq. ntid) .and. (dimlens(dim) .eq. 29)) THEN
         dimlens(dim) = 28
      ENDIF
      status = nf90_def_dim(  outid, trim(name), dimlens(dim), &
                  dimids(dim))
   ENDDO

   ! Define variables in output file
   IF (trim(selvarnamesin(1)) .eq. '') THEN
       allvars=.true.
       nvarso=numvars
   ELSE
       allvars=.false.

       !check variable names
       nvarso = 0
       do v = 1, nvarsreq
          status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
          IF (status .eq. nf90_noerr) nvarso = nvarso + 1
       enddo

       do v = 1, nselvars
          status = nf90_inq_varid(inid,trim(selvarnamesin(v)),varid)
          IF (status .EQ. nf90_noerr) nvarso = nvarso + 1
       enddo

       !save variable names
       allocate(selvarnames(nvarso))
       selvarnames(:) = ''
       nvarso = 0
       do v = 1, nvarsreq
          status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
          IF (status .eq. nf90_noerr) THEN
             nvarso = nvarso + 1
             selvarnames(nvarso) = varsreq(v)
          ENDIF
       enddo

       do v = 1, nselvars
          status = nf90_inq_varid(inid,trim(selvarnamesin(v)),varid)
          IF (status .eq. nf90_noerr) THEN
              nvarso = nvarso + 1
              selvarnames(nvarso) = selvarnamesin(v)
          ENDIF
       enddo
   ENDIF

   IF (f .eq. 1) &
      print('(a,i4)'),'    Number of variables saving: ',nvarso

   allocate(vardims(numdims))
   do v = 1, nvarso
      IF (allvars) THEN
          vartid = v
      ELSE
          status = nf90_inq_varid(inid,trim(selvarnames(v)),vartid)
      ENDIF
      status = nf90_inquire_variable(inid,vartid,name=name, &
                xtype=xtype, ndims=numdims, dimids=vardims, natts=numatts)
 
      status = nf90_def_var(outid,trim(name),xtype, &
                 vardims(1:numdims), varid)
      if (use_deflate) then
          status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
      endif

       do att=1,numatts
          status = nf90_inq_attname(inid,vartid,att,name=name)
          status = nf90_copy_att(inid,vartid,trim(name),outid,varid)
       enddo
   enddo

   ! stop defining variables
   status = nf90_enddef(outid)

   ! copy multi-year mean of variables to output file
   do v = 1, nvarso
      if (allvars) then
         vartid = v
      else
         status = nf90_inq_varid(inid,trim(selvarnames(v)),vartid)
      endif

      status = nf90_inquire_variable(inid, vartid, &
         name=name, xtype=xtype, &
         ndims=numdims, dimids=vardims)

      if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
         allocate(dvalues(dimlens(vardims(1))))
         status = nf90_get_var(inid,vartid,dvalues)
         status = nf90_put_var(outid, v, dvalues)
         deallocate(dvalues)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
         allocate(fvalues(dimlens(vardims(1))))
         status = nf90_get_var(inid,vartid,fvalues)
         status = nf90_put_var(outid, v, fvalues)
         deallocate(fvalues)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
         allocate(fvalues2(dimlens(vardims(1)),dimlens(vardims(2))))
         allocate(fvalues2all(dimlens(vardims(1)),dimlens(vardims(2))))
         status = nf90_get_var(inid,vartid,fvalues2all)

         if (vardims(2) .eq. ntid) then
            do yr=2, nyrs
               status = nf90_open(trim(infiles(yr,f)), &
                           nf90_nowrite, ncid)

               status = nf90_get_var(ncid,vartid,fvalues2)
               fvalues2all = fvalues2all + fvalues2
               
               status = nf90_close(ncid)
            enddo

            fvalues2all = fvalues2all / nyrs
         endif
         status = nf90_put_var(outid,v,fvalues2all)
         deallocate(fvalues2,fvalues2all)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
          allocate(fvalues3(dimlens(vardims(1)), &
                   dimlens(vardims(2)),dimlens(vardims(3))))
          allocate(fvalues3all(dimlens(vardims(1)), &
                   dimlens(vardims(2)),dimlens(vardims(3))))
          status = nf90_get_var(inid,vartid,fvalues3all)

          if (vardims(3) .eq. ntid) then
             do yr=2, nyrs
                status = nf90_open(trim(infiles(yr,f)), &
                     nf90_nowrite, ncid)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Opening File: '
                   print*,'  ',trim(infiles(yr,f))
                   print*,'Stopping.'
                   STOP
                ENDIF
                
                status = nf90_get_var(ncid,vartid,fvalues3)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Getting Variable: ', &
                        trim(name)
                   print*,'Stopping.'
                   STOP
                ENDIF
                
                fvalues3all = fvalues3all + fvalues3
                status = nf90_close(ncid)
             enddo
             fvalues3all = fvalues3all / nyrs
          endif

          status = nf90_put_var(outid,v,fvalues3all)
          deallocate(fvalues3,fvalues3all)

       elseif ((xtype .eq. 5) .and. (numdims .eq. 4)) then
          allocate(fvalues4(dimlens(vardims(1)), &
               dimlens(vardims(2)), dimlens(vardims(3)), &
               dimlens(vardims(4))))
          allocate(fvalues4all(dimlens(vardims(1)), &
               dimlens(vardims(2)), dimlens(vardims(3)), &
               dimlens(vardims(4))))
          status = nf90_get_var(inid,vartid,fvalues4all)
          
          if (vardims(4) .eq. ntid) then
             do yr=2, nyrs
                status = nf90_open(trim(infiles(yr,f)), &
                     nf90_nowrite, ncid)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Opening File: '
                   print*,'  ',trim(infiles(yr,f))
                   print*,'Stopping.'
                   STOP
                ENDIF
                
                status = nf90_get_var(ncid,vartid,fvalues4)
                IF (status .ne. nf90_noerr) THEN
                   print*,'Error Getting Variable: ', &
                        trim(name)
                   print*,'Stopping.'
                   STOP
                ENDIF
                  
                fvalues4all = fvalues4all + fvalues4
                status = nf90_close(ncid)
             enddo
             fvalues4all = fvalues4all / nyrs
          endif

          status = nf90_put_var(outid,v,fvalues4all)
          deallocate(fvalues4,fvalues4all)
          
      elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
          allocate(ivalues2(dimlens(vardims(1)),dimlens(vardims(2))))
          allocate(ivalues2all(dimlens(vardims(1)),dimlens(vardims(2))))
          status = nf90_get_var(inid,vartid,ivalues2all)

          if (vardims(2) .eq. ntid) then
             do yr=2, nyrs
                status = nf90_open(trim(infiles(yr,f)), &
                           nf90_nowrite, ncid)
                status = nf90_get_var(ncid,vartid,ivalues2)
                ivalues2all = ivalues2all + ivalues2
                status = nf90_close(ncid)
             enddo
             ivalues2all = ivalues2all / nyrs
          endif

          status = nf90_put_var(outid,v,ivalues2all)
          deallocate(ivalues2,ivalues2all)

       elseif ((xtype .eq. 4) .and. (numdims .eq. 3)) then
          allocate(ivalues3all(dimlens(vardims(1)),dimlens(vardims(2)), &
               dimlens(vardims(3))))
          status = nf90_get_var(inid,vartid,ivalues3all)
          if (vardims(3) .eq. ntid) then
             print*,'3-D Int Array Needs Help!'
             print*,'Stopping.'
             STOP
          endif
          status = nf90_put_var(outid,v,ivalues3all)
          deallocate(ivalues3all)
          
      elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
          if (dimlens(vardims(1)) .eq. 3) then
             allocate(cvalues3(dimlens(vardims(2))))
             status = nf90_get_var( inid, vartid, cvalues3)
             status = nf90_put_var( outid, v, cvalues3)
             deallocate(cvalues3)
          elseif (dimlens(vardims(1)) .eq. 6) then
             allocate(cvalues6(dimlens(vardims(2))))
             status = nf90_get_var( inid, vartid, cvalues6)
             status = nf90_put_var( outid, v, cvalues6)
             deallocate(cvalues6)
          elseif (dimlens(vardims(1)) .eq. 7) then
              allocate(cvalues7(dimlens(vardims(2))))
              status = nf90_get_var( inid, vartid, cvalues7)
              status = nf90_put_var( outid, v, cvalues7)
              deallocate(cvalues7)
          else
              print*,'Unrecognized Character Dimensons.'
              stop
          endif

      elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
          allocate(bvalues1(dimlens(vardims(1))))
          status = nf90_get_var( inid, vartid, bvalues1)
          status = nf90_put_var( outid, v, bvalues1)
          deallocate(bvalues1)       

      else
         print*,'Unexpected Output Type and/or Dimension.'
         print*,'   Var name and type: ',name,xtype
         print*,'   Number of dimensions: ',numdims
         print*,'   Dimension IDs: ',vardims
      endif
   enddo

   ! Close files
   status = nf90_close(inid)
   status = nf90_close(outid)

   deallocate(dimids,dimlens)
   deallocate(vardims)
   if (allocated(selvarnames)) deallocate(selvarnames)

ENDDO !f

end subroutine meanclim_output


!========================================
subroutine meanclim_parse( dref, &
     use_daily, use_monthly, &
     use_annual, use_multi, &
     startmon, startyr, stopmon, stopyr, &
     indir, outdir, &
     nvarsin, varnamesin)
!========================================

use sibinfo_module, only: &
    dayspermon, &
    siblabel, sibprefix, &
    sibsuffix
  
implicit none

!input variables
integer, intent(in) :: dref
logical, intent(in) :: use_daily, use_monthly, &
     use_annual, use_multi
integer, intent(in) :: startyr, startmon, stopyr, stopmon
character(len=180), intent(in) :: indir, outdir

integer, intent(in) :: nvarsin
character(len=20), dimension(*), intent(in) :: &
     varnamesin

!file variables
character(len=30) :: myprefixin, mysuffixin
character(len=30) :: myprefixout, mysuffixout
character(len=180) :: myindir, myoutdir

integer :: nfiles
character(len=256) :: tempfile
character(len=256), dimension(:,:), allocatable :: filesin
character(len=256), dimension(:), allocatable :: filesout

!misc variables
integer :: spos1, spos2, spos3

integer :: day, mon, yr
integer :: dayref, monref, yrref
integer :: numdays, nummons, numyrs
character(len=4) :: yrstring

!---------------------------------
!Print message
print*,''
IF (dref .ge. 1) THEN 
   print('(3a)'), 'Processing ',siblabel(dref),' Files'
   myprefixin = sibprefix(dref)
   myprefixout = sibprefix(dref)

   mysuffixin = sibsuffix(dref) 
   mysuffixout = sibsuffix(dref)
ELSE
   print('(3a)'), 'Processing Other Files'

   myprefixin = ''
   myprefixout = ''
   mysuffixin = ''
   mysuffixout = ''
ENDIF

!Get input file prefix
spos1=index(indir,'/',back=.true.)
spos2=len_trim(indir)
myindir = indir(1:spos1)
IF (spos1 .LT. spos2) THEN
   spos3=index(indir,'xxx')
   IF (spos3 .ge. 1) THEN
      myprefixin = indir(spos1+1:spos3-1)
   ELSE
      myprefixin = indir(spos1+1:spos2)
   ENDIF
ENDIF


!Get output file prefix
spos1=index(outdir,'/',back=.true.)
spos2=len_trim(outdir)
myoutdir=outdir(1:spos1)
IF (spos1 .LT. spos2) THEN
   spos3=index(outdir,'xxx')
   IF (spos3 .ge. 1) THEN
      myprefixout = outdir(spos1+1:spos3-1)
   ELSE
      myprefixout = outdir(spos1+1:spos2)
   ENDIF
ENDIF

!Get input file suffix
spos1=index(indir,'xxx',back=.true.)
IF (spos1 .ge. 1) THEN
   spos2=len_trim(indir)
   mysuffixin = indir(spos1+3:spos2)
ENDIF

!Get output file suffix
spos1=index(outdir,'xxx',back=.true.)
IF (spos1 .ge. 1) THEN
   spos2=len_trim(outdir)
   mysuffixout=outdir(spos1+3:spos2)
ENDIF

!Determine File Names
nummons = stopmon-startmon+1
numyrs = stopyr-startyr+1

IF (numyrs .lt. 2) THEN
   print*,'   Must Specify At Least 2 Years.'
   print*,'   Stopping.'
   STOP
ELSE
   write(yrstring,'(2i2.2)') &
       startyr-int(floor(real(startyr/100.))*100.), &
       stopyr-int(floor(real(stopyr/100.))*100.)
ENDIF


if (use_daily) then

  nfiles = 0
  do mon=startmon,stopmon
     nfiles = nfiles + dayspermon(mon)
  enddo

  allocate(filesin(numyrs,nfiles))
  filesin(:,:) = ''

  allocate(filesout(nfiles))
  filesout(:) = ''
   
   do yr=startyr,stopyr
      yrref = yr-startyr+1

      dayref = 1
      do mon=startmon,stopmon
         numdays = dayspermon(mon)
         DO day = 1, numdays
            call create_sib_nameday( tempfile, &
                 myindir, myprefixin, mysuffixin, &
                 yr, mon, day, .true.)
            IF (trim(tempfile) .EQ. '') THEN
               print*,'Missing File: '
               call create_sib_nameday( tempfile, &
                    myindir, myprefixin, mysuffixin, &
                    yr, mon, day, .false.)
               print*,'  ',trim(tempfile)
               print*,'Stopping.'
               STOP
            ENDIF

            filesin(yrref,dayref) = tempfile

            IF (yrref .eq. 1) THEN
               call create_sib_namedayc( &
                    filesout(dayref), &
                    myoutdir, myprefixout, mysuffixout, &
                    yrstring, mon, day, .false.)
            ENDIF

            dayref = dayref+1
         enddo !day
      enddo !mon
   enddo !yr

elseif (use_monthly) then

  nfiles = nummons
  allocate(filesin(numyrs,nfiles))
  filesin(:,:) = ''

  allocate(filesout(nfiles))
  filesout(:) = ''
   
  do yr=startyr,stopyr
     yrref = yr-startyr+1

     monref = 1
     do mon=startmon,stopmon
        call create_sib_namemon( tempfile, &
             myindir, myprefixin, mysuffixin, &
             yr, mon, .true.)

        IF (trim(tempfile) .eq. '') THEN
            print*,'Missing File: '
            call create_sib_namemon( &
                 tempfile, &
                 myindir, myprefixin, mysuffixin, &
                 yr, mon, .false.)
            print*,'  ',trim(tempfile)
            print*,'Stopping.'
            STOP
        ENDIF

        filesin(yrref,monref) = tempfile
      
        IF (yrref .eq. 1) THEN
            call create_sib_namemonc( &
                 filesout(monref), &
                 myoutdir, myprefixout, mysuffixout, &
                 yrstring, mon, .false.)
        ENDIF

        monref=monref+1
     enddo !mon
  enddo !yr

elseif (use_annual) then
   nfiles = 1
   allocate(filesin(numyrs,nfiles))
   filesin(:,:) = ''

   allocate(filesout(nfiles))
   filesout(:) = ''

   do yr=startyr,stopyr
      yrref = yr-startyr+1

      call create_sib_nameannual( &
           tempfile, myindir, myprefixin, mysuffixin, &
           yr, .true.)

      IF (trim(tempfile) .eq. '') THEN
          print*,'Missing File: '
          call create_sib_nameannual( &
               tempfile, myindir, myprefixin, mysuffixin, &
               yr, .false.)
           print*,' ',trim(tempfile)
           print*,'Stopping.'
           STOP
       ENDIF

       filesin(yrref,1) = tempfile

       IF (yrref .eq. 1) THEN
          call create_sib_nameannualc( &
               filesout(1), myoutdir, &
               myprefixout, mysuffixout, &
               yrstring, .false.)
       ENDIF
   enddo


elseif (use_multi) then
   print*,'Use_Multi Switch Not Working.'
   print*,'Please Modify meanclim_parse.'
   print*,'Stopping.'
   STOP
else
   print*,'Unknown I/O Option.'
   print*,'Stopping.'
   STOP
endif

call meanclim_output(numyrs, nfiles, &
     startyr, stopyr, filesin, filesout, &
     nvarsin, varnamesin)


end subroutine meanclim_parse
