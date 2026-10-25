!========================================
subroutine disjoin_output( &
     nfiles, infile, outfiles, &
     nselvar, selvarnamesin)

use sibinfo_module
use netcdf
implicit none


!input variables
integer, intent(in) :: nfiles, nselvar
character(len=20), dimension(nselvar) :: selvarnamesin
character(len=256), intent(in) :: infile
character(len=256), dimension(nfiles), intent(in) :: outfiles

!selected variable information
character(len=20), dimension(:), allocatable :: &
   selvarnames

!netcdf variables
integer :: status, inid, outid
integer :: ntid
integer :: numatts
integer, dimension(:), allocatable :: &
   dimids, dimlens, vardims

character(len=20) :: name
integer :: xtype
integer :: varid, vartid
integer :: numddims, numdims, numvars, nvarso

!data variables
logical :: allvars

integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7),dimension(:), allocatable :: cvalues7
character(len=10),dimension(:), allocatable :: cvalues10
real*8, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
real, dimension(:,:,:,:), allocatable :: fvalues4
integer, dimension(:,:), allocatable :: ivalues2
integer, dimension(:,:,:), allocatable :: ivalues3

!misc variables
integer :: a, d, f, v
integer :: dlen1, dlen2, dlen3, dlen4
integer :: ntimetot, tperday, tnow

!---------------------
! Open input file
status = nf90_open(trim(infile), nf90_nowrite, inid)

! get dimensions and attributes
status = nf90_inquire(inid, nDimensions=numddims, &
         nVariables=numvars, nAttributes=numatts)
allocate(dimlens(numddims))
allocate(dimids(numddims))

status = nf90_inq_dimid(inid, 'time', ntid)
status = nf90_inquire_dimension(inid, ntid, len=ntimetot)
tperday = ntimetot / nfiles

! get variable names to save
allocate( vardims(numddims) )
if (trim(selvarnamesin(1)) .eq. '') then
    allvars=.true.
    nvarso=numvars
else
    allvars=.false.

    !check variable names
    nvarso=0
    do v = 1, nvarsreq
       status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
       IF (status .eq. nf90_noerr) nvarso=nvarso + 1
    enddo

    do v = 1, nselvar
       status = nf90_inq_varid(inid,trim(selvarnamesin(v)),varid)
       if (status .eq. nf90_noerr) nvarso=nvarso+1
    enddo

    !save variable names
    allocate(selvarnames(nvarso))
    selvarnames(:)=''
    nvarso=0
    do v = 1, nvarsreq
       status = nf90_inq_varid(inid,trim(varsreq(v)),varid)
       if (status .eq. nf90_noerr) then
          nvarso=nvarso+1
          selvarnames(nvarso) = varsreq(v)
       endif
    enddo

    do v = 1, nselvar
       status = nf90_inq_varid(inid,trim(selvarnamesin(v)),varid)
       if (status .eq. nf90_noerr) then
           nvarso=nvarso+1
           selvarnames(nvarso) = selvarnamesin(v)
       endif
    enddo
endif
print('(a,i4)'),'   Number of variables saving: ',nvarso

! Loop over number of days in file
tnow = 1
DO f = 1, nfiles
   ! create output file
   IF (use_deflate) THEN
      status = nf90_create(trim(outfiles(f)),nf90_netcdf4,outid)
   ELSE
      status = nf90_create( trim(outfiles(f)), nf90_64bit_offset, outid )
   ENDIF
   IF (status .NE. nf90_noerr) THEN
      print('(a)'),'Error Creating Output File: '
      print('(2a)'),'     ',trim(outfiles(f))
      print('(a)'),'Stopping Processing.'
      RETURN
   ENDIF

   ! copy global attributes over to output file
   do a = 1, numatts
       status = nf90_inq_attname(inid, nf90_global, a, name )
       status = nf90_copy_att(outid, nf90_global, trim(name),  &
                 outid, nf90_global)
   enddo

   ! copy dimensions over to output file
   do d = 1, numddims
      status = nf90_inquire_dimension(inid, d, name=name, len=dimlens(d) )
      if (d .eq. ntid) dimlens(d)=tperday
      status = nf90_def_dim(outid, trim(name), dimlens(d), dimids(d))
   enddo

   ! define variables in disjoined output file 
   do v = 1, nvarso
      if (allvars) then
          vartid = v
      else
          status = nf90_inq_varid(inid,trim(selvarnames(v)),vartid)
      endif
      status = nf90_inquire_variable(inid, vartid, name=name, &
           xtype=xtype, ndims=numdims, dimids=vardims, natts=numatts )

      status = nf90_def_var(outid, trim(name), xtype,  &
               vardims(1:numdims), varid )
      IF (use_deflate) THEN
         status = nf90_def_var_deflate(outid,varid,1,1,deflev)
      ENDIF
      do a = 1, numatts
         status = nf90_inq_attname(inid, vartid, a, name=name)
         status = nf90_copy_att(inid, vartid, trim(name), outid, varid )
      enddo
   enddo

   ! stop defining variables
   status = nf90_enddef( outid )

   ! copy variables to output file
   do v=1, nvarso
      if (allvars) then
          vartid = v
      else
          status = nf90_inq_varid(inid,trim(selvarnames(v)),vartid)
      endif

      status = nf90_inquire_variable(inid, vartid, &
               name=name, xtype=xtype,  &
               ndims=numdims, dimids=vardims )
      dlen1 = dimlens(vardims(1))
      IF (numdims .ge. 2) dlen2 = dimlens(vardims(2))
      IF (numdims .ge. 3) dlen3 = dimlens(vardims(3))
      IF (numdims .ge. 4) dlen4 = dimlens(vardims(4))

      if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
          allocate(dvalues(dlen1))
          IF (trim(name) .eq. 'time') THEN
             status = nf90_get_var(inid, vartid, dvalues, &
                  count=(/dlen1/),start=(/tnow/))
          ELSE
             status = nf90_get_var(inid, vartid, dvalues)
          ENDIF
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Getting Variable: ',trim(name)
             print*,'Stopping.'
             STOP
          ENDIF
          status = nf90_put_var( outid, v, dvalues )
          IF (status .ne. nf90_noerr) THEN
             print*,'Error Writing Variable: ',trim(name)
             print*,'Stopping.'
             STOP
          ENDIF
          deallocate(dvalues)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
          allocate(fvalues(dlen1))
          if (vardims(1) .eq. ntid) then
             status = nf90_get_var(inid,vartid,fvalues, &
                       count=(/dlen1/),start=(/tnow/))
          else
             status = nf90_get_var(inid, vartid, fvalues)
          endif
          status = nf90_put_var( outid, v, fvalues)
          deallocate(fvalues)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
          allocate(fvalues2(dlen1,dlen2))
          if (vardims(2) .eq. ntid) then
              status = nf90_get_var(inid,vartid,fvalues2, &
                        count=(/dlen1,dlen2/),start=(/1,tnow/))
          else
              status = nf90_get_var(inid,vartid,fvalues2)
          endif
          status = nf90_put_var( outid, v, fvalues2)
          deallocate(fvalues2)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
          allocate(fvalues3(dlen1,dlen2,dlen3))
          if (vardims(3) .eq. ntid) then
              status = nf90_get_var(inid,vartid,fvalues3, &
                       count=(/dlen1,dlen2,dlen3/), &
                       start=(/1,1,tnow/))
          else
              status = nf90_get_var(inid,vartid,fvalues3)
          endif
          status = nf90_put_var(outid, v, fvalues3)
          deallocate(fvalues3)

      elseif ((xtype .eq. 5) .and. (numdims .eq. 4)) then
          allocate(fvalues4(dlen1,dlen2,dlen3,dlen4))
          if (vardims(4) .eq. ntid) then
              status = nf90_get_var(inid,vartid,fvalues4, &
                      count=(/dlen1,dlen2,dlen3,dlen4/), &
                      start=(/1,1,1,tnow/))
          else
              status = nf90_get_var(inid,vartid,fvalues4)
          endif
          status = nf90_put_var(outid,v,fvalues4)
          deallocate(fvalues4)

      elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
          allocate(ivalues2(dlen1,dlen2))
          if (vardims(2) .eq. ntid) then
              status = nf90_get_var(inid, vartid, ivalues2, &
                       count=(/dlen1,dlen2/), start=(/1,tnow/))
          else
              status = nf90_get_var(inid, vartid, ivalues2)
          endif
          status = nf90_put_var( outid, v, ivalues2)
          deallocate(ivalues2)

      elseif ((xtype .eq. 4) .and. (numdims .eq. 3)) then
          allocate(ivalues3(dlen1,dlen2,dlen3))
          if (vardims(3) .eq. ntid) then
              status = nf90_get_var(inid,vartid,ivalues3, &
                      count=(/dlen1,dlen2,dlen3/), &
                      start=(/1,1,tnow/))
          else
              status = nf90_get_var(inid,vartid,ivalues3)
          endif
          status = nf90_put_var(outid,v,ivalues3)
          deallocate(ivalues3)

      elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
          if (dlen1 .eq. 3) then
              allocate(cvalues3(dlen2))
              status = nf90_get_var(inid, vartid, cvalues3)
              status = nf90_put_var( outid, v, cvalues3)
              deallocate(cvalues3)
          elseif (dlen1 .eq. 6) then
              allocate(cvalues6(dlen2))
              status = nf90_get_var(inid, vartid, cvalues6)
              status = nf90_put_var( outid, v, cvalues6)
              deallocate(cvalues6)
          elseif (dlen1 .eq. 7) then
              allocate(cvalues7(dlen2))
              status = nf90_get_var(inid, vartid, cvalues7)
              status = nf90_put_var( outid, v, cvalues7)
              deallocate(cvalues7)
          elseif (dlen1 .eq. 10) then
              allocate(cvalues10(dlen2))
              if (vardims(2) .eq. ntid) then
                  status = nf90_get_var(inid,vartid,cvalues10, &
                            count=(/dlen1,dlen2/), start=(/1,tnow/))
              else
                  status = nf90_get_var(inid,vartid,cvalues10)
              endif
              status = nf90_put_var( outid, v, cvalues10)
              deallocate(cvalues10)
          else
              print*,'Unrecognized Character Dimensons.'
              stop
          endif

      elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
          allocate(bvalues1(dlen1))
          status = nf90_get_var(inid, vartid, bvalues1)
          status = nf90_put_var( outid, v, bvalues1)
          deallocate(bvalues1)       
      else
          print*,'Unexpected Output Type and/or Dimension.'
          print*,'   Var name and type: ',name,xtype
          print*,'   Number of dimensions: ',numdims
          print*,'   Dimension IDs: ',vardims
      endif

   enddo !v=1,numvars

   ! close output file
   status = nf90_close(outid)
   tnow = tnow + tperday
ENDDO  !f=1,nfiles

!close input file
status = nf90_close(inid)


end subroutine disjoin_output


!===========================================
subroutine disjoin_parse( &
   startmon, startyr, stopmon, stopyr, &
   dref, indir, outdir, &
   nvars, varnames)

use sibinfo_module
implicit none

!input variables
integer, intent(in) :: startmon, startyr, stopmon, stopyr
integer, intent(in) :: dref
character(len=180), intent(in) :: indir, outdir
integer, intent(in) :: nvars
character(len=20), dimension(nvars), intent(in) :: varnames

!file variables
integer :: nfiles
character(len=30) :: myprefixin, myprefixout
character(len=30) :: mysuffixin, mysuffixout
character(len=180) :: myindir, myoutdir
character(len=256) :: infile
character(len=256), dimension(:), allocatable :: &
   outfiles

!misc variables
integer :: day, mon, yr
integer :: mref1, mref2
integer :: spos1, spos2, spos3

!-----------------------------------------------
!Print warning messages/notifications
print*,''
IF (dref .GE. 1) THEN
   print('(3a)'),'Processing ',trim(siblabel(dref)), &
      ' Files'
ELSE
   print('(3a)'),'Processing Other Files'
ENDIF

!Get input and output directory, prefix, suffix
IF (dref .LT. 0) THEN !Other Files
   !Get input file prefix and dir
   spos1=index(indir,'/',back=.true.)
   spos2=index(indir,'xxx',back=.true.)
   myindir = indir
   IF (spos1 .LT. spos2) THEN
       myprefixin = indir(spos1+1:spos2-1)
       myindir = indir(1:spos1)
   ENDIF

   !Get output file prefix
   spos1=index(outdir,'/',back=.true.)
   spos2=len_trim(outdir)
   myoutdir = outdir
   IF (spos1 .LT. spos2) THEN
      spos2=index(outdir,'xxx',back=.true.)
      IF (spos2 .lt. 1) spos2=len_trim(outdir)
      myprefixout = outdir(spos1+1:spos2-1)
      myoutdir = outdir(1:spos1)
   ENDIF

   !Get input file suffix
   spos1=index(indir,'xxx',back=.true.)
   spos2=len_trim(indir)
   mysuffixin = ''
   IF (spos1 .EQ. spos2) THEN
       mysuffixin = '.nc'
   ELSE
      mysuffixin = indir(spos1+3:spos2)
   ENDIF

   !Get output file suffix
   spos1=index(outdir,'xxx',back=.true.)
   spos2=len_trim(outdir)
   mysuffixout = ''
   IF ((spos1 .LT. 1) .or. (spos1 .EQ. spos2)) THEN
       mysuffixout = '.nc'
   ELSE
      mysuffixout = outdir(spos1+3:spos2)
   ENDIF
ELSE !SiB4 Diagnostic Files

   !Get input file prefix, suffix, and dir
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
   ELSE
      myprefixin = sibprefix(dref)
   ENDIF

   !Get output file prefix
   spos1=index(outdir,'/',back=.true.)
   spos2=len_trim(outdir)
   myoutdir = outdir(1:spos1)
   IF (spos1 .LT. spos2) THEN
      spos3=index(outdir,'xxx')
      IF (spos3 .ge. 1) THEN
          myprefixout = outdir(spos1+1:spos3-1)
      ELSE
          myprefixout = outdir(spos1+1:spos2)
      ENDIF
   ELSE
      myprefixout = sibprefix(dref) 
   ENDIF

   !Get input file suffix
   spos1=index(indir,'xxx',back=.true.)
   IF (spos1 .ge. 1) THEN
      spos2=len_trim(indir)
      mysuffixin = indir(spos1+3:spos2)
   ELSE
      mysuffixin = sibsuffix(dref)
   ENDIF
   
   !Get output file suffix
   spos1=index(outdir,'xxx',back=.true.)
   IF (spos1 .ge. 1) THEN
       spos2=len_trim(outdir)
       mysuffixout=outdir(spos1+3:spos2)
   ELSE
       mysuffixout = sibsuffix(dref)
   ENDIF

ENDIF

!Get file names
nfiles = 0
do yr = startyr, stopyr
   mref1 = 1
   mref2 = 12

   IF (yr .eq. startyr) mref1=startmon
   IF (yr .eq. stopyr) mref2=stopmon

   do mon = mref1, mref2
      call create_sib_namemon(infile, &
           myindir, trim(myprefixin), trim(mysuffixin), &
           yr, mon, .true.)

      !check infile, trying different suffix
      !  if necessary
      IF (trim(infile) .EQ. '') THEN
         IF (trim(mysuffixin) .EQ. &
             trim(sibsuffix(dref))) THEN
            mysuffixin = gridsuffix(dref)

             call create_sib_namemon(infile, &
                  myindir, trim(myprefixin), trim(mysuffixin), &
                  yr, mon, .true.)

             IF (trim(infile) .EQ. '') THEN
                print*,'Missing Input File.'
                print*,'Tried:'
                call create_sib_namemon(infile, &
                       myindir, trim(myprefixin), &
                       trim(mysuffixin), &
                       yr, mon, .false.)
                print*,'  ',trim(infile)
                call create_sib_namemon(infile, &
                     myindir, trim(myprefixin), &
                     trim(mysuffixin), &
                     yr, mon, .false.)
                print*,'  ',trim(infile)
             ELSE
                IF (trim(mysuffixout) .EQ. &
                    trim(sibsuffix(dref))) THEN
                   mysuffixout = gridsuffix(dref)
                ENDIF
             ENDIF
         ELSE
            call create_sib_namemon(infile, &
                  myindir, trim(myprefixin), &
                  trim(mysuffixin), yr, mon, .false.)
            print*,'   Missing File:'
            print*,'     ',trim(infile)
            infile=''
         ENDIF
      ENDIF

      !proceed to process if file exists
      IF (trim(infile) .NE. '') THEN
         spos1=index(infile,'/',back=.true.)
         spos2=len_trim(infile)
         print*,'  Processing File: ',infile(spos1+1:spos2)
         
         nfiles=dayspermon(mon)
         IF ((mod(yr,4) .eq. 0) .and. (mon .eq. 2)) nfiles=nfiles+1
         allocate(outfiles(nfiles))

         do day=1, nfiles
            call create_sib_nameday(outfiles(day), &
                 myoutdir, trim(myprefixout), &
                 trim(mysuffixout), yr, mon, day, &
                 .false.)
         enddo

         call disjoin_output(nfiles, &
               infile, outfiles, &
               nvars, varnames)

         deallocate(outfiles)
    ENDIF

   enddo !mon
enddo !yr


end subroutine disjoin_parse



