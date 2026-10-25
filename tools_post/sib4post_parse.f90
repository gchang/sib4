!========================================
subroutine sib4post_parse( sibprogram, &
    use_daily, use_monthly, &
    use_annual, use_multi, &
    startmon, startyr, stopmon, stopyr, &
    dref, indir, outdir, &
    prefixin, prefixout, &
    nvarsin, varnamesin, varnnamesin, &
    changel, varlnamesin, &
    changet, vartnamesin, &
    changeu, varunitsin,  &
    mytot, usearea, &
    isglobal, pbiome, ppft, &
    wratio, wbresp, &
    cropfilein, regionfilein, &
    regridfilein, bsibfilein, csibfilein)
!========================================

use sibinfo_module
implicit none

!input variables
character(len=*), intent(in) :: sibprogram
logical, intent(in) :: use_daily, use_monthly
logical, intent(in) :: use_annual, use_multi
integer, intent(in) :: startyr, startmon, stopyr, stopmon
integer, intent(in) :: dref
character(len=180), intent(in) :: indir, outdir

character(len=*), intent(in), optional :: &
  prefixin, prefixout

integer, intent(in), optional :: nvarsin
character(len=20), dimension(*), &
     intent(in), optional :: &
     varnamesin, varnnamesin

logical, intent(in), optional :: &
     changel, changet, changeu
character(len=45), dimension(*), &
     intent(in), optional :: &
     varlnamesin, vartnamesin, varunitsin

logical, intent(in), optional :: mytot, usearea
logical, intent(in), optional :: isglobal, pbiome, ppft
logical, intent(in), optional :: wratio, wbresp

character(len=256), intent(in), optional :: &
  cropfilein, regionfilein, regridfilein
character(len=256), intent(in), optional :: &
  bsibfilein, csibfilein

!file variables
integer :: spos1, spos2, spos3
character(len=30) :: myprefixin, myprefixout
character(len=30) :: mysuffixin, mysuffixout
character(len=180) :: myindir, myoutdir
character(len=256) :: cropfile, regionfile, regridfile
character(len=256) :: bsibfile, csibfile

integer :: nfiles, mynfiles
character(len=256) :: tempfilename
character(len=256), dimension(:), allocatable :: &
   filenamesin, filenamesout

!local variables
integer :: nvars
logical :: change_lnames, change_tnames, change_units
logical :: use_aweight, is_global, print_biome, print_pft
logical :: write_ratio, write_bresp

character(len=20), dimension(:), allocatable :: &
   varnames, varnnames
character(len=45), dimension(:), allocatable :: &
   varlnames, vartnames, varunits

integer :: yr, mon, day
integer :: mydays, mref1, mref2
logical :: use_mytot

!--------------------------
!define optional variables
IF (present(prefixin)) THEN
   myprefixin = prefixin
ELSE
   IF (dref .GE. 1) THEN
       myprefixin = sibprefix(dref)
   ELSE
      myprefixin = 'sib4_'
   ENDIF
ENDIF

IF (present(prefixout)) THEN
   myprefixout = prefixout
ELSE
   IF (dref .GE. 1) THEN
      myprefixout = sibprefix(dref)
   ELSE
      myprefixout = 'sib4_'
   ENDIF
ENDIF

IF (present(nvarsin)) THEN
   nvars = nvarsin
ELSE
   nvars = 1
ENDIF

allocate(varnames(nvars))
IF (present(varnamesin)) THEN
  varnames(:) = varnamesin(1:nvars)
ELSE
  varnames(:) = ''
ENDIF

allocate(varnnames(nvars))
IF (present(varnnamesin)) THEN
  varnnames(:) = varnnamesin(1:nvars)
ELSE
  varnnames(:) = ''
ENDIF

IF (present(changel)) THEN
  change_lnames = changel
ELSE
  change_lnames = .false.
ENDIF

allocate(varlnames(nvars))
varlnames(:) = ''
IF (present(varlnamesin)) THEN
   varlnames(:) = varlnamesin(1:nvars)
ENDIF

IF (present(changet)) THEN
  change_tnames = changet
ELSE
  change_tnames = .false.
ENDIF

allocate(vartnames(nvars))
vartnames(:) = ''
IF (present(vartnamesin)) THEN
   vartnames(:) = vartnamesin(1:nvars)
ENDIF

IF (present(changeu)) THEN
   change_units = changeu
ELSE
   change_units = .false.
ENDIF

allocate(varunits(nvars))
varunits(:) = ''
IF (present(varunitsin)) THEN
   varunits(:) = varunitsin(1:nvars)
ENDIF

IF (present(mytot)) THEN
   use_mytot = mytot
ELSE
   use_mytot = .true.
ENDIF

IF (present(usearea)) THEN
   use_aweight = usearea
ELSE
   use_aweight = .false.
ENDIF

IF (present(isglobal)) THEN
   is_global = isglobal
ELSE
   is_global = .true.
ENDIF

IF (present(pbiome)) THEN
   print_biome = pbiome
ELSE
   print_biome = .false.
ENDIF

IF (present(ppft)) THEN
   print_pft = ppft
ELSE
   print_pft = .false.
ENDIF

IF (present(wratio)) THEN
   write_ratio = wratio
ELSE
  write_ratio = .false.
ENDIF

IF (present(wbresp)) THEN
   write_bresp = wbresp
ELSE
   write_bresp = .false.
ENDIF

IF (present(cropfilein)) THEN
   cropfile = cropfilein
ELSE
   cropfile = ''
ENDIF

IF (present(regionfilein)) THEN
   regionfile = regionfilein
ELSE
   regionfile = ''
ENDIF

IF (present(regridfilein)) THEN
   regridfile = regridfilein
ELSE
   regridfile = ''
ENDIF


IF (present(bsibfilein)) THEN
   bsibfile = bsibfilein
ELSE
   bsibfile = ''
ENDIF

IF (present(csibfilein)) THEN
   csibfile = csibfilein
ELSE
   csibfile = ''
ENDIF

!----------------------
!Print warning messages/notifications
print*,''
IF (dref .GE. 1) THEN
   print('(3a)'),'Processing ',trim(siblabel(dref)), &
      ' Files'
ELSE
   print('(3a)'),'Processing Other Files'
ENDIF

IF (use_annual) THEN
   IF ((startmon .NE. 1) .or. &
       (stopmon .NE. 12)) THEN
      print('(a)'),'  Requested annual files.'
      print('(a)'),'  Beginning Jan and ending Dec.'
   ENDIF
ENDIF

IF ((.not. use_annual) .and. &
    (.not. use_multi) .and. &
    (.not. use_monthly) .and. &
    (.not. use_daily))  THEN
    print(fa), '  Please specify the output file frequency.'
    print(f3a), '  In the ',sibprogram,'_xxx.txt file, select either'
    print(fa), '    use_daily, use_monthly, use_annual, or use_multi.'
    RETURN
ENDIF

!Get input and output file 
!  directory, prefix, and suffix
IF (dref .LT. 0) THEN  !Other Files
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
      IF (sibprogram .eq. 'combine') THEN
         mysuffixout = sibsuffix(dref-1)
      ELSEIF (sibprogram .eq. 'grid') THEN
         mysuffixout = gridsuffix(dref)
      ELSEIF (sibprogram .eq. 'unstack') THEN
         mysuffixout = ustacksuffix(dref)
      ELSE
          mysuffixout = sibsuffix(dref)
      ENDIF
   ENDIF
ENDIF

!-------------------
!Parse to process
IF (use_multi) THEN  !multi-year files
   nfiles = 0
   call create_sib_nameyr(tempfilename, &
          myindir, trim(myprefixin), trim(mysuffixin), &
          startmon, startyr, stopmon, stopyr, .true.)

   IF (trim(tempfilename) .NE. '') THEN
      nfiles = nfiles + 1
      allocate(filenamesin(nfiles))
      allocate(filenamesout(nfiles))
      filenamesin(nfiles) = tempfilename

      call create_sib_nameyr(filenamesout(nfiles), &
          myoutdir, trim(myprefixout), trim(mysuffixout), &
          startmon, startyr, stopmon, stopyr, .false.)

      call sib4post_control(sibprogram, &
          nfiles, filenamesin, filenamesout, &
          cropfile, regionfile, regridfile, &
          bsibfile, csibfile, &
          nvars, varnames, varnnames, &
          change_lnames, varlnames, &
          change_tnames, vartnames, &
          change_units, varunits, use_aweight, &
          is_global, print_biome, print_pft, &
          write_ratio, write_bresp)
   ELSE
      call create_sib_nameyr(tempfilename, &
           myindir, trim(myprefixin), trim(mysuffixin), &
           startmon, startyr, stopmon, stopyr, .false.)
      spos1=index(tempfilename,'/',back=.true.)
      spos2=len_trim(tempfilename)
      print('(2a)'),'  File Not Found: ',tempfilename(spos1+1:spos2)
   ENDIF
ENDIF


IF (use_annual) THEN  !annual files

   !...get number of valid files
   nfiles = 0
   DO yr = startyr, stopyr
      call create_sib_nameyro(tempfilename, &
           myindir, trim(myprefixin), trim(mysuffixin), &
           yr, yr, .true.)

      IF (tempfilename .NE. '') THEN
         nfiles = nfiles + 1
      ENDIF
   ENDDO

   IF (nfiles .EQ. 0) THEN
      print*, '  No Files Found.'
   ELSE
      !...save valid filenames
      allocate(filenamesin(nfiles))
      allocate(filenamesout(nfiles))
      nfiles = 0
      DO yr = startyr, stopyr
         call create_sib_nameyro(tempfilename, &
              myindir, trim(myprefixin), trim(mysuffixin), &
              yr, yr, .true.)

         IF (tempfilename .NE. '') THEN
             nfiles = nfiles + 1
             filenamesin(nfiles) = tempfilename
             call create_sib_nameyro(filenamesout(nfiles), &
                   myoutdir, trim(myprefixout), trim(mysuffixout), &
                   yr, yr, .false.)
         ENDIF    
      ENDDO
     
      call sib4post_control(sibprogram, &
            nfiles, filenamesin, filenamesout, &
            cropfile, regionfile, regridfile, &
            bsibfile, csibfile, &
            nvars, varnames, varnnames,  &
            change_lnames, varlnames, &
            change_tnames, vartnames, &
            change_units, varunits, use_aweight, &
            is_global, print_biome, print_pft, &
            write_ratio, write_bresp)
   ENDIF

ENDIF


IF (use_monthly) THEN  !monthly files
   IF (use_mytot) THEN

       !...find number of valid files
       nfiles = 0
       mynfiles = 0
       do yr=startyr, stopyr
          mref1=1
          mref2=12
          IF (yr .eq. startyr) mref1=startmon
          IF (yr .eq. stopyr) mref2=stopmon
          do mon = mref1, mref2
             call create_sib_namemon(tempfilename, &
                  myindir, trim(myprefixin), trim(mysuffixin), &
                  yr, mon, .true.)
             IF (trim(tempfilename) .NE. '') THEN 
                nfiles = nfiles + 1
             ELSEIF (dref .gt. 0) THEN
                IF (trim(mysuffixin) .EQ. trim(sibsuffix(dref))) THEN
                   call create_sib_namemon(tempfilename, &
                        myindir, trim(myprefixin), trim(gridsuffix(ndiag)), &
                        yr, mon, .true.)
                   IF (trim(tempfilename) .NE. '') nfiles = nfiles + 1
                ENDIF
             ENDIF
             mynfiles = mynfiles + 1
          enddo
       enddo

       IF (nfiles .EQ. 0) THEN
           print*,'  No Files Found.'
       ELSE
           IF (mynfiles .NE. nfiles) THEN
              print(fa), '  Beware: Missing Files.'
              print(fai),'  Number Expected: ',mynfiles
              print(fai),'  Number Found: ',nfiles
           ENDIF

           allocate(filenamesin(nfiles))
           allocate(filenamesout(nfiles))
           nfiles = 0
           do yr=startyr, stopyr
              mref1=1
              mref2=12
              IF (yr .eq. startyr) mref1=startmon
              IF (yr .eq. stopyr) mref2=stopmon

              do mon = mref1, mref2
                 call create_sib_namemon(tempfilename, &
                      myindir, trim(myprefixin), trim(mysuffixin), &
                      yr, mon, .true.)

                 IF ((trim(tempfilename) .EQ. '') .AND. &
                     (dref .gt. 0)) THEN
                      IF (trim(mysuffixin) .EQ. trim(sibsuffix(dref))) THEN
                          call create_sib_namemon(tempfilename, &
                               myindir, trim(myprefixin), trim(gridsuffix(ndiag)), &
                               yr, mon, .true.)
                      ENDIF
                 ENDIF

                 IF (trim(tempfilename) .NE. '') THEN
                     nfiles = nfiles + 1
                     filenamesin(nfiles) = tempfilename
                     IF (sibprogram .EQ. 'balance') THEN
                         call create_sib_nameyr(filenamesout(nfiles), &
                              myoutdir, trim(ratioprefix(dref)), &
                              trim(mysuffixout), startmon, startyr, &
                              stopmon, stopyr, .false.)
                     ELSEIF (sibprogram .EQ. 'cropdist') THEN
                         call create_sib_nameyr(filenamesout(nfiles), &
                              myoutdir, trim(cropprefix(dref)), &
                              trim(mysuffixout), startmon, startyr, &
                              stopmon, stopyr, .false.)

                     ELSEIF (sibprogram .EQ. 'conjoin') THEN
                        call create_sib_nameyr(filenamesout(nfiles), &
                              myoutdir, trim(myprefixout), trim(mysuffixout), &
                              startmon, startyr, stopmon, stopyr, &
                               .false.)
                     ELSE
                         call create_sib_namemon(filenamesout(nfiles), &
                              myoutdir, trim(myprefixout), trim(mysuffixout), &
                              yr, mon, .false.)
                     ENDIF
                 ELSE
                     call create_sib_namemon(tempfilename, &
                          myindir, trim(myprefixin), trim(mysuffixin), &
                          yr, mon, .false.)
                     print(fa),'   Missing File: '
                     print(fa),'    ',trim(tempfilename)
                 ENDIF
              enddo
           enddo

           IF ((startyr .eq. stopyr) .and. &
               (startmon .eq. stopmon)) THEN
               print('(3a,i4)'), '  Processing ', &
                 monnames(startmon), ' ', startyr
           ELSEIF (startyr .eq. stopyr) THEN
               print('(5a,i4)'), '  Processing ', &
                 monnames(startmon), ' - ', &
                 monnames(stopmon), ' ', startyr
           ELSE
               print('(3a,i4,3a,i4)'),'  Processing ', &
                   monnames(startmon), ' ', startyr,  &
                   ' - ', monnames(stopmon), ' ', stopyr
           ENDIF

           call sib4post_control(sibprogram, &
                nfiles, filenamesin, filenamesout, &
                cropfile, regionfile, regridfile, &
                bsibfile, csibfile, &
                nvars, varnames, varnnames, &
                change_lnames, varlnames, &
                change_tnames, vartnames, &
                change_units, varunits, use_aweight, &
                is_global, print_biome, print_pft, &
                write_ratio, write_bresp)

       ENDIF !monthly files found, mytot=.true.
   ELSE  !mytot=.false.
       do yr=startyr, stopyr
          mref1=1
          mref2=12
          IF (yr .eq. startyr) mref1=startmon
          IF (yr .eq. stopyr) mref2=stopmon

          nfiles = mref2-mref1+1
          allocate(filenamesin(nfiles))
          allocate(filenamesout(nfiles))
          
          do mon=mref1,mref2
             call create_sib_namemon(filenamesin(mon), &
                   myindir, trim(myprefixin), trim(mysuffixin), &
                   yr, mon, .true.)
             IF (trim(filenamesin(mon)) .EQ. '') THEN
                print(fa),'Missing File: '
                print(f2a),'  ',trim(filenamesin(mon))
                print(fa),'Stopping Calculation.'
                RETURN
             ELSE
                IF (sibprogram .EQ. 'cropdist') THEN
                   call create_sib_nameyr(filenamesout(mon), &
                        myoutdir, trim(cropprefix(dref)), &
                        trim(mysuffixout), mref1, yr,  &
                        mref2, yr, .false.)
                  
                ELSEIF (sibprogram .EQ. 'conjoin') THEN
                   call create_sib_nameyr(filenamesout(mon), &
                        myoutdir, trim(myprefixout), trim(mysuffixout), &
                        mref1, yr, mref2, yr, .false.)
                ELSE
                   call create_sib_namemon(filenamesout(mon), &
                        myoutdir, trim(myprefixout), trim(mysuffixout), &
                        yr, mon, .false.)
                ENDIF
             ENDIF
          enddo !mon=1,12

          IF (yr .ne. startyr) print(fa),''
          print(fa), '  -----------------------'
          print(fai),'  Processing ',yr
          call sib4post_control(sibprogram, &
               nfiles, filenamesin, filenamesout, &
               cropfile, regionfile, regridfile, &
               bsibfile, csibfile, &
               nvars, varnames, varnnames, &
               change_lnames, varlnames, &
               change_tnames, vartnames, &
               change_units, varunits, use_aweight, &
               is_global, print_biome, print_pft, &
               write_ratio, write_bresp)

          deallocate(filenamesin)
          deallocate(filenamesout)
       enddo !yr

    ENDIF !mytot
ENDIF  !monthly files

IF (use_daily) THEN !hourly files
   if (use_mytot) THEN
       !...find number of valid files
       nfiles = 0
       mynfiles = 0
       do yr=startyr, stopyr
          mref1=1
          mref2=12
          IF (yr .eq. startyr) mref1=startmon
          IF (yr .eq. stopyr) mref2=stopmon
          do mon = mref1, mref2

             mydays = dayspermon(mon)
             IF ((mod(yr,4) .eq. 0) .and. (mon .eq. 2)) mydays=mydays+1
             do day = 1, mydays
                 call create_sib_nameday(tempfilename, &
                  myindir, trim(myprefixin), trim(mysuffixin), &
                  yr, mon, day, .true.)
                 IF (trim(tempfilename) .NE. '') THEN 
                     nfiles = nfiles + 1
                 ELSEIF (dref .gt. 0) THEN
                     IF (trim(mysuffixin) .EQ. trim(sibsuffix(dref))) THEN
                         call create_sib_nameday(tempfilename, &
                               myindir, trim(myprefixin), trim(gridsuffix(ndiag)), &
                               yr, mon, day, .true.)
                         IF (trim(tempfilename) .NE. '') nfiles = nfiles + 1
                     ENDIF
                 ENDIF
                 mynfiles = mynfiles + 1
             enddo !day=1,mydays
          enddo !mon=mref1,mref2
       enddo !yr=startyr,stopyr

       IF (nfiles .EQ. 0) THEN
          print*,'  No Files Found.'
          print*,'  First File Tried: '
          call create_sib_nameday(tempfilename, &
               myindir, trim(myprefixin), trim(mysuffixin), &
               startyr, startmon, 1, .false.)
          print*,'   ',trim(tempfilename)
       ELSE
           IF (mynfiles .NE. nfiles) THEN
              print(fa), '  Beware: Missing Files.'
              print(fai),'  Number Expected: ',mynfiles
              print(fai),'  Number Found: ',nfiles
           ENDIF

           allocate(filenamesin(nfiles))
           allocate(filenamesout(nfiles))
           nfiles = 0
           do yr=startyr, stopyr
              mref1=1
              mref2=12
              IF (yr .eq. startyr) mref1=startmon
              IF (yr .eq. stopyr) mref2=stopmon

              do mon = mref1, mref2

                 mydays = dayspermon(mon)
                 IF ((mod(yr,4) .eq. 0) .and. (mon .eq. 2)) mydays=mydays+1
                 do day = 1, mydays
                    call create_sib_nameday(tempfilename, &
                         myindir, trim(myprefixin), trim(mysuffixin), &
                         yr, mon, day, .true.)
                    IF ((trim(tempfilename) .EQ. '') .and. &
                         (dref .gt. 0)) THEN
                       IF (trim(mysuffixin) .EQ. &
                           trim(sibsuffix(dref))) THEN
                          call create_sib_nameday(tempfilename, &
                               myindir, trim(myprefixin), &
                               trim(gridsuffix(ndiag)), &
                               yr, mon, day, .true.)
                       ENDIF
                     ENDIF

                     IF (trim(tempfilename) .NE. '') THEN
                         nfiles = nfiles + 1
                         filenamesin(nfiles) = tempfilename
                         IF (sibprogram .EQ. 'balance') THEN
                             call create_sib_nameyr(filenamesout(nfiles), &
                                  myoutdir, trim(ratioprefix(dref)), &
                                  trim(mysuffixout), startmon, startyr, &
                                  stopmon, stopyr, .false.)
                         ELSEIF (sibprogram .EQ. 'cropdist') THEN
                             call create_sib_nameyr(filenamesout(nfiles), &
                                  myoutdir, trim(cropprefix(dref)), &
                                  trim(mysuffixout), startmon, startyr, &
                                  stopmon, stopyr, .false.)
                         ELSEIF (sibprogram .EQ. 'conjoin') THEN
                             call create_sib_nameyr(filenamesout(nfiles), &
                                  myoutdir, trim(myprefixout), trim(mysuffixout), &
                                  startmon, startyr, stopmon, stopyr, &
                                  .false.)
                         ELSE
                             call create_sib_nameday(filenamesout(nfiles), &
                              myoutdir, trim(myprefixout), trim(mysuffixout), &
                              yr, mon, day, .false.)
                         ENDIF
                     ELSE
                         call create_sib_nameday(tempfilename, &
                          myindir, trim(myprefixin), trim(mysuffixin), &
                          yr, mon, day, .false.)
                         print(fa),'   Missing File: '
                         print(f2a),'    ',trim(tempfilename)
                     ENDIF
                 enddo !day=1,mydays
              enddo !mon=mref1,mref2
           enddo !yr=startyr,stopyr

           IF ((startyr .eq. stopyr) .and. &
               (startmon .eq. stopmon)) THEN
               print('(3a,i4)'), '  Processing ', &
                 monnames(startmon), ' ', startyr
           ELSEIF (startyr .eq. stopyr) THEN
               print('(5a,i4)'), '  Processing ', &
                 monnames(startmon), ' - ', &
                 monnames(stopmon), ' ', startyr
           ELSE
               print('(3a,i4,3a,i4)'),'  Processing ', &
                   monnames(startmon), ' ', startyr,  &
                   ' - ', monnames(stopmon), ' ', stopyr
           ENDIF

           call sib4post_control(sibprogram, &
                nfiles, filenamesin, filenamesout, &
                cropfile, regionfile, regridfile, &
                bsibfile, csibfile, &
                nvars, varnames, varnnames, &
                change_lnames, varlnames, &
                change_tnames, vartnames, &
                change_units, varunits, use_aweight, &
                is_global, print_biome, print_pft, &
                write_ratio, write_bresp)

       ENDIF !monthly files found, mytot=.true.

   else !use_mytot = .false.
       print*,'Not Capable Of Performing Annual Processes.'
       print*,'Please either:'
       print*,'   1) set use_mytot = .false.'
       print*,'   2) update sib4post_parse.f90'
       STOP
    endif
ENDIF

end subroutine sib4post_parse



