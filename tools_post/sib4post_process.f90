!========================================
subroutine sib4post_process(sibprogram, specfile, myyear)
!========================================

use sibinfo_module
implicit none

interface 
   subroutine sib4post_parse (sibprogram, &
       daily, monthly, annual, multi, &
       smon, syear, emon, eyear, &
       dref, indir, outdir, &
       prefixin, prefixout, &
       nvarsin, varnamesin, varnnamesin,  &
       changel, varlnamesin,  &
       changet, vartnamesin,  &
       changeu, varunitsin,   &
       mytot, usearea, &
       isglobal, pbiome, ppft, &
       wratio, wbresp, &
       cropfilein, regionfilein, &
       regridfilein, bsibfilein, csibfilein)

       character(len=*), intent(in) :: sibprogram
       logical, intent(in) :: daily, monthly, annual, multi
       integer, intent(in) :: smon, syear, emon, eyear
       integer, intent(in) :: dref
       character(len=180), intent(in) :: indir, outdir

       character(len=*), intent(in), optional :: prefixin, prefixout

       integer, intent(in), optional :: nvarsin
       character(len=20), dimension(*), intent(in), optional :: &
             varnamesin, varnnamesin

       logical, intent(in), optional :: changel, changet, changeu
       character(len=45), dimension(*), intent(in), optional :: &
             varlnamesin, vartnamesin, varunitsin

       logical, intent(in), optional :: mytot, usearea
       logical, intent(in), optional :: isglobal, pbiome, ppft
       logical, intent(in), optional :: wratio, wbresp

       character(len=256), intent(in), optional :: cropfilein, regionfilein
       character(len=256), intent(in), optional :: regridfilein
       character(len=256), intent(in), optional :: bsibfilein, csibfilein

   end subroutine sib4post_parse
end interface

!input variables
character(len=*), intent(in) :: sibprogram
character(len=*), intent(in) :: specfile
integer, intent(in) :: myyear

!user specified values
integer :: startyr, stopyr, startmon, stopmon
integer :: numprocs, nsfiles
logical :: use_daily, use_monthly, use_annual, use_multi
logical :: use_other
logical :: use_mytot, use_regions, use_aweight
logical :: merge_requib, merge_restart
logical, dimension(:), allocatable :: sib_diagnostic
character(len=180) :: indir, outdir, indiro, outdiro
character(len=180) :: myindir
character(len=256), dimension(:), allocatable :: &
    sfilesin, sfilesout

!specific file variables
integer :: nfiles
character(len=256), dimension(:), allocatable :: &
   filenamesin, filenamesout
logical :: file_exist

!special variables
character(len=256) :: cropfile, regionfile
character(len=256) :: regridfile
character(len=256) :: bsibfile, csibfile

integer :: nselvar
character(len=20), dimension(:), allocatable :: &
   varnames, varnnames
character(len=45), dimension(:), allocatable :: &
   varlnames, vartnames, varunits
logical :: use_selvar
logical :: change_lnames, change_tnames, change_units

logical :: isglobal, print_biome, print_pft
logical :: write_ratio, write_bresp

!misc variables
integer :: f
integer :: dref, myref, spos
integer :: spos1, spos2
logical :: add_bresp, add_cresp

character(len=20) :: cres
character(len=45) :: junk


!---------------------------------
!Get program running from specfile
!spos = index(specfile,'_')
!sibprogram = specfile(1:spos-1)
nselvar = -1
nsfiles = -1
indir = ''
indiro = ''
outdir = ''
outdiro = ''
merge_requib = .false.
merge_restart = .false.
use_monthly = .false.
use_annual = .false.
use_multi = .false.
use_other = .false.
isglobal = .false.
print_biome = .false.
print_pft = .false.
use_daily = .false.

allocate(sib_diagnostic(ndiag))
sib_diagnostic(:) = .FALSE.

!print*,'Opening SpecFile: ',trim(specfile)
open(unit=11,file=trim(specfile),form='formatted')
read(11,fj) junk
read(11,fj) junk
read(11,fji) junk, startmon
read(11,fji) junk, startyr
read(11,fji) junk, stopmon
read(11,fji) junk, stopyr

spos = index(specfile,'/',back=.true.)
IF (specfile(spos+1:spos+8) .EQ. 'sib4post') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fjl) junk, merge_restart
   read(11,fjl) junk, merge_requib
   read(11,fji) junk, numprocs
   read(11,fjl) junk, use_mytot
   read(11,fjl) junk, use_regions
   read(11,fjl) junk, isglobal
   read(11,fjl) junk, print_biome
   read(11,fjl) junk, print_pft
   read(11,fjl) junk, use_aweight
   read(11,fjl) junk, add_bresp
   read(11,fjl) junk, add_cresp

   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro
   
   read(11,fj) junk
   read(11,ff) bsibfile
   read(11,fj) junk
   read(11,ff) csibfile
   read(11,fj) junk
   read(11,ff) cropfile
   read(11,fj) junk
   read(11,ff) regionfile
   read(11,fj) junk
   read(11,ff) regridfile
   
   read(11,fj) junk
   read(11,fi) nselvar
   IF (nselvar .lt. 1) THEN
       nselvar=1
       use_selvar = .false.
       allocate(varnames(1))
       read(11,fv) varnames(1)
       varnames(1) = ''
   ELSE
       use_selvar = .true.
       allocate(varnames(nselvar))
       do dref=1,nselvar
          read(11,fv) varnames(dref)
       enddo
   ENDIF

   read(11,fj) junk
   allocate(varnnames(nselvar))
   do dref=1,nselvar
      read(11,fv) varnnames(dref)
   enddo
   IF (.not. use_selvar) varnnames(:)=''

   read(11,fj) junk
   read(11,fl) change_lnames
   allocate(varlnames(nselvar))
   IF (use_selvar) THEN
      do dref=1,nselvar
         read(11,fj) varlnames(dref)
      enddo
   ELSE
      varlnames(:) = ''
      read(11,fj) junk
   ENDIF

   read(11,fj) junk
   read(11,fl) change_tnames
   allocate(vartnames(nselvar))
   IF (use_selvar) THEN
      do dref=1,nselvar
         read(11,fj) vartnames(dref)
      enddo
   ELSE
      vartnames(:) = ''
      read(11,fj) junk
   ENDIF

   read(11,fj) junk
   read(11,fl) change_units
   allocate(varunits(nselvar))
   IF (use_selvar) THEN
       do dref=1,nselvar
          read(11,fj) varunits(dref)
       enddo
   ELSE
       varunits(:) = ''
       read(11,fj) junk
   ENDIF

   IF (nsfiles .ge. 1) THEN
      allocate(sfilesin(nsfiles))
      read(11,fj) junk
      do dref=1,nsfiles
         read(11,ff) sfilesin(dref)
      enddo

      allocate(sfilesout(nsfiles))
      read(11,fj) junk
      do dref=1,nsfiles
         read(11,ff) sfilesout(dref)
      enddo
   ENDIF
   
ELSEIF (trim(sibprogram) .EQ. 'addresp') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo

   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,ff) bsibfile

ELSEIF (trim(sibprogram) .EQ. 'balance') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo

   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, write_ratio
   read(11,fjl) junk, write_bresp
   read(11,fj)  junk
   read(11,fd)  indir
   read(11,fj)  junk
   read(11,fd)  outdir

ELSEIF (trim(sibprogram) .EQ. 'checkbal') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo

   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,ff) bsibfile
   read(11,fj) junk
   read(11,ff) csibfile

ELSEIF (trim(sibprogram) .EQ. 'combine') THEN
   myref=2
   do dref=1,ndiagpft
      read(11,fjl) junk, sib_diagnostic(myref)
      myref = myref + 2
   enddo

   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,fd) indir
   outdir = indir

   if (nsfiles .ge. 1) THEN
       allocate(sfilesin(nsfiles),sfilesout(nsfiles))
       read(11,fj) junk
       do dref=1,nsfiles
          read(11,ff) sfilesin(dref)
       enddo

       read(11,fj) junk
       do dref=1,nsfiles
          read(11,ff) sfilesout(dref)
       enddo
   endif

ELSEIF (trim(sibprogram) .EQ. 'conjoin') THEN
  do dref=1,ndiag
     read(11,fjl) junk, sib_diagnostic(dref)
  enddo
  read(11,fjl) junk, use_mytot
  read(11,fj) junk
  read(11,fd) indir
  read(11,fj) junk
  read(11,fd) outdir
  read(11,fj) junk
  read(11,fi) nselvar
  if (nselvar .ge. 1) then
     allocate(varnames(nselvar))
     do dref=1,nselvar
        read(11,fv) varnames(dref)
     enddo
  endif

  use_monthly = .true.

ELSEIF (trim(sibprogram) .EQ. 'copy') THEN

   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro

   if (nsfiles .ge. 1) THEN
       allocate(sfilesin(nsfiles),sfilesout(nsfiles))
       read(11,fj) junk
       do dref=1,nsfiles
          read(11,ff) sfilesin(dref)
       enddo

       read(11,fj) junk
       do dref=1,nsfiles
          read(11,ff) sfilesout(dref)
       enddo
   endif


ELSEIF (trim(sibprogram) .EQ. 'cropdist') THEN
   
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo

   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_mytot
   read(11,fjl) junk, use_regions
   read(11,fj) junk
   read(11,ff) cropfile
   read(11,fj) junk
   read(11,ff) regionfile
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,ff) outdir

   IF (.not. use_regions) regionfile=''

ELSEIF (trim(sibprogram) .eq. 'disjoin') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro
   read(11,fj) junk
   read(11,fi) nselvar
   IF (nselvar .lt. 1) THEN
      nselvar=1
      allocate(varnames(nselvar))
      varnames(:) = ''
   ELSE
      allocate(varnames(nselvar))
      do dref=1,nselvar
         read(11,fv) varnames(dref)
      enddo
   ENDIF
   
ELSEIF (trim(sibprogram) .eq. 'gpptot') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fjl) junk, isglobal
   read(11,fjl) junk, print_biome
   read(11,fjl) junk, print_pft
   read(11,fj) junk
   read(11,fd) indir

   read(11,fj) junk
   read(11,fd) indiro

   if (nsfiles .ge. 1) THEN
       allocate(sfilesin(nsfiles))
       allocate(sfilesout(nsfiles))
       read(11,fj) junk
       do dref=1,nsfiles
          read(11,ff) sfilesin(dref)
       enddo

       read(11,fj) junk
       do dref=1,nsfiles
          read(11,ff) sfilesout(dref)
       enddo
   endif

ELSEIF (trim(sibprogram) .EQ. 'grid') THEN
   read(11,fjl) junk, isglobal
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro

   if (nsfiles .ge. 1) then
      allocate(sfilesin(nsfiles))
      allocate(sfilesout(nsfiles))

      read(11,fj) junk
      DO dref=1,nsfiles
          read(11,ff) sfilesin(dref)
      ENDDO

      read(11,fj) junk
      DO dref=1,nsfiles
         read(11,ff) sfilesout(dref)
      ENDDO
   endif

ELSEIF (trim(sibprogram) .EQ. 'meanclim') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other

   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fi) nselvar
   if (nselvar .ge. 1) THEN
      allocate(varnames(nselvar))
      do dref=1,nselvar
         read(11,fv) varnames(dref)
      enddo
   endif

ELSEIF (trim(sibprogram) .EQ. 'merge') THEN
   read(11,fjl) junk, merge_restart
   read(11,fjl) junk, merge_requib
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fji) junk, numprocs

   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir

   read(11,fj) junk
   read(11,fi) nselvar
   if (nselvar .ge. 1) THEN
      allocate(varnames(nselvar))
      do dref=1,nselvar
         read(11,fv) varnames(dref)
      enddo
   endif

ELSEIF (trim(sibprogram) .EQ. 'pullvar') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro

   read(11,fj) junk
   read(11,fi) nselvar
   IF (nselvar .LT. 1) THEN
      print(fa),'Expecting at least one variable to pullvar.'
      print(fa),'Stopping.'
      STOP
   ENDIF
   allocate(varnames(nselvar))
   do dref=1,nselvar
      read(11,fv) varnames(dref)
   enddo

   if (nsfiles .ge. 1) then
      allocate(sfilesin(nsfiles))
      allocate(sfilesout(nsfiles))

      read(11,fj) junk
      DO dref=1,nsfiles
          read(11,ff) sfilesin(dref)
      ENDDO

      read(11,fj) junk
      DO dref=1,nsfiles
         read(11,ff) sfilesout(dref)
      ENDDO
   endif


ELSEIF (trim(sibprogram) .EQ. 'regrid') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl)  junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,ff) regridfile
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro

   if (nsfiles .ge. 1) then
      allocate(sfilesin(nsfiles))
      allocate(sfilesout(nsfiles))

      read(11,fj) junk
      DO dref=1,nsfiles
          read(11,ff) sfilesin(dref)
      ENDDO

      read(11,fj) junk
      DO dref=1,nsfiles
         read(11,ff) sfilesout(dref)
      ENDDO
   endif

ELSEIF (trim(sibprogram) .EQ. 'renamevar') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) indiro

   read(11,fj) junk
   read(11,fi) nselvar
   IF (nselvar .LT. 1) THEN
      print(fa),'Expecting at least one variable to rename.'
      print(fa),'Stopping.'
      STOP
   ENDIF
   allocate(varnames(nselvar))
   do dref=1,nselvar
      read(11,fv) varnames(dref)
   enddo
 
   allocate(varnnames(nselvar))
   read(11,fj) junk
   do dref=1,nselvar
      read(11,fv) varnnames(dref)
   enddo

   allocate(varlnames(nselvar))
   read(11,fj) junk
   read(11,fl) change_lnames
   do dref=1,nselvar
      read(11,fj) varlnames(dref)
   enddo

   allocate(vartnames(nselvar))
   read(11,fj) junk
   read(11,fl) change_tnames
   do dref=1,nselvar
      read(11,fj) vartnames(dref)
   enddo

   allocate(varunits(nselvar))
   read(11,fj) junk
   read(11,fl) change_units
   do dref=1,nselvar
      read(11,fj) varunits(dref)
   enddo

   if (nsfiles .ge. 1) then
      allocate(sfilesin(nsfiles))
      allocate(sfilesout(nsfiles))
      
      read(11,fj) junk
      DO dref=1,nsfiles
          read(11,ff) sfilesin(dref)
      ENDDO
      sfilesout(:) = ''
   endif

ELSEIF (trim(sibprogram) .EQ. 'replaceresp') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo

   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fj) junk
   read(11,ff) bsibfile
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir

ELSEIF (trim(sibprogram) .EQ. 'spmerge') THEN
   read(11,fjl) junk, merge_restart
   read(11,fjl) junk, merge_requib
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fji) junk, numprocs

   read(11,fj) junk
   read(11,ff) regridfile
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir

   read(11,fj) junk
   read(11,fi) nselvar
   if (nselvar .ge. 1) THEN
      allocate(varnames(nselvar))
      do dref=1,nselvar
         read(11,fv) varnames(dref)
      enddo
   endif
   
ELSEIF (trim(sibprogram) .EQ. 'unstack') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl) junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fjl) junk, use_aweight
   read(11,fjl) junk, add_bresp
   read(11,fjl) junk, add_cresp
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,ff) bsibfile
   read(11,fj) junk
   read(11,ff) csibfile
   read(11,fj) junk

   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro
   read(11,fj) junk

   read(11,fj) junk
   read(11,fi) nselvar
   IF (nselvar .GE. 1) THEN
      allocate(varnames(nselvar))
      do dref=1,nselvar
         read(11,fv) varnames(dref)
      enddo
   ELSE
      nselvar=1
      allocate(varnames(nselvar))
      varnames(:) = ''
   ENDIF

   if (nsfiles .ge. 1) then
      allocate(sfilesin(nsfiles))
      allocate(sfilesout(nsfiles))

      read(11,fj) junk
      read(11,fj) junk
      DO dref=1,nsfiles
          read(11,ff) sfilesin(dref)
      ENDDO

      read(11,fj) junk
      DO dref=1,nsfiles
         read(11,ff) sfilesout(dref)
      ENDDO
   endif

   IF (.not. add_bresp) bsibfile=''
   IF (.not. add_cresp) csibfile=''

ELSEIF (trim(sibprogram) .EQ. 'vec2vec') THEN
   do dref=1,ndiag
      read(11,fjl) junk, sib_diagnostic(dref)
   enddo
   read(11,fjl)  junk, use_other
   read(11,fjl) junk, use_daily
   read(11,fjl) junk, use_monthly
   read(11,fjl) junk, use_annual
   read(11,fjl) junk, use_multi
   read(11,fji) junk, nsfiles
   read(11,fj) junk
   read(11,ff) regridfile
   read(11,fj) junk
   read(11,fd) indir
   read(11,fj) junk
   read(11,fd) outdir
   read(11,fj) junk
   read(11,fd) indiro
   read(11,fj) junk
   read(11,fd) outdiro

   if (nsfiles .ge. 1) then
      allocate(sfilesin(nsfiles))
      allocate(sfilesout(nsfiles))

      read(11,fj) junk
      DO dref=1,nsfiles
          read(11,ff) sfilesin(dref)
      ENDDO

      read(11,fj) junk
      DO dref=1,nsfiles
         read(11,ff) sfilesout(dref)
      ENDDO
   endif

ELSE
   print(fa),''
   print(fa),'Need A Template To Read Input File!'
   print(fa),'Please Add This In SiB4Post_Process.'
   print(f2a),'Program: ',trim(sibprogram)
   print(fa),'Stopping.'
   print(fa),''
   STOP
ENDIF
close(11)


!set nselvar if not already....
IF (nselvar .lt. 1) THEN
    nselvar = 1
    allocate(varnames(nselvar))
    varnames(1) = ''
ENDIF

IF (myyear .gt. 0) THEN
   startyr = myyear
   stopyr = myyear
ENDIF

!-------------------------
! Diagnostic Output
!------------------------
IF (trim(sibprogram) .EQ. 'merge') THEN
   call merge_parse( &
        startmon, startyr, stopmon, stopyr,  &
        merge_restart, merge_requib, &
        sib_diagnostic, numprocs, &
        indir, outdir, nselvar, varnames)
   RETURN
ELSE IF (trim(sibprogram) .EQ. 'spmerge') THEN
   call spmerge_parse( &
        startmon, startyr, stopmon, stopyr,  &
        merge_restart, merge_requib, &
        sib_diagnostic, numprocs, &
        regridfile, indir, outdir, nselvar, varnames)
   RETURN
ELSE
 do dref=1,ndiag
    if (sib_diagnostic(dref)) then

      IF (trim(sibprogram) .EQ. 'addresp') THEN
         !Check ratio file
         spos1=index(bsibfile,'.nc')
         IF (spos1 .lt. 1) THEN
            spos1=index(bsibfile,'/',back=.true.)
            spos2=len_trim(bsibfile)
            IF (spos1 .eq. spos2) THEN
                outdir=trim(bsibfile)
                cres=ratioprefix(dref)
            ELSE
                outdir=bsibfile(1:spos1)
                cres=bsibfile(spos1+1:spos2)
            ENDIF          
            call create_sib_nameyr(bsibfile, &
                 outdir, cres, sibsuffix(dref), &
                 startmon, startyr, stopmon, stopyr, .true.)
         ENDIF

         IF (trim(bsibfile) .EQ. '') THEN
            print*,'Missing Ratio File: '
            call create_sib_nameyr(bsibfile, &
                 outdir, cres, sibsuffix(dref), &
                 startmon, startyr, stopmon, stopyr, .false.)
            print*,'  ',trim(bsibfile)
            print*,'Stopping.'
         ELSE
            call sib4post_parse( trim(sibprogram), &
                 use_daily, use_monthly, &
                 use_annual, use_multi, &
                 startmon, startyr, stopmon, stopyr, &
                 dref, indir, outdir, &
                 bsibfilein=bsibfile)
         ENDIF

      ELSEIF (trim(sibprogram) .EQ. 'balance') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              wratio=write_ratio, wbresp=write_bresp)

      ELSEIF (trim(sibprogram) .EQ. 'checkbal') THEN
         !Check for PFTs
         spos1 = index(sibsuffix(dref),'.lu.')
         IF (spos1 .GE. 1) THEN
            spos1 = index(csibfile,'.nc')
            IF (spos1 .lt. 1) THEN
                spos1=index(csibfile,'/',back=.true.)
                spos2=len_trim(csibfile)
                IF (spos1 .eq. spos2) THEN
                   outdir=trim(csibfile)
                   cres=cropprefix(dref)
                ELSE
                   outdir=csibfile(1:spos1)
                   cres=csibfile(spos1+1:spos2)
                ENDIF

                call create_sib_nameyr(csibfile, &
                     outdir, cres, sibsuffix(dref), &
                     startmon, startyr, stopmon, stopyr, .true.)

                IF (trim(csibfile) .eq. '') THEN
                   print(fa),'Missing Crop Respiration File: '
                   call create_sib_nameyr(csibfile, &
                        outdir, cres, sibsuffix(dref), &
                        startmon, startyr, stopmon, stopyr, .false.)
                   print(f2a),'  ',trim(csibfile)
                   csibfile = ''
                ENDIF
            ENDIF
         ELSE
             csibfile=''
         ENDIF

         !Check ratio file
         spos1=index(bsibfile,'.nc')
         IF (spos1 .lt. 1) THEN
            spos1=index(bsibfile,'/',back=.true.)
            spos2=len_trim(bsibfile)
            IF (spos1 .eq. spos2) THEN
                outdir=trim(bsibfile)
                cres=ratioprefix(dref)
            ELSE
                outdir=bsibfile(1:spos1)
                cres=bsibfile(spos1+1:spos2)
            ENDIF          
            call create_sib_nameyr(bsibfile, &
                 outdir, cres, sibsuffix(dref), &
                 startmon, startyr, stopmon, stopyr, .true.)

            IF (trim(bsibfile) .eq. '') THEN
               print(fa),'Missing Balance Ratio File: '
               call create_sib_nameyr(bsibfile, &
                    outdir, cres, sibsuffix(dref), &
                    startmon, startyr, stopmon, stopyr, .false.)
               print(f2a),'  ',trim(bsibfile)
               bsibfile = ''
            ENDIF

         ENDIF

         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              bsibfilein=bsibfile, csibfilein=csibfile)

      ELSEIF (trim(sibprogram) .EQ. 'combine') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir)

      ELSEIF (trim(sibprogram) .EQ. 'conjoin') THEN
        call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              mytot=use_mytot, &
              nvarsin=nselvar, varnamesin=varnames)

      ELSEIF (trim(sibprogram) .EQ. 'cropdist') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              mytot=use_mytot, &
              cropfilein=cropfile, regionfilein=regionfile)

      ELSEIF (trim(sibprogram) .EQ. 'disjoin') THEN
         call disjoin_parse( &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              nselvar, varnames)

      ELSEIF (trim(sibprogram) .EQ. 'gpptot') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, isglobal=isglobal, &
              pbiome=print_biome, ppft=print_pft)

      ELSEIF (trim(sibprogram) .EQ. 'grid') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, isglobal=isglobal, &
              prefixout=trim(gridprefix(dref)))

      ELSEIF (trim(sibprogram) .EQ. 'meanclim') THEN
         call meanclim_parse( dref, &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              indir, outdir, nselvar, varnames)

      ELSEIF (trim(sibprogram) .EQ. 'pullvar') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              prefixout=trim(pullprefix(dref)), &
              nvarsin=nselvar, varnamesin=varnames)

      ELSEIF (trim(sibprogram) .EQ. 'regrid') THEN
         !Get resolution string
         spos1=index(regridfile,'_',back=.true.)
         spos2=index(regridfile,'.nc',back=.true.)
         cres=regridfile(spos1+1:spos2-1)
         junk=trim(gridprefix(dref)) // trim(cres) // '_'
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              prefixin=trim(gridprefix(dref)), &
              prefixout=trim(junk), &
              regridfilein=regridfile)

      ELSEIF (trim(sibprogram) .EQ. 'renamevar') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              nvarsin=nselvar, varnamesin=varnames, &
              varnnamesin=varnnames, &
              changel=change_lnames, varlnamesin=varlnames, &
              changet=change_tnames, vartnamesin=vartnames, &
              changeu=change_units, varunitsin=varunits)

      ELSEIF (trim(sibprogram) .EQ. 'replaceresp') THEN
         !Check ratio file
         spos1=index(bsibfile,'.nc')
         IF (spos1 .lt. 1) THEN
            spos1=index(bsibfile,'/',back=.true.)
            spos2=len_trim(bsibfile)
            IF (spos1 .eq. spos2) THEN
                outdir=trim(bsibfile)
                cres=ratioprefix(dref)
            ELSE
                outdir=bsibfile(1:spos1)
                cres=bsibfile(spos1+1:spos2)
            ENDIF          
            call create_sib_nameyr(bsibfile, &
                 outdir, cres, sibsuffix(dref), &
                 startmon, startyr, stopmon, stopyr, .true.)
         ENDIF

         IF (trim(bsibfile) .EQ. '') THEN
            print*,'Missing Ratio File: '
            call create_sib_nameyr(bsibfile, &
                 outdir, cres, sibsuffix(dref), &
                 startmon, startyr, stopmon, stopyr, .false.)
            print*,'  ',trim(bsibfile)
            print*,'Stopping.'
         ELSE
            call sib4post_parse( trim(sibprogram), &
                 use_daily, use_monthly, &
                 use_annual, use_multi, &
                 startmon, startyr, stopmon, stopyr, &
                 dref, indir, outdir, &
                 bsibfilein=bsibfile)
         ENDIF

      ELSEIF (trim(sibprogram) .EQ. 'unstack') THEN
         !Check if adding balanced respiration
         IF (add_bresp) THEN
            spos1 = index(bsibfile,'.nc')
            IF (spos1 .lt. 1) THEN
               inquire(file=trim(bsibfile),exist=file_exist)
               IF (file_exist) THEN
                   myindir = trim(bsibfile)
               ELSE
                   myindir = indir
               ENDIF
               call create_sib_nameyr(bsibfile, &
                    myindir, ratioprefix(dref), sibsuffix(dref), &
                    startmon, startyr, stopmon, stopyr, .true.)
               IF (trim(bsibfile) .EQ. '') THEN
                   call create_sib_nameyr(bsibfile, &
                         myindir, ratioprefix(dref), gridsuffix(dref), &
                         startmon, startyr, stopmon, stopyr, .true.)
                   IF (trim(bsibfile) .EQ. '') THEN
                         print(fa),'Missing Balanced Resp Ratio File'
                         call create_sib_nameyr(bsibfile, &
                              indir, ratioprefix(dref), sibsuffix(dref), &
                              startmon, startyr, stopmon, stopyr, .false.)
                         print(fa),'Tried: '
                         print(f2a),'  ',trim(bsibfile)
                         call create_sib_nameyr(bsibfile, &
                              indir, ratioprefix(dref), gridsuffix(dref), &
                              startmon, startyr, stopmon, stopyr, .false.)
                         print(f2a),'  ',trim(bsibfile)
                         bsibfile = ''
                  ENDIF
               ENDIF
            ENDIF
         ELSE
             bsibfile=''
         ENDIF

         !Check if adding crop harvest respiration
         IF (add_cresp) THEN
            spos1 = index(csibfile,'.nc')
            IF (spos1 .lt. 1) THEN
                inquire(file=trim(csibfile),exist=file_exist)
                if (file_exist) THEN
                    myindir = trim(csibfile)
                else
                    myindir = indir
                endif
                call create_sib_nameyr(csibfile, &
                     myindir, cropprefix(dref), sibsuffix(dref), &
                     startmon, startyr, stopmon, stopyr, .true.)
                IF (csibfile .EQ. '') THEN
                   call create_sib_nameyr(csibfile, &
                        myindir, cropprefix(dref), gridsuffix(dref), &
                        startmon, startyr, stopmon, stopyr, .true.)
                   IF (trim(csibfile) .eq. '') THEN
                       print(fa),'Missing Crop Resp File'
                       print(fa),'Tried: '
                       call create_sib_nameyr(csibfile, &
                            indir, cropprefix(dref), sibsuffix(dref), &
                            startmon, startyr, stopmon, stopyr, .false.)
                       print(f2a),'  ',trim(csibfile)
                       call create_sib_nameyr(csibfile, &
                            indir, cropprefix(dref), gridsuffix(dref), &
                            startmon, startyr, stopmon, stopyr, .false.)
                       print(f2a),' ',trim(csibfile)
                       csibfile = ''
                  ENDIF
                ENDIF
            ENDIF
        ELSE
            csibfile = ''
        ENDIF

         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              prefixout=trim(ustackprefix(dref)), &
              nvarsin=nselvar, varnamesin=varnames, &
              usearea=use_aweight, &
              bsibfilein=bsibfile, csibfilein=csibfile)

      ELSEIF (trim(sibprogram) .EQ. 'vec2vec') THEN
         !Get resolution string
         spos1=index(regridfile,'_',back=.true.)
         spos2=index(regridfile,'.nc',back=.true.)
         cres=regridfile(spos1+1:spos2-1)
         junk=trim(gridprefix(dref)) // trim(cres) // '_'
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indir, outdir, &
              prefixin=trim(gridprefix(dref)), &
              prefixout=trim(junk), &
              regridfilein=regridfile)

      ELSE
          print*,''
          print*,'Unknown Parsing Instructions: ',trim(sibprogram)
          print*,'Add This Program To SiB4Post_Process Parse List.'
          print*,''
      ENDIF
   endif !sib_diagnostic test
 enddo !dref=1,ndiag
ENDIF !loop through for all but merge

!-------------------------
! Other Output
!------------------------
if (use_other) then
   dref = -1

   IF (trim(sibprogram) .EQ. 'copy') THEN
       call sib4post_parse( trim(sibprogram), &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro)

   ELSEIF (trim(sibprogram) .EQ. 'disjoin') THEN
       call disjoin_parse( &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro, &
            nselvar, varnames)

   ELSEIF (trim(sibprogram) .EQ. 'gpptot') THEN
       call sib4post_parse( trim(sibprogram), &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro, &
            isglobal=isglobal, pbiome=print_biome, &
            ppft=print_pft)

   ELSEIF (trim(sibprogram) .EQ. 'grid') THEN
       call sib4post_parse( trim(sibprogram), &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro, isglobal=isglobal)

  ELSEIF (trim(sibprogram) .EQ. 'meanclim') THEN
       call meanclim_parse( 0, &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            indir, outdir, nselvar, varnames)

   ELSEIF (trim(sibprogram) .EQ. 'pullvar') THEN
       call sib4post_parse( trim(sibprogram), &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro, &
            nvarsin=nselvar, varnamesin=varnames)

   ELSEIF (trim(sibprogram) .EQ. 'regrid') THEN
       call sib4post_parse( trim(sibprogram), &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro, &
            regridfilein=regridfile)

  ELSEIF (trim(sibprogram) .EQ. 'renamevar') THEN
      call sib4post_parse( trim(sibprogram), &
           use_daily, use_monthly, &
           use_annual, use_multi, &
           startmon, startyr, stopmon, stopyr, &
           dref, indiro, outdiro, &
           nvarsin=nselvar, varnamesin=varnames, &
           varnnamesin=varnnames, &
           changel=change_lnames, varlnamesin=varlnames, &
           changet=change_tnames, vartnamesin=vartnames, &
           changeu=change_units, varunitsin=varunits)

   ELSEIF (trim(sibprogram) .EQ. 'unstack') THEN
         call sib4post_parse( trim(sibprogram), &
              use_daily, use_monthly, &
              use_annual, use_multi, &
              startmon, startyr, stopmon, stopyr, &
              dref, indiro, outdiro, &
              nvarsin=nselvar, varnamesin=varnames, &
              csibfilein=csibfile, usearea=use_aweight)

   ELSEIF (trim(sibprogram) .EQ. 'vec2vec') THEN
       call sib4post_parse( trim(sibprogram), &
            use_daily, use_monthly, &
            use_annual, use_multi, &
            startmon, startyr, stopmon, stopyr, &
            dref, indiro, outdiro, &
            regridfilein=regridfile)

   ELSE
       print(fa),''
       print(fa),'Cannot Process Other Files: '
       print(fa),'Unknown Other File Parsing Specifications.'
       print(fa),'Please Add Instructions In SiB4Post_Process.'
       print(fa),''
   ENDIF

endif


!-------------------------
! Process Specific Files
!------------------------
if (nsfiles .GE. 1) then
    print(fa),''
    print(fa),'Processing Specific Files'

    !...find number of files valid
    nfiles = 0
    DO f=1,nsfiles
       inquire(file=trim(sfilesin(f)), exist=file_exist)
       IF (file_exist) nfiles = nfiles + 1
    ENDDO

    allocate(filenamesin(nfiles))
    allocate(filenamesout(nfiles))
    nfiles = 0
    DO f=1,nsfiles
       inquire(file=trim(sfilesin(f)), exist=file_exist)
       IF (file_exist) THEN
          nfiles = nfiles + 1
          filenamesin(nfiles) = sfilesin(f)
          filenamesout(nfiles) = sfilesout(f)
       ENDIF
     ENDDO

     !...save valid file names
     IF (nfiles .EQ. 0) THEN
        print*,'   No Files Found.'
     ELSE
        IF (nselvar .LE. 0) THEN
           nselvar = 1
           allocate(varnames(nselvar))
           allocate(varnnames(nselvar))
           allocate(varlnames(nselvar))
           allocate(vartnames(nselvar))
           allocate(varunits(nselvar))
           varnames(:) = ''
           varnnames(:) = ''
           varlnames(:) = ''
           vartnames(:) = ''
           varunits(:) = ''
           
           change_lnames = .false.
           change_tnames = .false.
           change_units = .false.
        ELSE
           IF (.not. allocated(varnnames)) &
               allocate(varnnames(nselvar))
           IF (.not. allocated(varlnames)) THEN
              allocate(varlnames(nselvar))
              change_lnames = .false.
           ENDIF
           IF (.not. allocated(vartnames)) THEN
              allocate(vartnames(nselvar))
              change_tnames = .false.
           ENDIF
           IF (.not. allocated(varunits)) THEN
              allocate(varunits(nselvar))
              change_units = .false.
           ENDIF
        ENDIF

        call sib4post_control(trim(sibprogram), &
              nfiles, filenamesin, filenamesout, &
              cropfile, regionfile, regridfile, &
              bsibfile, csibfile, &
              nselvar, varnames, varnnames, &
              change_lnames, varlnames, &
              change_tnames, vartnames, &
              change_units, varunits, use_aweight, &
              isglobal, print_biome, print_pft, &
              write_ratio, write_bresp)
    ENDIF

endif !use_specific

end subroutine sib4post_process


