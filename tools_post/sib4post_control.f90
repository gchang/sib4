subroutine sib4post_control(sibprogram, &
   nfiles, filenamesin, filenamesout, &
   cropfile, regionfile, regridfile, &
   bsibfile, csibfile, &
   nvars, varnames, varnnames, &
   change_lnames, varlnames, &
   change_tnames, vartnames, &
   change_units, varunits, &
   use_aweight, isglobal, &
   print_biome, print_pft, &
   write_ratio, write_bresp)

implicit none

!input variables
character(len=*), intent(in) :: sibprogram
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: &
   filenamesin, filenamesout
character(len=256) :: &
   cropfile, regionfile, regridfile, bsibfile, csibfile

integer, intent(in) :: nvars
character(len=20), dimension(nvars), intent(in) :: &
   varnames, varnnames

logical, intent(in) :: &
   change_lnames, change_tnames, change_units
character(len=45), dimension(nvars), intent(in) :: &
   varlnames, vartnames, varunits

logical, intent(in) :: use_aweight
logical, intent(in) :: isglobal, print_biome, print_pft
logical, intent(in) :: write_ratio, write_bresp

!--------------------------------
!Call approrpriate routines:

IF (sibprogram .EQ. 'addresp') THEN
   call addresp_output( &
        nfiles, filenamesin, filenamesout, &
        bsibfile)

ELSEIF (sibprogram .EQ. 'balance') THEN
   call balance_output( &
        nfiles, filenamesin, filenamesout(1), &
        write_ratio, write_bresp)

ELSEIF (sibprogram .EQ. 'checkbal') THEN
   call checkbal_output( nfiles, &
        filenamesin, bsibfile, csibfile)

ELSEIF (sibprogram .EQ. 'combine') THEN
   call combine_output( nfiles, &
        filenamesin, filenamesout)

ELSEIF (sibprogram .EQ. 'conjoin') THEN
   call conjoin_output( nfiles, &
        filenamesin, filenamesout(1), &
        nvars, varnames)

ELSEIF (sibprogram .EQ. 'copy') THEN
   call copy_output( nfiles, &
        filenamesin, filenamesout)

ELSEIF (sibprogram .EQ. 'cropdist') THEN
   call cropdist_output( nfiles, &
        filenamesin, filenamesout(1), &
        cropfile, regionfile)

ELSEIF (sibprogram .EQ. 'gpptot') THEN
   call gpptot_output(nfiles, filenamesin, &
        isglobal, print_biome, print_pft)

ELSEIF (sibprogram .EQ. 'grid') THEN
   call grid_output(nfiles, &
        filenamesin, filenamesout, isglobal)

ELSEIF (sibprogram .EQ. 'neetot') THEN
   call neetot_output(nfiles, filenamesin, &
        isglobal, print_biome, print_pft)

ELSEIF (sibprogram .EQ. 'pullvar') THEN
   call pullvar_output(nfiles, &
        filenamesin, filenamesout, &
        nvars, varnames)

ELSEIF (sibprogram .EQ. 'regrid') THEN
   call regrid_output(nfiles, &
        filenamesin, filenamesout, &
        regridfile)

ELSEIF (sibprogram .EQ. 'rename') THEN
   call renamevar_output( &
        nfiles, filenamesin, &
        nvars, varnames, varnnames, &
        change_lnames, varlnames, &
        change_tnames, vartnames, &
        change_units, varunits)

ELSEIF (sibprogram .EQ. 'replaceresp') THEN
   call replaceresp_output( &
        nfiles, filenamesin, filenamesout, &
        bsibfile)

ELSEIF (sibprogram .EQ. 'unstack') THEN
   call unstack_output( &
        nfiles, filenamesin, bsibfile, csibfile, &
        filenamesout, nvars, varnames, use_aweight)

ELSEIF (sibprogram .EQ. 'vec2vec') THEN
   call vec2vec_output(nfiles, &
        filenamesin, filenamesout, &
        regridfile)

ELSE
   print*,''
   print*,'Unknown SiB4 Program To Call: ',sibprogram
   print*,'Please Add This Program In SiB4Post_Control.'
   print*,''
   RETURN
ENDIF

end subroutine sib4post_control
