!==================================================
subroutine renamevar_output( &
     nfiles, filenames,   &
     nvars, varnamesin, varnamesout, &
     change_lnames, longnames, &
     change_titles, titles, &
     change_units, units)
!==================================================

use netcdf
implicit none

!input variables
integer, intent(in) :: nfiles
character(len=256), dimension(nfiles), intent(in) :: filenames

integer, intent(in) :: nvars
character(len=20), dimension(nvars), intent(in) :: &
    varnamesin, varnamesout

logical, intent(in) :: &
    change_lnames, change_titles, change_units
character(len=45), dimension(nvars), intent(in) :: &
    longnames, titles, units

!file variables
integer :: stringloc, stringend
character(len=180) :: stringshort

!netcdf variables
integer :: status, ncid
integer :: varid

!local variables
integer :: f, var


!----------------------------------------
DO f=1,nfiles

  !Print file short name
  stringloc = index(filenames(f),'/',back=.true.)
  stringend = len_trim(filenames(f))
  stringshort = (filenames(f)(stringloc+1:stringend))
  print*,'   File: ',trim(stringshort)

  !Open file
  status = nf90_open(trim(filenames(f)),nf90_write,ncid)
  IF (status .NE. nf90_noerr) THEN
     print*,'Error Opening File: '
     print*,' ',trim(filenames(f))
     print*,' ',trim(nf90_strerror(status))
     print*,'Stopping.'
     STOP
  ENDIF

  status = nf90_redef(ncid)
  IF (status .NE. nf90_noerr) THEN
     print*,'Error Entering Define Mode.'
     print*,'  ',trim(nf90_strerror(status))
     print*,'Stopping.'
     STOP
  ENDIF

  !Change variable information
  do var = 1, nvars
     status = nf90_inq_varid(ncid,trim(varnamesin(var)),varid)

     IF (status .EQ. nf90_noerr) THEN

        !Rename variable
        status = nf90_rename_var(ncid,varid,trim(varnamesout(var)))
        !IF (status .ne. nf90_noerr) THEN
        !   print*,'Error Renaming Variable: ',trim(varnamesin(var))
        !   print*,'  ',trim(nf90_strerror(status))
        !   print*,'Stopping.'
        !   STOP
        !ENDIF

        !Change long_name
        IF (change_lnames) THEN
            status = nf90_put_att(ncid,varid, &
                      'long_name',trim(longnames(var)))
        ENDIF

        !Change title
        IF (change_titles) THEN
           IF (trim(titles(var)) .NE. '') THEN
               status = nf90_put_att(ncid,varid, &
                     'title',trim(titles(var)))
           ELSE
               status = nf90_del_att(ncid,varid,'title')
           ENDIF
        ENDIF

        !Change units
        IF (change_units) THEN
           status = nf90_put_att(ncid,varid, &
                    'units',trim(units(var)))
        ENDIF
     ENDIF
  enddo

  !Close file
  status = nf90_close(ncid)

enddo !f=1,nfiles

end subroutine renamevar_output
