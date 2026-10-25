!---------------------------------------------
program renamevar_sibgrid
!---------------------------------------------
!Program to rename specific variables
!  in a netcdf file.
!
!Notes:
!   This changes the original file!!
!
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make renamevar
!
!kdhaynes, 10/17

implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1
integer, parameter :: nvars=1
logical, parameter :: &
    change_lnames = .true., &
    change_tnames = .true., &
    change_units  = .true.

character(len=256), dimension(nfiles) :: &
   filenamesin
character(len=20), dimension(nvars) :: &
   varnames, varnnames
character(len=45), dimension(nvars) :: &
   varlnames, vartnames, varunits

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_200001.lu.qp3.nc'

varnames(1) = 'rstfac2'
varnnames(1) = 'moisture_stress'
varlnames(1) = 'Moisture Stress'
vartnames(1) = ''
varunits(1) = '1'

!------------------------------------------------------

call renamevar_output(nfiles, filenamesin, &
     nvars, varnames, varnnames, &
     change_lnames, varlnames, &
     change_tnames, vartnames, &
     change_units, varunits)

print*,''
print*,'Finished Changing Variable Names.'
print*,''

end program renamevar_sibgrid

