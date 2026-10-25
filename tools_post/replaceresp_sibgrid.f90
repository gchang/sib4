!---------------------------------------------
program replaceresp_sibgrid
!---------------------------------------------
!Program to replace the original respiration
!   with balanced respiration.
!   - Multiplies the respiration by the
!     saved ratio calculated from balance_sib.f90.
!
!Notes :
!   This program requires netcdf.
!   The netcdf library/include directories  
!       are specified in the Makefile.
!
!To compile:
!>make replaceresp
!
!kdhaynes, 2018/02
!
implicit none

!------------------------------------------------------
!Enter In Filenames:

integer, parameter :: nfiles=1

character(len=256), dimension(nfiles) :: &
   filenamesin, filenamesout
character(len=256) :: ratiofile
logical :: file_exist

filenamesin(1) = &
   '/Users/kdhaynes/Output/global/monthly/qsib_201702.lu.qp3.nc'
filenamesout(1) = &
   '/Users/kdhaynes/Output/global/monthlybal/qsib_201702.lu.qp3.nc'
ratiofile = &
   '/Users/kdhaynes/Output/global/monthly/qbal_199801_201712.lu.qp3.nc'
!------------------------------------------------------

!...check the files
inquire(file=trim(filenamesin(1)),exist=file_exist)
if (.not. file_exist) then
   print*,'Missing File: '
   print*,'  ',trim(filenamesin(1))
   STOP
endif

inquire(file=trim(ratiofile),exist=file_exist)
if (.not. file_exist) then
   print*,'Missing File: '
   print*,'  ',trim(ratiofile)
   STOP
endif

!...process the files
call replaceresp_output(nfiles, filenamesin, &
     filenamesout, ratiofile)

print*,''
print*, 'Finished Replacing Respiration.'
print*,''

end program replaceresp_sibgrid

