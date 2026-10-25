!Program to report annual GPP from Fluxnet data.
!  - Assumes annual (yy) files.
!
!kdhaynes, 05/16

program fluxnet_csv_yygpp

use netcdf
implicit none

character*120, parameter :: specfile='fluxnet_csv_yygpp.txt'
integer, parameter :: badval=-9999

integer :: startyr, stopyr
integer :: numvars, numyrs

integer, parameter :: slen=4800
character(len=slen) :: trash
character*260 :: filename

!file variables
character(len=36), dimension(:), allocatable :: varsname
real, dimension(:,:), allocatable :: varstemp
integer :: neecref,neevref
integer :: gppdtref,gppntref,recodtref,recontref
integer :: gppcdtref,gppcntref,recocdtref,recocntref

!misc variables
integer :: v, vlast, yr, yrref
real :: neetemp, gpptemp, recotemp
real :: neetot, gpptot, recotot
integer :: gppyrs, recoyrs, neeyrs

!----------------------------------------------------------
!----------------
!Read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,*) trash
read(11,*) filename
close(11)

!----------------
!Read the input file
v = index(filename,'/',back=.true.)
vlast = index(filename,'.csv')
print*,''
print('(a,a)'),'Opening the file: ',filename(v+1:vlast+3)
open(unit=11,file=trim(filename),form='formatted')
read(11,'(a)') trash
close(11)

v = index(filename,'YY_')
read(filename(v+3:v+6),'(I4)') startyr
read(filename(v+8:v+11),'(I4)') stopyr
numyrs=stopyr-startyr+1
print('(a,i4,a,i4,a,i4)'),'  Num Years: ',numyrs, '   ', startyr, '-', stopyr

numvars=0
do v=1,slen
  if (trash(v:v) .eq. ',') numvars=numvars+1
enddo
print('(a,i5)'),'  Num Vars: ',numvars

allocate(varstemp(numvars,numyrs))
allocate(varsname(numvars))
open(unit=11,file=trim(filename),form='formatted')
read(11,*) varsname
do yr=startyr,stopyr
  yrref=yr-startyr+1
  read(11,*) varstemp(:,yrref)
enddo
close(11)

!Find appropriate variables
neecref=0
neevref=0
gppdtref=0
gppntref=0
gppcdtref=0
gppcntref=0
recodtref=0
recontref=0
recocdtref=0
recocntref=0

do v=1, numvars
   if (trim(varsname(v)) .eq. 'NEE_CUT_REF') neecref=v
   if (trim(varsname(v)) .eq. 'NEE_VUT_REF') neevref=v
   if (trim(varsname(v)) .eq. 'GPP_DT_VUT_REF') gppdtref=v
   if (trim(varsname(v)) .eq. 'GPP_NT_VUT_REF') gppntref=v
   if (trim(varsname(v)) .eq. 'GPP_DT_CUT_REF') gppcdtref=v
   if (trim(varsname(v)) .eq. 'GPP_NT_CUT_REF') gppcntref=v
   if (trim(varsname(v)) .eq. 'RECO_DT_VUT_REF') recodtref=v
   if (trim(varsname(v)) .eq. 'RECO_NT_VUT_REF') recontref=v
   if (trim(varsname(v)) .eq. 'RECO_DT_CUT_REF') recocdtref=v
   if (trim(varsname(v)) .eq. 'RECO_NT_CUT_REF') recocntref=v
enddo
!print*,'NEE References: ',neecref, neevref
!print*,'GPP/RECO References: ',gppdtref,gppntref,recodtref,recontref
if ((gppcdtref .eq. 0) .or. (gppcntref .eq. 0)) then
    gppcdtref=gppdtref
    gppcntref=gppntref
endif
if ((recocdtref .eq. 0) .or. (recocntref .eq. 0)) then
    recocdtref=recodtref
    recocntref=recontref
endif
if (neecref .eq. 0) neecref=neevref

gpptot = 0.
recotot = 0.
neetot = 0.
gppyrs = 0
recoyrs = 0
neeyrs = 0

print*,''
print*,'Year     GPP        RE       NEE    (Mg C/ha)'
do yr=startyr,stopyr
   yrref=yr-startyr+1
   gpptemp = (varstemp(gppdtref,yrref)+varstemp(gppntref,yrref) + &
              varstemp(gppcdtref,yrref)+varstemp(gppcntref,yrref))/400.
   recotemp = (varstemp(recodtref,yrref)+varstemp(recontref,yrref) + &
               varstemp(recocdtref,yrref)+varstemp(recocntref,yrref))/400.
   neetemp = (varstemp(neecref,yrref)+varstemp(neevref,yrref))/200.

   print('(i5,3f10.3)'), yr, gpptemp, recotemp, neetemp
   if (gpptemp .gt. (badval/100.)) then
       gpptot = gpptot + gpptemp
       gppyrs = gppyrs+1
   endif

   if (recotemp .gt. (badval/100.)) then
       recotot = recotot + recotemp
       recoyrs = recoyrs+1
   endif

   if (neetemp .gt. (badval/100.)) then
       neetot = neetot + neetemp
       neeyrs = neeyrs + 1
   endif
enddo  !yr

print*,''
print('(a5,3f10.3)'), ' MEAN', gpptot/gppyrs, recotot/recoyrs, neetot/neeyrs
print*,''

end program fluxnet_csv_yygpp

