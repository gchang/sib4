program sib4post

use sibinfo_module, only: fa, f2a
implicit none

!local variables
integer :: n, myyear
character(len=20) :: sibprogramin
character(len=20) :: sibprogram
character(len=180):: specfile
character(len=60) :: finishtag

myyear=0
sibprogramin=''
sibprogram=''
specfile=''
finishtag=''
!-----------------------------

n = command_argument_count()

IF (n == 0) THEN
   print(fa),''
   print(fa),'Please Specify A Post-Processing Routine.'
   print(fa),''
   STOP
ELSEIF (n == 1) THEN
    call getarg(1, sibprogramin)
ELSEIF (n == 2) THEN
    call getarg(1, sibprogramin)
    call getarg(2, specfile)
    if (len(trim(specfile)) .eq. 4) then
       read (specfile,'(I4)') myyear
       specfile = ''
    endif   
ELSE
   print(fa),'Wrong Number Of Arguments.'
   STOP
ENDIF

IF (trim(sibprogramin) .EQ. 'addresp') THEN
   sibprogram = 'addresp'
   IF (trim(specfile) .EQ. '') specfile='addresp.txt'
   finishtag = 'Finished Adding Balanced Respiration To Files.'

ELSEIF (trim(sibprogramin) .EQ. 'balance') THEN
   sibprogram = 'balance'
   IF (trim(specfile) .EQ. '') specfile='balance.txt'
   finishtag = 'Finished Balancing Files.'

ELSEIF (trim(sibprogramin) .EQ. 'checkbal') THEN
   sibprogram = 'checkbal'
   IF (trim(specfile) .EQ. '') specfile='checkbal.txt'
   finishtag = 'Finished Checking Balance.'

ELSEIF (trim(sibprogramin) .EQ. 'combine') THEN
   sibprogram = 'combine'
   IF (trim(specfile) .EQ. '') specfile='combine.txt'
   finishtag = 'Finished Combining Files.'

ELSEIF (trim(sibprogramin) .EQ. 'conjoin') THEN
   sibprogram = 'conjoin'
   IF (trim(specfile) .EQ. '') specfile='conjoin.txt'
   finishtag = 'Finished Conjoining Files.'

ELSEIF (trim(sibprogramin) .EQ. 'copy') THEN
   sibprogram = 'copy'
   IF (trim(specfile) .EQ. '') specfile='copy.txt'
   finishtag = 'Finished Copying Files.'

ELSEIF (trim(sibprogramin) .EQ. 'cropdist') THEN
  sibprogram = 'cropdist'
  IF (trim(specfile) .EQ. '') specfile = 'cropdist.txt'
  finishtag = 'Finished Disbributing Crop Harvest.'

ELSEIF (trim(sibprogramin) .EQ. 'disjoin') THEN
  sibprogram = 'disjoin'
  IF (trim(specfile) .EQ. '') specfile = 'disjoin.txt'
  finishtag = 'Finished Disjoining Files.'

ELSEIF (trim(sibprogramin) .EQ. 'gpptot') THEN
   sibprogram = 'gpptot'
   IF (trim(specfile) .EQ. '') specfile='gpptot.txt'
   finishtag = 'Finished Calculating Total GPP.'

ELSEIF (trim(sibprogramin) .EQ. 'grid') THEN
   sibprogram = 'grid'
   IF (trim(specfile) .EQ. '') specfile='grid.txt'
   finishtag = 'Finished Gridding Files.'

ELSEIF (trim(sibprogramin) .EQ. 'meanclim') THEN
   sibprogram = 'meanclim'
   IF (trim(specfile) .EQ. '') specfile='meanclim.txt'
   finishtag = 'Finished Creating Climatological Files.'

ELSEIF (trim(sibprogramin) .EQ. 'merge') THEN
   sibprogram = 'merge'
   IF (trim(specfile) .EQ. '') specfile='merge.txt'
   finishtag = 'Finished Merging Files'

ELSEIF (trim(sibprogramin) .EQ. 'neetot') THEN
   sibprogram = 'neetot'
   IF (trim(specfile) .EQ. '') specfile='neetot.txt'
   finishtag = 'Finished Calculating Total NEE.'

ELSEIF (trim(sibprogramin) .EQ. 'pullvar') THEN
   sibprogram = 'pullvar'
   IF (trim(specfile) .EQ. '') specfile='pullvar.txt'
   finishtag = 'Finished Pulling Variables.'

ELSEIF (trim(sibprogramin) .EQ. 'regrid') THEN
   sibprogram = 'regrid'
   IF (trim(specfile) .EQ. '') specfile='regrid.txt'
   finishtag = 'Finished Re-Gridding Files.'

ELSEIF (trim(sibprogramin) .EQ. 'renamevar') THEN
   sibprogram = 'rename'
   IF (trim(specfile) .EQ. '') specfile='renamevar.txt'
   finishtag = 'Finished Renaming Variables in Files.'

ELSEIF (trim(sibprogramin) .EQ. 'replaceresp') THEN
   sibprogram = 'replaceresp'
   IF (trim(specfile) .EQ. '') specfile='replaceresp.txt'
   finishtag = 'Finished Replacing Respiration.'

ELSEIF (trim(sibprogramin) .EQ. 'spmerge') THEN
   sibprogram = 'spmerge'
   IF (trim(specfile) .EQ. '') specfile='spmerge.txt'
   finishtag = 'Finished SPMerging Files'
   
ELSEIF (trim(sibprogramin) .EQ. 'unstack') THEN
   sibprogram = 'unstack'
   IF (trim(specfile) .EQ. '') specfile='unstack.txt'
   finishtag = 'Finished Unstacking Files.'

ELSEIF (trim(sibprogramin) .EQ. 'vec2vec') THEN
   sibprogram = 'vec2vec'
   IF (trim(specfile) .EQ. '') specfile='vec2vec.txt'
   finishtag = 'Finished Creating New Vector Files.'

ELSE
   print(fa),''
   print(f2a),'Unknown Program: ',trim(sibprogramin)
   print(fa),'Please Add This Program To SiB4Post List.'
   print(fa),''
   STOP
ENDIF

call sib4post_process(trim(sibprogram), trim(specfile), myyear)
print(fa),''
print(fa),finishtag
print(fa),''

end program sib4post






