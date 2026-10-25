!Program to convert filled text data into a netcdf file.
!
!kdhaynes, 08/17

program ameriflux_csv_to_netcdf

use netcdf
implicit none

character*120, parameter :: specfile='ameriflux_csv_to_netcdf.txt'
integer, dimension(12), parameter :: &
    dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
integer, parameter :: badval=-9999

integer :: hrperday, timeout
integer :: startyr, stopyr, numyr
integer :: numvars

character*120 :: trash
character*120 :: fdirin, fnamein
character*120 :: fdirout
character(len=10000) :: trashall

!time variables
integer, dimension(:), allocatable :: numdayperyr

!file variables
character(len=36), dimension(:), allocatable :: varsname
character(len=5) :: suffixout
character*6 :: sitename
character*150 :: filein, fileout
real*8, dimension(:), allocatable :: time

!netcdf variables
integer :: ncid, timedid
integer :: timeid
integer, dimension(:), allocatable :: varsid
integer :: neecref, neevref, leref, shref
integer :: neecqcref, neevqcref, leqcref, shqcref
integer :: gppref, recoref
integer :: gppdtref, recodtref
integer :: gppntref, recontref

!data variables
integer :: numt, numtref, numttemp, tdays
real*8 :: timetemp
real, dimension(:), allocatable :: varstemp
real, dimension(:,:), allocatable :: varssave

!time-mean data variables
integer :: numtmean, newnumt
real*8 :: delt, startt
real*8, dimension(:), allocatable :: timemean
integer, dimension(:), allocatable :: varsanum
real, dimension(:,:), allocatable :: varsmean
real, dimension(:), allocatable :: varsout

!misc variables
integer :: count, countnew, counttmean
integer*4 :: yr, mon, day, hr, t
integer :: v, nlen
integer :: indx, sloop1, sloop2, tol
character(len=2) :: shortyrstart, shortyrstop
character(len=3) :: shortsite
character(len=4) :: ayrstart, ayrstop

!----------------------------------------------------------
!----------------
!Read the spec file
open (unit=11,file=trim(specfile),form='formatted')
read(11,*) startyr
read(11,*) stopyr
numyr=stopyr-startyr+1

read(11,('(i4)')) hrperday
read(11,('(i2)')) timeout
read(11,('(a6)')) sitename
read(11,*) trash
read(11,*) fdirin
read(11,*) trash
read(11,*) fnamein
read(11,*) trash
read(11,*) fdirout
close(11)


!---------------------
!Create references
write(ayrstart,'(i4)') startyr
write(ayrstop,'(i4)') stopyr
call to_lower(sitename(4:6), shortsite)
shortyrstart = ayrstart(3:4)
shortyrstop = ayrstop(3:4)

!Find the number of variables
indx = index(fdirin,'xxxxxx')
if (indx .ge. 1) fdirin(indx:indx+5) = sitename
indx = index(fdirin,'year')
if (indx .ge. 1) fdirin(indx:indx+3) = ayrstart
indx = index(fdirin,'year')
if (indx .ge. 1) fdirin(indx:indx+3) = ayrstop

indx = index(fnamein,'xxxxxx')
if (indx .ge. 1) fnamein(indx:indx+5) = sitename
indx = index(fnamein,'year')
if (indx .ge. 1) fnamein(indx:indx+3) = ayrstart
indx = index(fnamein,'year')
if (indx .ge. 1) fnamein(indx:indx+3) = ayrstop

filein = trim(fdirin) // trim(fnamein)

print('(a)'),'Opening the file: '
print('(2a)'),'  ',trim(filein)
open(unit=11,file=trim(filein),form='formatted')
read(11,('(a)')) trashall
read(11,('(a)')) trashall
read(11,('(a)')) trashall
nlen=len(trim(trashall))
numvars=1
do v=1,nlen
   if (trashall(v:v) .eq. ',') then
      numvars=numvars+1
   endif
enddo
print('(a,i6)'),'   Number of Variables: ',numvars
close(11)

!----------------
!Allocate arrays
allocate(numdayperyr(numyr))
do yr=startyr, startyr+numyr-1
    if (mod(yr,4) .eq. 0) then
        numdayperyr(yr-startyr+1)=366
    else
        numdayperyr(yr-startyr+1)=365
    endif
enddo
numt=sum(numdayperyr)*hrperday

allocate(time(numt))
allocate(varsname(numvars),varsid(numvars))
allocate(varstemp(numvars))
allocate(varssave(numvars,numt))

!---------------------
!Read the input file
!print*,'Opening the file: ',trim(filein)
open(unit=11,file=trim(filein),form='formatted')
read(11,*) trashall
read(11,*) trashall
read(11,*) varsname
!print*,'Variables: ',varsname

neecref=0
neevref=0
neecqcref=0
neevqcref=0
gppref=0
recoref=0
gppdtref=0
gppntref=0
recodtref=0
recontref=0
leref=0
shref=0
leqcref=0
shqcref=0
do v=1, numvars
   if (trim(varsname(v)) .eq. 'LE') varsname(v) = 'LH'

   if (trim(varsname(v)) .eq. 'NEE_CUT_REF') neecref=v
   if (trim(varsname(v)) .eq. 'NEE_VUT_REF') neevref=v
   if (trim(varsname(v)) .eq. 'NEE_CUT_REF_QC') neecqcref=v
   if (trim(varsname(v)) .eq. 'NEE_VUT_REF_QC') neevqcref=v

   if (trim(varsname(v)) .eq. 'GPP_DT_VUT_REF') gppdtref=v
   if (trim(varsname(v)) .eq. 'GPP_NT_VUT_REF') gppntref=v
   if (trim(varsname(v)) .eq. 'RECO_DT_VUT_REF') recodtref=v
   if (trim(varsname(v)) .eq. 'RECO_NT_VUT_REF') recontref=v

   if (trim(varsname(v)) .eq. 'GPP_f') gppref=v
   if (trim(varsname(v)) .eq. 'Reco') recoref=v

   if (trim(varsname(v)) .eq. 'LE_F_MDS') leref=v
   if (trim(varsname(v)) .eq. 'LE_F_MDS_QC') leqcref=v
   if (trim(varsname(v)) .eq. 'H_F_MDS') shref=v
   if (trim(varsname(v)) .eq. 'H_F_MDS_QC') shqcref=v
enddo
!print*,'NEE References: ',neecref, neevref, neecqcref, neevqcref
!print*,'GPP/RECO References: ',gppdtref,gppntref,recodtref,recontref
!print*,'LE/SH References: ',leref,shref,leqcref,shqcref

count=1
do yr=startyr, stopyr
   numtref=yr-startyr+1
   numttemp=numdayperyr(numtref)*hrperday
   delt=1./(hrperday*numdayperyr(yr-startyr+1))
   startt=yr + delt/2.
   do day=1, numdayperyr(yr-startyr+1)  !numttemp
      do hr=1,48  !hrperday
         read(11,*) varstemp
         varssave(:,count) = varstemp(:)

         !!!FIX GPP TO BE POSITIVE ONLY!!!
         IF (gppref .gt. 0) then
            IF ((varssave(gppref,count) .gt. badval) .and. &
                (varssave(gppref,count) .lt. 0.)) THEN
                IF (varssave(recoref,count) .gt. badval) THEN
                    varssave(recoref,count) = varssave(recoref,count) + &
                                        abs(varssave(gppref,count))
                ENDIF
                varssave(gppref,count) = 0.
             ENDIF
         ENDIF

         IF (gppdtref .gt. 0) then
            IF ((varssave(gppdtref,count) .gt. badval) .and. &
                (varssave(gppdtref,count) .lt. 0.)) THEN
                IF (varssave(recodtref,count) .gt. badval) THEN
                    varssave(recodtref,count) = varssave(recodtref,count) + &
                                        abs(varssave(gppdtref,count))
                ENDIF
                varssave(gppdtref,count) = 0.
             ENDIF
         ENDIF

         IF (gppntref .gt. 0) then
            IF ((varssave(gppntref,count) .gt. badval) .and. &
                (varssave(gppntref,count) .lt. 0.)) THEN
                IF (varssave(recontref,count) .gt. badval) THEN
                    varssave(recontref,count) = varssave(recontref,count) + &
                                        abs(varssave(gppntref,count))
                ENDIF
                varssave(gppntref,count) = 0.
             ENDIF
         ENDIF


         !!!REMOVE BAD NEE!!!
         IF (neecref .gt. 0) then
            IF (varssave(neecqcref,count) .ge. 3) &
                 varssave(neecref,count) = badval
         ENDIF
         IF (neevref .gt. 0) then
            IF (varssave(neevqcref,count) .ge. 3) &
                 varssave(neevref,count) = badval
         ENDIF

         !!!REMOVE BAD LE!!!
         IF (leref .gt. 0) then
            IF ((varssave(leqcref,count) .ge. 3) .or. &
                (varssave(leqcref,count) .lt. 0.)) &
                 varssave(leref,count) = badval
         ENDIF

         !!!REMOVE BAD SH!!!
         IF (shref .gt. 0) then
            IF ((varssave(shqcref,count) .ge. 3) .or. &
                (varssave(shqcref,count) .lt. 0.)) &
                 varssave(shref,count) = badval
         ENDIF

         if (count .gt. 1) then
             time(count) = time(count-1) + delt
         elseif (count .eq. 1) then
             time(count) = startt
         endif
         count=count+1
      enddo
   enddo  !hr
enddo  !yr
close(11)

!----------------------------------------------------
!Set choices
sloop1 = timeout
if (sloop1 .eq. 5) sloop1 = 1
sloop2 = timeout
if (sloop2 .eq. 5) sloop2 = 4

!Calculate the output
do tol = sloop1, sloop2
   if (tol == 0) then
      if (hrperday .eq. 48.) then
          numtmean=1
      else
          print*,'Cannot Output Half-Hourly Data'
          print*,'Stopping.'
          stop
      endif
   elseif (tol == 1) then
      if (hrperday .eq. 24.) then
         numtmean=1
      elseif (hrperday .eq. 48.) then
         numtmean=2
      else
         print*,'Unexpected Data Timestep'
         print*,'  Hr_Per_Day: ',hrperday
         print*,'Stopping.'
         stop
      endif
   elseif (tol == 2) then
      numtmean=hrperday
   elseif (tol == 3) then
      numtmean=hrperday*7
   endif

   if (tol < 4) then
       newnumt=numt/numtmean
   else
       newnumt=numyr*12.
   endif

   allocate(timemean(newnumt))
   allocate(varsanum(numvars))
   allocate(varsmean(numvars,newnumt))

   timetemp = 0.
   timemean(:) = 0.
   varstemp(:) = 0.
   varsanum(:) = 0.
   varsmean(:,:) = 0.

   count=1
   countnew=1
   counttmean=0
   do yr=startyr,startyr+numyr-1
      do mon=1,12
         if (tol .eq. 4) numtmean=dayspermon(mon)*hrperday

         tdays=dayspermon(mon)
         if ((mod(yr,4) .eq. 0) .and. &
             (mon .eq. 2)) then
              tdays=tdays+1
         endif
         do day=1,tdays
            do hr=1,hrperday

               timetemp = timetemp + time(count)
               do v=1,numvars
                  if (varssave(v,count) > badval) then
                      varsanum(v) = varsanum(v) + 1
                      varstemp(v) = varstemp(v) + varssave(v,count)
                  endif
               enddo

               count=count + 1
               counttmean=counttmean + 1

               if (counttmean .eq. numtmean) then
                  timemean(countnew) = timetemp/numtmean
                  do v=1,numvars
                     if (varsanum(v) > 0) then
                         varsmean(v,countnew) = varstemp(v)/varsanum(v)
                     else
                         varsmean(v,countnew) = badval
                     endif
                  enddo

                  countnew=countnew+1
                  counttmean=0
                  timetemp=0.
                  varstemp(:) = 0.
                  varsanum(:) = 0
                endif !counttmean == numtmean
             enddo  !hr
           enddo !day
        enddo !mon
    enddo !yr

!----------------------------------------------------
   !Write out the file
   if (tol == 0) then
      write(suffixout,'(a)') '_f.nc'
   elseif (tol == 1) then
      write(suffixout,'(a)') '_h.nc'
   elseif (tol == 2) then
      write(suffixout,'(a)') '_d.nc'
   elseif (tol == 3) then
      write(suffixout,'(a)') '_w.nc'
   elseif (tol == 4) then
      write(suffixout,'(a)') '_m.nc'
   else
      print*,'Invalid Time For Output.  Stopping'
      stop
   endif

   fileout = trim(fdirout) // sitename // &
       '/' // shortsite // '_' // &
       shortyrstart // shortyrstop // trim(suffixout)

   if (tol .eq. sloop1) print*,''
   print('(a)'),'Writing out the file: '
   print('(2a)'),'  ',trim(fileout)
   if (tol .eq. sloop2) print*

   call check ( nf90_create(trim(fileout), nf90_clobber, ncid) )
   call check ( nf90_def_dim( ncid, 'numt', newnumt, timedid ) )
   call check ( nf90_def_var( ncid, 'time', nf90_double,timedid,timeid ) )
   do v=1,numvars
      call check ( nf90_def_var( ncid, trim(varsname(v)), nf90_float,timedid,varsid(v)))
      !print*,'Defined variable: ',varsname(v)
   enddo
   call check ( nf90_enddef( ncid ) )

   call check ( nf90_put_var( ncid, timeid, timemean ) )

   allocate(varsout(newnumt))
   do v=1,numvars
      !do t=1,newnumt
      !   if ((timemean(t) .ge. 2011.) .and. &
      !       (timemean(t) .lt. 2012.)) then
      !        varsout(t) = varsmean(v,t)
      !   else
      !        varsout(t) = badval
      !   endif
      !enddo
      varsout(:) = varsmean(v,:)
      call check ( nf90_put_var( ncid, varsid(v), varsout ) )
   enddo

   call check ( nf90_close(ncid) )

   deallocate(timemean)
   deallocate(varsanum)
   deallocate(varsmean)
   deallocate(varsout)

enddo !tol loop

end program ameriflux_csv_to_netcdf

!===================================
! Subroutine to check netcdf routines
! kdcorbin, 02/11
 
subroutine check(status)

use netcdf
use typeSizes
implicit none

integer, intent ( in ) :: status
 
     if (status /= nf90_noerr) then
        !print *, trim(nf90_strerror(status))
        stop "Error with netcdf.  Stopped."
      end if
end subroutine check   


!==========================================
! Function to create lower case letters
subroutine to_lower(strIn, strOut)

implicit none

character(len=*), intent(in) :: strIn
character(len=len(strIn)) :: strOut
integer :: i,j

do i= 1, len(strIn)
   j = iachar(strIn(i:i))
   if (j >= iachar("A") .and. j <= iachar("Z")) then
      strOut(i:i) = achar(iachar(strIn(i:i))+32)
   else
      strOut(i:i) = strIn(i:i)
   endif
enddo

end subroutine to_lower
