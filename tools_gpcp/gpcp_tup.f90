!===================================
! Subroutine to get areal information
!    for regridding to a coarser grid.
!    (Upscaling...)
!
! kdhaynes, 10/17
 
subroutine regrid_upinfo( &
     nlonin, nlatin, &
     nlonout, nlatout, maxng, &
     lonin, latin, lonout, latout, &
     ngmin, refiin, refjin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin, areagin, areagout)

implicit none

!...parameters
logical, parameter :: print_area=.false.
real, parameter :: radius=6371000
real :: pi

!...input variables
integer, intent(in) :: nlonin, nlatin
integer, intent(in) :: nlonout, nlatout
integer, intent(in) :: maxng
real, dimension(nlonin), intent(in) :: lonin
real, dimension(nlatin), intent(in) :: latin
real, dimension(nlonout), intent(in) :: lonout
real, dimension(nlatout), intent(in) :: latout

integer, dimension(nlonin,nlatin), intent(inout) :: ngmin
integer, dimension(nlonin,nlatin,maxng), intent(inout) :: refiin, refjin
real*8, dimension(nlonin), intent(inout) :: arlonin
real*8, dimension(nlatin), intent(inout) :: arlatin
real*8, dimension(nlonout), intent(inout) :: arlonout
real*8, dimension(nlatout), intent(inout) :: arlatout
real*8, dimension(nlonin,nlatin,maxng), intent(inout) :: contribin
real*8, dimension(nlonin,nlatin), intent(inout) :: areagin
real*8, dimension(nlonout,nlatout), intent(inout) :: areagout

!...local variables
real :: deltalonin, deltalatin
real*8 :: areatotin
real, dimension(nlonin) :: loninE, loninW
real, dimension(nlatin) :: latinN, latinS

real :: deltalonout, deltalatout
real, dimension(nlonout+1) :: lonoutEe, lonoutWe
real, dimension(nlatout) :: latoutN, latoutS

real*8 :: latsr, latnr, lonwr, loner
real*8 :: londiff,sinlatdiff

!...misc variables
integer :: i,j,ii,jj
integer :: irefout, jrefout
real*8  :: ratio, temparea

!----------------------------------------
pi = acos(-1.0)
ngmin(:,:) = 0
refiin(:,:,:) = 0
refjin(:,:,:) = 0

!Calculate areal variables for original grid
deltalonin=(lonin(3)-lonin(2))/2.
deltalatin=(latin(3)-latin(2))/2.

do i=1,nlonin
   loninE(i)=lonin(i)+deltalonin
   loninW(i)=lonin(i)-deltalonin
   arlonin(i)=(loninE(i)-loninW(i))/360.
enddo
arlonin = arlonin*2*pi*radius*radius

do j=1,nlatin
   latinS(j) = latin(j) - deltalatin
   latinN(j) = latin(j) + deltalatin

   IF (latinS(j) .lt. -90.) THEN
      latinS(j) = -90.
   ENDIF

   IF (latinN(j) .gt. 90.) THEN
      latinN(j) = 90.
   ENDIF

   arlatin(j) = -sin(pi*latinS(j)/180.) + &
                 sin(pi*latinN(j)/180.)
enddo

areatotin=0.
do ii=1,nlonin
   do jj=1,nlatin
      latsr=latinS(jj)*pi/180.
      latnr=latinN(jj)*pi/180.
      lonwr=loninW(ii)*pi/180.
      loner=loninE(ii)*pi/180.
      londiff=loner-lonwr
      sinlatdiff=sin(latnr)-sin(latsr)
      areagin(ii,jj) = radius*radius*londiff*sinlatdiff
      areatotin = areatotin + areagin(ii,jj)
   enddo !jj
enddo !ii

!Calculate areal variables for new grid
deltalonout=(lonout(3)-lonout(2))/2.
deltalatout=(latout(3)-latout(2))/2.

do i=1,nlonout
   lonoutEe(i)=lonout(i)+deltalonout
   lonoutWe(i)=lonout(i)-deltalonout
   arlonout(i)=(lonoutEe(i)-lonoutWe(i))/360.
enddo
lonoutEe(nlonout+1)=lonout(nlonout)+3*deltalonout
lonoutWe(nlonout+1)=lonout(nlonout)+2*deltalonout
arlonout = arlonout*2*pi*radius*radius

do j=1,nlatout
   latoutS(j) = latout(j) - deltalatout
   latoutN(j) = latout(j) + deltalatout

   IF (latoutS(j) .lt. -90.) THEN
      latoutS(j) = -90.
   ENDIF

   IF (latoutN(j) .gt. 90.) THEN
      latoutN(j) = 90.
   ENDIF

   arlatout(j) = -sin(pi*latoutS(j)/180.) + &
                  sin(pi*latoutN(j)/180.)
enddo

do ii=1,nlonout+1
   do jj=1,nlatout
      latsr=latoutS(jj)*pi/180.
      latnr=latoutN(jj)*pi/180.
      lonwr=lonoutWe(ii)*pi/180.
      loner=lonoutEe(ii)*pi/180.
      londiff=loner-lonwr
      sinlatdiff=sin(latnr)-sin(latsr)

      IF (ii .LT. nlonout+1) THEN
         areagout(ii,jj) = radius*radius*londiff*sinlatdiff
      ENDIF
   enddo !jj
enddo !ii

!Area calculations
ngmin(:,:)=0
do i=1,nlonin
   do j=1,nlatin
       
      do ii=1,nlonout+1
         do jj=1,nlatout
             IF ((loninE(i) .GT. lonoutWe(ii)) .AND. &
                 (loninW(i) .LT. lonoutEe(ii)) .AND. &
                 (latinN(j) .GT. latoutS(jj))  .AND. &
                 (latinS(j) .LT. latoutN(jj))) THEN
                  latsr=MAX(latoutS(jj)*pi/180.,latinS(j)*pi/180.)
                  latnr=MIN(latoutN(jj)*pi/180.,latinN(j)*pi/180.)
                  lonwr=MAX(lonoutWe(ii)*pi/180.,loninW(i)*pi/180.)
                  loner=MIN(lonoutEe(ii)*pi/180.,loninE(i)*pi/180.)
                  londiff=loner-lonwr
                  sinlatdiff=sin(latnr)-sin(latsr)
                  temparea = radius*radius*londiff*sinlatdiff
                  IF (temparea .LT. 0.) THEN
                     print*,'Negative Area: ',temparea
                     print*,' (loninE,lonoutE,loninW,lonoutW): ', &
                         loninE(i),lonoutEe(ii),loninW(i),lonoutWe(ii),londiff
                     print*,' (latinS,latoutS,latinN,latoutN): ', &
                         latinS(j),latoutS(jj),latinN(j),latoutN(jj),sinlatdiff
                  ENDIF

                  ngmin(i,j) = ngmin(i,j) + 1
                  IF (ngmin(i,j) .GT. maxng) THEN
                      print*,'Too many original grid cell contributors: ', &
                              ngmin(i,j),i,j,lonin(i),latin(i)
                      stop
                  ENDIF
                  IF (ii .EQ. nlonout+1) THEN
                      irefout = 1
                  ELSE
                      irefout = ii
                  ENDIF
                  jrefout = jj

                  refiin(i,j,ngmin(i,j)) = irefout
                  refjin(i,j,ngmin(i,j)) = jrefout
                  contribin(i,j,ngmin(i,j)) = temparea

             ENDIF
            ENDDO !jj
        ENDDO !ii

   ENDDO !j
ENDDO !i      

!Rescale areas
ratio=areatotin/sum(areagout)
areagout=areagout*ratio
ratio=areatotin/sum(contribin)
contribin=contribin*ratio

!Check areas
if (print_area) then
   print('(a)'), '    Area should be: 0.50990436E+15 m2'
   print('(a)'), '    Area used for calculation:'
   print('(a,E15.8)'), '       Orig   = ',sum(areagin)
   print('(a,E15.8)'), '       New    = ',sum(areagout)
endif


end subroutine regrid_upinfo


!===================================
! Subroutine to output regridded data.
!
! kdhaynes, 01/2018
 
subroutine regrid_varup( &
     yrref, flag_mmpmon, &
     nlonin, nlatin, &
     nlonout, nlatout, &
     ntime, maxng, &
     lonout, latout, &
     ngmin, refiin, refjin, &
     arlonin, arlatin, arlonout, arlatout, &
     contribin, areagout, &
     datain, dataout)

use netcdf
implicit none

!input variables
integer, intent(in) :: yrref
logical, intent(in) :: flag_mmpmon
integer, intent(in) :: nlonin, nlatin
integer, intent(in) :: nlonout, nlatout
integer, intent(in) :: maxng, ntime
real, dimension(nlonout), intent(in) :: lonout
real, dimension(nlatout), intent(in) :: latout

integer, dimension(nlonin,nlatin), intent(in) :: ngmin
integer, dimension(nlonin,nlatin,maxng), intent(in) :: refiin, refjin
real*8, dimension(nlonin), intent(in) :: arlonin
real*8, dimension(nlatin), intent(in) :: arlatin
real*8, dimension(nlonout), intent(in) :: arlonout
real*8, dimension(nlatout), intent(in) :: arlatout
real*8, dimension(nlonin,nlatin,maxng), intent(in) :: contribin
real*8, dimension(nlonout,nlatout), intent(in) :: areagout

real, dimension(nlonin,nlatin,ntime), intent(in) :: datain
real, dimension(nlonout,nlatout,ntime), intent(inout) :: dataout

!data variables
real*8 :: ratio
real*8 :: sumvarin, sumvarout1, sumvarout2

!local variables
integer :: i,j,l,t
integer :: irefout, jrefout
integer :: monref
real :: myconvert

!parameters
logical, parameter :: print_sums=.true.
real*8, parameter :: areaEarth = 510072000000000.
character(len=3), dimension(12), parameter :: &
   monname = ['Jan','Feb','Mar','Apr','May','Jun', &
    'Jul','Aug','Sep','Oct','Nov','Dec']
integer, dimension(12), parameter :: &
   dayspermon = [31,28,31,30,31,30,31,31,30,31,30,31]

!-------------------------------------
!Process the data
monref = 1

do t=1,ntime
   do j=1,nlatin
      do i=1,nlonin
         do l=1,ngmin(i,j)
            irefout=refiin(i,j,l)
            jrefout=refjin(i,j,l)

            dataout(irefout,jrefout,t) = &
                 dataout(irefout,jrefout,t) + &
                 datain(i,j,t) * &
                 real(contribin(i,j,l)/areagout(irefout,jrefout))
          enddo !l=1,ngmin
       enddo !i=1,nlonin
    enddo !j=1,nlatin

    !calculate global totals
    sumvarin=0.0
    do j=1,nlatin
       do i=1,nlonin
          sumvarin = sumvarin + &
                 datain(i,j,t)*arlonin(i)*arlatin(j)
       enddo  !j=1,nlatin
    enddo !i=1,nlonin

    sumvarout1=0.0
    do j=1,nlatout
       do i=1,nlonout
          sumvarout1 = sumvarout1 + &
            dataout(i,j,t)*arlonout(i)*arlatout(j)
       enddo !i=1,nlonout
    enddo !j=1,nlatout

    !rescale
    if (sumvarout1 .ne. 0.) then
        ratio=sumvarin/sumvarout1
    else
        ratio=1.
    endif
    dataout(:,:,t) = dataout(:,:,t)*real(ratio)

    !recheck
    sumvarout2=0.0
    do j=1,nlatout
       do i=1,nlonout
          sumvarout2 = sumvarout2 + &
             (dataout(i,j,t))*arlonout(i)*arlatout(j)
      enddo  !i=1,nlonout
   enddo !j=1,nlatout

   if (print_sums) then
      print*,''
      print('(a,a,i5)'), &
         'Precipitation for ',monname(monref),yrref

      if (flag_mmpmon) then
         myconvert = 1.
      else
         myconvert = real(dayspermon(t))
      endif

      print*,'     Orig Max/Min Precip (mm/mon)  : ', &
             maxval(datain(:,:,t))*myconvert, &
             minval(datain(:,:,t))*myconvert
      print*,'     Regrid Max/Min Precip (mm/mon): ', &
             maxval(dataout(:,:,t))*myconvert, &
             minval(dataout(:,:,t))*myconvert
      print*,'     Global Totals (mm/mon)'
      print('(a,f15.8)'),'          In : ',sumvarin/areaEarth*myconvert
      print('(a,f15.8)'),'          Out: ',sumvarout2/areaEarth*myconvert
   endif

   monref = monref + 1

enddo !t=1,ntime


end subroutine regrid_varup

