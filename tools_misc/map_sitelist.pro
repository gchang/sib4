PRO MAP_SITELIST, psFile=psFile, USA=USA, ALASKA=ALASKA,  $
    CANADA=CANADA, SA=SA, TROPICS=TROPICS, $
    EUR=EUR, ASIA=ASIA, AFRICA=AFRICA, AUS=AUS

;;;Specify the file information
filecsv ='map_sitelist.csv'

xsize=1.6
xthick=2.0

csize=1.6
cthick=2.0

ccolor=240
clinethick=0.7
glinethick=1.2

;;;Read the site list
openr,lun,filecsv,/get_lun

var_s = ''
fchar = '#'
nheader=0L
while (fchar eq '#') do begin
   nheader += 1
   readf, lun, var_s
   fchar = strmid(var_s,0,1)
endwhile

varname = strtrim(strsplit(var_s,',',/extract),2)
ncol = n_elements(varname)
nrow = file_lines(filecsv)-1-nheader

lat=fltarr(nrow)
lon=fltarr(nrow)
elat=fltarr(nrow)
elon=fltarr(nrow)
site=strarr(nrow)
sitec=strarr(nrow)
sites=strarr(nrow)

tlat = 0.
tlon = 0.
tname = ''
tlate = 0.
tlone = 0.
for i=0,nrow-1 do begin
    readf,lun,tlat,tlon,tlate,tlone,tname

    ntchar=0L
    fchar=strmid(tname,ntchar,1)
    while ((fchar eq ',') or (fchar eq ' ')) do begin
          ntchar++
          fchar=strmid(tname,ntchar,1)
    endwhile

    lat(i) = tlat
    lon(i) = tlon
    elat(i) = tlate
    elon(i) = tlone
    site(i) = strmid(tname,ntchar,6)
    sitec(i) = strmid(tname,ntchar,2)
    sites(i) = strmid(tname,ntchar+3,3)
    ;print,format=('(a,i4,2f8.2,4a)'), $
    ;      '  ',i,tlat,tlon,'  ',sitec(i),'  ',sites(i)
endfor
free_lun, lun

;;;Check the countries
global=0
usa=1
for i=0,nrow-1 do begin
    if ((sitec(i) ne 'US') and (sitec(i) ne 'CA')) then begin
        usa=0
        global=1
    endif
endfor

;;;Plot the tower locations
IF (N_ELEMENTS(psFile) NE 0) THEN BEGIN
   set_display,psFile=psFile,/wide
ENDIF ELSE set_display

!x.thick=1.
!y.thick=1.
!z.thick=1.
!p.thick=1.
!p.charsize=1.
!p.charthick=2.


maxlat=70.
minlat=-60.
maxlon=160.
minlon=-140.
pname=''

IF (usa eq 1) then begin
   maxlat=49.5  
   minlat=22    
   maxlon=-58   
   minlon=-130  

   lons=[-135,-125,-115,-105,-95,-85,-75,-65,-55]
   lonnames=['',' -125',' -115',' -105',' -95',' -85',' -75',' -65','']
   lonlev=minlat

   lats=[20,24,28,32,36,40,44,48,52]
   latnames=['',' 24',' 28',' 32',' 36',' 40',' 44',' 48',' 52']

   lonlev=lats(1)-.8
   latlev=lons(size(lons,/n_elements)-2)

   linestyle=1
ENDIF ELSE $
IF (N_ELEMENTS(CANADA) NE 0) THEN BEGIN
   maxlat=60  ;68
   minlat=49
   maxlon=-58
   minlon=-141

   lons=[-140,-130,-120,-110,-100,-90,-80,-70,-60,-50]
   lonnames=['','-130','-120','-110','-100','-90','-80','-70','-60','']

   lats=[48,50,52,54,56,58,60]
   latnames=['','50','52','54','56','58','']

   lonlev=lats(size(lats,/n_elements)-1)-0.4
   latlev=lons(0)

ENDIF ELSE $
IF (N_ELEMENTS(ALASKA) NE 0) THEN BEGIN
   maxlat=73
   minlat=54
   maxlon=-141
   minlon=-170

   lons=[-170,-166,-162,-158,-154,-150,-146,-142,-138]
   lonnames=['','-166','-162','-158','-154','-150','-146','-142','']

   lats=[54,56,58,60,62,64,66,68,70,72,74]
   latnames=['','56','58','60','62','64','66','68','70','72','']

   lonlev=lats(1)-0.4
   latlev=lons(size(lons,/n_elements)-3)

ENDIF ELSE $
IF (N_ELEMENTS(SA) NE 0) THEN BEGIN
   maxlat=20
   minlat=-60
   minlon=-90
   maxlon=-30

   lons=[-90,-80,-70,-60,-50,-40,-30,-20]
   lonnames=['','-80','-70','-60','-50','-40','-30','']

   lats=[-60,-50,-40,-30,-20,-10,0,10,20]
   latnames=['','-50','-40','-30','-20','-10','0','10','']

   lonlev=lats(0)+0.4
   latlev=lons(size(lons,/n_elements)-3)

ENDIF ELSE $
IF (N_ELEMENTS(EUR) NE 0) THEN BEGIN
  maxlat=60
  minlat=30
  maxlon=60
  minlon=-15
ENDIF ELSE $
IF (N_ELEMENTS(ASIA) NE 0) THEN BEGIN
  maxlat=72
  minlat=-10
  maxlon=180
  minlon=90

   lons=[90,100,110,120,130,140,150,160,170,180]
   lonnames=['','100','110','120','130','140','150','160','170','180']

   lats=[-10,0,10,20,30,40,50,60,70,80]
   latnames=['-10','0','10','20','30','40','50','60','70']

   lonlev=lats(size(lats,/n_elements)-2)-0.4
   latlev=lons(size(lons,/n_elements)-2)

ENDIF ELSE $
IF (N_ELEMENTS(AFRICA) NE 0) THEN BEGIN
   maxlat=8
   minlat=-35
   maxlon=56
   minlon=-20
ENDIF ELSE $
IF (N_ELEMENTS(AUS) NE 0) THEN BEGIN
   maxlat=-10
   minlat=-48
   maxlon=100
   minlon=180
ENDIF ELSE $
IF (N_ELEMENTS(TROPICS) NE 0) THEN BEGIN
   maxlat=50
   minlat=-40
   maxlon=180
   minlon=-90
   csize=1.
ENDIF ELSE BEGIN
   pname='x'

   lons=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
   lonnames=['','-150','-120','-90','-60','-30','0', $
             '30','60','90','120','150','']
   lonlev=0

   ;lats=[-90,-60,-30,0,30,60,90]
   ;latnames=['','-60','-30','0','30','60','']
   lats=[-60,-30,0,30,60,90]
   latnames=['-60','-30','0','30','60','']
   latlev=0

   linestyle=0
ENDELSE

IF (global eq 1) THEN BEGIN
   MAP_SET,limit=[minlat,minlon,maxlat,maxlon], /noborder, $
       /robinson, e_horizon={fill:1,color:90}, /isotropic
      ;/mollweide is another option
   xyouts,0,maxlat+5,'SiB4 Sites',size=4.,align=0.5

ENDIF ELSE BEGIN
   midlon=-95
   MAP_SET,limit=[minlat,minlon,maxlat,maxlon], /noborder, $
      /mercator,/isotropic,e_horizon={fill:1,color:90}
   xyouts,midlon,maxlat+2,'Sites',size=3.,align=0.5
ENDELSE

MAP_CONTINENTS, color=255, /fill_continents
MAP_CONTINENTS, color=2, /continents

IF (USA EQ 1) THEN BEGIN
   MAP_CONTINENTS, /USA, mlinethick=mlinethick
ENDIF ELSE BEGIN
   MAP_GRID,label=1,lons=lons,lonnames=lonnames, $
      lats=lats,latnames=latnames, $
      glinestyle=linestyle,glinethick=glinethick,charsize=csize, $
      latalign=0.,lonalign=1.,lonlab=lonlev, latlab=latlev
ENDELSE

offlat=0.3
FOR i=0,nrow-1 DO BEGIN
    XYOUTS,lon(i),lat(i)-offlat,'X', $
          align=0.5, color=ccolor, $
          charsize=xsize, charthick=xthick

    ;XYOUTS,elon(i),elat(i)-offlat,sites(i), $
    ;      color=ccolor,align=0.5, $
    ;      charsize=csize,charthick=cthick
ENDFOR

IF (N_ELEMENTS(psFile) NE 0) THEN BEGIN
   DEVICE,/close
   SET_PLOT,'X'
ENDIF

END
