;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Draw grid lines and optionally map labels for a regular map
;; This routine is developed since IDL v5, where IDL's procedure MAP_GRID
;; no longer puts the labels where we wanted. Therefore, we do it manually
;; in the routine
;;
;; plot:       The plot structure
;; global:     True (1) if the map covers the globe, 0 otherwise
;;
PRO GEO4_GridLabel_Globe, plot

@ez4_BasicParams.com

;; Quick global test
IF (plot.region NE 'Globe') THEN BEGIN
    EZ4_Alert, 'Error in GridLabel: Not a Global Plot!'
    RETURN
ENDIF

;; Specify grid interval
ny     = 6
nx     = 6
latDel = 30
lonDel = 60

;; Define device coords for the map corners
xy   = CONVERT_COORD([plot.limits[1], plot.limits[3]], $
                     [plot.limits[0], plot.limits[2]], $
                       /DATA, /TO_DEVICE)
xmin = xy[0,0]
xmax = xy[0,1]
xcen = MEAN([xmin,xmax])
ymin = xy[1,0]
ymax = xy[1,1]
ycen = MEAN([ymin,ymax])

;; Determine the y-coord for longitude labels and the x-coord for latitude
;; labels in device coordinates
IF (dopsPlot) THEN BEGIN
  xmin = xmin - 50
  ymin = ymin - 400
ENDIF ELSE BEGIN
  xmin = xmin - 2
  ymin = ymin - 12
  ycen = ycen - 12
ENDELSE

;; Set the position for the longitude labels
IF (plot.MapProj EQ 'ROB') THEN xmin=xcen
CASE plot.LGridPos OF
   'bot': yllev = ymin
   'cen': yllev = ycen
   'top': yllev = ymax
END 
yllev += plot.lGridYOff

;; Determine lons and lats for labels in map coordinates
lats      = [ -90, -60, -30, 0, 30, 60, 85 ]
latLabels = [ 'SP', '-60', '-30', 'EQ', '30', '60', 'NP' ]
IF ((plot.MapProj EQ 'MER') OR (plot.MapProj EQ 'MIL')) THEN BEGIN
    latLabels[0]='' 
    latLabels[6]=''
ENDIF

IF (plot.MapCenter EQ 180) THEN BEGIN
   lons      = [ 0, 60, 120, 180, -120, -60, -1 ]
   lonLabels = [ '0', '60 E', '120 E', '180', '120 W', '60 W', '0' ]
ENDIF ELSE BEGIN
   lons      = [ -180, -120, -60, 0, 60, 120, 180 ]
   lonLabels = [ '180', '120 W', '60 W', '0', '60 E', '120 E', '180' ]
   IF (plot.lGridPos EQ 'cen') THEN lonLabels(3)=''

   IF (plot.MapProj EQ 'MOL') THEN BEGIN
       nx = 8
       lons = [ -160, -120, -80, -40, 0, 40, 80, 120, 160 ]
       lonLabels = [ '160 W', '120 W', '80 W', '40 W','', $
                     '40 E', '80 E', '120 E', '160 E']
   ENDIF
ENDELSE

IF (plot.drawGrid EQ 1) THEN BEGIN
    MAP_GRID, COLOR=black, LATDEL=latDel, LONDEL=lonDel, $
            GLINETHICK=1, LONS=lons, LATS=lats

  IF (plot.LGridPos EQ 'cen') THEN BEGIN
     mylats=0
     mylons=0
     IF (plot.MapProj EQ 'LAM') THEN mylons=20
     IF (plot.MapProj EQ 'ORT') THEN BEGIN 
         mylons=-60
         lonLabels(3)='0'
     ENDIF
     MAP_GRID, COLOR=black, LATDEL=180, LONDEL=360, $
            LINESTYLE=0, LATS=mylats, LONS=mylons, GLINETHICK=2
  ENDIF

ENDIF

;; Put the labels using device coords
IF (dopsplot) THEN EZ4_SetFont, NameFont=psFont, /TrueType
lgsize=1.0

IF (plot.labelGrid EQ 1) THEN BEGIN
   lats = (CONVERT_COORD(0, lats, /DATA, /TO_DEVICE))[1,*]
   lons = (CONVERT_COORD(lons, 0, /DATA, /TO_DEVICE))[0,*]
   FOR i=0,ny DO XYOUTS, ALIGNMENT=1.0, xmin, lats[i], $
                      CHARSIZE=lgsize, latLabels[i], /DEVICE
   FOR i=0,nx DO XYOUTS, ALIGNMENT=0.5, lons[i], $
                      CHARSIZE=lgsize, yllev, lonLabels[i], /DEVICE
ENDIF

END
