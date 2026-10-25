;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO GEO4_COLOR_FILL_PLOT, plot
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@ez4_BasicParams.com

;Set Local Values
data = plot.data
;badref = where(data EQ missing)
;IF (badref[0] GE 0) THEN data(badref) = 0.

lon = plot.lon
lat = plot.lat
limits = plot.limits
ncontours = plot.ncontours

IF (plot.mindata EQ plot.maxdata) THEN BEGIN
   ncontours = 3
   IF (plot.mindata NE 0.) THEN BEGIN
      cmin = (plot.mindata - 0.02*plot.mindata)
      cmax = (plot.maxdata + 0.02*plot.maxdata)
   ENDIF ELSE BEGIN
      cmin = -0.1
      cmax = 0.1
   ENDELSE

   contours = [missing, cmin, cmax]
   fillColors = [plot.fillColors[0], $
                 plot.fillColors[plot.ncontours-2], $
                 plot.fillColors[0]]

   plot.ncontours=ncontours
   plot.contours=contours
   plot.fillColors=fillColors
   plot.cbarpos[0] = 0.35
   plot.cbarpos[2] = 0.65
ENDIF ELSE $
IF (nContours EQ maxNColors) THEN BEGIN
   contours = plot.contours
   fillColors = plot.fillColors
ENDIF ELSE BEGIN
   contours = FLTARR(ncontours)
   fillColors = FLTARR(ncontours)
   contours(*) = plot.contours(0:ncontours-1)
   fillColors(*) = plot.fillColors(0:ncontours-1)
ENDELSE

;Double check contours are increasing and different
cdiff=contours(ncontours-1)-contours(ncontours-2)
IF (cdiff LT 1.E-6) THEN BEGIN
      ncontours = 3
      cval = contours(ncontours-1)
      IF (ABS(cval) LT 1.E-6) THEN BEGIN
          cmin = -0.01
          cmax = 0.01
      ENDIF ELSE BEGIN
          cmin = (cval - 0.02*ABS(cval))
          cmax = (cval + 0.02*ABS(cval))
      ENDELSE
      contours = [missing, cmin, cmax]
      fillColors = [plot.fillColors[0], $
                 plot.fillColors[plot.ncontours-2], $
                 plot.fillColors[0]]

      plot.ncontours=ncontours
      plot.contours=contours
      plot.fillColors=fillColors
      plot.cbarpos[0] = 0.35
      plot.cbarpos[2] = 0.65
ENDIF

whiteOut = plot.whiteOUt
drawLines = plot.drawLines
labelLines = plot.labelLines
labelSize = plot.labelSize
labelColorMatch = plot.labelColorMatch

CONTOUR, data, lon, lat, /CELL_FILL, LEVEL=contours, C_COLOR=fillColors, $
         /OVERPLOT

;; Overlay contour line plot if requested
IF (drawLines EQ 1) THEN BEGIN
  ;; With middle whiteOut, make the contours within the whiteOut area be white
  IF (whiteOut EQ 'middle') THEN BEGIN
    c_color = MAKE_ARRAY(ncontours, VALUE=black, /BYTE)
    IF nContours EQ 19 THEN BEGIN
      c_color[9] = white
      c_color[10] = white
    ENDIF ELSE c_color[6] = white
  ENDIF ELSE $
  IF (labelColorMatch EQ 1) THEN c_color = fillColors ELSE c_color = black

  IF (labelLines EQ 1) THEN BEGIN 
     FOLLOW=1 
     C_CHARSIZE=labelSize 
  ENDIF

  ;; Draw the lines and labels
  CONTOUR, data, lon, lat, LEVELS=contours, FOLLOW=FOLLOW, /OVERPLOT, $
           C_COLOR=c_color, C_LINESTYLE=(contours LT 0.0), $
           C_CHARSIZE=C_CHARSIZE

  ;;;;If you want the labels to stay black, uncomment below:
  ;CONTOUR, data, lon, lat, LEVELS=contours, FOLLOW=FOLLOW, /OVERPLOT,  $
  ;    C_CHARSIZE=C_CHARSIZE, C_COLOR=black, C_LINESTYLE=1, C_THICK=1.
  ;CONTOUR, data, lon, lat, LEVELS=contours, /OVERPLOT, $
  ;    C_COLOR=c_color, C_LINESTYLE=(contours LT 0.0)

ENDIF

;; Draw border around the map
IF (plot.drawBorder EQ 1) THEN BEGIN
   IF (dopsplot NE 0) THEN thick=1 ELSE thick=2

   ;; Convert limits to device coords
   xy = CONVERT_COORD([limits[1],limits[3]], [limits[0],limits[2]], $
                   /DATA, /TO_DEVICE)
   x0 = xy[0,0]  &  x1 = xy[0,1]  &  y0 = xy[1,0]  &  y1 = xy[1,1]
   PLOTS, COLOR=black, [x0,x0,x1,x1,x0], [y0,y1,y1,y0,y0], /DEVICE, THICK=thick
ENDIF

; Add grid lines and labels
IF ((plot.drawGrid EQ 1) OR (plot.labelGrid EQ 1)) THEN BEGIN
   IF (plot.region EQ 'Globe') THEN GEO4_GridLabel_Globe, plot $
   ELSE GEO4_GridLabel_Region, plot
ENDIF

; Add continents and countries
IF (plot.drawContinents EQ 1) THEN BEGIN
   MAP_CONTINENTS, COLOR=black, MLINETHICK=1, $
       HIRES=plot.drawHiRes, $
       COASTS=plot.drawCoasts, $
       COUNTRIES=plot.drawCountries, $
       USA=plot.drawUSA
ENDIF


END
