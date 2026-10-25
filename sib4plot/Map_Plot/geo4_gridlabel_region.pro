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
PRO GEO4_GridLabel_Region, plot

@ez4_BasicParams.com

;; Check for global plots
IF (plot.region EQ 'Globe') THEN BEGIN
   EZ4_Alert, 'Error in GridLabel_Region:  Global Plot When Expecting Region.'
   RETURN
ENDIF

;; Specify grid interval
ny = 3  ;5
nx = 3  ;6
latDel = ROUND(ROUND(((plot.limits[2]-plot.limits[0])/ny)*10.)*0.2)/2.0
lonDel = ROUND(ROUND(((plot.limits[3]-plot.limits[1])/nx)*10.)*0.2)/2.0

;; Determine lons and lats for labels in map coordinates
lats      = latDel*(1+INDGEN(ny)) + FLOOR(plot.limits[0]/latDel)*latDel
IF (plot.limits[0] LT lats[0]) THEN BEGIN
    lats=[ lats[0]-latdel, lats ]
    ny++
ENDIF
IF (plot.limits[2] GT lats[ny-1]) THEN BEGIN
    lats= [ lats, lats[ny-1]+latdel]
    ny++
 ENDIF
latLabels = EZ4_FormatLonLat(lats, /lat)
ny        = ny - 1

lons      = lonDel*(1+INDGEN(nx)) + FLOOR(plot.limits[1]/lonDel)*lonDel
IF (plot.limits[1] LT lons[0]) THEN BEGIN
   lons = [ lons[0]-londel, lons ]
   nx++
ENDIF
IF (plot.limits[3] GT lons[nx-1]) THEN BEGIN
   lons = [ lons, lons[nx-1]+londel]
   nx++
ENDIF
points    = WHERE(lons GT 180, n)
IF n GT 0 THEN lons[points] = lons[points] - 360
lonLabels = EZ4_FormatLonLat(lons, /lon)
nx        = nx - 1

lonlev = lons[1]
latlev = (plot.limits[0] + lats[1])/2.

csize=1.0
IF (plot.drawGrid EQ 0) THEN BEGIN
   MAP_GRID, COLOR=black, /NO_GRID, $
          LONS=lons, LATS=lats, $
          label=plot.labelGrid, latnames=latLabels, lonnames=lonLabels, $
          latlab=lonlev, lonlab=latlev, charsize=csize
ENDIF ELSE BEGIN
   MAP_GRID, COLOR=black, $
          LONS=lons, LATS=lats, $
          label=plot.labelGrid, latnames=latLabels, lonnames=lonLabels, $
          latlab=lonlev, lonlab=latlev, charsize=csize
ENDELSE

END
