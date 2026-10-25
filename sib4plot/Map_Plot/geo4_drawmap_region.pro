;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Map drawing routine.
;;  Set map properties, and add labels and color bar. 
;;
;; plot:       The plot structure
;;
PRO GEO4_DrawMap_Region, plot

@ez4_BasicParams.com

;;-------------------------------------
;; Global Check
IF (plot.region EQ 'Globe') THEN BEGIN
   EZ4_Alert,'Error in DrawMap: Expection Region, Not Globe'
   RETURN
ENDIF

;;-------------------------------------
;; Set local variables
mps_title=''
mps_titleYOff=0.0

IF (dopsplot) THEN mps_title=plot.title

;;-------------------------------------
;; Set the map positions
map_position = [0.05,0.2,0.95,0.85]
lgrid_position = 'bot'
cbar_position = [map_position[0] + 0.03, map_position[1] - 0.10, $
                 map_position[2] - 0.04, map_position[1] - 0.07]

;; set the left position for PS plots
IF (dopsPlot) THEN map_position[0] = 0.1

;; lower the bottom margin if no color bar drawn and no shading text
IF (plot.ColorBar EQ 0) THEN map_position[1] = map_position[1] - 0.1

overplot = 0

;;-------------------------------------
;; Determine lat and lon limits based on region
region = plot.Region

CASE region OF ;;limits=[minlat,minlon,maxlat,maxlon]
    'AFRICA': limits=[-37.,-17.,34.,54.]
    'ASIA': limits=[-8., 45., 77., 179.]
    'AUSTRALIA': limits=[-50., 112., 7., 179.]
    'EUROPE': limits=[36.,-12., 72., 45.]
    'NA': limits=[15., -170., 75., -55.]
    'SA': limits=[-57., -84., 15., -33.]

    'BRAZIL': limits=[-33., -84., 14., -33]
    'CHINA': limits=[ 6., 60., 50., 150.]
    'USA': limits=[23., -125., 52., -60.] 

    'BOREAS': limits=[50., -170., 72., -55.]
    'LBA': limits=[-33., -84., 14., -33]
    'TROPICS': limits=[-30., -180., 30., 180.]
 
    'CUSTOM': limits=plot.limits
ENDCASE

lat0 = MEAN([limits[0],limits[2]])
lon0 = MEAN([limits[1],limits[3]])

;;------------------------
;; Set variables back to plot
plot.limits = limits
plot.pos = map_position
plot.lGridPos = lgrid_position
plot.cBarPos = cbar_position
plot.drawborder = 0

;;------------------------
;; Initialize the map base
isotropic=plot.isotropic
CASE plot.mapProj OF
     'CYL' : BEGIN
        MAP_SET, lat0, lon0, /CYLINDRICAL, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END

     'HAM' : BEGIN
        MAP_SET, lat0, lon0, /HAMMER, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END

     'LAM' : BEGIN
        P0lat = plot.limits[0] + 0.5 * (plot.limits[2]-plot.limits[0])           
        P0lon = plot.limits[1] + 0.5 * (plot.limits[3]-plot.limits[1]) 
        MAP_SET, P0lat, P0lon, 0, /LAMBERT, /NOBORDER, ISOTROPIC=isotropic, $
             LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END

     'MER' : BEGIN
        MAP_SET, /MERCATOR, /NOBORDER, ISOTROPIC=isotropic, $
             LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END

     'MIL' : BEGIN
        MAP_SET, lat0, lon0, /MILLER_CYLINDRICAL, /NOBORDER, ISOTROPIC=isotropic, $
             LIMIT=limits, POSITION=map_position, NOERASE=overploat, /CONTINENTS
     END

     'MOL' : BEGIN
        MAP_SET, lat0, lon0, /MOLLWEIDE, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END

     'ORT' : BEGIN
        MAP_SET, lat0, lon0, /ORTHOGRAPHIC, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
      END
        
     'ROB' : BEGIN
        MAP_SET, lat0, lon0, /ROBINSON, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END

     'STE' : BEGIN
        MAP_SET, lat0, lon0, /STEREOGRAPHIC, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS
     END
ENDCASE

IF (dopsplot) THEN BEGIN
    centerX =  (plot.pos[2]+plot.pos[0])*0.5
    titleY = plot.pos[3]+mps_titleYOff
    XYOUTS, centerX, titleY, mps_title, $
         ALIGNMENT=0.5, /NORMAL, COLOR=black, CHARSIZE=1.
ENDIF

GEO4_Color_Fill_Plot, plot

END
