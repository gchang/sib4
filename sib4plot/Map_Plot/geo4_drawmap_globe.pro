;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Map drawing routine.
;;  Set map properties, and add labels and color bar. 
;;
;; plot:       The plot structure
;;
PRO GEO4_DrawMap_Globe, plot

@ez4_BasicParams.com

;;-------------------------------------
;; Quick global test
IF (plot.region NE 'Globe') THEN BEGIN
    EZ4_Alert, 'Error in DrawMap: Not A Global Plot!'
    RETURN
ENDIF

;;---------------------
;; Set local variables
title = ''
mps_title = ''

lgrid_pos = 'cen'
lgrid_yoff = 0.0

cbar_tyoff = 0.08
cbar_LBotOff = 0.03

mps_titleyOff = -0.04
mps_cbarYOff = 0.01
mps_cbarheight = 0.03

IF (dopsplot) THEN BEGIN
    mps_title=plot.title
ENDIF

;;-------------------------------------
;; Determine settings based on mapProj
mapProj = plot.MapProj
limits=[-90.,-180.,90.,180.]
lat0 = 0.
lon0 = plot.MapCenter
isotropic = plot.isotropic

CASE mapProj OF
     'CYL': BEGIN
          map_position = [0.075, 0.25, 0.975, 0.82]
          lgrid_position = 'bot'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.13, $
                           map_position[2] - 0.04, map_position[1] - 0.10]

          mps_titleyOff = -0.06
          ;IF (labelDefault) THEN mps_titleYOff-=0.05
          mps_cbaryOff = -0.02
      END

     'HAM': BEGIN
          map_position = [0.05, 0.22, 0.975, 0.85]
          lgrid_position = 'cen'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.11, $
                           map_position[2] - 0.04, map_position[1] - 0.08]

          mps_titleyOff = -0.05
          ;IF (labelDefault) THEN mps_titleYOff-=0.06
     END

     'LAM': BEGIN
          map_position = [0.15, 0.18, 0.85, 0.88]
          lgrid_position = 'cen'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.07, $
                           map_position[2] - 0.04, map_position[1] - 0.04]

          mps_titleyOff = 0.01
          ;IF (labelDefault) THEN mps_titleYOff-=0.04
          mps_cbaryOff = -0.03
     END

     'MER': BEGIN
          map_position = [0.05, 0.22, 0.95, 0.85]
          lgrid_position = 'bot'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.11, $
                           map_position[2] - 0.04, map_position[1] - 0.08]

          mps_titleyOff = -0.06
          ;IF (labelDefault) THEN mps_titleYOff-=0.02
          mps_cbaryOff = -0.02
     END

     'MIL': BEGIN
          map_position = [0.05, 0.25, 0.97, 0.80]
          lgrid_position = 'bot'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.12, $
                           map_position[2] - 0.04, map_position[1] - 0.09]

          mps_titleyOff = -0.06
          ;IF (labelDefault) THEN mps_titleYOff-=0.04
          mps_cbaryOff = -0.01
     END

     'MOL': BEGIN
          map_position = [0.04, 0.21, 0.96, 0.86]
          lgrid_position = 'cen'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.09, $
                           map_position[2] - 0.04, map_position[1] - 0.06]
     END

     'ORT': BEGIN
          map_position = [0.1,0.20,0.975,0.85]
          lgrid_position = 'cen'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.09, $
                           map_position[2] - 0.04, map_position[1] - 0.06]

          mps_titleyOff = 0.02
          ;IF (labelDefault) THEN mps_titleYOff-=0.04
          mps_cbaryOff = -0.01
     END
  
     'ROB': BEGIN
          map_position = [0.05,0.20,0.95,0.85]
          lgrid_position = 'cen'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.10, $
                           map_position[2] - 0.04, map_position[1] - 0.07]
     END

     'STE': BEGIN
          EZ4_Alert,'Stereographic Projection Not Available Globally'
          RETURN
      END

     ELSE: BEGIN
          map_position = [0.1,0.20,0.975,0.85]
          lgrid_position = 'bot'

          cbar_position = [map_position[0] + 0.03, map_position[1] - 0.13, $
                           map_position[2] - 0.04, map_position[1] - 0.10]
     END
ENDCASE
IF (dopsplot EQ 1) THEN map_position=plot.pos

;; lower the bottom margin if no color bar drawn and no shading text
IF (plot.ColorBar EQ 0) THEN map_position[1] -= 0.1

;; shift the plot up if no comment
IF (plot.comment EQ '') THEN BEGIN
      map_position[3] += 0.035
ENDIF

overplot=0
plot.limits = limits
plot.pos = map_position
plot.lGridPos = lgrid_position
plot.lGridYOff = lgrid_yoff

plot.cBarPos = cbar_position
plot.cBarTYOff = cbar_tyoff
plot.cBarLBotOff = cbar_lbotoff

;;------------------------
;; Initialize the map base
CASE mapProj OF
     'CYL' : BEGIN
        MAP_SET, lat0, lon0, /CYLINDRICAL, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     'HAM' : BEGIN
        MAP_SET, lat0, lon0, /HAMMER, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     'LAM' : BEGIN
        P0lat = plot.limits[0] + 0.5 * (plot.limits[2]-plot.limits[0])           
        P0lon = plot.limits[1] + 0.5 * (plot.limits[3]-plot.limits[1]) 
        p0lon=20          
        MAP_SET, P0lat, P0lon, 0, /LAMBERT, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     'MER' : BEGIN
        limits=[-60.,-180.,76.,180.]
        plot.limits=limits
        MAP_SET, lat0, lon0, /MERCATOR, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     'MIL' : BEGIN
        limits=[-60.,-180.,76.,180.]
        plot.limits=limits
        MAP_SET, lat0, lon0, /MILLER_CYLINDRICAL, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     'MOL' : BEGIN
        limits1 = [-90.,0.,90.,360.]
        limits2 = [-90.,-178.,90.,178.]
        plot.limits=limits2
        MAP_SET, lat0, lon0, /MOLLWEIDE, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits1, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     'ORT' : BEGIN
        lon0=-60
        MAP_SET, lat0, lon0, /ORTHOGRAPHIC, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
      END
        
     'ROB' : BEGIN
        plot.drawBorder=0
        MAP_SET, lat0, lon0, /ROBINSON, /NOBORDER, ISOTROPIC=isotropic, $
           LIMIT=limits, POSITION=map_position, NOERASE=overplot, /CONTINENTS, $
           TITLE=title
     END

     ELSE: BEGIN
        EZ4_Alert, 'Error in DrawMap: Unknown Projection'
        RETURN
     END
ENDCASE

;; Create the plot
GEO4_Color_Fill_Plot, plot


END
