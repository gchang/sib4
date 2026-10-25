;======================================================================;

PRO GEO4_Widget_Menu_Event, event

@ez4_BasicParams.com

WIDGET_CONTROL, event.handler, GET_UVALUE=windowInfo
WIDGET_CONTROL, event.id, GET_VALUE = buttonName

IF buttonName EQ 'Quit EZPLOT' THEN BEGIN
  IF quitPrompt THEN $
      IF EZ4_Confirm(GROUP_LEADER=event.top, 'Quit?') EQ 0 THEN RETURN
  deadEZPLOT = 1
  EXIT ;;WIDGET_CONTROL, /RESET
  RETURN
ENDIF

; The plot is described by the fields in the structure "plot"
; which is passed as a USER VALUE to the Geo_Plot base widget.
; See the code near the end of the main segment GEO_PLOT, and the
; discussion of this topic in IDL User's Guide pp. 20-13 to 20-14
WIDGET_CONTROL, event.top, GET_UVALUE=plot, /NO_COPY

; Save a copy of the plot
plotnew=plot

; Set the window index to the appropriate value                         
WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
WSET, windowIndex

; The user has pressed a button on the menu bar at the top of the plot.
; The structure "event" contains an element called "id" which is the
; widget ID of the button that was pressed. We query the widget to get 
; name of the button the user pressed. The CASE construct below takes
; actions based on the name of the button the user pressed:

CASE buttonName OF

;............
; FILE Menu actions
;............

  "Save"        : EZ4_SaveFile, plot, fileType='Map'
  "Save As ..." : EZ4_SaveFile, plot, fileType='Map', /newName
  'Print ...'   : EZ4_PrintPlot, plot, printer=defaultPrinter, type='map'
  'Close'       : BEGIN & WIDGET_CONTROL, event.top, /DESTROY & RETURN & END

;............
; Edit Menu actions
;............

  'Undo' : BEGIN
    WIDGET_CONTROL, plot.plotWindow, GET_UVALUE=undo, /NO_COPY
    N = N_ELEMENTS(undo)
    IF N GT 1 THEN BEGIN
      ;; Get the recent undo plot information
      plot = undo[N-1]
      undo = undo[0:N-2]
      WIDGET_CONTROL, plot.plotWindow, SET_UVALUE=undo, /NO_COPY
    ENDIF ELSE BEGIN
      ;; This is the only undo plot information, clear undo menu
      plot = undo
      WIDGET_CONTROL, plot.menuBar, GET_UVALUE=gp_menu
      WIDGET_CONTROL, gp_menu.undo, SENSITIVE=0
    ENDELSE
    GEO4_DrawPlot, plot
  END

  'Copy' : geoPasteBoard = plot
      
  'Paste Text' : BEGIN
    IF (N_ELEMENTS(geoPasteBoard) GT 0) THEN BEGIN
       GEO4_Undo, plot
       plot.title   = geoPasteBoard.title
       plot.units   = geoPasteBoard.units
       plot.comment = geoPasteBoard.comment
       GEO4_AddText, plot
    ENDIF ELSE EZ4_ALERT, 'Nothing to Paste!'
  END

  'Paste Parameters' : BEGIN
     IF (N_ELEMENTS(geoPasteBoard) GT 0) THEN BEGIN
       GEO4_Undo, plot
       plot.contours = geoPasteBoard.contours
       GEO4_DrawPlot, plot
    ENDIF ELSE EZ4_ALERT, 'Nothing to Paste!'
  END

  'Add PasteBoard' :      GEO4_modother, plot, geoPasteBoard, 'add'
  'Subtract PasteBoard' : GEO4_modother, plot, geoPasteBoard, 'subtract'
  'Multiply PasteBoard' : GEO4_modother, plot, geoPasteBoard, 'multiply'
  'Divide PasteBoard'   : GEO4_modother, plot, geoPasteBoard, 'divide'

  'Change Text'           : BEGIN
       GEO4_Undo, plot
       GEO4_ChangeText, plot
    END

  'Change Lat/Lon Labels' : EZ4_Alert, 'Under Construction' 
                                ;BEGIN & Undo, plot & ChangeLatLon,
                                ;plot, event.top END

  'Edit ColorBar' : BEGIN
        GEO4_Undo, plot
        GEO4_ChangeCBar, plot
   END

;............
; View Menu actions
;............

  'Toggle Date': BEGIN
       plot.showDate = 1 - plot.showDate
       GEO4_AddText, plot
   END

  'Toggle File Name': BEGIN
       plot.showFileName = 1 - plot.showFileName
       GEO4_AddText, plot
  END

  'Toggle Global Mean': BEGIN
      plot.showMean = 1 - plot.showMean
      GEO4_AddText, plot
   END

  'Toggle Simulation Label': BEGIN
       plot.showLabel = 1 - plot.showLabel
       GEO4_AddText, plot
   END

  'Toggle Border': BEGIN
      plot.drawBorder = 1 - plot.drawBorder
      GEO4_DrawPlot, plot
   END

  'Toggle Grid Lines' : BEGIN
      plot.drawGrid = 1 - plot.drawGrid
      GEO4_DrawPlot, plot
   END

  'Toggle Lat/Lon Labels': BEGIN
     plot.labelGrid = 1 - plot.labelGrid
     GEO4_DrawPlot, plot
   END

  'Toggle Map Center': BEGIN
     IF (plot.MapCenter EQ 0) THEN plot.MapCenter = 180. ELSE $
     IF (plot.MapCenter EQ 180) THEN plot.MapCenter = 0. $
     ELSE BEGIN
         EZ4_Alert, 'To Toggle Map Center, Expecting Center Longitude of 0 or 180.'
         RETURN
     ENDELSE 
    
     GEO4_DrawPlot, plot
  END

  'Toggle Isotropic': BEGIN
      plot.isotropic = 1 - plot.isotropic
      GEO4_DrawPlot, plot
   END

  'Toggle Continents' : BEGIN
     plot.drawContinents = 1 - plot.drawContinents
     WIDGET_CONTROL, plot.menuBar, GET_UVALUE=gp_menu
     WIDGET_CONTROL, gp_menu.hires, SENSITIVE=plot.drawContinents
     WIDGET_CONTROL, gp_menu.coasts, SENSITIVE=plot.drawContinents
     WIDGET_CONTROL, gp_menu.countries, SENSITIVE=plot.drawContinents
     WIDGET_CONTROL, gp_menu.usa, SENSITIVE=plot.drawContinents

     GEO4_DrawPlot, plot
  END

 'Toggle High Resolution' : BEGIN
     IF (plot.drawContinents EQ 1) THEN BEGIN
         plot.drawHiRes = 1 - plot.drawHiRes
         GEO4_DrawPlot, plot
     ENDIF ELSE BEGIN
         EZ4_Alert, 'Continents Must Be Drawn For High Resolution Map'
         RETURN
     ENDELSE
  END

  'Toggle Coastlines' : BEGIN
     IF (plot.drawContinents EQ 1) THEN BEGIN
         plot.drawCoasts = 1 - plot.drawCoasts
         GEO4_DrawPlot, plot
     ENDIF ELSE BEGIN
         EZ4_Alert, 'Continents Must Be Drawn For Coastlines'
         RETURN
     ENDELSE
  END

  'Toggle Countries' : BEGIN
     IF (plot.drawContinents EQ 1) THEN BEGIN
         plot.drawCountries = 1 - plot.drawCountries
         GEO4_DrawPlot, plot
     ENDIF ELSE BEGIN
         EZ4_Alert, 'Continents Must Be Drawn For Countries'
         RETURN
     ENDELSE
  END

  'Toggle USA States' : BEGIN
     IF (plot.drawContinents EQ 1) THEN BEGIN
         plot.drawUSA = 1 - plot.drawUSA
         GEO4_DrawPlot, plot
     ENDIF ELSE BEGIN
         EZ4_Alert, 'Continents Must Be Drawn For USA States'
     ENDELSE
  END 

   'Toggle Contour Lines' : BEGIN
      plot.drawLines = 1 - plot.drawLines
      IF (plot.drawLines EQ 0) THEN plot.labelLines=0
      GEO4_DrawPlot, plot
   END

  'Toggle Contour Labels' : BEGIN
      plot.labelLines = 1 - plot.labelLines
      IF (plot.labelLines EQ 1) THEN plot.drawLines=1
      GEO4_DrawPlot, plot
   END

  'Match Contour Lines/Shades': BEGIN
      plot.labelColorMatch = 1 - plot.labelColorMatch
      IF (plot.labelColorMatch EQ 1) THEN BEGIN
          plot.drawLines = 1
          plot.labelLines = 1
      ENDIF
      GEO4_DrawPlot, plot
  END

  'Toggle Color Bar' : BEGIN
     plot.ColorBar = 1 - plot.ColorBar
     GEO4_DrawPlot, plot
  END

  ;Color Options
  'Classic Rainbow' : BEGIN
      GEO4_Undo, plot

      colorChoice = 0
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
    END
    
  'Reverse Rainbow' : BEGIN
      GEO4_Undo, plot

      colorChoice = 1
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
     END
    
  '12-Color' : BEGIN   
      GEO4_Undo, plot

      colorChoice = 2
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot 
    END
    
  'Reverse 12-Color' : BEGIN 
      GEO4_Undo, plot

      colorChoice = 3
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
    END
    
  'Blue/White/Red' : BEGIN 
      GEO4_Undo, plot

      colorChoice = 4
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
    END
    
  'Reverse Blue/White/Red' : BEGIN 
      GEO4_Undo, plot

      colorChoice = 5
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
    END

  'White/Green': BEGIN
     GEO4_Undo, plot

     colorChoice = 6
     GEO4_Change_Colors, colorChoice, plot

     WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
     WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
     GEO4_DrawPlot, plot
  END

  'Gray-Scale' : BEGIN
      GEO4_Undo, plot

      colorChoice = 7
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
  END

  'Reverse Gray-Scale': Begin
      GEO4_Undo, plot

      colorChoice = 8
      GEO4_Change_Colors, colorChoice, plot

      WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
      WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval
      GEO4_DrawPlot, plot
  END

  ;Contour Options
  'Even Min to Max'          : BEGIN
      GEO4_Undo, plot
      GEO4_Change_Contours, plot, 0
  END

  'Center on Mean'  : BEGIN
      GEO4_Undo, plot
      GEO4_Change_Contours, plot, 1
   END

  'Center on Zero'  : BEGIN
      GEO4_Undo, plot
      GEO4_Change_Contours, plot, 2
   END

  'Center on Value' : BEGIN
       GEO4_Undo, plot
       GEO4_Change_Contours, plot, 3
   END

   'Custom ...' : BEGIN
       GEO4_Undo, plot
       GEO4_CustomContour, plot
   END

  ;Map Projection Options 
  'Cylindrical Equidistant' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'CYL'
       GEO4_DrawPlot, plot
    END

  'Hammer-Aitoff Equal Area' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'HAM'
       GEO4_DrawPlot, plot
    END 

  'Lambert Equal Area' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'LAM'
       GEO4_DrawPlot, plot
    END

  'Mercator' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'MER'
       GEO4_DrawPlot, plot
    END

  'Miller Cylindrical' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'MIL'
       GEO4_DrawPlot, plot
    END

  'Mollweide' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'MOL'
       GEO4_DrawPlot, plot
    END

  'Orthographic' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'ORT'
       GEO4_DrawPlot, plot
    END

  'Robinson Pseudo-Cylindrical' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'ROB'
       GEO4_DrawPlot, plot
    END

  'Stereographic' : BEGIN
       GEO4_Undo, plot

       plot.mapProj = 'STE'
       GEO4_DrawPlot, plot
    END

  ;WhiteOut Options
  'Lowest Values':  BEGIN
       GEO4_Undo, plot
       newWhiteOut = 'low'
       plot.WhiteOut = newWhiteOut
       fillColors = GEO4_Set_fillColors(plot.colorChoice, newWhiteOut)
       plot.fillColors = fillColors
 
       GEO4_DrawPlot, plot
    END

  'Center Values':  BEGIN
       GEO4_Undo, plot
       newWhiteOut = 'center'
       plot.WhiteOut = newWhiteOut
       fillColors = GEO4_Set_fillColors(plot.colorChoice, newWhiteOut)
       plot.fillColors = fillColors
 
       GEO4_DrawPlot, plot
   END

  'Middle Values': BEGIN
       GEO4_Undo, plot
       newWhiteOut = 'middle'
       plot.WhiteOut = newWhiteOut
       fillColors = GEO4_Set_fillColors(plot.colorChoice, newWhiteOut)
       plot.fillColors = fillColors
 
       GEO4_DrawPlot, plot
   END

  'High Values': BEGIN
       GEO4_Undo, plot
       newWhiteOut = 'high'
       plot.WhiteOut = newWhiteOut
       fillColors = GEO4_Set_fillColors(plot.colorChoice, newWhiteOut)
       plot.fillColors = fillColors
 
       GEO4_DrawPlot, plot
   END

  'Restore Colors': BEGIN
       GEO4_Undo, plot
       newWhiteOut = 'none'
       plot.WhiteOut = newWhiteOut
       fillColors = GEO4_Set_fillColors(plot.colorChoice, newWhiteOut)
       plot.fillColors = fillColors
 
       GEO4_DrawPlot, plot
  END


;............
; Analysis Menu actions
;............

  ;Aritmetic Options
  "Anomaly"                 : GEO4_ModAnalysis, plot, 'anomaly'
  "Reciprocal"              : GEO4_ModAnalysis, plot, 'reciprocal'
  "Add a constant"          : GEO4_ModConstant, plot, 'add'
  "Subtract a constant"     : GEO4_ModConstant, plot, 'subtract'
  "Multiply by constant"    : GEO4_ModConstant, plot, 'multiply'
  "Divide by constant"      : GEO4_ModConstant, plot, 'divide'


   ;Stats Options
  "Frequency Distribution" :  GEO4_FreqDist, plot
  "Single Grid Cell Value" :  GEO4_PickLonLat, plot
  "Zonal Mean" : GEO4_ZonalMean, plot

;............
; Region Menu actions
;............

  'AFRICA' :  BEGIN
        plotnew.region = 'AFRICA'
        GEO4_Plot, plotnew
   END

  'ASIA' :  BEGIN
        plotnew.region = 'ASIA'
        GEO4_Plot, plotnew
  END

  'AUSTRALIA' :  BEGIN
        plotnew.region = 'AUSTRALIA'
        GEO4_Plot, plotnew
  END

  'EUROPE' : BEGIN
      plotnew.region = 'EUROPE'
      GEO4_Plot, plotnew
  END

  'NA' :  BEGIN
      plotnew.region = 'NA'
      GEO4_Plot, plotnew
   END

  'SA' : BEGIN
      plotnew.region = 'SA'
      GEO4_Plot, plotnew
   END

  'BRAZIL' : BEGIN
      plotnew.region = 'BRAZIL'
      GEO4_Plot, plotnew
  END

  'CHINA' : BEGIN
      plotnew.region = 'CHINA'
      GEO4_Plot, plotnew
   END

  'USA'     :  BEGIN
      plotnew.region = 'USA'
      GEO4_Plot, plotnew
   END

  'BOREAS' :  BEGIN
      plotnew.region = 'BOREAS'
      GEO4_Plot, plotnew
   END

  'LBA' :  BEGIN
      plotnew.region = 'LBA'
      GEO4_Plot, plotnew
  END

  'TROPICS' :  BEGIN
      plotnew.region = 'TROPICS'
      GEO4_Plot, plotnew
  END

  'Globe' : BEGIN
      plotnew.region = 'Globe'
      GEO4_PLOT, plotnew
  END

  'Custom Region': BEGIN
      GEO4_CustomRegion, plotnew
  END

;............
; Window Menu actions (Switch to another window?)
;............

   '0 SiB4PLOT Main Menu' : BEGIN
     IF ezBase NE -1 THEN WIDGET_CONTROL, ezBase, /SHOW, ICONIFY=0 $
     ELSE ezplot
   END
       
   ELSE: EZ4_WindowSwitch, buttonName

ENDCASE

;....................

; Copy the revised state information back into the original structure,
; and return control to the XMANAGER to await more widget events
DONE:
  WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

END

