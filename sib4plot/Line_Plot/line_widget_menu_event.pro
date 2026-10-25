;==================================================================
PRO Line_Widget_Menu_Event, event
;==================================================================

@ez4_BasicParams.com

; Set local variables
linePBexist = (N_ELEMENTS(linePasteBoard) GT 0)

; Get event action
WIDGET_CONTROL, event.id, GET_VALUE = buttonName

IF buttonName EQ 'Quit SiB4PLOT' THEN BEGIN
  IF quitPrompt THEN $
      IF EZ4_Confirm(GROUP_LEADER=event.top, 'Quit?') EQ 0 THEN RETURN
  deadEZPLOT = 1 & EXIT
ENDIF

; Get current plot
WIDGET_CONTROL, event.top, GET_UVALUE=plot, /NO_COPY

; Save a copy of the plot
plotnew=plot

; Set the window index to the appropriate value                         
WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
WSET, windowIndex

CASE buttonName OF

;............
; File Menu
;............

  'Save'        : EZ4_SaveFile, plot, fileType='Line'
  'Save As ...' : EZ4_SaveFile, plot, fileType='Line', /newName
  'Print ...'   : BEGIN
      IF (N_ELEMENTS(plot.xdata) LT 0) THEN BEGIN
         EZ4_PrintPlot, plot, printer=defaultPrinter, type='geo4'
      ENDIF ELSE BEGIN
         EZ4_PrintPlot, plot, printer=defaultPrinter, type='line'
      ENDELSE
   END
  'Close Window' : BEGIN 
        WIDGET_CONTROL, event.top, /DESTROY 
        RETURN
   END

;............
; Edit Menu
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
      WIDGET_CONTROL, plot.menuBar, GET_UVALUE=lp_menu
      WIDGET_CONTROL, lp_menu.undo, SENSITIVE=0
    ENDELSE
    Line_DrawPlot, plot
  END

  'Copy' : linePasteBoard = plot

  'Paste Text' : BEGIN
    IF (linePBexist) THEN BEGIN
      EZ4_Undo, plot
      plot.title   = linePasteBoard.title
      plot.xtitle  = linePasteBoard.xtitle
      plot.ytitle  = linePasteBoard.ytitle
      plot.comment = linePasteBoard.comment
      Line_DrawPlot, plot
    ENDIF ELSE EZ4_ALERT, 'Nothing to Paste!'
  END

  'Paste Parameters' : BEGIN
     IF (linePBexist) THEN BEGIN
       EZ4_Undo, plot
       plot.minx = linePasteBoard.minx
       plot.maxx = linePasteBoard.maxx
       plot.miny = linePasteBoard.miny
       plot.maxy = linePasteBoard.maxy
       Line_DrawPlot, plot

       WIDGET_CONTROL, plot.xMinField, SET_VALUE=plot.minx
       WIDGET_CONTROL, plot.xMaxField, SET_VALUE=plot.maxx
       WIDGET_CONTROL, plot.yMinField, SET_VALUE=plot.miny
       WIDGET_CONTROL, plot.yMaxField, SET_VALUE=plot.maxy
    ENDIF ELSE EZ4_ALERT, 'Nothing to Paste!'
  END

  'Add PasteBoard' :      Line_ModOther, plot, linePasteBoard, 'add'
  'Subtract PasteBoard' : Line_ModOther, plot, linePasteBoard, 'subtract'
  'Multiply PasteBoard' : Line_ModOther, plot, linePasteBoard, 'multiply'
  'Divide PasteBoard'   : Line_ModOther, plot, linePasteBoard, 'divide'
  'Superimpose PasteBoard' : Line_SuperImpose, plot, linePasteBoard, $
                              nmaxColors=specNColors
                             
  'Change Axis Labels' : BEGIN
       EZ4_Undo, plot
       EZ4_ChangeAxis, plot
       Line_DrawPlot, plot
   END

  'Change Lines' : BEGIN
       EZ4_Undo, plot
       Line_ChangeLineLabel, plot
       Line_DrawPlot, plot
  END

  'Change Text' : BEGIN
       EZ4_Undo, plot
       EZ4_ChangeText, plot
       EZ4_AddText, plot
   END

  'Change Window Title' : BEGIN
       EZ4_Undo, plot
       Line_ChangeWindow, plot
       RETURN
  END

;............
; View Menu actions
;............

  'Toggle Date': BEGIN
     plot.showDate=1-plot.showDate
     EZ4_AddText, plot
   END
     
  'Toggle File Name': BEGIN
     plot.showFileName=1-plot.showfileName
     EZ4_AddText, plot
  END

  'Toggle Mean Value': BEGIN
    IF (plot.showMean GE 0) THEN BEGIN
       plot.showMean = 1 - plot.showMean
       EZ4_AddText, plot
    ENDIF ELSE BEGIN
       EZ4_Alert,'Mean Not Available.'
    ENDELSE
  END

  'Toggle Simulation Label': BEGIN
    plot.showSimLabel = 1 - plot.showSimLabel
    EZ4_AddText, plot
  END

  'Toggle Line Label': BEGIN
    plot.showLineLabel = 1 - plot.showLineLabel
    EZ4_AddLineLabel, plot
  END

  'Toggle Line Color/Style': BEGIN
    Line_ChangeLineStyle, plot
    Line_DrawPlot, plot
  END


;............
; Analysis Menu
;............

  'Switch GMT/LST': BEGIN
        EZ4_Undo, plot
        Line_SwitchTime, plot
   END
  'Diurnal Composite' : line_diurn, plot, GROUP_LEADER=event.top

  'Add a constant'       : line_modconstant, plot, 'add'
  'Subtract a constant'  : line_modconstant, plot, 'subtract'
  'Multiply by constant' : line_modconstant, plot, 'multiply'
  'Divide by constant'   : line_modconstant, plot, 'divide'

  'Anomaly'              : line_modanalysis, plot, 'anomaly'
  'Reciprocal'           : line_modanalysis, plot, 'reciprocal'

;............
; Window Menu
;............
  '0 SiB4PLOT Main Menu' : BEGIN
     IF ezBase NE -1 THEN WIDGET_CONTROL, ezBase, /SHOW, ICONIFY=0 $
     ELSE ezplot
  END

  ELSE: EZ4_WindowSwitch, buttonName

ENDCASE

DONE:
; Copy the revised state information back into the original structure,
; and return control to the XMANAGER to await more widget events
  ON_IOERROR, NULL
  WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY
  RETURN

END

