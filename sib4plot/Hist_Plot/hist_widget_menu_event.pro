;==================================================================
PRO Hist_Widget_Menu_Event, event
;==================================================================

@ez4_BasicParams.com

; Set local variables
histPBexist = (N_ELEMENTS(histPasteBoard) GT 0)

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

  'Save'        : EZ4_SaveFile, plot, fileType='Hist'
  'Save As ...' : EZ4_SaveFile, plot, fileType='Hist', /newName
  'Print ...'   : $
        EZ4_PrintPlot, plot, printer=defaultPrinter, type='Hist'
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
      WIDGET_CONTROL, plot.menuBar, GET_UVALUE=hist_menu
      WIDGET_CONTROL, hist_menu.undo, SENSITIVE=0
    ENDELSE
    Hist_DrawPlot, plot
  END

  'Copy' : histPasteBoard = plot

  'Add PasteBoard' :      Hist_ModOther, plot, histPasteBoard, 'add'
  'Subtract PasteBoard' : Hist_ModOther, plot, histPasteBoard, 'subtract'
  'Multiply PasteBoard' : Hist_ModOther, plot, histPasteBoard, 'multiply'
  'Divide PasteBoard'   : Hist_ModOther, plot, histPasteBoard, 'divide'
  'Superimpose PasteBoard' : Hist_SuperImpose, plot, histPasteBoard, $
                              nmaxColors=specNColors
                             
  'Change Axis Labels' : BEGIN
       EZ4_Undo, plot
       EZ4_ChangeAxis, plot
       Hist_DrawPlot, plot
   END

  'Change Colors/Labels' : BEGIN
       EZ4_Undo, plot
       Hist_ChangeLabel, plot
       Hist_DrawPlot, plot
  END

  'Change Text' : BEGIN
       EZ4_Undo, plot
       EZ4_ChangeText, plot
       EZ4_AddText, plot
  END

  'Change Window Title' : BEGIN
       EZ4_Undo, plot
       Hist_ChangeWindow, plot
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
     plot.showFileName=1-plot.showFileName
     EZ4_AddText, plot
   END

  'Toggle Mean Value': BEGIN
     plot.showMean=1-plot.showMean
     EZ4_AddText, plot
   END

  'Toggle Simulation Label': BEGIN
    plot.showSimLabel = 1 - plot.showSimLabel
    EZ4_AddText, plot
  END

  'Toggle Hist Label': BEGIN
    plot.showLineLabel = 1 - plot.showLineLabel
    Hist_DrawPlot, plot
  END

  'Toggle Hist Fill': BEGIN
    plot.linestyle = 1 - plot.linestyle
    Hist_DrawPlot, plot
  END


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

