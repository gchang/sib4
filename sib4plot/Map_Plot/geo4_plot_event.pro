;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Response to REDRAW button or any toolbar button
;;
PRO GEO4_Plot_Event, event

@ez4_BasicParams.com

WIDGET_CONTROL, event.id, GET_UVALUE=button
IF button EQ 'REDRAW' THEN BEGIN

  ;; Process ReDraw action
  WIDGET_CONTROL, event.top, GET_UVALUE=plot, /NO_COPY

  ; Set the window index to the appropriate value                         
  WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
  WSET, windowIndex

  IF (N_ELEMENTS(plot.cMinField) NE 0) THEN $
    WIDGET_CONTROL, plot.cMinField, GET_VALUE=minContour
  IF (N_ELEMENTS(plot.intervalField) NE 0) THEN $
     WIDGET_CONTROL, plot.intervalField, GET_VALUE=interval

  plot.contours(1:maxnColors-1) = minContour[0] + interval[0]*FINDGEN(maxnColors-1)
  plot.minContour = minContour
  plot.contourInterval = interval

  GEO4_DrawPlot, plot

  WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

ENDIF ELSE GEO4_WIDGET_MENU_EVENT, event

END

