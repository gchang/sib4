;=======================================
PRO LINE_PLOT_EVENT, event
;=======================================

WIDGET_CONTROL, event.id, GET_UVALUE=button

IF SIZE(button, /TYPE) EQ 8 THEN RETURN

IF button EQ 'REDRAW' OR button EQ 'FULL PLOT' THEN BEGIN
  WIDGET_CONTROL, event.top, GET_UVALUE=plot, /NO_COPY
  ; Set the window index to the appropriate value
  WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
  WSET, windowIndex
ENDIF

IF button EQ 'REDRAW' THEN BEGIN

   Line_ReDraw, plot, EVENT=event

ENDIF ELSE IF button EQ 'FULL PLOT' THEN BEGIN

  WIDGET_CONTROL, plot.xMinField, SET_VALUE=plot.savminx
  WIDGET_CONTROL, plot.xMaxField, SET_VALUE=plot.savmaxx
  plot.minx = plot.savminx
  plot.maxx = plot.savmaxx

  WIDGET_CONTROL, plot.yMinField, SET_VALUE=plot.savminy
  WIDGET_CONTROL, plot.yMaxField, SET_VALUE=plot.savmaxy
  plot.miny = plot.savminy
  plot.maxy = plot.savmaxy

  Line_DrawPlot, plot
  WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

ENDIF ELSE BEGIN
  EZ4_Alert,'***BAD SELECTION - ERROR IN LINE_PLOT***'
ENDELSE

END
