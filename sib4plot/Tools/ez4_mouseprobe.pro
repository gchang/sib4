;======================================================================
PRO ez4_mouseprobe, event
;======================================================================

IF event.clicks NE 2 THEN RETURN

WIDGET_CONTROL, event.top, GET_UVALUE=plot, /NO_COPY
WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
WSET, windowIndex

WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

END
