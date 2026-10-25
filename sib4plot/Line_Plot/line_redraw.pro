;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO Line_ReDraw, plot, EVENT=EVENT

 ;;;;;;Get the x-axis range
  WIDGET_CONTROL, plot.xminField, GET_VALUE=minx
  WIDGET_CONTROL, plot.xmaxField, GET_VALUE=maxx

  plot.minx=minx
  plot.maxx=maxx

  ;;;;;;;;Get the y-axis range
  WIDGET_CONTROL, plot.yMinField, GET_VALUE=miny
  WIDGET_CONTROL, plot.yMaxField, GET_VALUE=maxy

  plot.miny=miny
  plot.maxy=maxy

  ;;;;;;;;;Plot the results
  Line_DrawPlot, plot

  ;;;;;;;;;Reset widget for another event
  IF (N_ELEMENTS(EVENT) NE 0) THEN  $
      WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

END
