;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO Hist_ReDraw, plot, EVENT=EVENT

 ;;;;;;Get the y-axis range
  WIDGET_CONTROL, plot.yminField, GET_VALUE=miny
  WIDGET_CONTROL, plot.ymaxField, GET_VALUE=maxy

  plot.miny=miny
  plot.maxy=maxy

  ;;;;;;;;;Plot the results
  Hist_DrawPlot, plot
  WIDGET_CONTROL, plot.yMinField, GET_VALUE=miny
  WIDGET_CONTROL, plot.yMaxField, GET_VALUE=maxy

  ;;;;;;;;;Reset widget for another event
  IF (N_ELEMENTS(EVENT) NE 0) THEN  $
      WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

END
