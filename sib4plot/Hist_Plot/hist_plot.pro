;======================================================================
PRO Hist_Plot, plot
;======================================================================
; Control creating histogram plots.

;;Check plot is a data structure
IF (N_ELEMENTS(plot) LT 1) THEN BEGIN
   EZ4_Alert,['Histogram Plot Error', $
              'Expecting Plot Data Structure.']
   RETURN
ENDIF

;;Check plot type
IF (plot.type NE 'hist') THEN BEGIN
   string='Plot Type = ' + plot.type
   EZ4_Alert,['Histogram Plot Error',string]
   RETURN
ENDIF
   
;;Check data arrays
IF ((N_ELEMENTS(plot.xdata) LT 2) OR $
    (N_ELEMENTS(plot.ydata) LT 2)) THEN BEGIN
   EZ4_Alert, ['Histogram Plot Error', $
               'Incorrect Data To Plot']
   RETURN
ENDIF

; Build the window GUI for this plot
WIDGET_CONTROL, /HOURGLASS
Hist_Widget_Window, plot

; Call drawplot and display the map
Hist_DrawPlot, plot

plottest = CREATE_STRUCT(plot)
WIDGET_CONTROL, plot.windowBase, SET_UVALUE=plottest, /NO_COPY
XMANAGER, 'Hist_Plot', plot.windowBase, CLEANUP='EZ4_WindowDelete'
  
END
