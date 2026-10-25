;======================================================================
PRO Line_Plot, plot
;======================================================================
; Control creating line plots.

;;Check plot is a data structure
IF (N_ELEMENTS(plot) LT 1) THEN BEGIN
   EZ4_Alert,['Line Plot Error', $
              'Expecting Plot Data Structure.']
   RETURN
ENDIF

;;Check data arrays
IF ((N_ELEMENTS(plot.xdata) LT 2) OR $
    (N_ELEMENTS(plot.ydata) LT 2)) THEN BEGIN
   EZ4_Alert, ['Line Plot Error', $
               'Incorrect Data To Plot']
   RETURN
ENDIF

; Build the window GUI for this plot
WIDGET_CONTROL, /HOURGLASS
Line_Widget_Window, plot

; Call drawplot and display the map
Line_DrawPlot, plot

plottest = CREATE_STRUCT(plot)
WIDGET_CONTROL, plot.windowBase, SET_UVALUE=plottest, /NO_COPY
XMANAGER, 'Line_Plot', plot.windowBase, CLEANUP='EZ4_WindowDelete'
  
END
