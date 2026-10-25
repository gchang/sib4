;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;                       PROGRAM GEO_PLOT FOR IDL                      :
;          Overlay a contour plot of data on a map of the world       :
;                                                                     :
;                       Written by Scott Denning                      :
;                  CSU Dept. of Atmospheric Science                   :
;                                1994                                 :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO GEO4_PLOT, plot

@ez4_BasicParams.com

;;Check data arrays
IF (N_ELEMENTS(plot.data) LT 2) THEN BEGIN
   EZ4_Alert, 'GEO4 Error: Not Enough Data To Contour'
   RETURN
ENDIF

ssize=size(plot.data)
sdatax=ssize(1)
sdatay=ssize(2)
IF ((sdatax NE plot.nlon) OR $
    (sdatay NE plot.nlat)) THEN BEGIN
    EZ4_Alert, 'GEO Error: Data/Lon/Lat Mismatch'
    RETURN
ENDIF

; Build the window GUI for this plot
WIDGET_CONTROL, /HOURGLASS
GEO4_Widget_Window, plot

; Call drawplot and display the map
GEO4_DrawPlot, plot

plottest = CREATE_STRUCT(plot)
WIDGET_CONTROL, plot.windowBase, SET_UVALUE=plottest, /NO_COPY
XMANAGER, 'GEO4_Plot', plot.windowBase, CLEANUP='EZ4_WindowDelete'
  
END
