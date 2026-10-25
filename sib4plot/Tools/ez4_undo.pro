;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Make Undo menu active and store undo information
;;
PRO ez4_undo, plot
WIDGET_CONTROL, plot.menuBar, GET_UVALUE=plot_menu
WIDGET_CONTROL, plot_menu.undo, SENSITIVE=1
WIDGET_CONTROL, plot.plotWindow, GET_UVALUE=undo, /NO_COPY

IF N_ELEMENTS(undo) EQ 0 THEN undo = plot ELSE undo = [undo, plot]

;; Remove older undo information if more than 20 undos are stored
N = N_ELEMENTS(undo)
IF N GT 20 THEN undo = undo[N-20:N-1]
WIDGET_CONTROL, plot.plotWindow, SET_UVALUE=undo, /NO_COPY
WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
WSET, windowIndex

END
