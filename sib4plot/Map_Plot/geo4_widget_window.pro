
;======================================================================

PRO GEO4_Widget_Window, plot

COMMON Window_COM

; Set local variables from the saved window structure
blank='          '

windowTitle = plot.windowTitle
group_leader = plot.group_leader
menuBar = plot.menuBar

dataMin = plot.minData
dataMax = plot.maxData
minContour = plot.minContour
contourInterval = plot.contourInterval

; Set up the plot window widget and display it on the screen
IF (group_leader EQ -1) THEN group_leader=ezBase
windowBase = WIDGET_BASE(TITLE = windowTitle, /COLUMN, $
                         RESOURCE='GeoPlot', /BASE_ALIGN_CENTER, $
                         MBAR=menuBar, RNAME_MBAR='menuBar', $
                         KILL_NOTIFY='gp_DeleteWindow', $
                         GROUP_LEADER=GROUP_LEADER)
plot.windowBase=windowBase
plot.menuBar=menuBar
plot.group_leader=group_leader

; The top of the window contains a set of pull-down menus and  
; buttons that control the plot parameters and what the program
; will do next.  
GEO4_Widget_Menu, plot
WIDGET_CONTROL, menuBar, EVENT_PRO = 'GEO4_Widget_Menu_Event'

;.....................
;; Size the plotting window according to the total screen size
EZ4_PlotWindowSize, width, height

plotWindow = WIDGET_DRAW(windowBase, XSIZE=width, YSIZE=height, $
                          /FRAME)
;.....................
;; Message window used for max min labels, etc         
space = 10

messageBase = WIDGET_BASE(windowBase, /FRAME)
message     = WIDGET_LABEL(messageBase, SCR_XSIZE=width, VAL = $
                   '   Min = ' + STRCOMPRESS(STRING(dataMin)) + blank + $
    	           '   Max = ' + STRCOMPRESS(STRING(dataMax)))

; Next is a series of controls for contour level selection
row       = WIDGET_BASE(windowBase, /ROW, /FRAME, SPACE=space, $
                        UNAME='Contour Fields')
cMinField = CW_FIELD(row, TITLE='Min Contour:', XSIZE=8, VAL=minContour, $
                       /FLOAT)
intervalField = CW_FIELD(row, TITLE='Interval:', XSIZE=8, $
                         VAL=contourInterval, /FLOAT) 
redraw = WIDGET_BUTTON(row, VAL='REDRAW',UVAL='REDRAW')

;.....................
; Set the variables back into the window structure if necessary
plot.plotWindow=plotWindow
plot.cMinField=cMinField
plot.intervalField=intervalField

; Create the widget
WIDGET_CONTROL, windowBase, /REALIZE
WIDGET_CONTROL, plotWindow, GET_VALUE=windowIndex
WSET, windowIndex

EZ4_WindowAdd, id=windowBase, title=windowTitle, index=windowIndex, $
        menuBar=menuBar, /plot
 
END
