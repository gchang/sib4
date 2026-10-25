;======================================================================
PRO Hist_Widget_Window, plot
;======================================================================
; Create the widget window for histogram plots.

COMMON Window_COM

windowTitle = plot.windowTitle
group_leader = plot.group_leader
IF (group_leader EQ -1) THEN group_leader=ezBase

; Set up the plot window widget
;    and display it on the screen
windowBase = Widget_Base( $
             /TLB_Size_Events, /COLUMN, /BASE_ALIGN_CENTER, $
             GROUP_LEADER=GROUP_LEADER, $
             MBAR=menuBar, RNAME_MBAR='menuBar', $
             Title=windowTitle)
plot.windowBase=windowBase
plot.menuBar=menuBar
plot.group_leader=group_leader

; Set up menu bar
Hist_Widget_Menu, plot
Widget_Control, menuBar, EVENT_PRO = 'Hist_Widget_Menu_Event'

; Size the plotting window according to the total screen size
EZ4_PlotWindowSize, width, height
plotWindow = Widget_Draw(windowBase, $
               XSIZE=width, YSIZE=height, /Frame, $
               EVENT_PRO='ez4_mouseprobe', /BUTTON_EVENTS)

; Setup controls for zooming
space = 10
yaxiszoomBase = WIDGET_BASE(windowBase, /ROW, /FRAME, SPACE=space, $
                            UNAME='Yaxis Fields')

yMinField = CW_FIELD(yaxiszoomBase, TITLE='Y-AXIS ZOOM: Min Value:',  $
                        XSIZE=8, VALUE=LONG(plot.miny), /LONG)
yMaxField = CW_FIELD(yaxiszoomBase, TITLE='Max Value:',  $
                        XSIZE=8, VALUE=LONG(plot.maxy), /LONG)
item = WIDGET_BUTTON(yaxiszoomBase, VALUE='REDRAW',UVALUE='REDRAW')
item = WIDGET_BUTTON(yaxiszoomBase, VALUE='FULL PLOT',UVALUE='FULL PLOT')

xaxiszoomBase = WIDGET_BASE(windowBase, /ROW, /FRAME, SPACE=space, $
                           UNAME='Xaxis Fields')
xnBinField = CW_FIELD(xaxiszoomBase, TITLE='Bin Number:', $
                     XSIZE=4, VALUE=FIX(plot.numbins), /INTEGER)
xMinField = CW_FIELD(xaxiszoomBase, TITLE='Min Val: ', $
                     XSIZE=8, VALUE=plot.minx, /FLOATING)      
xIntField = CW_FIELD(xaxiszoomBase, TITLE='Interval: ', $
                     XSIZE=8, VALUE=plot.interval, /FLOATING)   
item = WIDGET_BUTTON(xaxiszoomBase, VALUE='CALCULATE',UVALUE='CALCULATE')


; Set the zooming variables back to plot structure
plot.plotWindow = plotWindow
plot.yMinField = yminField
plot.yMaxField = yMaxField
plot.xnBinField = xnBinField
plot.xMinField = xMinField
plot.xIntField = xIntField

; Create the widget
Widget_Control, windowBase, /Realize
Widget_Control, plotWindow, Get_Value=windowIndex
Wset, windowIndex

EZ4_WindowAdd, id=windowBase, title=windowTitle, index=windowIndex, $
              menuBar=menuBar, /plot

END
