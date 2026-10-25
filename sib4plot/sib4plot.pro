;====================================================================

;         E Z  P L O T    f o r   S i B 4

;   Plotting template for SiB4 data
;   Written by Scott Denning 1/94
;   Modified for SiB4 by Kathy Haynes 4/14

;====================================================================

PRO SiB4PLOT

; Draw the main menu window, with a series of pull-down
; menus for various files included

;====================================================================

@ez4_BasicParams.com
ez4_initialize

welcome_string = 'W e l c o m e   t o   E Z P L O T   f o r   S i B 4 !'
current_version_string = '  2 0 1 9 '

ezBase = WIDGET_BASE(TITLE='SiB4PLOT Main Menu', SPACE=10, /COLUMN, $
                     TLB_FRAME_ATTR=10, _EXTRA=extra)

;; Initialize a structure to store window properties
ezWindowList = { WINDOW_LIST_S, ezBase, 'SiB4PLOT Main Menu', 0, -1, -1 }

topBase = WIDGET_BASE(ezBase, /ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER, $
                      SPACE=10)
sub = WIDGET_BASE(topBase, /COLUMN, /ALIGN_CENTER, SPACE=10, $
                  /BASE_ALIGN_CENTER)
message = WIDGET_LABEL(sub, VALUE=welcome_string)
message = WIDGET_LABEL(sub, VALUE=current_version_string)
message2 = WIDGET_LABEL(sub, VALUE='Contact: Katherine.Haynes@colostate.edu')

item      = WIDGET_BASE(ezBase, /COLUMN)
menuBase   = WIDGET_BASE(ezBase, /FRAME, /COLUMN, /ALIGN_CENTER)
message    = WIDGET_LABEL(menuBase, VALUE='SiB4 OUTPUT')
bbase2 = WIDGET_BASE(menuBase, /ROW, /ALIGN_CENTER)
hrbutton = WIDGET_BUTTON(bbase2, VALUE='HR (Hourly)')
pbpbutton = WIDGET_BUTTON(bbase2, VALUE='PBP (Daily)')
qpbutton = WIDGET_BUTTON(bbase2, VALUE='QP (Monthly)')

bbase3 = WIDGET_BASE(menuBase, /ROW, /ALIGN_CENTER)
restartbutton = WIDGET_BUTTON(bbase3, VALUE='Restart File')
driverbutton = WIDGET_BUTTON(bbase3, VALUE='Driver Data')

wsizeX = 90
item      = WIDGET_BASE(ezBase, /COLUMN)
miscbase = WIDGET_BASE(ezBase, /COLUMN, /ALIGN_CENTER, /FRAME)
message = WIDGET_LABEL(miscbase, VALUE='NETCDF FILES')
obase = WIDGET_BASE(miscbase, /ROW, /ALIGN_CENTER)
mapbutton = WIDGET_BUTTON(obase, VALUE='Map', XSIZE=wsizeX)
linebutton = WIDGET_BUTTON(obase, VALUE='Line', XSIZE=wsizeX)

wsizeSX = 70
item      = WIDGET_BASE(ezBase, /COLUMN)
savebase  = WIDGET_BASE(ezBase, /COLUMN, /ALIGN_CENTER, /FRAME)
message = WIDGET_LABEL(savebase, VALUE='SAVED PLOTS')
sbase   = WIDGET_BASE(savebase, /ROW, /ALIGN_CENTER)
mapsavebutton = WIDGET_BUTTON(sbase, VALUE='Map', XSIZE=wsizeSX)
linesavebutton = WIDGET_BUTTON(sbase, VALUE='Line', XSIZE=wsizeSX)
histsavebutton = WIDGET_BUTTON(sbase, VALUE='Hist', XSIZE=wsizeSX)

done = WIDGET_BUTTON(ezBase, VALUE='Q U I T   S i B 4 P L O T')

state = { quitButton:done, $
          hrbutton:hrbutton, pbpbutton:pbpbutton, qpbutton:qpbutton, $
          restartbutton:restartbutton, driverbutton:driverbutton, $
          mapbutton:mapbutton, linebutton:linebutton, $
          mapsavebutton:mapsavebutton, linesavebutton:linesavebutton, $
          histsavebutton:histsavebutton}

WIDGET_CONTROL, ezBase, SET_UVALUE=state, /NO_COPY, $
                TLB_SET_YOFFSET=20, TLB_SET_XOFFSET=20
WIDGET_CONTROL, /REALIZE, ezBase

EXCEPT = 0
XMANAGER, CATCH=1
XMANAGER, 'ez4', ezBase

END
