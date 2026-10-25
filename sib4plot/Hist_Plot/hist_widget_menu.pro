;======================================================================
PRO Hist_Widget_Menu, plot
;======================================================================

; Set local variables
menuBar = plot.menuBar

;............
; File Menu
;............
fileMenu = WIDGET_BUTTON(menuBar, VALUE='File',/MENU, RESOURCE_NAME='fileMenu')
item = WIDGET_BUTTON(fileMenu, VALUE='Save', RESOURCE_NAME='Save')
item = WIDGET_BUTTON(fileMenu, VALUE='Save As ...', RESOURCE_NAME='Save')
item = WIDGET_BUTTON(fileMenu, VALUE='Print ...', RESOURCE_NAME='Print')
item = WIDGET_BUTTON(fileMenu, VALUE='Close Window', RESOURCE_NAME='Close')
item = WIDGET_BUTTON(fileMenu, VALUE='Quit SiB4PLOT',RESOURCE_NAME='Quit')

;............
; Edit Menu
;............
editMenu = WIDGET_BUTTON(menuBar, VALUE='Edit', /MENU, $
                         RESOURCE_NAME='editMenu')
undo = WIDGET_BUTTON(editMenu, VALUE='Undo')
WIDGET_CONTROL, undo, SENSITIVE=0
item = WIDGET_BUTTON(editMenu, VALUE='Copy', RESOURCE_NAME='Copy')
item = WIDGET_BUTTON(editMenu, VALUE='Change Axis Labels', /SEPARATOR)
item = WIDGET_BUTTON(editMenu, VALUE='Change Colors/Labels')
item = WIDGET_BUTTON(editMenu, VALUE='Change Text')
item = WIDGET_BUTTON(editMenu, VALUE='Change Window Title')
item = WIDGET_BUTTON(editMenu, VALUE='Add PasteBoard', /SEPARATOR)
item = WIDGET_BUTTON(editMenu, VALUE='Subtract PasteBoard')
item = WIDGET_BUTTON(editMenu, VALUE='Multiply PasteBoard')
item = WIDGET_BUTTON(editMenu, VALUE='Divide PasteBoard')
item = WIDGET_BUTTON(editMenu,VALUE='Superimpose PasteBoard')

;-----------
; View Menu
;-----------
viewMenu = WIDGET_BUTTON(menuBar, VALUE='View', /MENU)
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Date')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle File Name')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Mean Value')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Simulation Label')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Hist Label',/SEPARATOR)
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Hist Fill')

;............
; Window Menu 
;............
windowMenu = WIDGET_BUTTON(menuBar, VALUE='Windows', /MENU)

;...........
; Set back the variables necessary to save
plot.fileMenu = fileMenu
plot.editMenu = editMenu
plot.viewMenu = viewMenu
plot.windowMenu = windowMenu
plot.undo = undo

hist_menu = {file:fileMenu,edit:editMenu,view:viewMenu, $
             window:windowMenu,undo:undo}

WIDGET_CONTROL, menuBar, SET_UVALUE=hist_menu, /NO_COPY

END
