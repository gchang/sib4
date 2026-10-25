;======================================================================
PRO Line_Widget_Menu, plot
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
item = WIDGET_BUTTON(editMenu, VALUE='Paste Text')
item = WIDGET_BUTTON(editMenu, VALUE='Paste Parameters')
item = WIDGET_BUTTON(editMenu, VALUE='Change Axis Labels',/SEPARATOR)
item = WIDGET_BUTTON(editMenu, VALUE='Change Lines')
item = WIDGET_BUTTON(editMenu, VALUE='Change Text')
item = WIDGET_BUTTON(editMenu, VALUE='Change Window Title')
item = WIDGET_BUTTON(editMenu, VALUE='Add PasteBoard', /SEPARATOR)
item = WIDGET_BUTTON(editMenu, VALUE='Subtract PasteBoard')
item = WIDGET_BUTTON(editMenu, VALUE='Multiply PasteBoard')
item = WIDGET_BUTTON(editMenu, VALUE='Divide PasteBoard')
item = WIDGET_BUTTON(editMenu, VALUE='Superimpose PasteBoard')

;.............
; View Menu
;............
viewMenu = WIDGET_BUTTON(menuBar, VALUE='View', /MENU)
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Date')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle File Name')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Mean Value')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Simulation Label')
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Line Label',/SEPARATOR)
item = WIDGET_BUTTON(viewMenu, VALUE='Toggle Line Color/Style')

;............
; Analysis Menu (for all but Diurnal Composite windows)
;............
analysisMenu = WIDGET_BUTTON(menuBar, VALUE='Analysis', /MENU)
item = WIDGET_BUTTON(analysisMenu, VALUE='Switch GMT/LST')
diurnal = WIDGET_BUTTON(analysisMenu, VALUE='Diurnal Composite')
IF (plot.timeperday EQ 0) THEN BEGIN
    WIDGET_CONTROL, item, SENSITIVE=0
    WIDGET_CONTROL, diurnal, SENSITIVE=0
ENDIF
item = WIDGET_BUTTON(analysisMenu, VALUE='Add a constant', /SEPARATOR)
item = WIDGET_BUTTON(analysisMenu, VALUE='Subtract a constant')
item = WIDGET_BUTTON(analysisMenu, VALUE='Multiply by constant')
item = WIDGET_BUTTON(analysisMenu, VALUE='Divide by constant')
item = WIDGET_BUTTON(analysisMenu, VALUE='Anomaly', /SEPARATOR)
item = WIDGET_BUTTON(analysisMenu, VALUE='Reciprocal')

;............
; Window Menu 
;............
windowMenu = WIDGET_BUTTON(menuBar, VALUE='Windows', /MENU)

;.........................................
; Set back the variables necessary to save
plot.fileMenu=fileMenu
plot.editMenu=editMenu
plot.viewMenu=viewMenu
plot.analysisMenu=analysisMenu
plot.windowMenu=windowMenu
plot.undo=undo

line_menu = {file:fileMenu,edit:editMenu,view:viewMenu, $
           analysis:analysisMenu,window:windowMenu, $
           diurnal:diurnal, undo:undo}

WIDGET_CONTROL, menuBar, SET_UVALUE=line_menu, /NO_COPY

END
