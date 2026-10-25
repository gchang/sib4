;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Build pull-down menus and attach them to the menubar of the window
;;
PRO GEO4_Widget_Menu, plot

@ez4_BasicParams.com

; Set local variables needed
menuBar = plot.menuBar

;...................
; FILE MENU
;...................
fileMenu = WIDGET_BUTTON(menuBar,VAL='File',/MENU,RESOURCE='fileMenu')
item = WIDGET_BUTTON(fileMenu,VAL='Save',RESOURCE='Save')
item = WIDGET_BUTTON(fileMenu, VAL='Save As ...')
item = WIDGET_BUTTON(fileMenu,VAL='Print ...',RES='Print')
item = WIDGET_BUTTON(fileMenu,VAL='Close',RESOURCE='Close')
item = WIDGET_BUTTON(fileMenu,VAL='Quit EZPLOT',RES='Quit')

;...................
; EDIT MENU
;...................
editMenu = WIDGET_BUTTON(menuBar,VAL='Edit',/MENU,RESOURCE='editMenu')
undo = WIDGET_BUTTON(editMenu, VAL='Undo')
WIDGET_CONTROL, undo, SENSITIVE=0
item = WIDGET_BUTTON(editMenu, VAL='Copy', RESOURCE='Copy')
item = WIDGET_BUTTON(editMenu, VAL='Paste Text', /SEPARATOR)
item = WIDGET_BUTTON(editMenu, VAL='Paste Parameters')
item = WIDGET_BUTTON(editMenu,VAL='Change Text')
item = WIDGET_BUTTON(editMenu,VAL='Add PasteBoard', RES='Add', /SEPARATOR)
item = WIDGET_BUTTON(editMenu,VAL='Subtract PasteBoard', RES='Subtract')
item = WIDGET_BUTTON(editMenu,VAL='Multiply PasteBoard', RES='Multiply')
item = WIDGET_BUTTON(editMenu,VAL='Divide PasteBoard', RES='Divide')
;item = WIDGET_BUTTON(editMenu,VAL='Change Lat/Lon Labels')
item = WIDGET_BUTTON(editMenu,VAL='Edit ColorBar', /SEPARATOR)

;...................
; VIEW MENU 
;...................
viewMenu = WIDGET_BUTTON(menuBar, VAL='View', /MENU)
;item = WIDGET_BUTTON(viewMenu,VAL='Grid-box Plot')

; Color Section
colorMenu = WIDGET_BUTTON(viewMenu, VAL='Color Scheme', /MENU, /SEPARATOR)
item=WIDGET_BUTTON(colorMenu,VAL='Classic Rainbow')
item=WIDGET_BUTTON(colorMenu,VAL='Reverse Rainbow')
item=WIDGET_BUTTON(colorMenu, VAL='12-Color')
item=WIDGET_BUTTON(colorMenu, VAL='Reverse 12-Color')
item=WIDGET_BUTTON(colorMenu, VAL='Blue/White/Red')
item=WIDGET_BUTTON(colorMenu, VAL='Reverse Blue/White/Red')
item=WIDGET_BUTTON(colorMenu, VAL='White/Green')
item=WIDGET_BUTTON(colorMenu, VAL='Gray-Scale')
item=WIDGET_BUTTON(colorMenu, VAL='Reverse Gray-Scale')

; Contour Section
contourMenu = WIDGET_BUTTON(viewMenu, VAL='Contour', /MENU)
item = WIDGET_BUTTON(contourMenu, VAL='Even Min to Max')
item = WIDGET_BUTTON(contourMenu, VAL='Center on Mean')
item = WIDGET_BUTTON(contourMenu, VAL='Center on Zero')
item = WIDGET_BUTTON(contourMenu, VAL='Center on Value')
item = WIDGET_BUTTON(contourMenu, VAL='Custom ...')

; Map Projection Section
mapMenu = WIDGET_BUTTON(viewMenu, VAL='Projection', /MENU)
item = WIDGET_BUTTON(mapMenu, VAL='Cylindrical Equidistant')
item = WIDGET_BUTTON(mapMenu, VAL='Hammer-Aitoff Equal Area')
item = WIDGET_BUTTON(mapMenu, VAL='Lambert Equal Area')
item = WIDGET_BUTTON(mapMenu, VAL='Mercator')
item = WIDGET_BUTTON(mapMenu, VAL='Miller Cylindrical')
item = WIDGET_BUTTON(mapMenu, VAL='Mollweide')
item = WIDGET_BUTTON(mapMenu, VAL='Orthographic')
item = WIDGET_BUTTON(mapMenu, VAL='Robinson Pseudo-Cylindrical')
item = WIDGET_BUTTON(mapMenu, VAL='Stereographic')
IF (plot.region EQ 'Globe') THEN $
   WIDGET_CONTROL, item, SENSITIVE=0

; Toggle Section
toggleMenu = WIDGET_BUTTON(viewMenu, VAL='Toggle', /MENU)
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Date')
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle File Name')
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Global Mean')
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Simulation Label')

item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Border', /SEPARATOR)
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Grid Lines')
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Lat/Lon Labels')
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Map Center')

item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Isotropic', /SEPARATOR)

item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Continents', /SEPARATOR)
hires = WIDGET_BUTTON(toggleMenu, VAL='Toggle High Resolution')
coasts = WIDGET_BUTTON(toggleMenu, VAL='Toggle Coastlines')
countries = WIDGET_BUTTON(toggleMenu, VAL='Toggle Countries')
usa = WIDGET_BUTTON(toggleMenu, VAL='Toggle USA States')

item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Contour Lines', /SEPARATOR)
item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Contour Labels')
item = WIDGET_BUTTON(toggleMenu,VAL='Match Contour Lines/Shades')

item = WIDGET_BUTTON(toggleMenu, VAL='Toggle Color Bar', /SEPARATOR)

; White Out Section
whiteMenu = WIDGET_BUTTON(viewMenu, VAL='White Out', /MENU)
item = WIDGET_BUTTON(whiteMenu, VAL='Lowest Values')
item = WIDGET_BUTTON(whiteMenu, VAL='Center Values')
item = WIDGET_BUTTON(whiteMenu, VAL='Middle Values')
item = WIDGET_BUTTON(whiteMenu, VAL='High Values')
item = WIDGET_BUTTON(whiteMenu, VAL='Restore Colors')

;...................
; ANALYSIS MENU
;...................
analysisMenu = WIDGET_BUTTON(menuBar, VAL='Analysis', /MENU)

; Arithmatic Section
arithMenu = WIDGET_BUTTON(analysisMenu, VAL='Arithmetic', /MENU, /SEPARATOR)
item = WIDGET_BUTTON(arithMenu, VAL='Anomaly')
item = WIDGET_BUTTON(arithMenu, VAL='Reciprocal')
item=WIDGET_BUTTON(arithMenu,VAL='Add a constant', /SEPARATOR)
item = WIDGET_BUTTON(arithMenu, VAL='Subtract a constant')
item = WIDGET_BUTTON(arithMenu, VAL='Multiply by constant')
item = WIDGET_BUTTON(arithMenu, VAL='Divide by constant')


; Stats Section
statsMenu = WIDGET_BUTTON(analysisMenu, VAL='Statistics', /MENU)
item = WIDGET_BUTTON(statsMenu, VAL='Frequency Distribution')
item = WIDGET_BUTTON(statsMenu, VAL='Single Grid Cell Value')
item = WIDGET_BUTTON(statsMenu, VAL='Zonal Mean')

;.....................
; REGION MENU
;......................
regionMenu = WIDGET_BUTTON(menuBar, VAL='Region', /MENU, $
          RESOURCE='regionMenu')
continentMenu = WIDGET_BUTTON(regionMenu, VAL='Continents', /MENU)
item = WIDGET_BUTTON(continentMenu, VAL='AFRICA')
item = WIDGET_BUTTON(continentMenu, VAL='ASIA')
item = WIDGET_BUTTON(continentMenu, VAL='AUSTRALIA')
item = WIDGET_BUTTON(continentMenu, VAL='EUROPE')
item = WIDGET_BUTTON(continentMenu, VAL='NA')
item = WIDGET_BUTTON(continentMenu, VAL='SA')

countryMenu = WIDGET_BUTTON(regionMenu, VAL='Countries', /MENU)
item = WIDGET_BUTTON(countryMenu, VAL='BRAZIL')
item = WIDGET_BUTTON(countryMenu, VAL='CHINA')
item = WIDGET_BUTTON(countryMenu, VAL='USA')

regionMenu2 = WIDGET_BUTTON(regionMenu, VAL='Regions', /MENU)
item = WIDGET_BUTTON(regionMenu2, VAL='BOREAS')
item = WIDGET_BUTTON(regionMenu2, VAL='LBA')
item = WIDGET_BUTTON(regionMenu2, VAL='TROPICS')

item = WIDGET_BUTTON(regionMenu, VAL='Globe', /SEPARATOR)
item = WIDGET_BUTTON(regionMenu, VAL='Custom Region')

;...................
; WINDOW MENU
;...................
windowMenu = WIDGET_BUTTON(menuBar, VAL='Windows', /MENU)

;.........................................
; Set back the variables necessary to save
plot.fileMenu=fileMenu
plot.editMenu=editMenu
plot.viewMenu=viewMenu
plot.analysisMenu=analysisMenu
plot.regionMenu=regionMenu
plot.windowMenu=windowMenu
plot.undo=undo

geo4_menu = {file:fileMenu, edit:editMenu, $
             view:viewMenu, analysis:analysisMenu, $
             region:regionMenu,window:windowMenu, $
             undo:undo, hires:hires, coasts:coasts, $
             countries:countries, usa:usa }
WIDGET_CONTROL, menuBar, SET_UVALUE=geo4_menu, /NO_COPY


END
