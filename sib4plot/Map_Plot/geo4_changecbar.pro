;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO GEO4_ChangeCBar_Panel_Event, event

WIDGET_CONTROL, event.top, GET_UVALUE=infoBase
WIDGET_CONTROL, infoBase, GET_UVALUE=info

button = event.id
IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') THEN $
   button = info.cancButton

IF button EQ info.colorBarCheck THEN BEGIN
   info.showColorBar = event.select
   WIDGET_CONTROL, info.cBarTitleCheck, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarTitleUnits, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarTitleField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarTSizeField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarTColorField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarFormatField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarSizeField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarThickField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, info.cBarColorField, SENSITIVE=info.showColorBar
   WIDGET_CONTROL, infoBase, SET_UVALUE=info
   RETURN
ENDIF

IF button EQ info.cBarTitleCheck THEN BEGIN
   info.showCBarTitle = event.select
   WIDGET_CONTROL, info.cBarTitleUnits, SENSITIVE=info.showCBarTitle
   WIDGET_CONTROL, info.cBarTitleField, SENSITIVE=info.showCBarTitle
   WIDGET_CONTROL, info.cBarTSizeField, SENSITIVE=info.showCBarTitle
   WIDGET_CONTROL, info.cBarTColorField, SENSITIVE=info.showCBarTitle
   WIDGET_CONTROL, infoBase, SET_UVALUE=info
ENDIF

IF button EQ info.cBarTitleUnits THEN BEGIN
   IF (event.select) THEN info.cBarTitle=info.units
   WIDGET_CONTROL, info.cBarTitleField, SET_VALUE=info.cBarTitle
   WIDGET_CONTROL, info.cBarTitleField, SENSITIVE=info.cBarTitleUnits
ENDIF

IF button EQ info.okButton THEN BEGIN
   ;Get all the information
   WIDGET_CONTROL, info.cBartitleField, GET_VALUE=title
   info.cBarTitle=title
   WIDGET_CONTROL, info.cBarTSizeField, GET_VALUE=tsize
   info.cBarTSize=tsize
   WIDGET_CONTROL, info.cBarTColorField, GET_VALUE=tcolor
   info.cBarTColor=tcolor
   WIDGET_CONTROL, info.cBarFormatField, GET_VALUE=cbarFormat
   IF (cBarformat NE '') THEN $
       info.cBarFormat = '(' + cBarFormat + ')'
   WIDGET_CONTROL, info.cBarSizeField, GET_VALUE=cbarsize
   info.cBarSize = cbarsize
   WIDGET_CONTROL, info.cBarThickField, GET_VALUE=cbarthick
   info.cBarThick = cbarthick
   WIDGET_CONTROL, info.cBarColorField, GET_VALUE=cbarcolor
   info.cBarColor = cbarcolor

   info.ok = 1
   WIDGET_CONTROL, infoBase, SET_UVALUE=info
ENDIF ELSE info.ok = 0

IF button EQ info.okButton OR button EQ info.cancButton THEN $
   WIDGET_CONTROL, event.top, /DESTROY

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window to change the colorbar for contour plots
;;
FUNCTION GEO4_ChangeCBar_Panel, plot

units=plot.units

base = WIDGET_BASE(TITLE='Edit Contour Plot ColorBar', SPACE=20, $
                   /COLUMN, /TLB_KILL_REQUEST_EVENTS)

;; Units Fields
base1 = WIDGET_BASE(base, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
colorbarCheck = WIDGET_BUTTON(base1, VALUE='Show Color Bar')

base2 = WIDGET_BASE(base, /ROW, /ALIGN_LEFT, XPAD=0, YPAD=0, /NONEXCLUSIVE)
cbarTitleCheck = WIDGET_BUTTON(base2, VALUE='Show Title')

base3 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
cBarTitle = plot.cBarTitle
cbarTitleUnits = WIDGET_BUTTON(base3, VALUE='Set Title To Units')
cbarTitleField = CW_FIELD(base3, TITLE='Title:', VALUE=cBarTitle, XSIZE=20)

base4 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
cBarTSize = plot.cBarTSize
cBarTSizeField = CW_FIELD(base4, TITLE='Title Size:', VALUE=cBarTSize, XSIZE=3)
cBarTColor = plot.cBarTColor
cBarTColorField = CW_FIELD(base4, TITLE='Title Color:', VALUE=cBarTColor, XSIZE=2)

base5 = WIDGET_BASE(base, /ROW, /ALIGN_LEFT)
cBarFormat = STRMID(plot.cbarLFormat, 1, STRLEN(plot.cbarLFormat)-2)
cBarFormatField = CW_FIELD(base5, TITLE='ColorBar Format:', VALUE=cBarFormat, XSIZE=6)
cBarSize = plot.cbarLSize
cBarSizeField = CW_FIELD(base5, TITLE='Size:', VALUE=cBarSize, XSIZE=3)
cBarThick = plot.cbarLThick
cBarThickField = CW_FIELD(base5, TITLE='Thick:',VALUE=cBarThick, XSIZE=3)
cBarColor = plot.cbarLColor
cBarColorField = CW_FIELD(base5, TITLE='Color:', VALUE=cBarColor, XSIZE=2)

showColorBar = plot.colorbar
WIDGET_CONTROL, colorbarCheck, SET_BUTTON=showColorBar
WIDGET_CONTROL, cBarTitleCheck, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarTitleUnits, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarTitleField, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarTSizeField, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarTColorField, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarFormatField, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarSizeField, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarThickField, SENSITIVE=showColorBar
WIDGET_CONTROL, cBarColorField, SENSITIVE=showColorBar

showcBarTitle = plot.showcBarTitle
IF (showColorBar) THEN BEGIN
   WIDGET_CONTROL, cbarTitleCheck, SET_BUTTON=showcBarTitle
   WIDGET_CONTROL, cBarTitleUnits, SENSITIVE=showCBarTitle
   WIDGET_CONTROL, cBarTitleField, SENSITIVE=showCBarTitle
   WIDGET_CONTROL, cBarTSizeField, SENSITIVE=showCBarTitle
   WIDGET_CONTROL, cBarTColorField, SENSITIVE=showCBarTitle
ENDIF

setcBarTitleUnits = (plot.cBarTitle EQ plot.units)
WIDGET_CONTROL, cbarTitleUnits, SET_BUTTON=setcBarTitleUnits

;; OK and Cancel Buttons
row = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, SPACE=100, /FRAME)
okButton = WIDGET_BUTTON(row, VALUE='  OK  ')
cancButton = WIDGET_BUTTON(row, VALUE=' CANCEL ')

info = { ok:0, units:units, $
         showColorBar:showColorBar, colorBarCheck:colorBarCheck, $
         showcBarTitle:showCBarTitle, cbarTitleCheck:cbarTitleCheck, $
         setcBarTitleUnits:setCBarTitleUnits, cbarTitleUnits:cbarTitleUnits, $
         cBarTitle:cBarTitle, cBarTitleField:cBarTitleField, $
         cBarTSize:cBarTSize, cBarTSizeField:cBarTSizeField, $
         cBarTColor:cBarTColor, cBarTColorField:cBarTColorField, $
         cBarFormat:cBarFormat, cBarFormatField:cBarFormatField, $
         cBarSize:cBarSize, cBarSizeField:cBarSizeField, $
         cBarThick:cBarThick, cBarThickField:cBarThickField, $
         cBarColor:cBarColor, cBarColorField:cBarColorField, $
         okButton:okButton, cancButton:cancButton}

infoBase = WIDGET_BASE(UVALUE=info)
WIDGET_CONTROL, base, /REALIZE, /SHOW, SET_UVALUE=infoBase
XMANAGER, 'GEO4_ChangeCBar_Panel', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=info, /DESTROY

RETURN, info
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Change various colorbar items
;;
PRO GEO4_ChangeCBar, plot

;; Display text panel to change units, color, etc ..
info = GEO4_ChangeCBar_Panel(plot)

IF info.ok THEN BEGIN
    ;Set the information to the new values
    plot.colorbar = info.showColorBar
    plot.cBarTitle = info.cBarTitle
    plot.showCBarTitle = info.showCBarTitle
    plot.cBarTSize = info.cBarTSize
    plot.cBarTColor = info.cBarTColor

    plot.cBarLFormat = info.cBarFormat
    plot.cBarLSize = info.cBarSize
    plot.cBarLThick = info.cBarThick
    plot.cBarLColor = info.cBarColor

ENDIF

GEO4_DrawPlot, plot

END
