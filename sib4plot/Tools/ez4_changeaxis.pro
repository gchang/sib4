;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
PRO EZ4_ChangeAxis_Panel_Event, event

WIDGET_CONTROL, event.top, GET_UVALUE=infoBase
WIDGET_CONTROL, infoBase, GET_UVALUE=info

button = event.id
IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
  button = info.cancButton

IF button EQ info.ytitleUnits THEN BEGIN
   IF (event.select) THEN info.ytitle=info.units
   WIDGET_CONTROL, info.ytitleField, SET_VALUE=info.ytitle
   WIDGET_CONTROL, info.ytitleField, SENSITIVE=info.ytitleunits
ENDIF

IF button EQ info.okButton THEN BEGIN

  ;Get x and y title info
  WIDGET_CONTROL, info.xtitleField, GET_VALUE=xtitle
  WIDGET_CONTROL, info.ytitleField, GET_VALUE=ytitle
  IF xtitle[0] EQ '' THEN xtitle = ' ' ELSE xtitle = xtitle[0]
  IF ytitle[0] EQ '' THEN ytitle = ' ' ELSE ytitle = ytitle[0]
  info.xtitle = xtitle
  info.ytitle = ytitle

  WIDGET_CONTROL, info.xySizeField, GET_VALUE=xySize
  info.xySize = xySize
  WIDGET_CONTROL, info.xyColorField, GET_VALUE=xyColor
  info.xyColor = xyColor

  info.ok = 1
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
ENDIF ELSE info.ok = 0

IF button EQ info.okButton OR button EQ info.cancButton THEN $
  WIDGET_CONTROL, event.top, /DESTROY

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window to change plot axis labels
;;
FUNCTION EZ4_ChangeAxis_Panel, plot


;; Determine some fields to be added according to plot type
nDataSets = TOTAL(plot.drawLines)

base = WIDGET_BASE(TITLE='Edit Axis Text', SPACE=20, $
                   /COLUMN, /TLB_KILL_REQUEST_EVENTS)

;; Fields for xtitle and ytitle
base1 = WIDGET_BASE(base, /ROW, /ALIGN_LEFT, SPACE=10)
myxtitle      = plot.xtitle
xtitleField = CW_FIELD(base1, TITLE='X-Axis Label:', VALUE=myxtitle, XSIZE=35)
myxySize = plot.xySize
xySizeField = CW_FIELD(base1, TITLE='X-Y Size:', VALUE=myxySize, XSize=4)
myxyColor = plot.xyColor
XYColorField = CW_FIELD(base1, TITLE='X-Y Color:', VALUE=myxyColor, XSize=4)

base2 = WIDGET_BASE(base,/ROW,/ALIGN_LEFT)
myytitle      = plot.ytitle
ytitleField = CW_FIELD(base2, TITLE='Y-Axis Label:', VALUE=myytitle, XSIZE=35)
ytitleUnits   = WIDGET_BUTTON(base2, VALUE=' Set Y-Axis Label to Units ')

units=plot.units
setytitleUnits = (plot.ytitle EQ plot.units)
WIDGET_CONTROL, ytitleUnits, SET_BUTTON=setytitleUnits

row = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, SPACE=100, /FRAME)
okButton   = WIDGET_BUTTON(row, VALUE='   OK   ' )
cancButton = WIDGET_BUTTON(row, VALUE=' CANCEL ' )

info = { ok:0, nDataSets:nDataSets, units:units, $
         xtitle:myxtitle, xtitleField:xtitleField,     $
         setytitleUnits:setyTitleUnits, ytitleUnits:yTitleUnits, $
         ytitle:myytitle, ytitleField:ytitleField,     $
         xysize:myxySize, xySizeField:xySizeField,     $
         xyColor:myxyColor, xyColorField:xyColorField, $
         okButton:okButton, cancButton:cancButton}

infoBase = WIDGET_BASE(UVALUE=info)

WIDGET_CONTROL, base, /REALIZE, /SHOW, SET_UVALUE=infoBase

XMANAGER, 'EZ4_ChangeAxis_Panel', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=info, /DESTROY

RETURN, info

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Change plot axis labels
;;
PRO EZ4_ChangeAxis, plot

;; Display text panel to change title, labels, etc ..
text = EZ4_ChangeAxis_Panel(plot)

IF text.ok THEN BEGIN

;Set X and Y Title Information
plot.xtitle = text.xtitle
plot.ytitle = text.ytitle
plot.xysize = text.xysize
plot.xycolor = text.xycolor

ENDIF

END
