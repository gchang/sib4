PRO EZ4_ALERT_EVENT, event

COMMON ALERT_COM, parent, ok

CASE event.id OF

  ok: BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
    WIDGET_CONTROL, parent, /DESTROY
  END

ENDCASE

END


;======================================================================
;;
;;  Display alert and errors messages
;;
PRO EZ4_ALERT, message

COMMON ALERT_COM, parent, ok

parent  = WIDGET_BASE()        ; dummy parent to make the alert window a modal widget
base    = WIDGET_BASE(TLB_FRAME_ATTR=7, GROUP_LEADER=parent, /MODAL)

subBase = WIDGET_BASE(base, SPACE=10, FRAME=10, /COLUMN)
text    = WIDGET_LABEL(subBase, VALUE='E Z P L O T    A L E R T')

textBase = WIDGET_BASE(subBase, /COLUMN, FRAME=5, SPACE=5)
text     = WIDGET_LABEL(textBase, VALUE=' ')

xsize = (MAX(STRLEN(message)) > 60)*8
FOR i=0, N_ELEMENTS(message)-1 DO $
  text = WIDGET_LABEL(textBase, VALUE=message[i], XSIZE=xsize, /ALIGN_CENTER)

text = WIDGET_LABEL(textBase, VALUE=' ')

bttonBase  = WIDGET_BASE(subBase, /COLUMN, XSIZE=xsize, /BASE_ALIGN_CENTER)
buttonBase = WIDGET_BASE(bttonBase, /ROW, SPACE=80)
ok = WIDGET_BUTTON(buttonBase, VALUE='  O K  ')

WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'ez4_alert', base

END
