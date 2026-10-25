
PRO EZ4_MESSAGE_EVENT, event

WIDGET_CONTROL, event.top, /DESTROY

END


;======================================================================
;;
;;  Display messages
;;
PRO EZ4_Message, message, TITLE=title

IF (N_ELEMENTS(title) EQ 0) THEN title='Message'
base = WIDGET_BASE(TITLE=title, /ALIGN_CENTER, /COLUMN, $
                /TLB_KILL_REQUEST_EVENTS)

text = WIDGET_LABEL(base, VALUE=' ')

xsize = (MAX(STRLEN(message)) > 20)*10
ndims = (SIZE(message))[0]
IF (ndims EQ 1) THEN nrows=(SIZE(message))[1] ELSE nrows=1
ysize = MIN([40,MAX([20,nrows*2])])

FOR i=0, N_ELEMENTS(message)-1 DO $
  text = WIDGET_LABEL(base, VALUE=message[i], $
          XSIZE=xsize, YSIZE=ysize, /ALIGN_CENTER)

text = WIDGET_LABEL(base, VALUE=' ')

buttonBase  = WIDGET_BASE(base, /ROW, XSIZE=30, /ALIGN_CENTER, /FRAME)
ok = WIDGET_BUTTON(buttonBase, VALUE='OK')

WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'ez4_message', base

END
