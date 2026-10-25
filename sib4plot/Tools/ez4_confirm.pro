;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO EZ4_Confirm_Event, event

WIDGET_CONTROL, event.id, GET_UVALUE=button
IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
  button = 'CANCEL'

IF button EQ 'OK' OR button EQ 'CANCEL' THEN BEGIN
  WIDGET_CONTROL, event.top, GET_UVALUE=infoBase, /DESTROY
  WIDGET_CONTROL, infoBase, SET_UVALUE=(button EQ 'OK')
  RETURN
ENDIF

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GROUP_LEADER:  widget id of parent window
;; messageLines:  text to be displayed. if an array, each element is
;;                displayed in a line
;;
;; Options:
;;
;; OK_BUTTON:     name of OK button, default is Ok
;; CANCEL_BUTTON: name of CANCEL button, default is Cancel
;; XOFFSET:       horizontal offset relative to parent (from left)
;; YOFFSET:       vertical offset relative to parent (from top)
;; NO_BEEP:       do not make beep sound
;;
FUNCTION EZ4_Confirm, GROUP_LEADER=GROUP_LEADER, messageLines, OK_BUTTON=ok, $
                  CANCEL_BUTTON=cancel, XOFFSET=x, YOFFSET=y

IF N_ELEMENTS(ok) EQ 0 THEN ok = '  OK  '
IF N_ELEMENTS(cancel) EQ 0 THEN cancel = 'Cancel'

;; Dummy widget to hold returned info and to be a default GROUP_LEADER
infoBase = WIDGET_BASE()
IF N_ELEMENTS(GROUP_LEADER) EQ 0 THEN GROUP_LEADER = infoBase

select = 0
IF N_ELEMENTS(x) NE 0 THEN BEGIN
  g = WIDGET_INFO(GROUP_LEADER, /GEOMETRY)
  x = x + g.XOFFSET
ENDIF
IF N_ELEMENTS(y) NE 0 THEN BEGIN
  g = WIDGET_INFO(GROUP_LEADER, /GEOMETRY)
  y = y + g.YOFFSET
ENDIF

base = WIDGET_BASE(TITLE='Confirm', /COLUMN,GROUP_LEADER=GROUP_LEADER, $
                   XOFFSET=x, YOFFSET=y, SPACE=30, /TLB_KILL_REQUEST_EVENTS, $
                   /MODAL)
FOR i=0, N_ELEMENTS(messageLines)-1 DO $
  item = WIDGET_LABEL(base, VALUE=messageLines[i], UVALUE='TEXT')
sub  = WIDGET_BASE(base, /ROW, SPACE=60, /ALIGN_CENTER)
item = WIDGET_BUTTON(sub, VALUE=ok, UVALUE='OK')
item = WIDGET_BUTTON(sub, VALUE=cancel, UVALUE='CANCEL')
WIDGET_CONTROL, base, /REALIZE, SET_UVALUE=infoBase

IF KEYWORD_SET(NO_BEEP) EQ 0 THEN PRINT, FORMAT='($, A)', STRING(7b)

XMANAGER, 'EZ4_Confirm', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=select, /DESTROY

RETURN, select

END
