
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO EZ4_GetField_Event, event

  COMMON GetField_COM, newValue, exitCode

  WIDGET_CONTROL, event.id, GET_UVALUE=name
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') THEN $
    name='CANCEL'

  IF name EQ 'FIELD' THEN BEGIN
    IF event.update THEN name = 'OK' ELSE newValue = event.value
  ENDIF

  IF name EQ 'OK' OR name EQ 'CANCEL' THEN BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
    IF name EQ 'OK' THEN exitCode = 1 ELSE exitCode = 0
    RETURN
  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window for the user to input a value
;;
;;  outValue:		variable that will hold the typed value upon exit
;;  title:		Optional title
;;  prompt:		Optional propmt
;;  init_value:		Optional initial value to be displayed
;;  FLOATING, INTEGER:	Optional data type, default is string
;;
;;  The function returns 1 if 'OK' button pressed an 0 if 'Cancel'
;;
FUNCTION EZ4_GetField, outValue, TITLE=title, PROMPT=prompt, VALUE=init_value, $
                   FLOATING=FLOATING, INTEGER=INTEGER, XSIZE=xs

  COMMON GetField_COM, newValue, exitCode

  newValue = (N_ELEMENTS(init_value) NE 0) ?init_value :''
  IF N_ELEMENTS(title) EQ 0 THEN title = 'Enter Field'
  IF N_ELEMENTS(prompt) EQ 0 THEN prompt = ''
  IF N_ELEMENTS(xs) EQ 0 THEN xs = 50
  floating = KEYWORD_SET(FLOATING)
  integer  = KEYWORD_SET(INTEGER)
  string   = NOT floating AND NOT integer
  IF floating AND integer THEN integer = 0
  IF N_ELEMENTS(init_value) EQ 0 THEN BEGIN
    IF floating THEN init_value = 0.0 ELSE IF integer THEN init_value = 0 $
    ELSE init_value = ''
  ENDIF

  base  = WIDGET_BASE(TITLE=title, /COLUMN, /TLB_KILL_REQUEST_EVENTS)
  field = CW_FIELD(base, TITLE=prompt, VALUE=init_value, UVALUE='FIELD', $
                   XSIZE=xs, /ALL_EVENTS, FLOATING=floating, INTEGER=integer)
  sub   = WIDGET_BASE(base, /ROW, SPACE=10, /ALIGN_CENTER)
  done  = WIDGET_BUTTON(sub, VALUE='OK', UVALUE='OK')
  cancl = WIDGET_BUTTON(sub, VALUE='Cancel', UVALUE='CANCEL')

  WIDGET_CONTROL, base, /REALIZE
  XMANAGER, 'EZ4_GetField', base, /MODAL

  IF exitCode EQ 1 THEN outValue = newValue
  RETURN, exitCode

END
