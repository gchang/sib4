;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO Geo4_GetLonLat_Event, event

  COMMON GetLonLat_COM, lonField, latField, $
          outLon, outLat, exitCode

  WIDGET_CONTROL, event.id, GET_UVALUE=name
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') THEN $
    name='CANCEL'

  IF (name EQ 'OK' OR name EQ 'CANCEL') THEN BEGIN
    IF (name EQ 'OK') THEN BEGIN
       WIDGET_CONTROL, lonField, GET_VALUE=outLon
       WIDGET_CONTROL, latField, GET_VALUE=outLat
       exitCode = 1 
    ENDIF ELSE exitCode = 0
    WIDGET_CONTROL, event.top, /DESTROY
    RETURN
  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window to specify a latitude and longitude
;;
;;  outLon:     specified longitude
;;  outLat:	specified latitude
;;
;;  The function returns 1 if 'OK' button pressed an 0 if 'Cancel'
;;
FUNCTION Geo4_GetLonLat, ULon, ULat

  COMMON GetLonLat_COM, lonfield, latfield, $
          outLon, outLat, exitCode

  xs = 8
  outLon = 999.
  outLat = 999.
  base  = WIDGET_BASE(TITLE='Specify Lon/Lat', /COLUMN, $
            /ALIGN_CENTER, /TLB_KILL_REQUEST_EVENTS)
  lonfield = CW_FIELD(base, TITLE='Longitude', $
             XSIZE=xs, /FLOATING)
  latfield = CW_FIELD(base, TITLE='Latitude', $
             XSIZE=xs, /FLOATING)
  sub   = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
  done  = WIDGET_BUTTON(sub, VALUE='OK', UVALUE='OK')
  cancl = WIDGET_BUTTON(sub, VALUE='CANCEL', UVALUE='CANCEL')

  WIDGET_CONTROL, base, /REALIZE
  XMANAGER, 'Geo4_GetLonLat', base, /MODAL

  IF (exitCode EQ 1) THEN BEGIN
      ULon = OutLon
      ULat = OutLat
  ENDIF
  RETURN, exitCode

END
