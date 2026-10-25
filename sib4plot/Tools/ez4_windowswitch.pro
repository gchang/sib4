;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Show and set focus to the requested plot window
;;
PRO ez4_WindowSwitch, window_name

@ez4_BasicParams.com

N = N_ELEMENTS(ezWindowList)
wcount=1
wref=-1
FOR ii=0,N-1 DO BEGIN
    IF ezWindowList[ii].plot THEN BEGIN
       name = STRING(wcount,') ', ezWindowList[ii].name, $
              FORMAT=ezWindowFormat)
       IF (name EQ window_name) THEN wref=ii
       wcount++
    ENDIF
ENDFOR

IF wref NE -1 THEN BEGIN
  IF ezWindowList[wref].ID NE -1 THEN BEGIN
    WIDGET_CONTROL, ezWindowList[wref].ID, /SHOW, ICONIFY=0
    IF ezWindowList[wref].plot THEN BEGIN
      WSET, ezWindowList[wref].index
      RETURN
    ENDIF
  ENDIF
ENDIF

END
