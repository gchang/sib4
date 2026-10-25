PRO ez4_WindowDelete, id

@ez4_BasicParams.com

IF ez4_dead THEN RETURN

; Find the number of windows with plots
N = N_ELEMENTS(ezWindowList)
NP = 0
FOR ii=0,N-1 DO BEGIN
   IF ezWindowList[ii].plot THEN BEGIN
      NP++
   ENDIF
ENDFOR

; Find the info for the deleted window
window  = (WHERE(ezWindowList[*].ID EQ id))[0]
old_one = ezWindowList[window].name

; Update the window list
;...Shift window list elements if needed,
;.. then delete last one
IF window LT N-1 THEN ezWindowList[window:N-2] = ezWindowList[window+1:N-1]
ezWindowList = ezWindowList[0:N-2]
N = N - 1

;Update Windows menu lists for all plot windows
FOR window = 0, N-1 DO BEGIN
  IF ezWindowList[window].plot THEN BEGIN
    menu = ezWindowList[window].menuBar
    WIDGET_CONTROL, menu, GET_UVALUE=gp_menu

    ;Delete old window lists
    FOR ii=0,NP-1 DO BEGIN
        item = WIDGET_INFO(gp_menu.window, /CHILD)
        WIDGET_CONTROL,item,/DESTROY
    ENDFOR

    ;Create new window lists
    wcount=1
    FOR ii=0,N-1 DO BEGIN
        IF ezWindowList[ii].plot THEN BEGIN
           swcount=STRING(wcount)
           menu_name = STRING(wcount,') ', ezWindowList[ii].name, $
                        FORMAT=ezWindowFormat)
           item = WIDGET_BUTTON(gp_menu.window,VALUE=menu_name)
           wcount++
        ENDIF
    ENDFOR

 ENDIF   ;plotting window
ENDFOR ;all windows

END  

