;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO ez4_WindowAdd, id=id, title=title, index=index, menuBar=menuBar, $
                  plot=plot

@ez4_BasicParams.com

IF N_ELEMENTS(title) EQ 0 THEN title=''
IF N_ELEMENTS(index) EQ 0 THEN index = -1
IF N_ELEMENTS(menuBar) EQ 0 THEN menuBar = -1
plot = KEYWORD_SET(plot)
name = title

;; Add parameters for the newly-created window
newwl = {WINDOW_LIST_S, id, name[0], plot, index, menubar}
ezWindowList = [ezWindowList, newwl]
N = N_ELEMENTS(ezWindowList)

; Add all windows to the new window's menu
wcount=1
menu = ezWindowList[N-1].menuBar
IF (menu GE 0) THEN BEGIN
   WIDGET_CONTROL, menu, GET_UVALUE=gp_menu
   FOR window = 1, N-2 DO BEGIN
      IF ezWindowList[window].plot THEN BEGIN
          menu_name = STRING(wcount,') ', ezWindowList[window].name, $
                      FORMAT=ezWindowFormat)
          item = WIDGET_BUTTON(gp_menu.window,VALUE=menu_name)
          wcount++
       ENDIF
   ENDFOR
ENDIF

; Add the new window to the window menu of each plot in the list
IF ezWindowList[N-1].plot THEN BEGIN
menu_name = STRING(wcount,') ', ezWindowList[N-1].name, $
             FORMAT=ezWindowFormat)
FOR window = 1, N-1 DO BEGIN
  IF ezWindowList[window].plot THEN BEGIN
    menu = ezWindowList[window].menuBar
    WIDGET_CONTROL, menu, GET_UVALUE=gp_menu
    item = WIDGET_BUTTON(gp_menu.window, VALUE= menu_name)
  ENDIF
ENDFOR
ENDIF

END  
