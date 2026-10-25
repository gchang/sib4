;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Change window title
;;
PRO Hist_ChangeWindow, plot

COMMON Window_COM

;; Display text panel to change title, labels, etc ..
ok = EZ4_GetField(newTitle, TITLE='Change Window Title', $
          PROMPT='New Window Title: ', $
          ;VALUE=plot.windowTitle, $
          XSIZE=30)

IF ok THEN BEGIN

   plot.windowTitle = newTitle
   Hist_Plot, plot

   wref = N_ELEMENTS(ezWindowList) - 2
   WIDGET_CONTROL, ezWindowList[wref].ID, /DESTROY

ENDIF

END
