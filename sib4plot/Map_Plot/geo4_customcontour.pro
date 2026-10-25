;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO GEO4_CustomContour_Panel_Event, event

WIDGET_CONTROL, event.top, GET_UVALUE=infoBase
WIDGET_CONTROL, infoBase, GET_UVALUE=info

button = event.id
IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
    button = info.cancButton

IF button EQ info.okButton THEN BEGIN
   count=1.
   FOR i=0, info.nrows-1 DO BEGIN
       FOR j=0, info.ncols-1 DO BEGIN
           IF (count LT info.ncontours) THEN BEGIN  
               WIDGET_CONTROL, info.cFields(i,j), GET_VALUE=temp
               info.cVals(count) = temp
               count++
           ENDIF
        ENDFOR
    ENDFOR

   info.ok = 1
   WIDGET_CONTROL, infoBase, SET_UVALUE=info
ENDIF ELSE info.ok = 0

IF button EQ info.okButton OR button EQ info.cancButton THEN $
   WIDGET_CONTROL, event.top, /DESTROY

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display a window to get new contour values
;;
FUNCTION GEO4_CustomContour_Panel, plot

base = WIDGET_BASE(TITLE='Specify Custom Contour Levels', SPACE=20, $
       /COLUMN, /TLB_KILL_REQUEST_EVENTS)

ncols=6.
nrows=CEIL(plot.ncontours/ncols)

cVals = plot.contours
cFields = LONARR(nrows,ncols)

ncontours = plot.ncontours
contours = plot.contours
count = 1.
FOR i=0, nrows-1 DO BEGIN
    baset = WIDGET_BASE(base, /ROW, /ALIGN_LEFT)
    FOR j=0, ncols-1 DO BEGIN
        IF (count LT ncontours) THEN BEGIN   
            myTitle='L' + STRING(count,FORMAT='(I2.2)') + ':'
            cFields(i,j) = CW_FIELD(baset, TITLE=mytitle, $
                 VALUE=contours(count), XSIZE=8)
            count++
        ENDIF
     ENDFOR
 ENDFOR

row = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, SPACE=100, /FRAME)
okButton = WIDGET_BUTTON(row, VALUE='  OK  ')
cancButton = WIDGET_BUTTON(row, VALUE=' CANCEL ')

info = { ok:0, nrows:nrows, ncols:ncols, $
         ncontours:ncontours, cVals:cVals, cFields:cFields, $
         okButton:okButton, cancButton:cancButton}

infoBase = WIDGET_BASE(UVALUE=info)

WIDGET_CONTROL, base, /REALIZE, /SHOW, SET_UVALUE=infoBase

XMANAGER, 'GEO4_CustomContour_Panel', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=info, /DESTROY

RETURN, info       

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Change the contours to user specifications
;;
PRO GEO4_CustomContour, plot

;; Display text panel to get the new contours
info = GEO4_CustomContour_Panel(plot)

IF info.ok THEN BEGIN
   plot.contours = info.cVals
ENDIF

GEO4_DrawPlot, plot

END
