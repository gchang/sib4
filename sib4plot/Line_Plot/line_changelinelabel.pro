;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
PRO Line_ChangeLineLabel_Panel_Event, event

WIDGET_CONTROL, event.top, GET_UVALUE=infoBase
WIDGET_CONTROL, infoBase, GET_UVALUE=info

eventName = WIDGET_INFO(event.id,/UNAME)

CASE eventName OF

  'CANCEL' : WIDGET_CONTROL,event.top,/DESTROY
  'OK' : BEGIN
      FOR i=0, info.nDataSets-1 DO BEGIN
          WIDGET_CONTROL, info.linenameField[i], GET_VALUE=tmp
          IF (tmp[0] EQ '') THEN info.linename[i] = ' ' $
          ELSE info.linename[i] = tmp[0]
          WIDGET_CONTROL, info.linecolorField[i], GET_VALUE=tmp
          info.linecolor[i]=tmp
          WIDGET_CONTROL, info.linestyleField[i], GET_VALUE=tmp
          info.linestyle[i]=tmp
          WIDGET_CONTROL, info.linethickField[i], GET_VALUE=tmp
          info.linethick[i]=tmp
      ENDFOR
      WIDGET_CONTROL, infobase, SET_UVALUE=info
      WIDGET_CONTROL, event.top, /DESTROY
  END

  'DRAW LINE' : BEGIN
      FOR i=0, info.nDataSets-1 DO BEGIN
          bset = WIDGET_INFO(info.linedrawid[i], /BUTTON_SET)
          IF (info.linedraw[i] NE bset) THEN BEGIN
             info.linedraw[i] = 1 - info.linedraw[i]
          ENDIF

          IF (info.linedraw[i] EQ 0) THEN BEGIN
              WIDGET_CONTROL, info.linenameField[i], SENSITIVE=0
              WIDGET_CONTROL, info.linecolorField[i], SENSITIVE=0
              WIDGET_CONTROL, info.linestyleField[i], SENSITIVE=0
              WIDGET_CONTROL, info.linethickField[i], SENSITIVE=0
          ENDIF ELSE BEGIN
              IF (info.showlabels EQ 1) THEN $
                  WIDGET_CONTROL, info.linenameField[i], SENSITIVE=1
              WIDGET_CONTROL, info.linecolorField[i], SENSITIVE=1
              WIDGET_CONTROL, info.linestyleField[i], SENSITIVE=1
              WIDGET_CONTROL, info.linethickField[i], SENSITIVE=1
           ENDELSE
      ENDFOR
      WIDGET_CONTROL, infobase, SET_UVALUE=info   
  END

  'SHOW LABELS' : BEGIN
       info.showlabels = 1. - info.showlabels
       IF (info.showlabels EQ 0) THEN BEGIN
          FOR i=0, info.nDataSets-1 DO BEGIN
              WIDGET_CONTROL, info.linenameField[i], SENSITIVE=0
          ENDFOR
       ENDIF ELSE BEGIN
          FOR i=0, info.nDataSets-1 DO BEGIN
              IF (info.linedraw[i] EQ 1) THEN $
                  WIDGET_CONTROL, info.linenameField[i], SENSITIVE=1
          ENDFOR
       ENDELSE
       WIDGET_CONTROL, infobase, SET_UVALUE=info
   END    
  
ENDCASE ;eventName


END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window to change plot line, including whether to draw and
;;  line color and line style.
;;
FUNCTION Line_ChangeLineLabel_Panel, plot

;; Set local variables
nDataSets  = plot.numlevs
showlabels = plot.showLineLabel

linedraw  = plot.drawlines
linename  = plot.linename
linecolor = plot.linecolor
linestyle = plot.linestyle
linethick = plot.linethick

;; Setup widget window
base = WIDGET_BASE(TITLE='Edit Lines', SPACE=20, $
                   /COLUMN, /TLB_KILL_REQUEST_EVENTS)

sub1 = WIDGET_BASE(base, /ALIGN_CENTER, /NONEXCLUSIVE)
labelid = WIDGET_BUTTON(sub1, /ALIGN_CENTER, $
   VALUE='Show Line Labels', UNAME='SHOW LABELS')
IF (showlabels EQ 1) THEN $
   WIDGET_CONTROL, labelid, SET_BUTTON=1 $
ELSE WIDGET_CONTROL, labelid, SET_BUTTON=0


;; Title, color, and linestyle fields for each line
linedrawid = LONARR(nDataSets, /NOZERO)
linenameField  = LONARR(nDataSets, /NOZERO)
linecolorField = linenameField
linestyleField = linenameField
linethickField = linenameField

sub = WIDGET_BASE(base, /COLUMN, SPACE=2)
row = WIDGET_BASE(sub, /ROW, /BASE_ALIGN_CENTER)

FOR i=0, nDataSets-1 DO BEGIN
    row = WIDGET_BASE(sub, /ROW, /BASE_ALIGN_CENTER)
    linenameField[i] = CW_FIELD(row, TITLE='Line Name ' + STRCOMPRESS(STRING(i+1)), $
                                VALUE=linename[i], XSIZE=15)
    linecolorField[i] = CW_FIELD(row, TITLE='Color ', $
                                VALUE=fix(linecolor[i]), XSIZE=3)
    linestyleField[i] = CW_FIELD(row, TITLE='Style ', $
                                VALUE=fix(linestyle[i]), XSIZE=3)
    linethickField[i] = CW_FIELD(row, TITLE='Thick ', $
                                VALUE=fix(linethick[i]), XSIZE=3)

    brow = WIDGET_BASE(row, /ROW, /BASE_ALIGN_RIGHT, /NONEXCLUSIVE)
    linedrawid[i] = WIDGET_BUTTON(brow, /ALIGN_RIGHT, $
                         VALUE='Draw', UNAME='DRAW LINE')
    IF (linedraw[i] EQ 0) THEN BEGIN
        WIDGET_CONTROL, linedrawid[i], SET_BUTTON=0
        WIDGET_CONTROL, linenameField[i], SENSITIVE=0
        WIDGET_CONTROL, linecolorField[i], SENSITIVE=0
        WIDGET_CONTROL, linestyleField[i], SENSITIVE=0
        WIDGET_CONTROL, linethickField[i], SENSITIVE=0
    ENDIF ELSE BEGIN
        WIDGET_CONTROL, linedrawid[i], SET_BUTTON=1
    ENDELSE

ENDFOR

IF (showlabels EQ 0) THEN BEGIN
   FOR i=0,nDataSets-1 DO BEGIN
       WIDGET_CONTROL, linenameField[i], SENSITIVE=0
   ENDFOR
ENDIF ELSE BEGIN
   FOR i=0,nDataSets-1 DO BEGIN
       IF (linedraw[i] EQ 1) THEN BEGIN
           WIDGET_CONTROL, linenameField[i], SENSITIVE=1
       ENDIF ELSE BEGIN
           WIDGET_CONTROL, linenameField[i], SENSITIVE=0
       ENDELSE
   ENDFOR
ENDELSE

row = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, SPACE=100, /FRAME)
okButton   = WIDGET_BUTTON(row, VALUE='   OK   ', UNAME='OK' )
cancButton = WIDGET_BUTTON(row, VALUE=' CANCEL ', UNAME='CANCEL' )

info = { nDataSets:nDataSets, $
         labelid:labelid, showlabels:showlabels, $
         linename:linename, linenameField:linenameField,     $
         linestyle:linestyle, linestyleField:linestyleField, $
         linecolor:linecolor, linecolorField:linecolorField, $
         linethick:linethick, linethickField:linethickField, $
         linedrawid:linedrawid, linedraw:linedraw}

infoBase = WIDGET_BASE(UVALUE=info)

WIDGET_CONTROL, base, /REALIZE, /SHOW, SET_UVALUE=infoBase

XMANAGER, 'Line_ChangeLineLabel_Panel', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=info, /DESTROY

RETURN, info

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Change plot line information
;;
PRO Line_ChangeLineLabel, plot

;; Display text panel
info = Line_ChangeLineLabel_Panel(plot)
plot.showlineLabel = info.showlabels
FOR i=0,plot.numlevs-1 DO BEGIN
    plot.drawlines(i) = info.linedraw(i)
    plot.linename(i)  = info.linename(i)
    plot.linecolor(i) = info.linecolor(i)
    plot.linestyle(i) = info.linestyle(i)
    plot.linethick(i) = info.linethick(i)
ENDFOR

END
