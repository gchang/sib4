PRO Line_ChangeLineStyle, plot

@ez4_BasicParams.com

;Change line color/style if toggling between them
maxstyle = MAX(plot.linestyle)
changetostyle=1
IF (maxstyle GT 0) THEN changetostyle=0

IF (changetostyle EQ 0) THEN BEGIN
    col = 1 & sty = 0
    coladd = 1 & styadd = 0
ENDIF ELSE BEGIN
    col = 0 & sty = 2
    coladd = 0 & styadd = 1
ENDELSE

FOR i=1,plot.numlevs-1 DO BEGIN
    IF (plot.drawlines[i] GT 0) THEN BEGIN
        plot.linecolor[i] = col
        col += coladd
        plot.linestyle[i] = sty
        sty += styadd
     ENDIF
ENDFOR

END
