;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Compute y-axis ticks, labels, and scale
;;
PRO ez4_CompYTicks, ydatamin, ydatamax, $
                numyticks, ytv, ytlabels

datadiff = ydatamax-ydatamin
IF ((datadiff EQ 0.) OR $
    ((datadiff LT 1.E-3) AND (ydatamin GT 1.))) THEN BEGIN
    numyticks = 5
    tickint = 0.1

    ytv = FLTARR(numyticks)
    ytv = [ydatamin-2*tickint,ydatamin-tickint,ydatamin, $
           ydatamin+tickint,ydatamin+2*tickint]
    fmt = '(f10.1)'

ENDIF ELSE BEGIN
  numyticks = 8.
  tickint = (ydatamax - ydatamin)/(numyticks-1.)

  ytv = DBLARR(numyticks)
  ytv[0:numyticks-1] = ydatamin + FINDGEN(numyticks)*tickint
  fmt = ez4_niceformat(tickint,/FLT)

ENDELSE

ytlabels = string(ytv,format=fmt)


END
