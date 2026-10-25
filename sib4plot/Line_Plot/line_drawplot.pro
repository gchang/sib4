;======================================================================
PRO Line_DrawPlot, plot, _EXTRA=extra
;======================================================================
; draw a line plot

@ez4_BasicParams.com

EZ4_SetFont
IF (NOT dopsplot) THEN BEGIN
   EZ4_SetFont, NameFont=labelFont, SizeFont=labelSize
ENDIF

; x-axis settings
xrmin = plot.minx
xrmax = plot.maxx
EZ4_COMPXTICKS, xrmin, xrmax, numxticks, xtv

recmin=0
recmax=plot.numdata-1
FOR i=0,plot.numdata-2 DO BEGIN
        IF ((xrmin GE plot.xdata(0,i)) AND $
        (xrmin LT plot.xdata(0,i+1))) THEN BEGIN
         recmin = i
    ENDIF
    IF ((xrmax GE plot.xdata(0,i)) AND $
        (xrmax LT plot.xdata(0,i+1))) THEN BEGIN
        recmax = i
     ENDIF
ENDFOR

; y-axis settings 
; if min/max are not changed from previously, then
; base the values on the current time range
ymin = plot.miny
ymax = plot.maxy
EZ4_COMPYTICKS, ymin, ymax, numyticks, ytv, ytlabels
yminplot = min(ytv)
ymaxplot = max(ytv)

; Plot the data
title=''
numlabel = TOTAL(plot.drawlines)
yshift = MIN([ceil(numlabel/2.),3])
plot.pos = [dposX1L,dposY1L,dposX2L,dposY2L]

IF ((numlabel GT 0) AND (plot.showlinelabel EQ 1)) THEN BEGIN
     plot.pos[1] = plot.pos[1] + 0.06*yshift
     plot.pos[3] = plot.pos[3] + 0.04
ENDIF

IF (plot.showlinelabel EQ 0) THEN plot.pos[3] += 0.04

IF (STRLEN(ytlabels[0]) GT 5) THEN plot.pos[0]+= 0.03
IF (STRLEN(ytlabels[0]) GT 7) THEN plot.pos[0]+= 0.03
IF (STRLEN(ytlabels[0]) GT 10) THEN plot.pos[0]+= 0.03

;;set the values for plots and labels
pos = plot.pos
xstart = plot.pos[0]
xval   = xstart
IF (dopsplot EQ 1) THEN xdif = 0.013 ELSE xdif = 0.016

yval1 = plot.pos[3] + 0.007
yval2 = plot.pos[3] + 0.044
IF (numlabel LT 4) THEN yval=yval1 ELSE yval=yval2

EZ4_setfont

;;Check units to change characters
units = plot.ytitle
EZ4_setunits, units, yytitle
plot.ytitle=yytitle

;;Draw the plot
tempdata=plot.ydata[0,recmin:recmax]
goodref=where(tempdata GT missing)
plot.meanvalue = mean(tempdata(goodref))

PLOT, plot.xdata[0,*], plot.ydata[0,*], /NODATA, $
  POSITION=pos, NOERASE=noerase, $
  XRANGE=[xrmin,xrmax], XTITLE=plot.xtitle, XTICKNAME=xtlabels, $
  XTICKV=xtv, XMINOR=-1, /XSTYLE, XTICKS=numxticks-1,  $
  YTITLE=yytitle, /YNOZERO, YMINOR=-1, /YSTYLE, YTICKV=ytv, $
  YTICKS=numyticks-1, YRANGE=[yminplot, ymaxplot], YTICKNAME=ytlabels, $
  TITLE=title,CHARSIZE=plot.xysize,COLOR=plot.xycolor

IF (dopsplot) THEN BEGIN
    IF (N_ELEMENTS(xdif) GT 0) THEN xdif+=0.003
ENDIF

;Plot the lines
FOR i=0,plot.numlevs-1 DO BEGIN
   IF (plot.drawlines(i) gt 0) THEN BEGIN
       OPLOT, plot.xdata(i,*), plot.ydata(i,*), $
       THICK=plot.linethick(i), $
       LINESTYLE=plot.linestyle(i), $
       COLOR=specColors[plot.linecolor(i)], $
       MIN_VAL=missing+1
    ENDIF
ENDFOR

;Add the text
ez4_addText, plot, POSITION=pos

;Add line labels
IF (plot.showlinelabel EQ 1) THEN $
    ez4_addlinelabel, plot


END
