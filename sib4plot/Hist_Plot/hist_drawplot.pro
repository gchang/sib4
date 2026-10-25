;======================================================================
PRO Hist_DrawPlot, plot, _EXTRA=extra
;======================================================================
; draw a histogram plot

@ez4_BasicParams.com

EZ4_SetFont
IF (NOT dopsplot) THEN BEGIN
   EZ4_SetFont, NameFont=labelFont, SizeFont=labelSize
ENDIF

; x-axis settings
xrmin=plot.minx
xrmax=plot.maxx
numxticks=plot.numbins+1
xtv=plot.xdata

myformat=ez4_niceformat(plot.interval)
IF (numxticks GT 12) THEN modval=3 ELSE $
IF (numxticks GT  8) THEN modval=2 ELSE $
modval=1

xtlabels=STRARR(plot.numbins+1)
FOR i=0,plot.numbins DO BEGIN
    IF ((i MOD modval) EQ 0) THEN $
        xtlabels(i) = STRTRIM(STRING(xtv(i),FORMAT=myformat),2) $
    ELSE xtlabels(i) = ' '
ENDFOR

; zoom information
recmin=0
recmax=plot.numbins-1
FOR i=0,plot.numbins-2 DO BEGIN
        IF ((xrmin GE plot.xdata(i)) AND $
        (xrmin LT plot.xdata(i+1))) THEN BEGIN
         recmin = i
    ENDIF
    IF ((xrmax GE plot.xdata(i)) AND $
        (xrmax LT plot.xdata(i+1))) THEN BEGIN
        recmax = i
     ENDIF
ENDFOR

; y-axis settings 
; if min/max are not changed from previously, then
; base the values on the current range
ymin = plot.miny
ymax = plot.maxy
EZ4_COMPYTICKS, ymin, ymax, numyticks, ytv, ytlabels
yminplot=min(ytv)
ymaxplot=max(ytv)

; Plot the data
title=''
numlabel = TOTAL(plot.drawlines)
yshift = MIN([ceil(numlabel/2.),3])
plot.pos = [dposX1,dposY1,dposX2,dposY2]

IF ((numlabel EQ 0) OR (plot.showlinelabel EQ 1)) THEN BEGIN
     plot.pos[1] = plot.pos[1] + 0.06*yshift
     plot.pos[3] = plot.pos[3] + 0.04
ENDIF

IF (STRLEN(ytlabels[0]) GT 5) THEN plot.pos[0]+= 0.03
IF (STRLEN(ytlabels[0]) GT 7) THEN plot.pos[0]+= 0.03
IF (STRLEN(ytlabels[0]) GT 10) THEN plot.pos[0]+= 0.03

IF (STRLEN(plot.comment) LT 2) THEN BEGIN
   plot.pos[3] = plot.pos[3] + 0.03
ENDIF

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
units=plot.xtitle
EZ4_setunits,units,xxtitle
plot.xtitle=xxtitle

;;Draw the plot
tempdata=plot.ydata[*,recmin:recmax]
goodref=where(tempdata GT missing)
plot.meanvalue = mean(tempdata(goodref))

PLOT, plot.xdata, /NODATA, $
  POSITION=pos, NOERASE=noerase, $
  XRANGE=[xrmin,xrmax], XTITLE=plot.xtitle, XTICKNAME=xtlabels, $
  XTICKV=xtv, XMINOR=-1, /XSTYLE, XTICKS=numxticks-1,  $
  YTITLE=plot.ytitle, /YNOZERO, YMINOR=-1, /YSTYLE, YTICKV=ytv, $
  YTICKS=numyticks-1, YRANGE=[yminplot, ymaxplot], YTICKNAME=ytlabels, $
  TITLE=title,CHARSIZE=plot.xysize,COLOR=plot.xycolor

IF (dopsplot) THEN BEGIN
    IF (N_ELEMENTS(xdif) GT 0) THEN xdif+=0.003
ENDIF

;Plot the lines   
minval=0.
FOR i=0,plot.numplots-1 DO BEGIN
   IF (plot.drawlines(i) gt 0) THEN BEGIN
       IF (plot.linestyle(i) EQ 0) THEN BEGIN
           FOR j=0, plot.numbins-1 DO BEGIN
               ez4_box, plot.xdata[j], minval, $
                   plot.xdata[j+1], plot.ydata(i,j), $
                   COLOR=specColors[plot.linecolor(i)]
            ENDFOR
        ENDIF
    ENDIF
ENDFOR

FOR i=0,plot.numplots-1 DO BEGIN
   IF (plot.drawlines(i) GT 0) THEN BEGIN
       IF (plot.linestyle(i) EQ 1) THEN BEGIN
           FOR j=0, plot.numbins-1 DO BEGIN
               ez4_boxline, plot.xdata[j], minval, $
                    plot.xdata[j+1], plot.ydata(i,j), $
                    COLOR=specColors[plot.linecolor(i)], $
                    THICK=plot.linethick(i)
            ENDFOR
       ENDIF
    ENDIF
ENDFOR

;;Add the text
ez4_addText, plot, POSITION=pos

;;Add line labels
plottemp = plot
plottemp.linestyle = 0
IF (plot.showlinelabel EQ 1) THEN $
    ez4_addlinelabel, plottemp


END



