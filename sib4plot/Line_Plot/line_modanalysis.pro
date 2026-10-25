;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Modify data from plot by:
;;  1) Taking the reciprical
;;  2) Taking the mean anomaly
;;
PRO Line_ModAnalysis, oldPlot, option

@ez4_BasicParams.com

plot = oldPlot
data = oldPlot.ydata

IF (N_ELEMENTS(option) EQ 0) THEN option=''
IF (option EQ 'reciprocal') THEN BEGIN
   ;; Cannot divide by zero
   zerosub = 1.E-15
   nonzeroref = WHERE(plot.ydata GT zerosub)
   plot.ydata(nonzeroref) = 1./plot.ydata(nonzeroref)
   tcomment = '(Reciprocal)'
ENDIF

IF (option EQ 'anomaly') THEN BEGIN
   FOR i=0,plot.numlevs-1 DO BEGIN
       meanVal = MEAN(plot.ydata(i,*))
       plot.ydata(i,*) = plot.ydata(i,*) - meanVal
       tcomment = '(Anomaly)'
   ENDFOR
ENDIF


;; Put modified data into plot
plot.miny = MIN(plot.ydata)
plot.maxy = MAX(plot.ydata)
plot.savminy = plot.miny
plot.savmaxy = plot.maxy

title=plot.title[0]
varname = EZ4_GetVarName(title)
plot.title = varname + '  ' + tcomment
plot.windowTitle = plot.title

Line_PLOT, plot

END

