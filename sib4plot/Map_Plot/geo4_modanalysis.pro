;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Modify data from plot by:
;;  1) Taking the reciprical
;;  2) Taking the global mean anomaly
;;
PRO GEO4_ModAnalysis, oldPlot, option

@ez4_BasicParams.com

plot = oldPlot
data = plot.data

IF (N_ELEMENTS(option) EQ 0) THEN option=''
IF (option EQ 'reciprocal') THEN BEGIN
   ;; Cannot divide by zero
   zerosub = 1.E-15
   nonzeroref = WHERE((abs(gooddata) GT zerosub) $
                      AND (data GT missing))
   data(nonzeroref) = 1./data(nonzeroref)
   tcomment = '(Reciprocal)'
ENDIF ELSE $
IF (option EQ 'anomaly') THEN BEGIN
   goodref = WHERE(data GT missing)
   gooddata = data(goodref)
   meanVal = MEAN(gooddata)

   data(goodref) = data(goodref) - meanVal
   tcomment = '(Anomaly)'
   plot.whiteOut = 'center'
ENDIF ELSE BEGIN
   EZ4_Alert, 'Invalid Analysis Option'
   RETURN
ENDELSE

;; Put modified data into plot
plot.data = data

goodref = where(data GT missing)
gooddata = data(goodref)
plot.mindata = MIN(gooddata)
plot.maxdata = MAX(gooddata)
plot.meanValue = mean(gooddata)

nContours=coltabNColors(plot.colorChoice)-1
GEO4_Set_Contours, nContours, plot.mindata, plot.maxdata, meanValue, dcontours
plot.contours = [missing+1, dcontours]
plot.minContour = dcontours(0)
plot.contourInterval = (dcontours(3)-dcontours(2))
plot.fillColors = GEO4_Set_FillColors(plot.colorChoice, plot.whiteOut)

contour_range=(MAX(dcontours)-MIN(dcontours))
plot.cbarLFormat=EZ4_NiceFormat(contour_range)

varname = EZ4_GetVarName(plot.title[0])
plot.title = varname + '  ' + tcomment
plot.windowTitle = plot.title

GEO4_PLOT, plot

END

