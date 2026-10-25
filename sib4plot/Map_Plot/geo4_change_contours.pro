PRO GEO4_Change_Contours, plot, option

COMMON Colors_COM
COMMON Missing_COM

dataMin = plot.MinData
dataMax = plot.MaxData

CASE Option OF
   0: whiteOut='low'
   1: whiteOut='center'
   2: whiteOut='center'
   3: whiteOut='center'
 ELSE: whiteOut='low'
ENDCASE

mean = (option EQ 1) ? plot.meanValue :0
IF option EQ 3 THEN BEGIN
  IF EZ4_GetField(value, TITLE="Center Colorbar on Value ", /FLOAT, $
              PROMPT="Enter value to center colorbar: ") EQ 0 THEN RETURN
  option = 1
  mean   = value
ENDIF

nContours = coltabNColors(plot.colorChoice)-1
GEO4_Set_Contours, ncontours, dataMin, dataMax, mean, dcontours, option=option
contours = [ missing+1, dcontours ] 
fillColors = GEO4_Set_FillColors(plot.colorChoice, whiteout)

contour_range=(MAX(dcontours)-MIN(dcontours))
cbarformat=EZ4_NiceFormat(contour_range)

plot.contours=contours
plot.fillColors = fillColors
plot.whiteOut = whiteOut
plot.minContour = dcontours(0)
plot.contourInterval = dcontours(2)-dcontours(1)
plot.cBarLFormat = cbarformat

WIDGET_CONTROL, plot.cMinField, SET_VALUE=plot.minContour
WIDGET_CONTROL, plot.intervalField, SET_VALUE=plot.contourInterval

GEO4_DrawPlot, plot

END
