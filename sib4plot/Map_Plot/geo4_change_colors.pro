PRO GEO4_Change_Colors, colorChoice, plot

COMMON Colors_COM
COMMON Missing_COM

;Set new colors
fillColors = GEO4_Set_FillColors(colorChoice, plot.whiteOut)
nFillColors = SIZE(fillColors,/N_ELEMENTS)
IF (nfillColors NE maxNcolors) THEN BEGIN
    newFillColors = FLTARR(maxNColors)
    newFillColors(0:nFillColors-1) = fillColors(*)
    FOR i=nFillColors, maxNcolors-1 DO BEGIN
        newFillColors(i) = missing
     ENDFOR
    fillColors = newFillColors
ENDIF

;Set new contours
nContours = coltabNColors(colorChoice)-1
GEO4_Set_Contours, ncontours, plot.mindata, plot.maxdata, $
   plot.meanValue, dcontours
nContours ++
contours = [ missing+1, dcontours ]
minContour = dcontours(0)
contourInterval = dcontours(3)-dcontours(2)
contour_range = (MAX(dcontours)-MIN(dcontours))
cbarLFormat = EZ4_NiceFormat(contour_range)

IF (ncontours NE maxNColors) THEN BEGIN
    newcontours = FLTARR(maxNColors)
    newcontours(0:ncontours-1) = contours(*)
    FOR i=nContours, maxNcolors-1 DO BEGIN
        newcontours(i) = missing
    ENDFOR
    contours = newcontours
ENDIF

;Save variables
plot.ncontours=ncontours
plot.contours=contours
plot.minContour=minContour
plot.contourInterval=contourInterval
plot.colorChoice=colorChoice
plot.fillColors=fillColors
plot.cBarLFormat=cBarLFormat


END
