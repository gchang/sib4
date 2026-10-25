PRO GEO4_Struct_Data, plot, $
        nlon=nlon, nlat=nlat, $
        data=data, lon=lon, lat=lat, $
        mindata=mindata, maxdata=maxdata, pos=pos, $
        title=title, tfont=tfont, tsize=tsize, tcolor=tcolor, $ 
        comment=comment, cfont=cfont, csize=csize, ccolor=ccolor, $
        date=date, dfont=dfont, dsize=dsize, dcolor=dcolor, showDate=showDate, $
        fileName=fileName, ffont=ffont, fsize=fsize, $
        fcolor=fcolor, showFileName=showFileName, $
        simlabel=simlabel, lfont=lfont, lsize=lsize, $
        lcolor=lcolor, showLabel=showLabel, $
        meanValue=meanValue, mFont=mFont, mSize=mSize, $
        mColor=mColor, mFormat=mFormat, showMean=showMean, $
        varname=varname, units=units, $
        mapProj=mapProj, mapCenter=mapCenter, isotropic=isotropic, $
        drawBorder=drawBorder, drawGrid=drawGrid, $
        labelGrid=labelGrid, lGridPos=lGridPos, lGridYOff=lGridYOff, $
        drawContinents=drawContinents, drawHiRes=drawHiRes, $
        drawCoasts=drawCoasts, drawCountries=drawCountries, drawUSA=drawUSA, $
        limits=limits, region=region, $
        nContours=nContours, contours=contours, $
        minContour=minContour, contourInterval=contourInterval, $
        fillColors=fillColors, colorChoice=colorChoice, whiteOut=whiteOut, $
        drawLines=drawLines, labelLines=labelLines, labelSize=labelSize, $
        labelColorMatch=labelColorMatch, $
        colorBar=colorBar, cBarPos=cBarPos, $
        cBarTitle=cBarTitle, showcBarTitle=showcBarTitle, $
        cBarTSize=cBarTSize, cBarTColor=cBarTColor, cBarTYOff = cBarTYOff, $
        cBarLFormat=cBarLFormat, cBarLBotOff=cBarLBotOff, $
        cbarLSize=cbarLSize, cbarLThick=cbarLThick, cbarLColor=cbarLColor, $
        savefile=savefile

@ez4_BasicParams.com

type='map'
IF N_ELEMENTS(nlon) EQ 0 THEN nlon=1
IF N_ELEMENTS(nlat) EQ 0 THEN nlat=1
IF N_ELEMENTS(lon) EQ 0 THEN lon=FLTARR(nlon)+missing
IF N_ELEMENTS(lat) EQ 0 THEN lat=FLTARR(nlat)+missing

IF N_ELEMENTS(data) EQ 0 THEN data=FLTARR(nlon,nlat)+missing
goodref = where(data gt missing)
IF N_ELEMENTS(mindata) EQ 0 THEN mindata=min(data(goodref))
IF N_ELEMENTS(maxdata) EQ 0 THEN maxdata=max(data(goodref))
IF N_ELEMENTS(pos) EQ 0 THEN pos=[dPosX1,dPosY1,dPosX2,dPosY2]

IF N_ELEMENTS(title) EQ 0 THEN title=''
IF N_ELEMENTS(tfont) EQ 0 THEN tfont=titleFont
IF N_ELEMENTS(tsize) EQ 0 THEN tsize=psTitleSize
IF N_ELEMENTS(tcolor) EQ 0 THEN tcolor=black
IF N_ELEMENTS(comment) EQ 0 THEN comment=''
IF N_ELEMENTS(cfont) EQ 0 THEN cfont=commentFont
IF N_ELEMENTS(csize) EQ 0 THEN csize=psCommentSize
IF N_ELEMENTS(ccolor) EQ 0 THEN ccolor=black

IF N_ELEMENTS(date) EQ 0 THEN BEGIN
   txt = STRSPLIT(SYSTIME(), /EXTRACT)
   date = txt[1] + ' ' + txt[2] + ', ' + txt[4] 
ENDIF
IF N_ELEMENTS(dfont) EQ 0 THEN dfont=dateFont
IF N_ELEMENTS(dsize) EQ 0 THEN dSize=psDateSize
IF N_ELEMENTS(dcolor) EQ 0 THEN dcolor=black
IF N_ELEMENTS(showDate) EQ 0 THEN showDate=defaultDateDisplay

IF N_ELEMENTS(fileName) EQ 0 THEN fileName=''
IF N_ELEMENTS(ffont) EQ 0 THEN ffont=fileNameFont
IF N_ELEMENTS(fsize) EQ 0 THEN fsize=psFileNameSize
IF N_ELEMENTS(fcolor) EQ 0 THEN fcolor=black
IF N_ELEMENTS(showFileName) EQ 0 THEN showFileName=defaultFileNameDisplay

IF N_ELEMENTS(simlabel) EQ 0 THEN simlabel=''
IF N_ELEMENTS(lfont) EQ 0 THEN lfont=labelFont
IF N_ELEMENTS(lsize) EQ 0 THEN lsize=psLabelSize
IF N_ELEMENTS(lcolor) EQ 0 THEN lcolor=black
IF N_ELEMENTS(showLabel) EQ 0 THEN showLabel=defaultLabelDisplay

IF N_ELEMENTS(meanValue) EQ 0 THEN meanValue=MEAN(data(goodref))
IF N_ELEMENTS(mFont) EQ 0 THEN mFont=meanFont
IF N_ELEMENTS(mSize) EQ 0 THEN mSize=psMeanSize
IF N_ELEMENTS(mColor) EQ 0 THEN mColor=black
IF N_ELEMENTS(mFormat) EQ 0 THEN mFormat=defaultMeanFormat
IF N_ELEMENTS(showMean) EQ 0 THEN showMean=defaultMeanDisplay

IF N_ELEMENTS(varname) EQ 0 THEN varname=''
IF N_ELEMENTS(units) EQ 0 THEN units=''

IF N_ELEMENTS(mapProj) EQ 0 THEN mapProj='MOL'
IF N_ELEMENTS(mapCenter) EQ 0 THEN mapCenter=0.
IF N_ELEMENTS(isotropic) EQ 0 THEN isotropic=1

IF N_ELEMENTS(drawBorder) EQ 0 THEN drawBorder=1
IF N_ELEMENTS(drawGrid) EQ 0 THEN drawGrid=1
IF N_ELEMENTS(labelGrid) EQ 0 THEN labelGrid=1
IF N_ELEMENTS(lGridPos) EQ 0 THEN lGridPos='bot'
IF N_ELEMENTS(lGridYOff) EQ 0 THEN lGridYOff=0.

IF N_ELEMENTS(drawContinents) EQ 0 THEN drawContinents=1
IF N_ELEMENTS(drawHiRes) EQ 0 THEN drawHiRes=0
IF N_ELEMENTS(drawCoasts) EQ 0 THEN drawCoasts=1
IF N_ELEMENTS(drawCountries) EQ 0 THEN drawCountries=1
IF N_ELEMENTS(drawUSA) EQ 0 THEN drawUSA=0

IF N_ELEMENTS(limits) EQ 0 THEN limits=[-90.,-180.,90.,180.]
IF N_ELEMENTS(region) EQ 0 THEN region='Globe'

IF N_ELEMENTS(colorChoice) EQ 0 THEN colorChoice=defaultColorChoice
IF N_ELEMENTS(whiteOut) EQ 0 THEN whiteOut='none'
IF N_ELEMENTS(fillColors) EQ 0 THEN $
    fillColors=GEO4_Set_FillColors(colorChoice, whiteout)

nFillColors = SIZE(fillColors,/N_ELEMENTS)
IF (nfillColors NE maxNcolors) THEN BEGIN
    newFillColors = FLTARR(maxNColors)
    newFillColors(0:nFillColors-1) = fillColors(*)
    FOR i=nFillColors, maxNcolors-1 DO BEGIN
        newFillColors(i) = missing
     ENDFOR
    fillColors = newFillColors
ENDIF

IF N_ELEMENTS(contours) EQ 0 THEN BEGIN
   ncontours=coltabNColors(colorChoice)-1
   GEO4_Set_Contours, ncontours, mindata, maxdata, meanValue, dcontours
   ncontours++
   contours = [ missing+1, dcontours ]
ENDIF

IF N_ELEMENTS(ncontours) EQ 0 THEN ncontours=SIZE(contours,/N_ELEMENTS)
IF (ncontours NE maxNColors) THEN BEGIN
    newcontours = FLTARR(maxNColors)
    newcontours(0:ncontours-1) = contours(*)
    FOR i=nContours, maxNcolors-1 DO BEGIN
        newcontours(i) = missing
    ENDFOR
    contours = newcontours
ENDIF
 
IF N_ELEMENTS(minContour) EQ 0 THEN minContour=contours(1)
IF N_ELEMENTS(contourInterval) EQ 0 THEN contourInterval=(contours(3)-contours(2))
IF N_ELEMENTS(drawLines) EQ 0 THEN drawLines=0
IF N_ELEMENTS(labelLines) EQ 0 THEN labelLines=0
IF N_ELEMENTS(labelSize) EQ 0 THEN labelSize=defaultcBarLabelSize
IF N_ELEMENTS(labelColorMatch) EQ 0 THEN labelColorMatch=0

IF N_ELEMENTS(colorBar) EQ 0 THEN colorBar=1
IF N_ELEMENTS(cBarPos) EQ 0 THEN BEGIN
   left = pos[0] + 0.03
   right = pos[2] - 0.04
   bottom = pos[1] - 0.13
   top = bottom + 0.03
   cbarpos = [left, bottom, right, top]
ENDIF
IF N_ELEMENTS(cBarTitle) EQ 0 THEN cBarTitle=units
IF N_ELEMENTS(showCBarTitle) EQ 0 THEN showCBarTitle=1
IF N_ELEMENTS(cBarTSize) EQ 0 THEN cBarTSize=defaultcbarSize
IF N_ELEMENTS(cBarTColor) EQ 0 THEN cBarTColor=black
IF N_ELEMENTS(cBarTYOff) EQ 0 THEN cbarTYOff=0.08
IF N_ELEMENTS(cBarLFormat) EQ 0 THEN cbarLFormat=''

IF N_ELEMENTS(cBarLBotOff) EQ 0 THEN cBarLBotOff=0.03
IF N_ELEMENTS(cBarLSize) EQ 0 THEN cBarLSize=defaultcbarSize
IF N_ELEMENTS(cBarLThick) EQ 0 THEN cBarLThick=1.2
IF N_ELEMENTS(cBarLColor) EQ 0 THEN cBarLColor=black

IF N_ELEMENTS(saveFile) EQ 0 THEN saveFile=''

plot = {type:type, nlon:nlon, nlat:nlat, lon:lon, lat:lat, $
        data:data, mindata:mindata, maxdata:maxdata, pos:pos, $
        title:title, tfont:tfont, tsize:tsize, tcolor:tcolor, $ 
        comment:comment, cfont:cfont, csize:csize, ccolor:ccolor, $
        date:date, dfont:dfont, dsize:dsize, dcolor:dcolor, showDate:showDate, $
        fileName:fileName, ffont:ffont, fsize:fsize, $
        fcolor:fcolor, showFileName:showFileName, $
        simlabel:simlabel, lfont:lfont, lsize:lsize, $
        lcolor:lcolor, showLabel:showLabel, $
        meanValue:meanValue, mFont:mFont, mSize:mSize, mColor:mColor, $
        mFormat:mFormat, showMean:showMean, $
        varname:varname, units:units, $
        mapProj:mapProj, mapCenter:mapCenter, isotropic:isotropic, $
        drawBorder:drawBorder, drawGrid:drawGrid, $
        labelGrid:labelGrid, lGridPos:lGridPos, lGridYOff:lGridYoff, $
        drawContinents:drawContinents, drawHires:drawHires, $
        drawCoasts:drawCoasts, drawCountries:drawCountries, drawUSA:drawUSA, $
        limits:limits, region:region, $
        nContours:nContours, contours:contours, $
        minContour:minContour, contourInterval:contourInterval, $
        fillColors:fillColors, colorChoice:colorChoice, whiteOut:whiteOut, $
        drawLines:drawLines, labelLines:labelLines, labelSize:labelSize, $
        labelColorMatch:labelColorMatch, $
        colorBar:colorBar, cBarPos:cBarPos, $
        cBarTitle:cBarTitle, showCBarTitle:showCBarTitle, $
        cBarTSize:cBarTSize, cBarTColor:cBarTColor, cBarTYOff:cBarTYOff, $
        cBarLFormat:cBarLFormat, cBarLBotOff:cBarLBotOff, $
        cbarLSize:cbarLSize, cbarLThick:cbarLThick, cbarLColor:cbarLColor, $ 
        savefile:savefile}

END
