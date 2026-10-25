
PRO Hist_Struct_Data, plot, error, $
     origdata=origdata, $
     xData=xdata, yData=ydata, $
     numbins=numbins, numplots=numplots, $
     minx=minx, maxx=maxx, interval=interval, $
     miny=miny, maxy=maxy, savminy=savminy, savmaxy=savmaxy, $
     units=units, position=pos, $
     savefile=savefile, $
     title=title, tfont=tfont, tsize=tsize, tcolor=tcolor, $
     xtitle=xtitle, ytitle=ytitle, xysize=xysize, xycolor=xycolor, $
     comment=comment, cfont=cfont, csize=csize, ccolor=ccolor, $
     date=date, dfont=dfont, dsize=dsize, dcolor=dcolor, showDate=showDate, $
     filename=filename, ffont=ffont, fsize=fsize, fcolor=fcolor, $
     showFileName=showFileName,  $
     simlabel=simlabel, simfont=simfont, simsize=simsize, simcolor=simcolor, $
     showsimLabel=showsimLabel, $
     meanValue=meanValue, mfont=mfont, msize=msize, mcolor=mcolor, $
     meanformat=meanformat, showmean=showmean, $
     linecolor=linecolor, linestyle=linestyle, linethick=linethick, $
     drawLines=drawLines, showlinelabel=showlinelabel, $
     linename=linename, lfont=lfont, lsize=lsize

@ez4_BasicParams.com

type='hist'

IF N_ELEMENTS(xdata) EQ 0 THEN BEGIN
   error=1
   EZ4_Alert,'Unspecified Frequency Bins.'
   RETURN
ENDIF

IF N_ELEMENTS(ydata) EQ 0 THEN BEGIN
    error=1
    EZ4_Alert,'Unspecified Frequency Distribution Data.'
    RETURN
ENDIF

IF N_ELEMENTS(origdata) EQ 0 THEN BEGIN
    error=1
    EZ4_Alert,'Unspecified Original Data For Bin Manipulation.'
    RETURN
ENDIF

IF N_ELEMENTS(units) EQ 0 THEN units = ''
IF N_ELEMENTS(pos) EQ 0 THEN pos = [dPosX1,dposY1,dposX2,dposY2]

ssize = SIZE(ydata)
IF N_ELEMENTS(numbins) EQ 0 THEN numbins = (size(xdata))[1]-1
IF N_ELEMENTS(numplots) EQ 0 THEN numplots = (size(ydata))[1]

IF N_ELEMENTS(minx) EQ 0 THEN minx=xdata[0]
IF N_ELEMENTS(maxx) EQ 0 THEN maxx=xdata[numbins]
IF N_ELEMENTS(interval) EQ 0 THEN BEGIN
  interval = xdata[1]-xdata[0]
ENDIF

IF N_ELEMENTS(miny) EQ 0 THEN miny = MIN(ydata)
IF N_ELEMENTS(maxy) EQ 0 THEN maxy = MAX(ydata)
IF N_ELEMENTS(savminy) EQ 0 THEN savminy = miny
IF N_ELEMENTS(savmaxy) EQ 0 THEN savmaxy = maxy

IF N_ELEMENTS(savefile) EQ 0 THEN savefile = ''

IF N_ELEMENTS(title) EQ 0 THEN title = ''
IF N_ELEMENTS(tfont) EQ 0 THEN tfont = titleFontL
IF N_ELEMENTS(tsize) EQ 0 THEN tsize = psTitleSizeL
IF N_ELEMENTS(tcolor) EQ 0 THEN tcolor = black
IF N_ELEMENTS(xtitle) EQ 0 THEN xtitle = ''
IF N_ELEMENTS(ytitle) EQ 0 THEN ytitle = ''
IF N_ELEMENTS(xysize) EQ 0 THEN xysize = defaultxySizeL
IF N_ELEMENTS(xycolor) EQ 0 THEN xycolor = black

IF N_ELEMENTS(comment) EQ 0 THEN comment = ''
IF N_ELEMENTS(cfont) EQ 0 THEN cfont = commentFontL
IF N_ELEMENTS(csize) EQ 0 THEN csize = psCommentSizeL
IF N_ELEMENTS(ccolor) EQ 0 THEN ccolor = black

IF N_ELEMENTS(date) EQ 0 THEN BEGIN
   txt = STRSPLIT(SYSTIME(), /EXTRACT)
   date = txt[1] + ' ' + txt[2] + ', ' + txt[4]
ENDIF
IF N_ELEMENTS(dfont) EQ 0 THEN dfont=datefontL
IF N_ELEMENTS(dsize) EQ 0 THEN dsize=psDateSizeL
IF N_ELEMENTS(dcolor) EQ 0 THEN dcolor=black
IF N_ELEMENTS(showDate) EQ 0 THEN showDate=defaultDateDisplayL

IF N_ELEMENTS(filename) EQ 0 THEN filename=''
IF N_ELEMENTS(ffont) EQ 0 THEN ffont = fileNameFontL
IF N_ELEMENTS(fsize) EQ 0 THEN fsize = psFileNameSizeL
IF N_ELEMENTS(fcolor) EQ 0 THEN fcolor = black
IF N_ELEMENTS(showFileName) EQ 0 THEN showFileName=0

IF N_ELEMENTS(simlabel) EQ 0 THEN simlabel = ''
IF N_ELEMENTS(simfont) EQ 0 THEN simfont = simFontL
IF N_ELEMENTS(simsize) EQ 0 THEN simsize = psSimSizeL
IF N_ELEMENTS(simcolor) EQ 0 THEN simcolor = black
IF N_ELEMENTS(showSimLabel) EQ 0 THEN showSimLabel=defaultSimDisplayL

IF N_ELEMENTS(meanValue) EQ 0 THEN meanValue=MEAN(ydata,/NaN)
IF N_ELEMENTS(mfont) EQ 0 THEN mfont = meanFontL
IF N_ELEMENTS(msize) EQ 0 THEN msize = psMeanSizeL
IF N_ELEMENTS(mcolor) EQ 0 THEN mcolor = black
IF N_ELEMENTS(meanFormat) EQ 0 THEN meanFormat = defaultMeanFormatL
IF N_ELEMENTS(showMean) EQ 0 THEN showMean = 0

numlevs=numplots
IF N_ELEMENTS(linecolor) EQ 0 THEN linecolor=intarr(numlevs)
IF N_ELEMENTS(linestyle) EQ 0 THEN linestyle=intarr(numlevs)
IF N_ELEMENTS(linethick) EQ 0 THEN linethick=intarr(numlevs)+2
IF N_ELEMENTS(drawlines) EQ 0 THEN drawlines=intarr(numlevs)+1
IF N_ELEMENTS(linename) EQ 0 THEN linename=strarr(numlevs)
IF N_ELEMENTS(lFont) EQ 0 THEN lFont = labelFontL
IF N_ELEMENTS(lSize) EQ 0 THEN lSize = pslabelSizeL
IF N_ELEMENTS(showlinelabel) EQ 0 THEN showlinelabel=0

plot = { type:type, origData:origData, $
     xData:xdata, yData:ydata, $
     units:units, pos:pos, $
     numbins:numbins, numplots:numplots, $
     minx:minx, maxx:maxx, interval:interval, $
     miny:miny, maxy:maxy, savminy:savminy, savmaxy:savmaxy, $
     savefile:savefile, $
     title:title, tfont:tfont, tsize:tsize, tcolor:tcolor, $
     xtitle:xtitle, ytitle:ytitle, xysize:xysize, xycolor:xycolor, $
     comment:comment, cfont:cfont, csize:csize, ccolor:ccolor, $
     date:date, dfont:dfont, dsize:dsize, dcolor:dcolor, showDate:showDate, $
     filename:filename, ffont:ffont, fsize:fsize, fcolor:fcolor, $
     showFileName:showFileName, $
     simlabel:simlabel, simfont:simfont, simsize:simsize, simcolor:simcolor, $
     showSimLabel:showSimLabel, $
     meanValue:meanValue, mfont:mfont, msize:msize, mcolor:mcolor, $
     meanformat:meanformat, showmean:showmean, $
     linecolor:linecolor, linestyle:linestyle, linethick:linethick, $
     drawLines:drawLines, showlinelabel:showlinelabel, $
     linename:linename, lfont:lfont, lsize:lsize}

END
