;======================================================================
PRO GEO4_addText, plot, POSITION=pos
;======================================================================

@ez4_BasicParams.com

centerX = 0.5
titleY  = 0.94
commentY = 0.89
IF (plot.colorbar EQ 0) THEN BEGIN
    titleY = 0.90
    commentY = 0.85
ENDIF

dateX = datePosX
dateY = datePosY
dateXleft = dateX - 0.20
dateXright = dateX + 0.05
dateYtop = dateY + 0.04
dateYbot = dateY - 0.006

filenameX = fileposX
filenameY = fileposY
fileXleft = filenameX - 0.05
fileXright = filenameY + 0.20
fileYtop = filenameY + 0.04
fileYbot = filenameY - 0.006

labelX = labelPosX
labelY = labelPosY
labelXleft = labelX - 0.1
labelXright = labelX + 0.1
labelYtop = labelY + 0.04
labelYbot = labelY - 0.006

meanX = meanPosX
meanY = meanPosY
meanXleft = meanX - 0.20
meanXright = meanX + 0.05
meanYtop = meanY + 0.04
meanYbot = meanY

nfontSizeList = 5
fontSizeList = [8,10,12,14,18]

;;;;;White out the necessary labels (for toggling)
; white out the title
POLYFILL, [0.01,0.99,0.99,0.01], [titleY,titleY,1.,1.], COLOR=white, /NORMAL

; white out the comment, if not blank
IF (STRLEN(plot.comment) GT 1) THEN BEGIN
   POLYFILL, [0.01,0.99,0.99,0.01], [commentY,commentY,titleY,titleY], $
         COLOR=white, /NORMAL
ENDIF

; white out the date
IF (plot.showDate NE -1) THEN BEGIN
    POLYFILL, [dateXleft,dateXright,dateXright,dateXleft], $
              [dateYtop,dateYtop,dateYbot,dateYbot], $
              COLOR=white, /NORMAL
ENDIF

; white out the file name
IF (plot.showFileName NE -1) THEN BEGIN
   POLYFILL, [fileXleft,fileXright,fileXright,fileXleft], $
             [fileYtop,fileYtop,fileYbot,fileYbot], $
             COLOR=white, /NORMAL
ENDIF

; white out the label
IF (plot.showLabel NE -1) THEN BEGIN
   POLYFILL, [labelXleft,labelXright,labelXright,labelXleft], $
             [labelYtop,labelYtop,labelYbot,labelYbot], $
             COLOR=white, /NORMAL
ENDIF

; white out the mean
IF (plot.showMean NE -1) THEN BEGIN
   POLYFILL, [meanXleft,meanXright,meanXright,meanXleft], $
             [meanYtop,meanYtop,meanYbot,meanYBot], $
             COLOR=white, /NORMAL
ENDIF

;;;;;Write out the labels if selected
; write the main title 
title = plot.title
tfont = plot.tFont
tsize = plot.tSize
tcolor = plot.tColor

IF (dopsplot) THEN BEGIN
   tfont=psFont+ ' BOLD'
   tsize *= 1.7
ENDIF ELSE BEGIN
   myfontsize=EZ4_GetFontSize(tfont)
 
   fref=WHERE(fontSizeList EQ myFontSize)
   result = strlen(strtrim(title,2))
   IF ((fref[0] GT 0) AND (result GT 55)) THEN myfontsize=fontSizeList(fref-1)
   IF ((fref[0] GT 1) AND (result GT 70)) THEN myfontsize=fontSizeLIst(fref-2)

   IF (myfontsize GT 9) THEN newsizeFont=STRING(myfontSize,FORMAT='(I2)') $
   ELSE newsizeFont=STRING(myfontSize,FORMAT='(I1)')

   tfont=EZ4_setFontSize(tfont,newsizeFont)
   
ENDELSE

IF (dopsplot) THEN EZ4_SetFont, NameFont=tFont, /TrueType $
ELSE EZ4_SetFont, NameFont=tFont
XYOUTS, centerX, titleY, title, ALIGNMENT=0.5, /NORMAL, $
          COLOR=tcolor, CHARSIZE=tsize

; write the comment text
comment = plot.comment
cfont = plot.cFont
csize = plot.cSize
ccolor = plot.cColor

IF (dopsplot) THEN cfont=psFont+' BOLD'
IF (dopsplot) THEN EZ4_SetFont, NameFont=cFont, /TrueType $
ELSE EZ4_SetFont, NameFont=cFont
XYOUTS, centerX, commentY, comment, ALIGNMENT=0.5, /NORMAL, $
    COLOR=ccolor, CHARSIZE=csize


;; write the date
IF plot.showDate THEN BEGIN
  date=plot.date
  dFont=plot.dFont
  dSize=plot.dSize
  dColor=plot.dColor

  IF (dopsplot) THEN dFont=psFont
  IF (dopsplot) THEN EZ4_SetFont, NameFont=dFont, /TrueType $
  ELSE EZ4_SetFont, NameFont=dFont
  XYOUTS, dateX, dateY, date, ALIGNMENT=dateAlign, /NORMAL, $
         COLOR=dColor, CHARSIZE=dSize
ENDIF

;; write the filename
IF plot.showFileName THEN BEGIN
  fileName=plot.fileName
  fFont=plot.fFont
  fSize=plot.fSize
  fColor=plot.fColor

  IF (dopsplot) THEN fFont=psFont
  IF (dopsplot) THEN EZ4_SetFont, NameFont=fFont, /TrueType $
  ELSE EZ4_SetFont, NameFont=fFont
  XYOUTS, filenameX, filenameY, fileName, ALIGNMENT=fileAlign, /NORMAL, COLOR=fColor, $
          CHARSIZE=fSize
ENDIF

; write the simulation label
IF plot.showLabel THEN BEGIN
   simlabel = plot.simlabel
   lFont = plot.lFont
   lSize = plot.lSize
   lColor = plot.lColor

   IF (dopsplot) THEN lFont=psFont+' BOLD'
   IF (dopsplot) THEN EZ4_SetFont, NameFont=lFont, /TrueType $
   ELSE EZ4_SetFont, NameFont=lFont
   XYOUTS, labelX, labelY, simlabel, ALIGNMENT=labelAlign, /NORMAL, $
        COLOR=lColor, CHARSIZE=lSize
ENDIF

;; write the mean
IF (plot.showMean EQ 1) THEN BEGIN
    meanValue=plot.meanValue
    mFont=plot.mFont
    mSize=plot.mSize
    mColor=plot.mColor

    IF (dopsplot) THEN mFont=psFont
    IF (dopsplot) THEN EZ4_SetFont, NameFont=mFont, /TrueType $
    ELSE EZ4_SetFont, NameFont=mFont

    tempFormat = ez4_niceformat(meanValue)
    meanString = STRCOMPRESS('Global Mean = ' $
                  + STRING(meanValue, FORMAT=tempFormat))
    XYOUTS, meanX, meanY, meanString, ALIGNMENT=meanAlign, /NORMAL, $
            COLOR=mColor, CHARSIZE=mSize
ENDIF

;; restore font settings
EZ4_SetFont

END

