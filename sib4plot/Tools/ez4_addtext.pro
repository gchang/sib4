;======================================================
PRO EZ4_addText, plot, POSITION=pos
;======================================================

@ez4_BasicParams.com

leftX = 0.13 & rightX = 0.83
centerX = 0.5
titleY  = 0.94
commentY = 0.89

dateX = datePosXL
dateY = datePosYL
dateXleft = dateX - 0.10
dateXright = dateX + 0.05
dateYtop = dateY + 0.04
dateYbot = dateY - 0.006

filenameX = fileposXL
filenameY = fileposYL
fileXleft = filenameX - 0.05
fileXright = filenameY + 0.20
fileYtop = filenameY + 0.04
fileYbot = filenameY - 0.006

meanX = meanPosXL
meanY = meanPosYL
meanXleft = meanX - 0.10
meanXright = meanX + 0.05
meanYtop = meanY + 0.04
meanYbot = meanY - 0.006

simX = simPosXL
simY = simPosYL
simXleft = simX - 0.01
simXright = simX + 0.15
simYtop = simY + 0.04
simYbot = simY - 0.006

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

; white out the mean
IF (plot.showMean NE -1) THEN BEGIN
   POLYFILL, [meanXleft,meanXright,meanXright,meanXleft], $
             [meanYtop,meanYtop,meanYbot,meanYBot], $
             COLOR=white, /NORMAL
ENDIF

; white out the simulation label
IF (plot.showSimLabel NE -1) THEN BEGIN
    POLYFILL, [simXleft,simXright,simXright,simXleft], $
          [simYtop,simYtop,simYbot,simYbot], $
          COLOR=white, /NORMAL
ENDIF

;;;;;Write out the labels if selected
; write the main title 
title = plot.title
tfont = plot.tfont
tsize = plot.tsize
tcolor = plot.tcolor

IF (dopsplot) THEN BEGIN
   tfont=psFont+ ' BOLD'
   tsize*=1.9
ENDIF ELSE BEGIN
   myfontsize=EZ4_GetFontSize(tfont)

   fref=WHERE(fontsizelist EQ myfontsize)
   result = strlen(strtrim(title,2))
   IF ((fref[0] GT 0) AND (result GT 55)) THEN myfontsize=fontsizelist(fref-1)
   IF ((fref[0] GT 0) AND (result GT 70)) THEN myfontsize=fontsizelist(fref-2)

   IF (myfontsize GT 9) THEN newsizefont=STRING(myfontsize,FORMAT='(I2)')  $
   ELSE newsizeFont=STRING(myfontsize,FORMAT='(I1)')

   tfont=EZ4_setfontsize(tfont,newsizeFont)

ENDELSE

IF (dopsplot) THEN EZ4_SetFont, NameFont=tFont, /TrueType $
ELSE EZ4_SetFont, NameFont=tFont
XYOUTS, centerX, titleY, title, ALIGNMENT=0.5, /NORMAL, $
          COLOR=tcolor, CHARSIZE=tsize

; write the comment text
comment = plot.comment
cfont = plot.cfont
csize = plot.csize
ccolor = plot.ccolor

IF (dopsplot) THEN cfont=psFont+' BOLD'
If (dopsplot) THEN EZ4_SetFont, NameFont=cFont, /TrueType $
ELSE EZ4_SetFont, NameFont=cfont
XYOUTS, centerX, commentY, comment, ALIGNMENT=0.5, /NORMAL, $
     COLOR=ccolor, CHARSIZE=csize


;; write the date
IF plot.showDate THEN BEGIN
  date=plot.date
  dFont=plot.dFont
  dSize=plot.dSize
  dColor=plot.dColor

  IF (dopsplot) THEN BEGIN
       dFont=psFontL
       EZ4_SetFont, NameFont=dFont, /TrueType
  ENDIF ELSE EZ4_SetFont, NameFont=dFont

  XYOUTS, dateX, dateY, date, ALIGNMENT=dateAlignL, /NORMAL, $
         COLOR=dColor, CHARSIZE=dSize
ENDIF


;; write the filename
IF plot.showFileName THEN BEGIN
   fileName=plot.fileName[0]
   fFont=plot.fFont
   fSize=plot.fSize
   fColor=plot.fColor

  IF (dopsplot) THEN EZ4_SetFont, NameFont=fFont, /TrueType $
  ELSE EZ4_SetFont, NameFont=fFont
  XYOUTS, filenameX, filenameY, fileName, ALIGNMENT=fileAlignL, /NORMAL, $
          COLOR=fColor, CHARSIZE=fsize
ENDIF


;; write the mean
IF (plot.showMean EQ 1) THEN BEGIN
    meanValue = plot.meanValue
    mFont = plot.mFont
    mSize = plot.mSize
    mColor= plot.mColor

    IF (dopsplot) THEN mFont=psFontL
    IF (dopsplot) THEN EZ4_SetFont, NameFont=mFont, /TrueType $
    ELSE EZ4_SetFont, NameFont=mFont

    tempFormat = ez4_niceformat(meanValue)
    plot.meanFormat = tempFormat

    meantxt = 'Mean = '
    meanString = STRCOMPRESS(meantxt + STRING(plot.meanvalue, FORMAT=plot.meanFormat))    
    XYOUTS, meanX, meanY, meanString, ALIGNMENT=meanAlignL, /NORMAL, $
            COLOR=mColor, CHARSIZE=meanSize
ENDIF


; write the simulation label
IF (plot.showSimLabel EQ 1) THEN BEGIN
   simlabel = plot.simlabel
   simFont = plot.simFont
   simSize = plot.simSize
   simColor = plot.simColor

   IF (dopsplot) THEN simFont=psFont+ ' BOLD'
   IF (dopsplot) THEN EZ4_SetFont, NameFont=simFont, /TrueType $
   ELSE EZ4_SetFont, NameFont=simFont
   XYOUTS, simX, simY, simlabel, ALIGNMENT=simAlignL, /NORMAL, $
        COLOR=simColor, CHARSIZE=simSize
ENDIF

;; restore font settings
EZ4_SetFont

END

