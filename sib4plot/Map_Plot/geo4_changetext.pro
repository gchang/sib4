;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
PRO GEO4_ChangeText_Panel_Event, event

WIDGET_CONTROL, event.top, GET_UVALUE=infoBase
WIDGET_CONTROL, infoBase, GET_UVALUE=info

button = event.id
IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
  button = info.cancButton

IF button EQ info.meanCheck THEN BEGIN
  info.showMean = event.select
  WIDGET_CONTROL, info.meanFormatField, SENSITIVE=info.showMean
  WIDGET_CONTROL, info.meanSizeField, SENSITIVE=info.showMean
  WIDGET_CONTROL, info.meanPSSizeField, SENSITIVE=info.showMean
  WIDGET_CONTROL, info.meanColorField, SENSITIVE=info.showMean
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
  RETURN
ENDIF

IF button EQ info.dateCheck THEN BEGIN
  info.showDate = event.select
  WIDGET_CONTROL, info.dateField, SENSITIVE=info.showDate
  WIDGET_CONTROL, info.dateSizeField, SENSITIVE=info.showDate
  WIDGET_CONTROL, info.datePSSizeField, SENSITIVE=info.showDate
  WIDGET_CONTROL, info.dateColorField, SENSITIVE=info.showDate
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
  RETURN
ENDIF

IF button EQ info.fileNameCheck THEN BEGIN
  info.showfileName = event.select
  WIDGET_CONTROL, info.fileNameField, SENSITIVE=info.showfileName
  WIDGET_CONTROL, info.fileNameSizeField, SENSITIVE=info.showfileName
  WIDGET_CONTROL, info.fileNamePSSizeField, SENSITIVE=info.showfileName
  WIDGET_CONTROL, info.fileNameColorField, SENSITIVE=info.showfileName
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
  RETURN
ENDIF

IF button EQ info.okButton THEN BEGIN
  ;Get Title Information
  WIDGET_CONTROL, info.titleField, GET_VALUE=title
  info.title=title[0]
  WIDGET_CONTROL, info.titleSizeField, GET_VALUE=tsize
  info.titleSize=tsize
  WIDGET_CONTROL, info.titlePSSizeField, GET_VALUE=tpssize
  info.titlePSSize=tpssize
  WIDGET_CONTROL, info.titleColorField, GET_VALUE=tcolor
  info.titleColor=tcolor

  ;Get Comment Information
  WIDGET_CONTROL, info.commentField, GET_VALUE=comment
  info.comment = comment[0]
  WIDGET_CONTROL, info.commentSizeField, GET_VALUE=csize
  info.commentSize=csize
  WIDGET_CONTROL, info.commentPSSizeField, GET_VALUE=cpssize
  info.commentPSSize=cpssize
  WIDGET_CONTROL, info.commentColorField, GET_VALUE=ccolor
  info.commentColor=ccolor

  ;Get Sim Label Information
  WIDGET_CONTROL, info.simlabelField, GET_VALUE=simlabel
  info.simlabel = simlabel[0]
  WIDGET_CONTROL, info.simlabelSizeField, GET_VALUE=simlabelsize
  info.simlabelsize = simlabelsize
  WIDGET_CONTROL, info.simlabelPSSizeField, GET_VALUE=simlabelpssize
  info.simlabelPSSize = simlabelpssize
  WIDGET_CONTROL, info.simlabelColorField, GET_VALUE=simlabelcolor
  info.simlabelColor = simlabelcolor

  ;Get Date Information
  IF (info.showDate EQ 1) THEN BEGIN
      WIDGET_CONTROL, info.dateField, GET_VALUE=date
      info.date = date
      WIDGET_CONTROL, info.dateSizeField, GET_VALUE=datesize
      info.dateSize = datesize
      WIDGET_CONTROL, info.datePSSizeField, GET_VALUE=datepssize
      info.datePSSize = datepssize
      WIDGET_CONTROL, info.dateColorField, GET_VALUE=datecolor
      info.dateColor = datecolor
  ENDIF   

  ;Get File Name Information
  IF (info.showFileName EQ 1) THEN BEGIN
      WIDGET_CONTROL, info.fileNameField, GET_VALUE=fileName
      info.fileName = fileName
      WIDGET_CONTROL, info.fileNameSizeField, GET_VALUE=fileNamesize
      info.fileNameSize = fileNamesize
      WIDGET_CONTROL, info.fileNamePSSizeField, GET_VALUE=fileNamepssize
      info.fileNamePSSize = fileNamepssize
      WIDGET_CONTROL, info.fileNameColorField, GET_VALUE=fileNamecolor
      info.fileNameColor = fileNamecolor
  ENDIF   

  ;Get Mean Information
  IF (info.showMean EQ 1) THEN BEGIN
      WIDGET_CONTROL, info.meanFormatField, GET_VALUE=meanFormat
      info.meanFormat = meanFormat
      IF info.meanFormat NE '' THEN $
         info.meanFormat = '('+info.meanFormat+')'
      WIDGET_CONTROL, info.meanSizeField, GET_VALUE=meansize
      info.meanSize = meansize
      WIDGET_CONTROL, info.meanPSSizeField, GET_VALUE=meanpssize
      info.meanPSSize = meanpssize
      WIDGET_CONTROL, info.meanColorField, GET_VALUE=meancolor
      info.meanColor = meancolor
  ENDIF   

  info.ok = 1
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
ENDIF ELSE info.ok = 0

IF button EQ info.okButton OR button EQ info.cancButton THEN $
  WIDGET_CONTROL, event.top, /DESTROY

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window to change plot title and comments contour plots
;;
FUNCTION GEO4_ChangeText_Panel, plot

@ez4_BasicParams.com

base = WIDGET_BASE(TITLE='Edit Contour Plot Text Fields', SPACE=20, $
                   /COLUMN, /TLB_KILL_REQUEST_EVENTS)

;; Title Fields
mytitle   = plot.title
titleField = CW_FIELD(base, TITLE='Plot Title:  ', VALUE=mytitle, $
                       XSIZE=70)

base2 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
mytitleSize = FIX(EZ4_getFontSize(plot.tFont))
titleSizeField = CW_FIELD(base2, TITLE='Title Font Size: ', VALUE=mytitleSize, $
                    XSIZE=2)
mytitlePSSize = plot.tSize
titlePSSizeField = CW_FIELD(base2, TITLE='Title Char Size: ', VALUE=mytitlePSSize, $
                    XSIZE=4)
mytitleColor = plot.tColor
titleColorField = CW_FIELD(base2, TITLE='Title Color: ', VALUE=mytitleColor, $
                    XSIZE=4)

;Comment Fields
mycomment = plot.comment
base3 = WIDGET_BASE(base, /ROW, /BASE_ALIGN_LEFT)
commentField = CW_FIELD(base3, TITLE='Comment:', VALUE=mycomment, $
                    XSIZE=40)
mycommentSize = FIX(EZ4_getFontSize(plot.cFont))
commentSizeField = CW_FIELD(base3, TITLE='Font Size:', VALUE=mycommentSize, $
                    XSIZE=2)
mycommentPSSize = plot.cSize
commentPSSizeField = CW_FIELD(base3, TITLE='Char Size:', VALUE=mycommentPSSize, $
                    XSIZE=4)
mycommentColor = plot.cColor
commentColorField = CW_FIELD(base3, TITLE='Color:', VALUE=mycommentColor, $
                    XSIZE=4)

;Sim Label Fields
base4 = WIDGET_BASE(base, /ROW, /BASE_ALIGN_LEFT)
mysimlabel = plot.simlabel
simlabelField = CW_FIELD(base4, TITLE='Sim Label:', VALUE=mysimlabel, $
                 XSIZE=40)
mysimSize = FIX(EZ4_getFontSize(plot.lFont))
simlabelsizeField = CW_FIELD(base4, TITLE='Font Size:', VALUE=mysimSize, $
                 XSIZE=2)
mysimPSSize = plot.lSize
simlabelPSSizeField = CW_FIELD(base4, TITLE='Char Size:', VALUE=mysimPSSize, $
                 XSIZE=4)
mysimColor = plot.lColor
simlabelcolorField = CW_FIELD(base4, TITLE='Color:', VALUE=mysimColor, $
                 XSIZE=4)

;Date Fields
base5 = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base6 = WIDGET_BASE(base5, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
dateCheck = WIDGET_BUTTON(base6, VALUE='Show Date')

base7 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
mydate = plot.date
dateField = CW_FIELD(base7, TITLE='Date:', VALUE=mydate, XSIZE=30)
mydateSize = FIX(EZ4_getFontSize(plot.dFont))
datesizeField = CW_FIELD(base7, TITLE='Font Size:', VALUE=mydateSize, XSIZE=2)
mydatePSSize = plot.dSize
datePSSizeField = CW_FIELD(base7, TITLE='Char Size:', VALUE=mydatePSSize, XSIZE=4)
mydateColor = plot.dColor
datecolorField = CW_FIELD(base7, TITLE='Color:', VALUE=mydateColor, XSIZE=4)


WIDGET_CONTROL, dateCheck, SET_BUTTON=plot.showDate
showDate = plot.showDate

;File Name Fields
base8 = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base9 = WIDGET_BASE(base8, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
fileNameCheck = WIDGET_BUTTON(base9, VALUE='Show File Name')

base10 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
myFileName = plot.fileName
fileNameField = CW_FIELD(base10, TITLE='File Name:', VALUE=myFileName, XSIZE=30)
myFileNameSize = FIX(EZ4_getFontSize(plot.fFont))
fileNameSizeField = CW_FIELD(base10, TITLE='Font Size:', VALUE=myfileNameSize, XSIZE=2)
myFileNamePSSize = plot.fSize
fileNamePSSizeField = CW_FIELD(base10, TITLE='Char Size:', VALUE=myfileNamePSSize, XSIZE=4)
myFileNameColor = plot.fColor
fileNameColorField = CW_FIELD(base10, TITLE='Color:',VALUE=myfileNameColor, XSIZE=4)

WIDGET_CONTROL, fileNameCheck, SET_BUTTON=plot.showFileName
showFileName = plot.showFileName

;; Fields for mean value and format
base1         = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base2         = WIDGET_BASE(base1, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
meanCheck     = WIDGET_BUTTON(base2, VALUE='Show Mean Value')

base3         = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, XPAD=0, YPAD=0)
mymeanFormat    = STRMID(plot.mFormat, 1, STRLEN(plot.mFormat)-2)
meanFormatField = CW_FIELD(base3, TITLE='Mean Value Format:', VALUE=mymeanFormat, $
                  XSIZE=10)
mymeanSize = FIX(EZ4_GetFontSize(plot.mFont))
meanSizeField = CW_FIELD(base3, TITLE='Mean Font Size:', VALUE=mymeanSize, $
                  XSIZE=2)
mymeanPSSize = plot.mSize
meanPSSizeField = CW_FIELD(base3, TITLE='Mean Char Size:', VALUE=mymeanPSSize, $
                  XSIZE=4)
mymeanColor = plot.mColor
meanColorField = CW_FIELD(base3, TITLE='Mean Color:',VALUE=mymeanColor, $
                  XSIZE=4)

WIDGET_CONTROL, meanCheck, SET_BUTTON=plot.showMean
showMean = plot.showMean

row = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, SPACE=100, /FRAME)
okButton   = WIDGET_BUTTON(row, VALUE='   OK   ' )
cancButton = WIDGET_BUTTON(row, VALUE=' CANCEL ' )

info = { ok:0, $
         title:mytitle, titleField:titleField,     $
         titlesize:mytitlesize, titlesizeField:titlesizeField, $
         titlePSSize:mytitlePSSize, titlePSSizeField:titlePSSizeField, $
         titlecolor:mytitlecolor, titlecolorField:titlecolorField, $
         comment:mycomment, commentField:commentField, $
         commentsize:mycommentsize, commentsizeField:commentsizeField, $
         commentPSSize:mycommentPSSize, commentPSsizeField:commentPSSizeField, $
         commentcolor:mycommentcolor, commentcolorField:commentcolorField, $
         simlabel:mysimlabel, simlabelField:simlabelField, $
         simlabelsize:mysimsize, simlabelsizeField:simlabelsizeField, $
         simlabelPSSize:mysimPSSize, simlabelPSSizeFIeld:simlabelPSSizeField, $
         simlabelcolor:mysimcolor, simlabelcolorField:simlabelcolorField, $
         showDate:showDate, dateCheck:dateCheck, $
         date:mydate, dateField:dateField, $
         datesize:mydatesize, datesizeField:datesizeField, $
         datePSSize:mydatePSSize, datePSSizeField:datePSSizeField, $
         datecolor:mydatecolor, datecolorField:datecolorField, $
         showFileName:showFileName, fileNameCheck:fileNameCheck, $
         fileName:myFileName, fileNameField:fileNameField, $
         fileNameSize:myFileNameSize, fileNameSizeField:fileNameSizeField, $
         fileNamePSSize:myFileNamePSSize, fileNamePSSizeField:fileNamePSSizeField, $
         fileNameColor:myFileNameColor, fileNameColorField:fileNameColorField, $
         showmean:showmean, meanCheck:meanCheck,  $
         meanFormat:mymeanFormat, meanformatField:meanformatField, $
         meansize:mymeanSize, meansizeField:meansizeField, $
         meanPSSize:mymeanPSSize, meanPSSizeFIeld:meanPSSizeField, $
         meancolor:mymeancolor, meancolorField:meancolorField, $
         okButton:okButton, cancButton:cancButton}

infoBase = WIDGET_BASE(UVALUE=info)

WIDGET_CONTROL, base, /REALIZE, /SHOW, SET_UVALUE=infoBase

XMANAGER, 'GEO4_ChangeText_Panel', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=info, /DESTROY

;; If OK button is pressed, add field values to the returned struct
IF info.ok THEN BEGIN

   IF (info.meanFormat NE '') THEN BEGIN
       info.meanFormat = '(' + info.meanFormat + ')'
   ENDIF ELSE BEGIN
       info.meanFormat = plot.mFormat
   ENDELSE

ENDIF ELSE info = { ok:0 }

RETURN, info

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Change plot title, comments, data sets properties, ...
;;
PRO GEO4_ChangeText, plot

;; Display text panel to change title, comments, etc ..
info = GEO4_ChangeText_Panel(plot)

IF info.ok THEN BEGIN
  ;Set Title Information
  plot.title      = info.title
  tfontSize = FIX(EZ4_GetFontSize(plot.tFont))
  IF (tfontSize NE info.titleSize) THEN $
     plot.tFont = EZ4_setFontSize(plot.tFont,info.titleSize)
  plot.tSize      = info.titlePSSize
  plot.tColor     = info.titleColor

  ;Set Comment Information
  plot.comment    = info.comment
  cfontSize = FIX(EZ4_GetFontSize(plot.cFont))
  IF (cfontSize NE info.commentSize) THEN $
      plot.cFont = EZ4_setFontSize(plot.cFont,info.commentSize)
  plot.cSize      = info.commentPSSize
  plot.cColor     = info.commentColor

  ;Set Sim Label Information
  plot.simlabel = info.simlabel
  simfontSize = FIX(EZ4_GetFontSize(plot.lFont))
  IF (simFontSize NE info.simLabelSize) THEN $
      plot.lFont = EZ4_setFontSize(plot.lFont,info.simLabelSize)
  plot.lSize = info.simLabelPSSize
  plot.lColor = info.simLabelColor

  ;Set Date Information
  plot.showDate = info.showDate
  IF (plot.showDate EQ 1) THEN BEGIN
      plot.date = info.date
      dfontSize = FIX(EZ4_GetFontSize(plot.dFont))
      IF (dfontSize NE info.dateSize) THEN $
         plot.dFont = EZ4_setFontSize(plot.dFont,info.dateSize)
      plot.dSize = info.datePSSize
      plot.dColor = info.dateColor
   ENDIF

  ;Set File Name Information
   plot.showFileName = info.showFileName
   IF (plot.showFileName EQ 1) THEN BEGIN
       plot.fileName = info.fileName
       ffontSize = FIX(EZ4_GetFontSize(plot.fFont))
      IF (ffontSize NE info.fileNameSize) THEN $
         plot.fFont = EZ4_setFontSize(plot.fFont,info.fileNameSize)
      plot.fSize = info.fileNamePSSize
      plot.fColor = info.fileNameColor
   ENDIF

  ;Set Mean Information
  plot.showMean = info.showMean
  IF (plot.showMean EQ 1) THEN Begin
     mfontSize = FIX(EZ4_GetFontSize(plot.mFont))
     IF (mfontSize NE info.meanSize) THEN $
       plot.mFont = EZ4_setFontSize(plot.mFont,info.meanSize)
     plot.mSize = info.meanPSSize
     plot.mColor = info.meanColor
     plot.mFormat = info.meanFormat
  ENDIF
ENDIF ;info.ok

  GEO4_DrawPlot, plot

END
