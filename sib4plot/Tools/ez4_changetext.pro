;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
PRO EZ4_ChangeText_Panel_Event, event

WIDGET_CONTROL, event.top, GET_UVALUE=infoBase
WIDGET_CONTROL, infoBase, GET_UVALUE=info

button = event.id
IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN $
  button = info.cancButton

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

IF button EQ info.meanCheck THEN BEGIN
  info.showMean = event.select
  WIDGET_CONTROL, info.meanFormatField, SENSITIVE=info.showMean
  WIDGET_CONTROL, info.meanSizeField, SENSITIVE=info.showMean
  WIDGET_CONTROL, info.meanPSSizeField, SENSITIVE=info.showMean
  WIDGET_CONTROL, info.meanColorField, SENSITIVE=info.showMean
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
  RETURN
ENDIF

IF button EQ info.simLabelCheck THEN BEGIN
  info.showSimLabel = event.select
  WIDGET_CONTROL, info.simLabelField, SENSITIVE=info.showSimLabel
  WIDGET_CONTROL, info.simLabelSizeField, SENSITIVE=info.showSimLabel
  WIDGET_CONTROL, info.simLabelPSSizeField, SENSITIVE=info.showSimLabel
  WIDGET_CONTROL, info.simLabelColorField, SENSITIVE=info.showSimLabel
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

  ;Get Sim Label Information
  IF (info.showSimLabel EQ 1) THEN BEGIN
     WIDGET_CONTROL, info.simlabelField, GET_VALUE=simlabel
     info.simlabel = simlabel[0]
     WIDGET_CONTROL, info.simlabelSizeField, GET_VALUE=simlabelsize
     info.simlabelsize = simlabelsize
     WIDGET_CONTROL, info.simlabelPSSizeField, GET_VALUE=simlabelpssize
     info.simlabelPSSize = simlabelpssize
     WIDGET_CONTROL, info.simlabelColorField, GET_VALUE=simlabelcolor
     info.simlabelColor = simlabelcolor
  ENDIF

  info.ok = 1
  WIDGET_CONTROL, infoBase, SET_UVALUE=info
ENDIF ELSE info.ok = 0

IF button EQ info.okButton OR button EQ info.cancButton THEN $
  WIDGET_CONTROL, event.top, /DESTROY

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Display a window to change plot title, comments, labels,
;;  line color and line style.
;;
FUNCTION EZ4_ChangeText_Panel, plot


;; Determine some fields to be added according to plot type
nDataSets = TOTAL(plot.drawLines)

base = WIDGET_BASE(TITLE='Edit Text Fields', SPACE=20, $
                   /COLUMN, /TLB_KILL_REQUEST_EVENTS)

;; Fields for plot title
mytitle   = plot.title
titleField = CW_FIELD(base, TITLE='Plot Title:  ', VALUE=mytitle,XSIZE=70)

base2 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
mytitleSize = FIX(EZ4_getFontSize(plot.tFont))
titleSizeField = CW_FIELD(base2, TITLE='Title Font Size:', VALUE=mytitleSize, XSIZE=2)
mytitlePSSize = plot.tSize
titlePSSizeField = CW_FIELD(base2, TITLE='Title Char Size:', VALUE=mytitlePSSize, $
        XSIZE=4)
mytitleColor = plot.tColor
titleColorField = CW_FIELD(base2, TITLE='Title Color:', VALUE=mytitleColor, $
        XSIZE=4)

;Comment Fields
mycomment = plot.comment
base3 = WIDGET_BASE(base, /ROW, /ALIGN_LEFT)
commentField = CW_FIELD(base3, TITLE='Comment:', VALUE=mycomment, XSIZE=40)
mycommentSize = FIX(EZ4_getFontSize(plot.cFont))
commentSizeField = CW_FIELD(base3, TITLE='Font Size:', VALUE=mycommentSize, $
                    XSIZE=2)
mycommentPSSize = plot.cSize
commentPSSizeField = CW_FIELD(base3, TITLE='Char Size:', VALUE=mycommentPSSize, $
                    XSIZE=4)
mycommentColor = plot.cColor
commentColorField = CW_FIELD(base3, TITLE='Color:', VALUE=mycommentColor, $
                    XSIZE=4)

;Date Fields
base4 = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base5 = WIDGET_BASE(base4, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
dateCheck = WIDGET_BUTTON(base5, VALUE='Show Date')

base6 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
mydate = plot.date
dateField = CW_FIELD(base6, TITLE='Date:', VALUE=mydate, XSIZE=30)
mydateSize = FIX(EZ4_getFontSize(plot.dFont))
datesizeField = CW_FIELD(base6, TITLE='Font Size:', VALUE=mydateSize, XSIZE=2)
mydatePSSize = plot.dSize
datePSSizeField = CW_FIELD(base6, TITLE='Char Size:', VALUE=mydatePSSize, XSIZE=4)
mydateColor = plot.dColor
datecolorField = CW_FIELD(base6, TITLE='Color:', VALUE=mydateColor, XSIZE=4)

showDate = plot.showDate
WIDGET_CONTROL, dateCheck, SET_BUTTON=plot.showDate
WIDGET_CONTROL, dateField, SENSITIVE=showDate
WIDGET_CONTROL, dateSizeField, SENSITIVE=showDate
WIDGET_CONTROL, datePSSizeField, SENSITIVE=showDate
WIDGET_CONTROL, dateColorField, SENSITIVE=showDate

;File Name Fields
base7 = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base8 = WIDGET_BASE(base7, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
fileNameCheck = WIDGET_BUTTON(base8, VALUE='Show File Name')

base9 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER)
myFileName = plot.fileName
fileNameField = CW_FIELD(base9, TITLE='File Name:', VALUE=myFileName, XSIZE=30)
myFileNameSize = FIX(EZ4_getFontSize(plot.fFont))
fileNameSizeField = CW_FIELD(base9, TITLE='Font Size:', VALUE=myfileNameSize, XSIZE=2)
myFileNamePSSize = plot.fSize
fileNamePSSizeField = CW_FIELD(base9, TITLE='Char Size:', VALUE=myfileNamePSSize, XSIZE=4)
myFileNameColor = plot.fColor
fileNameColorField = CW_FIELD(base9, TITLE='Color:',VALUE=myfileNameColor, XSIZE=4)

showFileName = plot.showFileName
WIDGET_CONTROL, fileNameCheck, SET_BUTTON=showFileName
WIDGET_CONTROL, fileNameField, SENSITIVE=showfileName
WIDGET_CONTROL, fileNameSizeField, SENSITIVE=showfileName
WIDGET_CONTROL, fileNamePSSizeField, SENSITIVE=showfileName
WIDGET_CONTROL, fileNameColorField, SENSITIVE=showfileName

;Sim Label Fields
base10 = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base11 = WIDGET_BASE(base10, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
simLabelCheck = WIDGET_BUTTON(base11, VALUE='Show Sim Label')

base12 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, XPAD=0, YPAD=0)
mysimlabel = plot.simlabel
simlabelField = CW_FIELD(base12, TITLE='Sim Label:', VALUE=mysimlabel, XSIZE=40)
mysimSize = FIX(EZ4_getFontSize(plot.simFont))
simlabelsizeField = CW_FIELD(base12, TITLE='Font Size:', VALUE=mysimSize, $
                 XSIZE=2)
mysimPSSize = plot.simSize
simlabelPSSizeField = CW_FIELD(base12, TITLE='Char Size:', VALUE=mysimPSSize, $
                 XSIZE=4)
mysimColor = plot.simColor
simlabelcolorField = CW_FIELD(base12, TITLE='Color:', VALUE=mysimColor, $
                 XSIZE=4)

showSimLabel = plot.showSimLabel
WIDGET_CONTROL, simLabelCheck, SET_BUTTON=showSimLabel
WIDGET_CONTROL, simLabelField, SENSITIVE=showSimLabel
WIDGET_CONTROL, simLabelSizeField, SENSITIVE=showSimLabel
WIDGET_CONTROL, simLabelPSSizeField, SENSITIVE=showSimLabel
WIDGET_CONTROL, simLabelColorField, SENSITIVE=showSimLabel


;; Fields for mean value and format
base13     = WIDGET_BASE(base, /COLUMN, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
base14     = WIDGET_BASE(base13, /COLUMN, XPAD=0, YPAD=0, /NONEXCLUSIVE)
meanCheck  = WIDGET_BUTTON(base14, VALUE='Show Mean Value')

base15 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, XPAD=0, YPAD=0)
mymeanFormat = STRMID(plot.meanFormat, 1, STRLEN(plot.meanFormat)-2)
meanFormatField = CW_FIELD(base15, TITLE='Mean Value Format:', VALUE=mymeanFormat, XSIZE=10)
mymeanSize = FIX(EZ4_GetFontSize(plot.mFont))
meanSizeField = CW_FIELD(base15, TITLE='Mean Font Size:', VALUE=mymeanSize, $
                  XSIZE=2)
mymeanPSSize = plot.mSize
meanPSSizeField = CW_FIELD(base15, TITLE='Mean Char Size:', VALUE=mymeanPSSize, $
                  XSIZE=4)
mymeanColor = plot.mColor
meanColorField = CW_FIELD(base15, TITLE='Mean Color:',VALUE=mymeanColor, $
                  XSIZE=4)

showMean = plot.showMean
WIDGET_CONTROL, meanCheck, SET_BUTTON=showMean
WIDGET_CONTROl, meanFormatField, SENSITIVE=showMean
WIDGET_CONTROL, meanSizeField, SENSITIVE=showMean
WIDGET_CONTROL, meanPSSizeField, SENSITIVE=showMean
WIDGET_CONTROL, meanColorField, SENSITIVE=showMean

;OK and Cancel
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
         showSimLabel:showSimLabel, simLabelCheck:simLabelCheck, $
         simlabel:mysimlabel, simlabelField:simlabelField, $
         simlabelsize:mysimsize, simlabelsizeField:simlabelsizeField, $
         simlabelPSSize:mysimPSSize, simlabelPSSizeFIeld:simlabelPSSizeField, $
         simlabelcolor:mysimcolor, simlabelcolorField:simlabelcolorField, $

         okButton:okButton, cancButton:cancButton}

infoBase = WIDGET_BASE(UVALUE=info)

WIDGET_CONTROL, base, /REALIZE, /SHOW, SET_UVALUE=infoBase

XMANAGER, 'EZ4_ChangeText_Panel', base, /MODAL

WIDGET_CONTROL, infoBase, GET_UVALUE=info, /DESTROY

;; If OK button is pressed, add the different field values to the returned 
;; struct
IF info.ok THEN BEGIN

   IF (info.meanFormat NE '') THEN BEGIN
       info.meanFormat = '(' + info.meanFormat + ')'
   ENDIF ELSE BEGIN
       info.meanFormat = plot.meanFormat
   ENDELSE

ENDIF ELSE info = { ok:0 }

RETURN, info

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Change plot title, comments, data sets properties, ...
;;
PRO EZ4_ChangeText, plot

;; Display text panel to change title, labels, etc ..
text = EZ4_ChangeText_Panel(plot)

IF text.ok THEN BEGIN
  ;Set Title Information
  plot.title      = text.title
  tfontSize = FIX(EZ4_getFontSize(plot.tFont))
  IF (tfontSize NE text.titleSize) THEN $
      plot.tFont = EZ4_setFontSize(plot.tFont,text.titleSize)
  plot.tSize = text.titlePSSize
  plot.tColor = text.titleColor

  ;Set Comment Information
  plot.comment    = text.comment
  cfontSize = FIX(EZ4_GetFontSize(plot.cFont))
  IF (cFontSize NE text.commentSize) THEN $
     plot.cFont = EZ4_setFontSize(plot.cFont, text.commentSize)
   plot.cSize = text.commentPSSize
   plot.cColor = text.commentColor

  ;Set Date Information
  plot.showDate = text.showDate
  IF (plot.showDate EQ 1) THEN BEGIN
      plot.date = text.date
      dfontSize = FIX(EZ4_GetFontSize(plot.dFont))
      IF (dfontSize NE text.dateSize) THEN $
         plot.dFont = EZ4_setFontSize(plot.dFont,text.dateSize)
      plot.dSize = text.datePSSize
      plot.dColor = text.dateColor
   ENDIF

  ;Set File Name Information
   plot.showFileName = text.showFileName
   IF (plot.showFileName EQ 1) THEN BEGIN
       plot.fileName[0] = text.fileName[0]
       ffontSize = FIX(EZ4_GetFontSize(plot.fFont))
      IF (ffontSize NE text.fileNameSize) THEN $
         plot.fFont = EZ4_setFontSize(plot.fFont,text.fileNameSize)
      plot.fSize = text.fileNamePSSize
      plot.fColor = text.fileNameColor
   ENDIF

  ;Set Mean Information
  plot.showMean = text.showMean
  IF (plot.showMean EQ 1) THEN Begin
     mfontSize = FIX(EZ4_GetFontSize(plot.mFont))
     IF (mfontSize NE text.meanSize) THEN $
       plot.mFont = EZ4_setFontSize(plot.mFont,text.meanSize)
     plot.mSize = text.meanPSSize
     plot.mColor = text.meanColor
     plot.meanFormat = text.meanFormat
  ENDIF

  ;Set Sim Label Information
  plot.showsimLabel = text.showsimLabel
  IF (plot.showsimLabel EQ 1) THEN BEGIN
     plot.simlabel = text.simlabel
     simfontSize = FIX(EZ4_GetFontSize(plot.simFont))
     IF (simFontSize NE text.simLabelSize) THEN $
        plot.simFont = EZ4_setFontSize(plot.simFont,text.simLabelSize)
     plot.simSize = text.simLabelPSSize
     plot.simColor = text.simLabelColor
   ENDIF

ENDIF

END
