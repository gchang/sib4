;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Save a plot to a file in various formats
;;  
;;  Line:       If set, will offer to save a line plot
;;  Map:        If set, will offer to save a map plot
;;  newName:	If set, will prompt for another filename to save to as well as
;;		the file type. Otherwise the same file name and type are used.
;;  textSize:	Text size used for saving postscript files
;;
PRO EZ4_SaveFile, plot, fileType=fileType, newName=newName, textSize=textSize

@ez4_BasicParams.com

;; If unknown name or newName is requested, prompt for file name
fileName = plot.saveFile
dotChar = STRPOS(fileName,'.')

IF ((dotChar LT 0) OR (KEYWORD_SET(newName))) THEN BEGIN
    extType = ''
    fileName = SiB4_PickSave(PATH=inputDir, GET_PATH=outputDir, $
               EXTTYPE=extType, /FIX_FILTER, SAVETYPE=fileType)
    IF fileName EQ '' THEN RETURN

    inputDir = outputDir
    plot.saveFile = fileName
    title = fileName
ENDIF ELSE BEGIN
    fileName = plot.saveFile
    extType = STRMID(fileName, dotChar+1)
ENDELSE

CASE extType OF

  "hp" : SAVE, FILE=fileName, plot
  "lp" : SAVE, FILE=fileName, plot
  "mp" : SAVE, FILE=fileName, plot
  "map"  : SAVE, FILE=fileName, plot

  "pdf" : BEGIN
    ext = '.pdf'
    dotChar = STRLEN(fileName) - STRLEN(ext)
    IF STRMID(fileName,dotChar,dotChar+2) NE ext THEN fileName = fileName + ext
    IF (plot.type EQ 'map') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /PDF, type='map'
    ENDIF ELSE $
    IF (plot.type EQ 'line') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /PDF, type='line'
    ENDIF ELSE $
    IF (plot.type EQ 'hist') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /PDF, type='hist'
    ENDIF ELSE $
    IF (plot.type EQ 'mult') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /PDF, type='mult'
    ENDIF ELSE BEGIN
        EZ4_Alert,'Unknown Plot Type To Save'
        RETURN
    ENDELSE
  END

  "eps" : BEGIN
    IF (plot.type EQ 'map') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /EPS, type='map'
    ENDIF ELSE $
    IF (plot.type EQ 'line') THEN BEGIN  
        EZ4_PrintPlot, plot, filename=fileName, /EPS, type='line'
    ENDIF ELSE $
    IF (plot.type EQ 'hist') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /EPS, type='hist'
    ENDIF ELSE $
    IF (plot.type EQ 'mult') THEN BEGIN
        EZ4_PrintPlot, plot, filename=fileName, /EPS, type='mult'
    ENDIF ELSE BEGIN
        EZ4_Alert,'Unknown Plot Type To Save'
        RETURN
    ENDELSE
  END

  "jpg" : BEGIN
    ON_IOERROR, badWrite
    WIDGET_CONTROL, /HOURGLASS

    IF (plot.type EQ 'map') THEN BEGIN
       Geo4_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'line') THEN BEGIN
       Line_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'hist') THEN BEGIN
       Hist_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'mult') THEN BEGIN
       Mult_DrawPlot, plot
    ENDIF ELSE BEGIN
       EZ4_Alert, 'Unknown Plot Type To Save'
       RETURN
    ENDELSE

    IF !D.N_COLORS GT 256 THEN BEGIN
      img = TVRD(TRUE=3)
      WRITE_JPEG, fileName, TEMPORARY(img), QUALITY=100, TRUE=3
    ENDIF ELSE BEGIN
      img = TVRD()
      img = [ [[reds[img]]], [[greens[img]]], [[blues[img]]] ]
      WRITE_JPEG, fileName, TEMPORARY(img), QUALITY=100.
    ENDELSE

  END

  "png" : BEGIN
    ON_IOERROR, badWrite
    WIDGET_CONTROL, /HOURGLASS

    IF (plot.type EQ 'map') THEN BEGIN
       Geo4_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'line') THEN BEGIN
       Line_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'hist') THEN BEGIN
       Hist_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'mult') THEN BEGIN 
       Mult_DrawPlot, plot
    ENDIF ELSE BEGIN
       EZ4_Alert, 'Unknown Plot Type To Save'
       RETURN
    ENDELSE

    img = TVRD(/TRUE)
    WRITE_PNG, fileName, TEMPORARY(img)
  END

  "bmp" : BEGIN
    ON_IOERROR, badWrite
    WIDGET_CONTROL, /HOURGLASS

    IF (plot.type EQ 'map') THEN BEGIN
        Geo4_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'line') THEN BEGIN
        Line_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'hist') THEN BEGIN
        Hist_DrawPlot, plot
    ENDIF ELSE $
    IF (plot.type EQ 'mult') THEN BEGIN
        Mult_DrawPlot, plot
    ENDIF ELSE BEGIN
        EZ4_Alert, 'Unknown Plot Type To Save'
        RETURN
    ENDELSE

    image = ez4_cgsnapshot(Filename=fileName,/BMP)
  END

ENDCASE

EZ4_Message,'Plot Saved Successfully!'

RETURN
;....................

badWrite:
EZ4_ALERT, ['EZPLOT encountered an error trying to write your file!', $
        '        ', $
        'The most common cause of this error is that you do not have ',$
        'permission to write files in the current directory. ', $
        '  ',$
        'Check to make sure you are running IDL in a writable directory.']
  
END
