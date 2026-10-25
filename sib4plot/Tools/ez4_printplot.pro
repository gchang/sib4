;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Print a plot or save it into a (e)ps file
;;
;;  TYPE: Type of plot to save
;;  PRINTER: Printer name, otherwise will prompt for it
;;  FILENAME: Filename to save to, rather than printing
;;  EPS: Saves the plot in eps format
;;  PDF: Saves the plot in pdf format
;;
PRO EZ4_PrintPlot, plot, TYPE=plot_type, $
      FILENAME=filename, PRINTER=printerName, $
      EPS=EPS, PDF=PDF

@ez4_BasicParams.com

;; Set up variables
copyplot = plot
oldFont = !P.FONT

dopsplot=1
myheight = 6.5
mywidth = 7.5
bpp = 8
psSize = 10.

!P.FONT=1

IF (N_ELEMENTS(filename) EQ 0) THEN filename='sib4plot.eps'
IF (N_ELEMENTS(PDF) NE 0) THEN BEGIN
   pdfFileName = filename
   dotpos = STRPOS(filename,'.',/REVERSE_SEARCH)
   filename = STRMID(filename,0,dotpos)+'.eps'
ENDIF

;; Pause widget and check for errors
WIDGET_CONTROL, /HOURGLASS
ON_IOERROR, badWrite   

;; Set up the device for postscripts
;SET_PLOT, 'PS'
;DEVICE, FILENAME=FileName, /COLOR, DECOMPOSED=0, BITS_PER_PIXEL=bpp, $
;    /INCHES, XSIZE=mywidth, YSIZE=myheight, $
;    SET_FONT=psFont, FONT_SIZE=psSize, $
;    /TT_FONT, /ENCAPSULATED

SET_PLOT, 'PS'
;!p.font = 1
DEVICE, FILENAME=FileName, $
     DECOMPOSED=0, COLOR=1, $
     /INCHES, XSIZE=mywidth, YSIZE=myheight, $
     /TT_FONT, BITS_PER_PIXEL=bpp, $
     /ENCAPSULATED

IF (N_ELEMENTS(plot_type) LT 0) THEN BEGIN
   ez4_Alert,'Undefined Plot Type To Save.'
   RETURN
ENDIF

IF (plot_type EQ 'map') THEN BEGIN
   geo4_DrawPlot, copyplot
ENDIF ELSE $
IF (plot_type EQ 'line') THEN BEGIN
   copyplot.linethick*=2.
   line_DrawPlot, copyplot
ENDIF ELSE $
IF (plot_type EQ 'hist') THEN BEGIN
   hist_DrawPlot, copyplot
ENDIF ELSE $
IF (plot_type EQ 'mult') THEN BEGIN
   mult_DrawPlot, copyplot 
ENDIF ELSE BEGIN
   ez4_Alert,'Unknown Plot Type To Save.'
   RETURN
ENDELSE


;; Close Postscript device and restore settings
DEVICE, /CLOSE
SET_PLOT, 'X'
!P.FONT=oldFont
dopsplot=0

;;;;;Print files to a printer
IF (N_ELEMENTS(printerName) NE 0) THEN BEGIN

   if (N_ELEMENTS(printerName) EQ 1) THEN printerName=defaultPrinter

   IF !VERSION.OS EQ 'hp-ux' THEN $
       printCommand = 'lp -d ' + printerName + ' ' + FileName $
   ELSE printCommand = 'lp -d ' + printerName + ' -o fit-to-page ' + FileName
   SPAWN, printCommand

   fdelete = ez4_confirm([ '   Do you want to delete the    ', $
                           '     extra EPS file created?    '], $
                 OK_BUTTON='YES', CANCEL_BUTTON='No')
   IF (fdelete) THEN FILE_DELETE, FileName

   string='File Sent To Printer: ' + printerName
   EZ4_Message,string

ENDIF

;;;;;;Print files to a PDF file
IF (N_ELEMENTS(PDF) GT 0) THEN BEGIN

   CASE !VERSION.OS OF
     'IRIX' :  command = 'ps2pdf -g5400x4700 '+FileName+' '+ pdfFileName
     'darwin': command = 'pstopdf '+FileName+' -o '+ pdfFileName
     ELSE:     command = ''
   ENDCASE
   SPAWN, command

   fdelete = ez4_confirm([ '   Do you want to delete the    ', $
                           '     extra EPS file created?    '], $
                 OK_BUTTON='YES', CANCEL_BUTTON='No')
   IF (fdelete) THEN FILE_DELETE, FileName

ENDIF

RETURN

;;;;;;;;
; Error handler for no write permission in directory
badWrite:
  EZ4_Alert, ['        ', $
          'YOU DO NOT HAVE WRITE PERMISSION IN THIS DIRECTORY!', $
          '        ', $
          'Please run IDL only in a writable directory.']

; if we got an error during drawing into PS, reset device to screen and reset
; colors and font
IF !D.NAME EQ 'PS' THEN BEGIN
  DEVICE, /CLOSE
  SET_PLOT, 'X'
  !P.FONT=oldFont
ENDIF

ON_IOERROR, NULL

END
