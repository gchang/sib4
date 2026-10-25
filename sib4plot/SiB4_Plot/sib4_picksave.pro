; PURPOSE: 
;       This function allows the user to interactively pick a file.  A file 
;       selection tool with a graphical user interface is created.  Files 
;       can be selected from the current directory or other directories. 
; 
; KEYWORD PARAMETERS: 
; 
; 
;    PATH:   The initial path to select files from.
;            If not set, the current directory is used. 
;
;    GET_PATH: Set to a named variable. Returns the path at the 
;              Returns the selected path.
; 
;    FIX_FILTER: When this keyword is set, make sure the
;              file being saved has the correct suffix.
; 
;    EXTTYPE: Set to a named variable.
;             Returns the selected path extension. 
;
;    SAVETYPE: Type of plot, used to save specific IDL file
;              that can quickly be restored.
;
; OUTPUTS: 
;       Returns a string that contains the name of the file selected. 
;       If no file is selected, PICKFILE returns a null string. 
; 
;---------------------------------------------------------------------- 
;  This is the actual routine that creates the widget and 
;  registers it with the Xmanager.
;  It also determines the operating system and sets the specific 
;  file designators for that operating system. 
;--------------------------------------------------------------------- 
FUNCTION SiB4_PickSave, PATH=PATH, GET_PATH=GET_PATH, $
         FIX_FILTER=Fix_Filter, EXTTYPE=extType, $
         SAVETYPE=saveType

COMMON File_COM
COMMON picksave, pathtxt, filttxt, dirlist, filelist, $
              selecttxt, ok, cancel, $
              here, thefile, separator, $
              filetypeNum, typesChoice, fixFilter

thefile = '' 
separator = '/'
title = 'SAVE AS ...'
textSizeField = -1 
IF (N_ELEMENTS(Fix_Filter) GT 0) THEN $
   fixFilter=1 ELSE fixFilter=0

myxs = 10
auto_exit = 0
existflag = 1
CD, CURRENT = dirsave 
 
IF (N_ELEMENTS(PATH) EQ 0) THEN BEGIN 
  PATH = dirSave + separator 
  here = PATH 
ENDIF ELSE BEGIN     
  IF(STRPOS(PATH, separator,STRLEN(PATH)- 1) EQ -1) AND $
    (PATH NE separator) THEN PATH = PATH + separator 
  CATCH, cdError
    IF cdError NE 0 THEN BEGIN
      Alert, ['Uh Oh!    ' + !SYSERR_STRING, PATH, $
              '', '#Changing to current directory']
      PATH = dirSave + separator
    ENDIF
    CD, PATH         
  CATCH, /CANCEL                   
  here = PATH                        
ENDELSE 
 
;; Set some variable for Save Panel
IF (N_ELEMENTS(saveType) GT 0) THEN BEGIN
     IF (saveType EQ 'Hist') THEN BEGIN
        tsavetype='Histogram (.hp)'
        tsaveext = 'hp'
     ENDIF
     IF (saveType EQ 'Line') THEN BEGIN
        tsavetype='Line Plot (.lp)'
        tsaveext ='lp'
     ENDIF
     IF (saveType EQ 'Map') THEN BEGIN
        tsavetype='Map (.map)'
        tsaveext = 'map'
     ENDIF
     IF (saveType EQ 'Multi') THEN BEGIN
        tsavetype='Multi Plot (.mp)'
        tsaveext = 'mp'
     ENDIF
ENDIF

fileTypes = ['BMP file (.bmp)'   $
           , 'PNG file (.png)'   $
           , 'JPEG file (.jpg) ' $
           , 'Encapsulated PostScript file (.eps) ']
extTypes   = ['bmp', 'png', 'jpg', 'eps']

IF (n_elements(tsavetype) GT 0) THEN BEGIN
    filetypes = [tsavetype, fileTypes]
    extTypes = [tsaveext, extTypes]
ENDIF

;; Check for PDF conversion programs (SGI and MacOSX only)
IF !VERSION.OS EQ 'IRIX' OR !VERSION.OS EQ 'darwin' THEN BEGIN
   IF !VERSION.OS EQ 'IRIX' THEN ps2pdf = 'ps2pdf' ELSE ps2pdf = 'pstopdf'
   IF FILE_WHICH(GETENV('PATH'), ps2pdf) NE '' THEN BEGIN
      fileTypes = [fileTypes, 'PDF file (.pdf)']
      extTypes  = [extTypes, 'pdf']
   ENDIF
ENDIF

filetypeNum = 0
filt = '*.' + extTypes[filetypeNum]
file = '' + filt

directories = EZ4_GetDirs() 
files       = EZ4_GetFiles(filt) 
 
version = WIDGET_INFO(/VERSION) 
IF (version.style EQ 'Motif') THEN osfrm = 0 ELSE osfrm = 1 

base = WIDGET_BASE(Title=title, /COLUMN, EVENT_PRO='sib4_picksave_event')
Pickfilebase = WIDGET_BASE(base, TITLE = TITLE, /COLUMN) 

fspecbase = WIDGET_BASE(Pickfilebase, /COLUMN, /FRAME)
widebase = WIDGET_BASE(fspecbase, /ROW) 
label    = WIDGET_LABEL(widebase, VALUE="Path:") 
pathtxt  = WIDGET_TEXT(widebase, VALUE=here, /EDIT, FR=osfrm, XS=50) 

filtbase = WIDGET_BASE(fspecbase, /ROW)
filtlbl  = WIDGET_LABEL(filtbase, VALUE = "Filter:") 
filttxt  = WIDGET_TEXT(filtbase, VALUE=filt, /EDIT, XS=myxs, FR=osfrm) 

selections = WIDGET_BASE(fspecbase, /ROW, SPACE=30)
dirs    = WIDGET_BASE(selections, /COLUMN, /FRAME) 
lbl     = WIDGET_LABEL(dirs, VALUE="Subdirectories          ") 
dirlist = WIDGET_LIST(dirs, VALUE=directories, XS=myxs, YS=8, $
              UVALUE=directories)
fls     = WIDGET_BASE(selections, /COLUMN, /FRAME) 
lbl     = WIDGET_LABEL(fls, VALUE="Files                   ") 
filelist= WIDGET_LIST(fls, VALUE=files, YS=8, XS=myxs, $
              MULTI=MULTI, UVALUE=files)

IF nCustomDirs GT 0 THEN BEGIN
  rows      = FIX(nCustomDirs/6) + 1
  dirBase   = WIDGET_BASE(fspecbase, ROW=rows, /GRID_LAYOUT)
  dirButton = INTARR(nCustomDirs)
  FOR iDir = 0, nCustomDirs-1 DO $
    dirButton[iDir] = WIDGET_BUTTON(dirBase, value=dirButtonName[iDir])
ENDIF

sep_string = '---------------------------------------------------------'
fileTypeBase = WIDGET_BASE(Pickfilebase, /COLUMN, /FRAME)
typesChoice = WIDGET_DROPLIST(fileTypeBase,  VALUE=fileTypes, $
                TITLE='File Type To Be Saved  ', UVALUE=extTypes)

widebase  = WIDGET_BASE(fileTypebase, /ROW) 
label     = WIDGET_LABEL(widebase, VALUE="File Name:") 
nameabr = "." + extTypes[filetypeNum]
selecttxt = WIDGET_TEXT(widebase, VAL=nameabr, XS=42, FRAME=osfrm, $
               /EDITABLE)

rowbase   = WIDGET_BASE(Pickfilebase, SPACE=20, /ROW, /ALIGN_CENTER) 
ok        = WIDGET_BUTTON(rowbase, VALUE="     OK     ", UVALUE=auto_exit) 
cancel    = WIDGET_BUTTON(rowbase, VALUE="   Cancel   ", UVALUE=existflag) 


WIDGET_CONTROL, base, /REALIZE 
XManager, "SiB4_PickSave", base, /MODAL
                         
CD, dirsave 
filt = "" 
GET_PATH=here 

; if $HOME is the current path, expand it
IF STRMID(GET_PATH, 0, 5) EQ '$HOME' THEN $
  GET_PATH = userHome + STRMID(GET_PATH, 5)

; Check if the name starts with $HOME
FOR i=0, N_ELEMENTS(thefile)-1 DO $
  IF STRMID(thefile[i], 0, 5) EQ '$HOME' THEN $
    thefile[i] = userHome + STRMID(thefile[i], 5)

IF KEYWORD_SET(SAVEAS) THEN textSize = textSizeVal
IF (N_ELEMENTS(extTypes) GT 0) THEN extType = extTypes[fileTypeNum]

RETURN, thefile 
 
END 
