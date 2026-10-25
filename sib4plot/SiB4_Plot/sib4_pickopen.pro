; PURPOSE: 
;       This function allows the user to interactively pick a file.  A file 
;       selection tool with a graphical user interface is created.  Files 
;       can be selected from the current directory or other directories. 
; 
; KEYWORD PARAMETERS: 
; 
;       GET_PATH: Set to a named variable. Returns the path at the 
;               time of selection. 
; 
;       PATH:   The initial path to select files from.  If this keyword is  
;               not set, the current directory is used. 
; 
;       FILTER: A string value for filtering the files in the file list.  This 
;               keyword is used to reduce the number of files to choose from. 
;               The user can modify the filter unless the FIX_FILTER keyword 
;               is set.  Example filter values might be "*.pro" or "*.dat". 
; 
; OUTPUTS: 
;       Returns a string that contains the name of the file selected. 
;       If no file is selected, returns a null string. 
; 
;------------------------------------------------------------------------------ 
;  This is the actual routine that creates the widget, registering it with the 
;  Xmanager.  It also determines the operating system and sets the specific 
;  file designators for that operating system. 
;------------------------------------------------------------------------------ 
FUNCTION sib4_PickOpen, FILTER=FILTER, PATH=PATH, GET_PATH=GET_PATH

COMMON pickopen, pathtxt, filttxt, dirlist, filelist, $
              selecttxt, sfileok, sfilecancel, $
              prefixtxt, suffixtxt, $
              yrstartfield,yrstopfield,monstartfield,monstopfield, $
              yrstartvalue,yrstopvalue,monstartvalue,monstopvalue, $
              gcheck, lucheck, gorluvalue, $
              mfok, mfcancel, timeok, timecancel, $
              here, thefile, separator

@ez4_BasicParams.com

;set initial values/parameters
file = ''
thefile = ""
separator = '/'
title = "File Selection"
IF (N_ELEMENTS(FILTER) NE 0) THEN filt = FILTER ELSE filt = "*" 
auto_exit = 0 ;flag that automatically selects the first file clicked on,
              ;rather than waiting for 'OK'
existflag = 1 

;set defaults in case not used
selecttxt = ''
sfileok = -1
sfilecancel = -1
prefixtxt = ''
suffixtxt = ''
yrstartfield = -1
yrstopfield = -1
monstartfield = -1
monstopfield = -1
yrstartvalue = 0
yrstopvalue = 0
monstartvalue = 0
monstopvalue = 0

gcheck = -1
lucheck = -1
gorluvalue = -1
mfok = -1
mfcancel = -1
timeok = -1
timecancel = -1


;get the path
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

directories = EZ4_GetDirs() 
files       = EZ4_GetFiles(filt) 
IF file EQ '' THEN BEGIN
  fileNameOnly = ''
  filePosition = -1
ENDIF ELSE BEGIN
  EZ4_FileBreak, file, file=fileNameOnly 
  filePosition = (WHERE(files EQ fileNameOnly))[0]
ENDELSE
 
version = WIDGET_INFO(/VERSION) 
IF (version.style EQ 'Motif') THEN osfrm = 0 ELSE osfrm = 1 

base = WIDGET_BASE(Title=title, /COLUMN, EVENT_PRO='sib4_pickopen_event')
Pickfilebase = WIDGET_BASE(base, TITLE = TITLE, /COLUMN) 

fspecbase = WIDGET_BASE(Pickfilebase, /COLUMN, /FRAME)
widebase = WIDGET_BASE(fspecbase, /ROW) 
label    = WIDGET_LABEL(widebase, VALUE="Path:") 
pathtxt  = WIDGET_TEXT(widebase, VAL=here, /EDIT, FR=osfrm, XS=50) 

filtbase = WIDGET_BASE(fspecbase, /ROW)
filtlbl  = WIDGET_LABEL(filtbase, VALUE = "Filter:") 
filttxt  = WIDGET_TEXT(filtbase, VAL = filt, /EDIT, XS = 12, FR = osfrm) 

xs = 15
selections = WIDGET_BASE(fspecbase, /ROW, SPACE=30)
dirs    = WIDGET_BASE(selections, /COLUMN, /FRAME) 
lbl     = WIDGET_LABEL(dirs, VALUE="Subdirectories          ") 
dirlist = WIDGET_LIST(dirs, VALUE=directories, XS=xs, YS=8, UVALUE=directories)
fls     = WIDGET_BASE(selections, /COLUMN, /FRAME) 
lbl     = WIDGET_LABEL(fls, VALUE="Files                   ") 
filelist= WIDGET_LIST(fls, VALUE=files, UVALUE=files, YS=8, XS=xs, MULTI=MULTI)

IF nCustomDirs GT 0 THEN BEGIN
  rows      = FIX(nCustomDirs/6) + 1
  dirBase   = WIDGET_BASE(fspecbase, ROW=rows, /GRID_LAYOUT)
  dirButton = INTARR(nCustomDirs)
  FOR iDir = 0, nCustomDirs-1 DO $
    dirButton[iDir] = WIDGET_BUTTON(dirBase, value=dirButtonName[iDir])
ENDIF

sep_string = '---------------------------------------------------------'
orbase = WIDGET_BASE(pickfilebase,/COLUMN,/ALIGN_CENTER)
message = WIDGET_LABEL(orbase, VALUE=sep_string)

fileselect_string = 'Select A Single File'
ssbase = WIDGET_BASE(pickfilebase,/COLUMN,/ALIGN_CENTER,/FRAME)
sub = WIDGET_BASE(ssbase,/COLUMN,/ALIGN_CENTER,SPACE=10,/BASE_ALIGN_CENTER)
;message = WIDGET_LABEL(sub, VALUE=sep_string)
message = WIDGET_LABEL(sub, VALUE=fileselect_string)

widebase  = WIDGET_BASE(ssbase, /ROW, /BASE_ALIGN_LEFT) 
label     = WIDGET_LABEL(widebase, VALUE="Selection:") 
selecttxt = WIDGET_TEXT(widebase, VAL=fileNameOnly, XS=42, FRAME=osfrm) 

okbase   = WIDGET_BASE(ssbase, SPACE=20, /ROW, /ALIGN_CENTER)
sfileok        = WIDGET_BUTTON(okbase, VALUE="     OK     ", UVALUE=auto_exit) 
sfilecancel    = WIDGET_BUTTON(okbase, VALUE="   Cancel   ", UVALUE=existflag) 


;Add multi-file capability for SiB4 output files:
IF ((filt EQ 'hsib_*.nc') OR (filt EQ 'psib_*.nc') OR $
    (filt EQ 'qsib_*.nc')) THEN BEGIN

   orbase = WIDGET_BASE(pickfilebase,/COLUMN,/ALIGN_CENTER)
   message = WIDGET_LABEL(orbase, VALUE='---- OR ----')

   timeselect_string = 'Select Starting and Stopping Times From Files List'
   tyrbase = WIDGET_BASE(pickfilebase, /COLUMN, /ALIGN_CENTER,/FRAME)
   sub = WIDGET_BASE(tyrbase,/COLUMN,/ALIGN_CENTER,SPACE=10,/BASE_ALIGN_CENTER)
   ;message = WIDGET_LABEL(sub, VALUE=sep_string)
   message = WIDGET_LABEL(sub, VALUE=timeselect_string)

   myrbase   = WIDGET_BASE(tyrbase, /ROW, /BASE_ALIGN_LEFT)
   yrstartfield = CW_FIELD(myrbase, TITLE='Yr Start: ', XSIZE=4)
   yrstopfield  = CW_FIELD(myrbase, TITLE='Yr Stop: ', XSIZE=4)
   monstartfield = CW_FIELD(myrbase, TITLE='Mon Start: ', XSIZE=2)
   monstopfield  = CW_FIELD(myrbase, TITLE='Mon Stop: ', XSIZE=2)

   base1 = WIDGET_BASE(tyrbase, /COLUMN, /ALIGN_CENTER)
   base2 = WIDGET_BASE(base1, /ROW, /EXCLUSIVE)
   gcheck = WIDGET_BUTTON(base2, VALUE='Grid Cell Total')
   lucheck = WIDGET_BUTTON(base2, VALUE='LU/PFT')

   gorluvalue=0
   WIDGET_CONTROL, gcheck, SET_BUTTON=1

   ok2base = WIDGET_BASE(tyrbase, SPACE=20, /ROW, /ALIGN_CENTER)
   timeok = WIDGET_BUTTON(ok2base, VALUE="     OK     ", UVALUE=auto_exit) 
   timecancel = WIDGET_BUTTON(ok2base, VALUE="   Cancel   ", UVALUE=existflag) 
ENDIF ELSE $
IF (filt EQ '*.nc') THEN BEGIN
   orbase = WIDGET_BASE(pickfilebase,/COLUMN,/ALIGN_CENTER)
   message = WIDGET_LABEL(orbase, VALUE='---- OR ----')

   timeselect_string = 'Open Multiple Files'
   tyrbase = WIDGET_BASE(pickfilebase, /COLUMN, /ALIGN_CENTER,/FRAME)
   sub = WIDGET_BASE(tyrbase,/COLUMN,/ALIGN_CENTER,SPACE=10,/BASE_ALIGN_CENTER)
   message = WIDGET_LABEL(sub, VALUE=timeselect_string)

   mypsbase = WIDGET_BASE(tyrbase, /ROW, /ALIGN_CENTER,/BASE_ALIGN_CENTER)
   prefixlabel = WIDGET_LABEL(mypsbase, VALUE = "Prefix: ") 
   prefixtxt = WIDGET_TEXT(mypsbase, VAL=prefix, /EDIT, XS = 12)
   suffixlabel = WIDGET_LABEL(mypsbase, VALUE="   Suffix: ")
   suffixtxt = WIDGET_TEXT(mypsbase, VAL=suffix, /EDIT, XS = 12)

   myrbase   = WIDGET_BASE(tyrbase, /ROW, /BASE_ALIGN_LEFT)
   yrstartfield = CW_FIELD(myrbase, TITLE='Yr Start: ', XSIZE=4)
   yrstopfield  = CW_FIELD(myrbase, TITLE='Yr Stop: ', XSIZE=4)
   monstartfield = CW_FIELD(myrbase, TITLE='Mon Start: ', XSIZE=2)
   monstopfield  = CW_FIELD(myrbase, TITLE='Mon Stop: ', XSIZE=2)

   ok2base = WIDGET_BASE(tyrbase, SPACE=20, /ROW, /ALIGN_CENTER)
   mfok = WIDGET_BUTTON(ok2base, VALUE="     OK     ", UVALUE=auto_exit) 
   mfcancel = WIDGET_BUTTON(ok2base, VALUE="   Cancel   ", UVALUE=existflag) 

ENDIF 

WIDGET_CONTROL, base, /REALIZE 

IF filePosition NE -1 THEN BEGIN
  WIDGET_CONTROL, fileList, SET_LIST_SELECT=filePosition
ENDIF
XManager, "SiB4_PickOpen", base, /MODAL
                         
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

RETURN, thefile 
 
END 
