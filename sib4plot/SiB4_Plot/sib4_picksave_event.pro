;------------------------------------------------------------------ 
; This procedure processes the events being sent by the XManager. 
;------------------------------------------------------------------ 
PRO SiB4_PickSave_Event, event 

COMMON File_COM 
COMMON picksave, pathtxt, filttxt, dirlist, filelist, $
              selecttxt, ok, cancel, $
              here, thefile, separator, $
              filetypeNum, typesChoice, fixFilter

WIDGET_CONTROL, filttxt, GET_VALUE = filt 
filt = filt[0]
 
CASE event.id OF 

  pathtxt: BEGIN 
      WIDGET_CONTROL, pathtxt, GET_VALUE = newpath 
      newpath = newpath[0] 
      len     = STRLEN(newpath) - 1 
      IF STRPOS(newpath, '/', len) NE -1 THEN $
          newpath = STRMID(newpath, 0, len) 
      IF FILE_TEST(newpath, /DIRECTORY, /EXECUTABLE) THEN BEGIN 
        here = newpath
        CD, here 
        GOTO, ChangeDirectory
      ENDIF ELSE WIDGET_CONTROL, pathtxt, SET_VALUE=here 
  END 

  filttxt: BEGIN 
      files = GetFiles(filt) 
      WIDGET_CONTROL, filelist, SET_VALUE=files, SET_UVALUE=files
  END 

  dirlist: BEGIN 
      WIDGET_CONTROL, dirlist, GET_UVALUE = directories 
      IF (event.index GT N_ELEMENTS(directories) - 1) THEN RETURN 
      IF FILE_TEST(directories[event.index], /DIRECTORY, /EXECUTABLE) EQ 0 $
      THEN RETURN
      CD, directories[event.index]
      CD, CURRENT = here 
      GOTO, ChangeDirectory
  END 
    
  filelist: BEGIN 
      WIDGET_CONTROL, filelist, GET_UVALUE=files 
      selected = WIDGET_INFO(filelist, /LIST_SELECT)
      IF files[0] NE '' AND selected[0] NE -1 THEN BEGIN 
        selected = files[selected]
        theFile  = here + selected
        N = N_ELEMENTS(selected)
        IF N GT 1 THEN selected[0:N-2] = selected[0:N-2] + ','
        WIDGET_CONTROl, ok, GET_UVALUE=auto_exit 
        IF (auto_exit) OR event.clicks EQ 2 THEN GOTO, checkfile
      ENDIF ELSE selected = ''
      WIDGET_CONTROL, selecttxt, SET_VALUE=selected
  END 

  typesChoice: BEGIN
    ;; Get the selected file type
    ;;    and change it in the filename if needed
    fileTypeNum = event.index
    IF (fixFilter EQ 1) THEN $
       EZ4_FixType, typesChoice, fileTypeNum, selecttxt, type

    ;; Display the corresponding files
    IF type NE '' THEN filt='*.' + type ELSE filt='*'

    files = EZ4_GetFiles(filt)
    WIDGET_CONTROL, filelist, SET_VALUE=files, SET_UVALUE=files
    WIDGET_CONTROL, filttxt, SET_VALUE=filt
  END

  cancel: BEGIN 
      theFile = ''
      WIDGET_CONTROL,event.top,/DESTROY
      RETURN
  END

  ok: GOTO, checkfile 
  selecttxt: GOTO, checkfile

  ELSE: BEGIN
    newDir = (dirName[WHERE(dirButton EQ event.id)])[0]
    IF STRMID(newDir,0,5) EQ '$HOME' THEN $
         newDir = userHome + STRMID(newDir, 5)
    CATCH, cdError
	IF cdError NE 0 THEN BEGIN
	    Alert, ['Uh Oh!    ' + !SYSERR_STRING, newDir]
	    RETURN
	ENDIF
	CD, newDir
    CATCH, /CANCEL                   
    here = newDir
ChangeDirectory:
    here = here + separator
    WIDGET_CONTROL, pathtxt, SET_VALUE = here 
    directories = EZ4_GetDirs() 
    files = EZ4_GetFiles(filt) 
    WIDGET_CONTROL, filelist, SET_VALUE=files, SET_UVALUE=files
    WIDGET_CONTROL, dirlist, SET_VALUE=directories, SET_UVALUE=directories
    WIDGET_CONTROL, selecttxt, SET_VALUE=filt
  END
   
ENDCASE 
RETURN 
 
checkfile: 

  IF fixFilter EQ 1 THEN BEGIN
    ;; Fix the filename extension if different than selected extension
    EZ4_FixType, typesChoice, fileTypeNum, selecttxt, type, thefile
    IF (strmid(thefile,0,1) eq '.') THEN $
       thefile = 'sib4plot' + thefile
    thefile = here + thefile
  ENDIF

  IF (FILE_TEST(theFile)) THEN BEGIN
      string2='File Already Exists:'
      replace = EZ4_confirm([string2,theFile], $
                OK_BUTTON='Replace', GROUP_LEADER=event.top)
      IF (replace NE 1) THEN theFile=''
  ENDIF

  strlen = STRLEN(thefile)
  extlen = STRLEN(filt) - 1
  IF (strlen LE extlen) THEN BEGIN
    EZ4_Alert,'Please choose a file of nonzero length!'
    RETURN
  ENDIF

  WIDGET_CONTROL, event.top, /DESTROY
 
END ;======== end of event handling routine task =============
