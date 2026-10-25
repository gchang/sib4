;------------------------------------------------------------------
; This procedure processes the events being sent by the XManager. 
;------------------------------------------------------------------- 
PRO SiB4_PickOpen_Event, event 
 
COMMON pickopen, pathtxt, filttxt, dirlist, filelist, $
              selecttxt, sfileok, sfilecancel, $
              prefixtxt, suffixtxt, $
              yrstartfield,yrstopfield,monstartfield,monstopfield, $
              yrstartvalue,yrstopvalue,monstartvalue,monstopvalue, $
              gcheck, lucheck, gorluvalue, $
              mfok, mfcancel, timeok, timecancel, $
              here, thefile, separator

@ez4_BasicParams.com
 
WIDGET_CONTROL, filttxt, GET_VALUE = filt 
filt = filt[0]
 
CASE event.id OF 

  filttxt: BEGIN 
      files = EZ4_GetFiles(filt) 
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
    
  pathtxt: BEGIN 
      WIDGET_CONTROL, pathtxt, GET_VALUE = newpath 
      newpath = newpath[0] 
      len     = STRLEN(newpath) - 1 
      IF STRPOS(newpath, '/', len) NE -1 THEN newpath = STRMID(newpath, 0, len) 
      IF FILE_TEST(newpath, /DIRECTORY, /EXECUTABLE) THEN BEGIN 
        here = newpath
        CD, here 
        GOTO, ChangeDirectory
      ENDIF ELSE WIDGET_CONTROL, pathtxt, SET_VALUE=here 
  END 
 
  filelist: BEGIN 
      WIDGET_CONTROL, filelist, GET_UVALUE=files 
      selected = WIDGET_INFO(filelist, /LIST_SELECT)
      IF files[0] NE '' AND selected[0] NE -1 THEN BEGIN 
        selected = files[selected]
        theFile  = here + selected
        N = N_ELEMENTS(selected)
        IF N GT 1 THEN selected[0:N-2] = selected[0:N-2] + ','
        WIDGET_CONTROl, sfileok, GET_UVALUE=auto_exit 
        IF (auto_exit) OR event.clicks EQ 2 THEN GOTO, checkfile
      ENDIF ELSE selected = ''
      WIDGET_CONTROL, selecttxt, SET_VALUE=selected
  END 
 
  mfcancel: BEGIN & thefile = "" & WIDGET_CONTROL,event.top,/DESTROY & END
  mfok: BEGIN
      WIDGET_CONTROL, prefixtxt, GET_VALUE=thePrefix
      WIDGET_CONTROL, suffixtxt, GET_VALUE=theSuffix

      WIDGET_CONTROL, yrstartfield, GET_VALUE=yrstartvalue
      WIDGET_CONTROL, yrstopfield, GET_VALUE=yrstopvalue
      WIDGET_CONTROL, monstartfield, GET_VALUE=monstartvalue
      WIDGET_CONTROL, monstopfield, GET_VALUE=monstopvalue

      WIDGET_CONTROL, filelist, GET_UVALUE=files 
      theFile=''
      IF (files[0] EQ '') THEN BEGIN
         EZ4_Alert,'First select a directory containing a list of files.'
         RETURN
      ENDIF ELSE BEGIN
         nfiles = size(files,/n_elements)
         yrmin=9999.
         yrmax=-9999.
         monmin=9999.
         monmax=-9999.
         fileyrstart = STRPOS(thePrefix,'_',/REVERSE_SEARCH)+1
         FOR f=0,nfiles-1 DO BEGIN
             IF (STRPOS(files[f],thePrefix) GE 0) THEN BEGIN
                yrtemp = FLOAT(STRMID(files[f],fileyrstart,4))
                IF (yrtemp LT yrmin) THEN yrmin=yrtemp
                IF (yrtemp GT yrmax) THEN yrmax=yrtemp

                montemp = FLOAT(STRMID(files[f],fileyrstart+4,2))
                IF (montemp LT monmin) THEN monmin=montemp
                IF (montemp GT monmax) THEN monmax=montemp
             ENDIF
         ENDFOR

         yrstartvalue = FLOAT(yrstartvalue[0])
         yrstopvalue = FLOAT(yrstopvalue[0])
         monstartvalue = FLOAT(monstartvalue[0])
         monstopvalue = FLOAT(monstopvalue[0])

         IF ((yrstartvalue LT yrmin) OR (yrstartvalue GT yrmax) OR $
             (yrstopvalue LT yrmin) OR (yrstopvalue GT yrmax) OR $
             (yrstartvalue GT yrstopvalue) OR $
             (monstartvalue LT monmin) OR (monstartvalue GT monmax) OR $
             (monstopvalue LT monmin) OR (monstopvalue GT monmax) OR $
             (monstartvalue GT monstopvalue)) THEN BEGIN
            EZ4_Alert, 'Invalid Time Range Specified.'
             RETURN
         ENDIF

         numyrs = yrstopvalue-yrstartvalue+1
         numyrs = ROUND(numyrs[0])

         monstart = INTARR(numyrs)
         monstop  = INTARR(numyrs)
         monstart[0] = monstartvalue
         monstop[numyrs-1] = monstopvalue
         
         FOR yr=yrstartvalue,yrstopvalue DO BEGIN
             yrref=yr-yrstartvalue
             mref1=1
             mref2=12
             IF (yr EQ yrstartvalue) THEN mref1=monstartvalue
             IF (yr EQ yrstopvalue) THEN mref2=monstopvalue
             FOR mon=mref1,mref2 DO BEGIN
                 yrmoncombo = STRING(yr,FORMAT='(I4)') + STRING(mon,FORMAT='(I2.2)')
                 preyrmoncombo = thePrefix + yrmoncombo + theSuffix
                 filetmatchall = STRPOS(files,preyrmoncombo)
                 filetmatchref = WHERE(filetmatchall GE 0)

                 IF (filetmatchref[0] EQ -1) THEN BEGIN
                    EZ4_Alert,'Specified Time Does Not Exist.'
                    RETURN
                 ENDIF ELSE BEGIN
                    newfile = here + files(filetmatchref[0])
                     IF (theFile[0] EQ '') THEN theFile=newfile ELSE $
                         theFile = ([theFile,newfile])
                 ENDELSE
             ENDFOR
          ENDFOR
          WIDGET_CONTROL,event.top,/DESTROY
      ENDELSE

  END


  sfilecancel: BEGIN & thefile = "" & WIDGET_CONTROL,event.top,/DESTROY & END 
  sfileok: GOTO, checkfile 
  selecttxt: GOTO, checkfile 

  timecancel: BEGIN & thefile = "" & WIDGET_CONTROL,event.top,/DESTROY & END
  timeok: BEGIN
      WIDGET_CONTROL, yrstartfield, GET_VALUE=yrstartvalue
      WIDGET_CONTROL, yrstopfield, GET_VALUE=yrstopvalue
      WIDGET_CONTROL, monstartfield, GET_VALUE=monstartvalue
      WIDGET_CONTROL, monstopfield, GET_VALUE=monstopvalue
      WIDGET_CONTROL, filelist, GET_UVALUE=files 

      theFile=''
      IF (files[0] EQ '') THEN BEGIN
         EZ4_Alert,'First select a directory containing a list of files.'
         RETURN
      ENDIF ELSE BEGIN
         nfiles = size(files,/n_elements)

         IF (gorluvalue EQ 0) THEN ftype='.g.' ELSE ftype='.lu.'
         IF ((STRMID(filt,0,4) EQ 'hsib') OR $
             (STRMID(filt,0,4) EQ 'psib') OR $
             (STRMID(filt,0,4) EQ 'qsib')) THEN BEGIN
            yrmin = MIN(FLOAT(STRMID(files,5,4)))
            yrmax = MAX(FLOAT(STRMID(files,5,4)))

            monfilesref = WHERE((STRMID(files,9,1) NE '_') AND $
                                (STRMID(files,9,1) NE '.'))
            monmin = MIN(FLOAT(STRMID(files(monfilesref),9,2)))
            monmax = MAX(FLOAT(STRMID(files(monfilesref),9,2)))

         ENDIF ELSE $
         IF (STRMID(filt,1,1) EQ '_') THEN BEGIN
            fileyrstart = STRPOS(files[0],'_',/REVERSE_SEARCH)+1
            yrmin = MIN(FLOAT(STRMID(files,fileyrstart,4)))
            yrmax = MAX(FLOAT(STRMID(files,fileyrstart,4)))
            monmin = MIN(FLOAT(STRMID(files,fileyrstart+4,2)))
            monmax = MAX(FLOAT(STRMID(files,fileyrstart+4,2)))
            ftype = '.'
         ENDIF

         yrstartvalue = FLOAT(yrstartvalue[0])
         yrstopvalue = FLOAT(yrstopvalue[0])
         monstartvalue = FLOAT(monstartvalue[0])
         monstopvalue = FLOAT(monstopvalue[0])

         IF ((yrstartvalue LT yrmin) OR (yrstartvalue GT yrmax) OR $
             (yrstopvalue LT yrmin) OR (yrstopvalue GT yrmax) OR $
             (yrstartvalue GT yrstopvalue) OR $
             (monstartvalue LT monmin) OR (monstartvalue GT monmax) OR $
             (monstopvalue LT monmin) OR (monstopvalue GT monmax) OR $
             (monstartvalue GT monstopvalue)) THEN BEGIN
             EZ4_Alert, 'Invalid Time Range Specified.'
             RETURN
         ENDIF

         numyrs = yrstopvalue-yrstartvalue+1
         numyrs = ROUND(numyrs[0])

         monstart = INTARR(numyrs)
         monstop  = INTARR(numyrs)
         monstart[0] = monstartvalue
         monstop[numyrs-1] = monstopvalue
         FOR i=0,numyrs-2 DO BEGIN
             monstart[i+1] = 1
             monstop[i] = 12
         ENDFOR
         
         FOR yr=yrstartvalue,yrstopvalue DO BEGIN
             yrref=yr-yrstartvalue
             FOR mon=monstart[yrref],monstop[yrref] DO BEGIN
                 yrmoncombo = STRING(yr,FORMAT='(I4)') + STRING(mon,FORMAT='(I2.2)')
                 filetmatchall = STRPOS(files,yrmoncombo)
                 fileglumatchall = STRPOS(files,ftype)

                 filetmatchref = WHERE(filetmatchall GE 0)
                 fileglumatchref = WHERE(fileglumatchall GE 0)

                 IF (filetmatchref[0] EQ -1) THEN BEGIN
                    EZ4_Alert,'Specified Time Does Not Exist.'
                    RETURN
                 ENDIF ELSE BEGIN
                    FOR i=0,n_elements(filetmatchref)-1 DO BEGIN
                        testref = WHERE(fileglumatchref EQ filetmatchref(i))
                        IF (testref[0] GE 0) THEN BEGIN
                          newfile = here + files(fileglumatchref(testref[0]))
                          IF (theFile[0] EQ '') THEN theFile=newfile ELSE $
                              theFile = ([theFile,newfile])
                        ENDIF
                     ENDFOR
                  ENDELSE
             ENDFOR
          ENDFOR
          WIDGET_CONTROL,event.top,/DESTROY
      ENDELSE

  END

  gcheck: gorluvalue=0
  lucheck: gorluvalue=1

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
    WIDGET_CONTROL, selecttxt, SET_VALUE=''
  END
   
ENDCASE 
RETURN 
 
checkfile: 

  IF (N_ELEMENTS(thefile) LT 2) THEN BEGIN
      WIDGET_CONTROL, selecttxt, GET_VALUE=thefile
      thefile = here + thefile[0]
  ENDIF
  WIDGET_CONTROL, sfilecancel, GET_UVALUE=existflag 
  IF existflag THEN BEGIN 
    ;; Check if the file(s) exists and non-zero length
    FOR i=0, N_ELEMENTS(file)-1 DO BEGIN
      IF FILE_TEST(file[i], /REGULAR) EQ 0 OR $
         FILE_TEST(file[i], /ZERO_LENGTH) THEN BEGIN
        ALERT, 'Please choose a file of nonzero length!'
        RETURN
      ENDIF
    ENDFOR
    WIDGET_CONTROL, event.top, /DESTROY
  ENDIF ELSE  WIDGET_CONTROL, event.top, /DESTROY
 
END ;============= end of Pickfile event handling routine task ================ 
