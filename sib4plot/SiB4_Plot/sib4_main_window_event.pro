;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO SIB4_MAIN_WINDOW_EVENT, event

WIDGET_CONTROL, event.top, GET_UVALUE=sibinfo, /NO_COPY

; Handle the current event depending on which action (widget) was 
; selected by the user
menuName = WIDGET_INFO(event.id, /UNAME)

; set nsib back (necessary if coming from single plot)
nsib = n_elements(sibinfo.lonsib)
sibinfo.nsib = nsib

; read the simulation label
WIDGET_CONTROL, sibinfo.simlabelField, GET_VALUE=simlabel
sibinfo.simlabel = simlabel

; grab/read the site
testpt = 0
IF (sibinfo.wdlpt GT -1) THEN BEGIN
   IF (sibinfo.mapsib EQ 0) THEN BEGIN
       testpt = 1
       IF (sibinfo.sitesib[0] EQ '') THEN BEGIN
           WIDGET_CONTROL, sibinfo.wdlpt, GET_VALUE=refpt
           refpt -= 1
       ENDIF ELSE BEGIN
           refpt = WIDGET_INFO(sibinfo.wdlpt, /List_Select)
       ENDELSE
   ENDIF

   IF (testpt EQ 1) THEN BEGIN
       IF (refpt LT 0) THEN BEGIN
          myformat=ez4_niceformat(refpt,/INT)
          string1 = 'Point ' + string(refpt,format=myformat) + $
                    ' Is Less Than 0.'
          EZ4_Alert,['Invalid Site Specified:',string1]
          RETURN
       ENDIF ELSE $
       IF (refpt GT nsib) THEN BEGIN
          myformat=ez4_niceformat(refpt,/INT)
          myformatn=ez4_niceformat(nsib,/INT)
          string1 = 'Point ' + string(refpt,format=myformat) + $
                    ' Is Greater Than ' + string(nsib,format=myformatn)
          EZ4_Alert,['Invalid Site Specified:',string1]
          RETURN
       ENDIF ELSE sibinfo.refpt = refpt
   ENDIF ;testpt == 1

   IF (sibinfo.wdlpftt GT -1) THEN BEGIN
       IF (sibinfo.refpft EQ 0) THEN BEGIN
           WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=1
           WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=0
       ENDIF ELSE BEGIN
           WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=0
           WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=1
        ENDELSE

   ENDIF
ENDIF

; grab the PFT selections
IF (sibinfo.wdlpft GT -1) THEN BEGIN
   refpft = WIDGET_INFO(sibinfo.wdlpft,/DropList_Select)
   sibinfo.refpft = refpft

   IF (refpft EQ 0) THEN BEGIN
       refpftt = WIDGET_INFO(sibinfo.wdlpftt,/DropList_Select)
       sibinfo.refpftt = refpftt
   ENDIF ELSE sibinfo.refpftt = 0

   IF (refpft GT 0) THEN BEGIN
       sibinfo.refpfta = WIDGET_INFO(sibinfo.wdlpfta,/DropList_Select)
   ENDIF ELSE sibinfo.refpfta = 0
ENDIF

; grab the variable selection
refvar = WIDGET_INFO(sibinfo.wdlvar,/List_Select)
sibinfo.refvar = refvar

;set values for events
CASE menuName OF

  'DONE' : WIDGET_CONTROL, event.top, /DESTROY
  'OK': BEGIN
        myerror=0
        IF (sibinfo.mapsib EQ 0) THEN BEGIN
           ; grab/read the lat/lon
           IF (sibinfo.wdllon GT -1) THEN BEGIN
               WIDGET_CONTROL, sibinfo.wdllon, GET_VALUE=reflon
               IF (reflon GT 180.) OR (reflon LT -180) THEN BEGIN
                   string1 = 'Longitude ' + string(reflon,format='(f7.2)') + $
                             ' Is Invalid.'
                   string2 = 'Please specify a value between -180 and 180.'
                   EZ4_Alert,[string1,string2]
                   myerror=1
                ENDIF

               WIDGET_CONTROL, sibinfo.wdllat, GET_VALUE=reflat
               IF ((reflat GT 90.) OR (reflat LT -90)) THEN BEGIN
                   string1 = 'Latitude ' + string(reflat,format='(f7.2)') + $
                             ' Is Invalid.'
                   string2 = 'Please specify a value between -90 and 90.'
                   EZ4_Alert,[string1,string2]
                   myerror=1
               ENDIF

                IF (myerror EQ 0) THEN BEGIN
                    ;...find the point with the specified lat/lon values
                    sib4_picklonlat, sibinfo.nsib, $
                           sibinfo.lonsib, sibinfo.latsib, $
                           reflon, reflat, refpt, 0
                    sibinfo.refpt = refpt
                ENDIF
           ENDIF
        ENDIF

        IF (myerror EQ 0) THEN BEGIN
           IF (N_ELEMENTS(sibinfo.fileNames) EQ 1) THEN $
               SiB4_GetData1, sibinfo $
           ELSE SiB4_GetData2, sibinfo
        ENDIF
   END 

  'PFT AREA': sibinfo.refpfta = event.index
  'PFT REF': BEGIN
       sibinfo.refpft = event.index
       IF (sibinfo.refpft GT 0) THEN BEGIN
           WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=0
           WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=1
       ENDIF ELSE BEGIN
           WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=1
           WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=0
        ENDELSE
   END
  'PFT TYPE': sibinfo.refpftt = event.index

  'POINT REF': sibinfo.refpt = event.index
  'POINT OR MAP': BEGIN
       IF (event.index EQ 1) THEN BEGIN
          sibinfo.mapsib = 0
          IF (sibinfo.wdlpt GT -1) THEN BEGIN
             WIDGET_CONTROL, sibinfo.wdlpt, SENSITIVE=1
             WIDGET_CONTROL, sibinfo.wdlpt, GET_VALUE=refpt
          ENDIF

          IF (sibinfo.wdllon GT -1) THEN BEGIN
             WIDGET_CONTROL, sibinfo.wdllon, SENSITIVE=1
             WIDGET_CONTROL, sibinfo.wdllon, GET_VALUE=reflon
          ENDIF

          IF (sibinfo.wdllat GT -1) THEN BEGIN
             WIDGET_CONTROL, sibinfo.wdllat, SENSITIVE=1
             WIDGET_CONTROL, sibinfo.wdllat, GET_VALUE=reflat
          ENDIF
 
          IF (sibinfo.wdlpftt GT -1) THEN BEGIN
              IF (sibinfo.refpft GT 0) THEN BEGIN
                 WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=0
              ENDIF ELSE BEGIN
                 WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=1
              ENDELSE
          ENDIF

          IF (sibinfo.wdltimes GT -1) THEN BEGIN
              WIDGET_CONTROL, sibinfo.wdltimes, SENSITIVE=1
              WIDGET_CONTROL, sibinfo.wdltimet, SENSITIVE=1
          ENDIF
       ENDIF ELSE BEGIN
          sibinfo.mapsib = 1
          IF (sibinfo.wdlpt GT -1) THEN $
              WIDGET_CONTROL, sibinfo.wdlpt, SENSITIVE=0
          IF (sibinfo.wdllon GT -1) THEN $
              WIDGET_CONTROL, sibinfo.wdllon, SENSITIVE=0
          IF (sibinfo.wdllat GT -1) THEN $
              WIDGET_CONTROL, sibinfo.wdllat, SENSITIVE=0
          IF (sibinfo.wdlpftt GT -1) THEN $
              WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=0
          IF (sibinfo.wdltimes GT -1) THEN $
              WIDGET_CONTROL, sibinfo.wdltimes, SENSITIVE=0
       ENDELSE
   END

  'TIME VALUE': sibinfo.reftime = event.value - 1
  'TIME MEAN': BEGIN
       IF (sibinfo.reftime EQ -1) THEN BEGIN
           WIDGET_CONTROL, sibinfo.wdltime, $
                 GET_VALUE=timeref, SENSITIVE=1
           sibinfo.reftime = timeref - 1
           IF (sibinfo.mapsib EQ 0) THEN BEGIN
              WIDGET_CONTROL, sibinfo.wdltimes, SENSITIVE=1
              WIDGET_CONTROL, sibinfo.wdltimet, SENSITIVE=1
           ENDIF
        ENDIF ELSE BEGIN
           sibinfo.reftime = -1
           WIDGET_CONTROL, sibinfo.wdltime, SENSITIVE=0
           WIDGET_CONTROL, sibinfo.wdltimem, SET_BUTTON=1
           WIDGET_CONTROL, sibinfo.wdltimes, SET_BUTTON=0
           WIDGET_CONTROL, sibinfo.wdltimet, SET_BUTTON=0
        ENDELSE
   END ; TIME MEAN
   'TIME SERIES': BEGIN
       IF (sibinfo.reftime EQ -2) THEN BEGIN
          WIDGET_CONTROL, sibinfo.wdltime, $
                GET_VALUE=timeref, SENSITIVE=1
          sibinfo.reftime = timeref - 1
          WIDGET_CONTROL, sibinfo.wdltimem, SENSITIVE=1
          WIDGET_CONTROL, sibinfo.wdltimet, SENSITIVE=1
       ENDIF ELSE BEGIN
          sibinfo.reftime = -2
          WIDGET_CONTROL, sibinfo.wdltime, SENSITIVE=0
          WIDGET_CONTROL, sibinfo.wdltimem, SET_BUTTON=0
          WIDGET_CONTROL, sibinfo.wdltimes, SET_BUTTON=1
          WIDGET_CONTROL, sibinfo.wdltimet, SET_BUTTON=0
       ENDELSE
   END ;TIME SERIES
   'TIME TOTAL': BEGIN
     IF (sibinfo.reftime EQ -3) THEN BEGIN
        WIDGET_CONTROL, sibinfo.wdltime, $
              GET_VALUE=timeref, SENSITIVE=1
         sibinfo.reftime = timeref - 1
        WIDGET_CONTROL, sibinfo.wdltimem, SENSITIVE=1
     ENDIF ELSE BEGIN
        sibinfo.reftime = -3
        WIDGET_CONTROL, sibinfo.wdltime, SENSITIVE=0
        WIDGET_CONTROL, sibinfo.wdltimem, SET_BUTTON=0
        WIDGET_CONTROL, sibinfo.wdltimes, SET_BUTTON=0
        WIDGET_CONTROL, sibinfo.wdltimet, SET_BUTTON=1
     ENDELSE
  END ;TIME TOTAL

  'VAR REF': sibinfo.refvar = event.index

ENDCASE ;menuName

; Copy the revised state information back into the original structure,
; and return control to the XMANAGER to await more widget events
DONE:
IF WIDGET_INFO(event.top, /VALID_ID) THEN $
   WIDGET_CONTROL, event.top, SET_UVALUE=sibinfo, /NO_COPY

END

