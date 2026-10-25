;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO NC_MAIN_WINDOW_EVENT, event


WIDGET_CONTROL, event.top, GET_UVALUE=ncinfo, /NO_COPY

; Handle the current event depending on which action (widget) was 
; selected by the user
menuName = WIDGET_INFO(event.id, /UNAME)

; set nsib back (necessary if coming from single plot)
nsib = ncinfo.nsibsave
ncinfo.nsib = nsib

; determine nfiles
nfiles = n_elements(ncinfo.filenames)

; set values for events
CASE menuName OF

  'DONE' : WIDGET_CONTROL, event.top, /DESTROY
  'OK': BEGIN
      ; read the simulation label
      WIDGET_CONTROL, ncinfo.simlabelField, GET_VALUE=simlabel
      ncinfo.simlabel = simlabel

      ; read the site info
      vallon=0
      vallat=0

      IF (ncinfo.wdllon GT -1) THEN BEGIN  
          IF (ncinfo.mapsib EQ 0) THEN BEGIN
              WIDGET_CONTROL, ncinfo.wdllon, GET_VALUE=vallon
              WIDGET_CONTROL, ncinfo.wdllat, GET_VALUE=vallat

              mylonmin = MIN(ncinfo.lon)
              mylonmax = MAX(ncinfo.lon)
              mylatmin = MIN(ncinfo.lat)
              mylatmax = MAX(ncinfo.lat)

              IF ((vallon LT mylonmin) OR (vallon GT mylonmax)) THEN BEGIN
                  string1 = 'Longitude ' + string(vallon,format='(F7.2)') $
                            + ' Is Invalid.'
                  string2 = '   Min Lon: ' + string(mylonmin,format='(F7.2)')
                  string3 = '   Max Lon: ' + string(mylonmax,format='(F7.2)')
                  EZ4_Alert,[string1,string2,string3]

                  WIDGET_CONTROL, event.top, SET_UVALUE=ncinfo, /NO_COPY
                  RETURN
             ENDIF ELSE $
             IF ((vallat LT mylatmin) OR (vallat GT mylatmax)) THEN BEGIN
                 string1 = 'Latitude ' + string(vallat,format='(F7.2)') $
                            + ' Is Invalid.'
                 string2 = '   Min Lat: ' + string(mylatmin,format='(F7.2)')
                 string3 = '   Max Lat: ' + string(mylatmax,format='(F7.2)')
                 EZ4_Alert,[string1,string2,string3]

                 WIDGET_CONTROL, event.top, SET_UVALUE=ncinfo, /NO_COPY
                 RETURN
             ENDIF

             ncinfo.vallon=vallon
             ncinfo.vallat=vallat

         ENDIF
      ENDIF

      IF (ncinfo.wdlpt GT -1) THEN BEGIN ;Site Data
          ncinfo.refpt = WIDGET_INFO(ncinfo.wdlpt, /List_Select)
          ncinfo.vallon = ncinfo.lonsib(ncinfo.refpt)
          ncinfo.vallat = ncinfo.latsib(ncinfo.refpt)
      ENDIF

      ; grab the variable selection
      refvar = WIDGET_INFO(ncinfo.wdlvar,/List_Select)
      ncinfo.refvar = refvar

      ; grab the pft selection
      IF (ncinfo.wdlpft GT -1) THEN BEGIN
          ncinfo.refpft = WIDGET_INFO(ncinfo.wdlpft,/DropList_Select)

           IF (ncinfo.refpft EQ 0) THEN BEGIN
               ncinfo.refpftt = WIDGET_INFO(ncinfo.wdlpftt,/DropList_Select)
           ENDIF ELSE ncinfo.refpftt = 0

           IF (ncinfo.wdlpfta GT -1) THEN BEGIN
              ncinfo.refpfta = WIDGET_INFO(ncinfo.wdlpfta,/DropList_Select)
           ENDIF ELSE ncinfo.refpfta = 0
      ENDIF

      IF (ncinfo.nsib LT 1) THEN BEGIN
          IF (nfiles EQ 1) THEN BEGIN
               NC_GetData_Grid1, ncinfo
          ENDIF ELSE BEGIN
               NC_GetData_Grid2, ncinfo
          ENDELSE
      ENDIF ELSE BEGIN
          IF (nfiles EQ 1) THEN BEGIN
              NC_GetData_Vector1, ncinfo
          ENDIF ELSE BEGIN
              NC_GetData_Vector2, ncinfo
          ENDELSE
      ENDELSE
   END

  'PFT AREA': ncinfo.refpfta = event.index
  'PFT REF': BEGIN
      ncinfo.refpft = event.index
      IF (ncinfo.refpft GT 0) THEN BEGIN
         WIDGET_CONTROL, ncinfo.wdlpftt, SENSITIVE=0
         ;WIDGET_CONTROL, ncinfo.wdlpfta, SENSITIVE=1
      ENDIF ELSE BEGIN
         IF (ncinfo.mapsib EQ 0) THEN $
            WIDGET_CONTROL, ncinfo.wdlpftt, SENSITIVE=1
            ;WIDGET_CONTROL, ncinfo.wdlpfta, SENSITIVE=0
      ENDELSE
   END
  'PFT TYPE': ncinfo.refpftt = event.index

  'POINT REF': ncinfo.refpt = event.index
  'POINT OR MAP': BEGIN
       IF (event.index EQ 1) THEN BEGIN
           ncinfo.mapsib = 0
           WIDGET_CONTROL, ncinfo.wdllon, SENSITIVE=1
           WIDGET_CONTROL, ncinfo.wdllon, GET_VALUE=vallon
 
           WIDGET_CONTROL, ncinfo.wdllat, SENSITIVE=1
           WIDGET_CONTROL, ncinfo.wdllat, GET_VALUE=vallat

           IF (ncinfo.wdlpftt GT -1) THEN BEGIN
              IF (ncinfo.refpft GT 0) THEN BEGIN
                 WIDGET_CONTROL, ncinfo.wdlpftt, SENSITIVE=0
              ENDIF ELSE BEGIN
                 WIDGET_CONTROL, ncinfo.wdlpftt, SENSITIVE=1
              ENDELSE
           ENDIF

           IF (ncinfo.wdltimes GT -1) THEN BEGIN
               WIDGET_CONTROL, ncinfo.wdltimes, SENSITIVE=1
               WIDGET_CONTROL, ncinfo.wdltimet, SENSITIVE=1
            ENDIF
        ENDIF ELSE BEGIN
           ncinfo.mapsib = 1
           WIDGET_CONTROL, ncinfo.wdllon, SENSITIVE=0
           WIDGET_CONTROL, ncinfo.wdllat, SENSITIVE=0
           IF (ncinfo.wdlpftt GT -1) THEN $
              WIDGET_CONTROL, ncinfo.wdlpftt, SENSITIVE=0
           IF (ncinfo.wdltimes GT -1) THEN $
                WIDGET_CONTROL, ncinfo.wdltimes, SENSITIVE=0
        ENDELSE
    END

  'TIME VALUE': ncinfo.reftime = event.value - 1
  'TIME MEAN': BEGIN
       IF (ncinfo.reftime EQ -1) THEN BEGIN
           WIDGET_CONTROL, ncinfo.wdltime, $
                 GET_VALUE=timeref, SENSITIVE=1
           ncinfo.reftime = timeref - 1
           IF (ncinfo.mapsib EQ 0) THEN $
               WIDGET_CONTROL, ncinfo.wdltimes, SENSITIVE=1
               WIDGET_CONTROL, ncinfo.wdltimet, SENSITIVE=1
        ENDIF ELSE BEGIN
            ncinfo.reftime = -1
            WIDGET_CONTROL, ncinfo.wdltime, SENSITIVE=0
            WIDGET_CONTROL, ncinfo.wdltimem, SET_BUTTON=1
            WIDGET_CONTROL, ncinfo.wdltimes, SET_BUTTON=0
            WIDGET_CONTROL, ncinfo.wdltimet, SET_BUTTON=0
         ENDELSE
    END ; TIME MEAN
   'TIME SERIES': BEGIN
        IF (ncinfo.reftime EQ -2) THEN BEGIN
            WIDGET_CONTROL, ncinfo.wdltime, $
                 GET_VALUE=timeref, SENSITIVE=1
            ncinfo.reftime = timeref - 1
            WIDGET_CONTROL, ncinfo.wdltimem, SENSITIVE=1
            WIDGET_CONTROL, ncinfo.wdltimet, SENSITIVE=1
         ENDIF ELSE BEGIN
            ncinfo.reftime = -2
            WIDGET_CONTROL, ncinfo.wdltime, SENSITIVE=0
            WIDGET_CONTROL, ncinfo.wdltimem, SET_BUTTON=0
            WIDGET_CONTROL, ncinfo.wdltimes, SET_BUTTON=1
            WIDGET_CONTROL, ncinfo.wdltimet, SET_BUTTON=0
         ENDELSE
      END ;TIME SERIES
   'TIME TOTAL': BEGIN
     IF (ncinfo.reftime EQ -3) THEN BEGIN
        WIDGET_CONTROL, ncinfo.wdltime, $
              GET_VALUE=timeref, SENSITIVE=1
         ncinfo.reftime = timeref - 1
        WIDGET_CONTROL, ncinfo.wdltimem, SENSITIVE=1
     ENDIF ELSE BEGIN
        ncinfo.reftime = -3
        WIDGET_CONTROL, ncinfo.wdltime, SENSITIVE=0
        WIDGET_CONTROL, ncinfo.wdltimem, SET_BUTTON=0
        WIDGET_CONTROL, ncinfo.wdltimes, SET_BUTTON=0
        WIDGET_CONTROL, ncinfo.wdltimet, SET_BUTTON=1
     ENDELSE
  END ;TIME TOTAL


  'VAR REF': ncinfo.refvar = event.index

ENDCASE ;menuName

; Copy the revised state information back into the original structure,
; and return control to the XMANAGER to await more widget events
DONE:
IF WIDGET_INFO(event.top, /VALID_ID) THEN $
   WIDGET_CONTROL, event.top, SET_UVALUE=ncinfo, /NO_COPY

END

