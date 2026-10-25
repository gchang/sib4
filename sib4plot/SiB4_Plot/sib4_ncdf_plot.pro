;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO SiB4_NCDF_PLOT, sibfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set local variables
  ncerror = -1

  ;; Determine how many files need to be combined (if any)
  nfiles = N_ELEMENTS(sibfile)
  filenamestart=STRPOS(sibfile[0],'/',/REVERSE_SEARCH)+1
  startyr = MIN(FLOAT(STRMID(sibfile,filenamestart+5,4)))

  fileyrstart=STRPOS(sibfile[0],'_',/REVERSE_SEARCH)+1
  stopyr = MAX(FLOAT(STRMID(sibfile,fileyrstart,4)))

  numyrs = stopyr-startyr+1

  ;; Set window file name
  IF (nfiles EQ 1) THEN BEGIN
      EZ4_FILEBREAK, sibfile[0], FILE=fNameOnly, DIR=fDir
      windowTitle = fNameOnly
   ENDIF ELSE BEGIN
      siblutype = STRMATCH(sibfile[0],'*.lu.*')
      IF (siblutype EQ 1) THEN sibtype='lu' ELSE sibtype='g'
      sibprefix=STRMID(sibfile[0],filenamestart,4)
      windowTitle = sibprefix + '_' + $
           STRMID(sibfile[0],filenamestart+5,6) + '_' + $
           STRMID(sibfile[nfiles-1],filenamestart+5,6) + $
           '.' + sibtype + '.nc'
   ENDELSE

  ;;Get time information
  SiB4_GETTIME, nfiles, sibfile, $
        numyrs, startyr, stopyr, $
        ntime, time, timenames,  $
        timeunit, timeflag, ntperday, ncerror
  IF (ncerror EQ 1) THEN BEGIN
        ez4_alert,'No Times Available.'
        RETURN
  ENDIF

  ;;Get lat/lon and variable information
  SiB4_GETVARS, sibfile[0], sitesib, $
       nsib, lonsib, latsib, mapsib, $
       nlon, nlat, lon, lat, lonref, latref, $
       nvarsib, namesib, titlesib, unitsib, $
       ncerror
  IF (ncerror eq 1) THEN RETURN

  ;;Set up a data structure to hold the information
  sibinfo = { fileNames:sibfile, $
             nsib:nsib, sitesib:sitesib, $
             lonsib:lonsib, latsib:latsib, $
             mapsib:mapsib, $
             lonref:lonref, latref:latref, $
             nlon:nlon, nlat:nlat, $
             lon:lon,lat:lat, $
             ntime:ntime, ntperday:ntperday, time:time, $
             timenames:timenames, timeunit:timeunit, $
             timeflag:timeflag, $ 
             nvars:nvarsib, vtitle:titlesib, $
             vname:namesib, vunits:unitsib, $
             wdllon:-1, wdllat:-1, wdlpt:-1, $
             wdlpft:-1, wdlpftt:-1, wdlpfta:-1, $
             wdltime:-1, wdltimem:-1, $
             wdltimes:-1, wdltimet:-1, $
             wdlvar:-1, refpt:-1L, $
             refpft:-1, refpftt:-1, refpfta:-1, $
             reftime:-1L, refvar:-1, $
             simlabel:'', simlabelField:-1, $
             windowTitle:windowTitle}

   SiB4_MAIN_WINDOW, sibinfo

END

