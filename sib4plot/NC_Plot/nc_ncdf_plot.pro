;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO NC_NCDF_PLOT, ncfile, SITE=SITE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set local variables
  ncerror = -1

  ;; Determine how many files need to be combined (if any)
  nfiles = n_elements(ncfile)
 
  IF (nfiles EQ 1) THEN BEGIN
      EZ4_FILEBREAK, ncfile[0], FILE=fNameOnly, DIR=fDir
      windowTitle = fNameOnly
  ENDIF ELSE BEGIN
      yrstart=STRPOS(ncfile[0],'_',/REVERSE_SEARCH)+1
      startyr = MIN(FLOAT(STRMID(ncfile,yrstart,4)))
      startmon = MIN(FLOAT(STRMID(ncfile,yrstart+4,2)))
      stopyr = MAX(FLOAT(STRMID(ncfile,yrstart,4)))
      stopmon = MAX(FLOAT(STRMID(ncfile,yrstart+4,2)))
      numyrs = stopyr-startyr+1

      filenamestart = STRPOS(ncfile[0],'/',/REVERSE_SEARCH)+1
      numchars = yrstart - filenamestart
      fileprefix = STRMID(ncfile[0],filenamestart,numchars)

      filesuffixstart = STRPOS(ncfile[0], $
                        string(startmon,format='(i2.2)'), $
                        /REVERSE_SEARCH)+2
      numchars = STRLEN(ncfile[0])-filesuffixstart
      filesuffix = STRMID(ncfile[0],filesuffixstart,numchars)

      windowTitle = fileprefix + $
                    string(startyr,format='(i4.4)') + $
                    string(startmon,format='(i2.2)') + $
                    '_' + string(stopyr,format='(i4.4)') + $
                    string(stopmon,format='(i2.2)') + $
                    filesuffix

   ENDELSE

  ;;Get time information
  numyrs = -1 & startyr = -1 & stopyr = -1
  SiB4_GETTIME, nfiles, ncfile, $
       numyrs, startyr, stopyr, $
       ntime, time, timenames,  $
       timeunit, timeflag, ntperday, ncerror

  IF (ncerror EQ 1) THEN BEGIN
       ntime = 1
       reftime = -4

       time = 0.0
       timenames = ''
       timeunit = ''
       timeflag = -1
       ntperday = 0
       ncerror = -1
   ENDIF ELSE reftime = -1

  ;;Get basic data information
  SiB4_GETVARS, ncfile[0], sitesib, $
       nsib, lonsib, latsib, mapsib, $
       nlon, nlat, lon, lat, lonref, latref, $
       nvarsnc, namenc, titlenc, unitsnc, $
       ncerror
  IF (ncerror eq 1) THEN RETURN

  ;;Get Levels/PFT information
  NC_GETLINFO, ncfile[0], nlu, nlevs, $
        levnames, levlabel

  ;;Set up a data structure to hold the information
  ncinfo = { fileNames:ncfile, $
             nsib:nsib, nsibsave:nsib, sitesib:sitesib, $
             lonsib:lonsib, latsib:latsib, $
             mapsib:mapsib, $
             lonref:lonref, latref:latref, $
             nlon:nlon, nlat:nlat, $
             lon:lon,lat:lat, $
             nlu:nlu, nlevs:nlevs, $
             levnames:levnames, levlabel:levlabel, $
             ntime:ntime, ntperday:ntperday, time:time, $
             timenames:timenames, timeunit:timeunit, $
             timeflag:timeflag, $ 
             nvars:nvarsnc, vtitle:titlenc, $
             vname:namenc, vunits:unitsnc, $
             wdlpt:-1, wdlpft:-1, wdlpftt:-1, $
             wdlpfta:-1, wdllat:-1, wdllon:-1, $
             wdltime:-1, wdltimem:-1, $
             wdltimes:-1, wdltimet:-1, $
             wdlvar:-1, $
             refpt:-1, refpft:-1, refpftt:-1, $
             refpfta:-1, reftime:reftime, refvar:-1, $
             reflat:-1, reflon:-1, $
             vallat:-999., vallon:-999., $
             simlabel:'', simlabelField:-1, $
             windowTitle:windowTitle}

   NC_MAIN_WINDOW, ncinfo

END

