PRO NC_GetData_Grid1, ncinfo

@ez4_BasicParams.com

WIDGET_CONTROL, /HOURGLASS


;;Set up basic variables
missingvalue = missing

nlon = ncinfo.nlon
nlat = ncinfo.nlat
npts = nlon*nlat
ntime = ncinfo.ntime

lon  = ncinfo.lon
lat  = ncinfo.lat

refv = ncinfo.refvar
varname = (ncinfo.vname[refv])[0]
fileName = ncinfo.fileNames

reftime = ncinfo.reftime
time = ncinfo.time
timeperday = ncinfo.ntperday
timeflag = ncinfo.timeflag
timeunits = ncinfo.timeunit

;;-------------------------------------------
;;If a point is selected, get lat/lon references
IF ((npts GT 1) AND (ncinfo.mapsib EQ 0)) THEN BEGIN
   latdiff = abs(lat - ncinfo.vallat)
   order = SORT(latdiff)
   reflat = order[0]
   ncinfo.reflat = reflat

   londiff = abs(lon - ncinfo.vallon)
   order = SORT(londiff)
   reflon = order[0]
   ncinfo.reflon=reflon

   ;print,'Lat Specified/Selected/Ref: ', $
   ;         ncinfo.vallat, lat(reflat), reflat
   ;print,'Lon Specified/Selected/Ref: ', $
   ;         ncinfo.vallon, lon(reflon), reflon
ENDIF

;;-----------------------------
;;Get the data
ncid = EZ4_OPENNCDF(fileName)
varid = NCDF_VARID(ncid,varname)
varinfo = NCDF_VARINQ(ncid,varid)

larea = -1
lpref = -1
IF (ncinfo.nlu EQ nlu) THEN BEGIN
    NCDF_VARGET, ncid, 'lu_area', larea
    NCDF_VARGET, ncid, 'lu_pref', lpref
ENDIF ELSE $
IF (ncinfo.nlevs GT 0) THEN BEGIN
   IF (varname NE 'resp_crop') THEN BEGIN
      varid = NCDF_VARID(ncid,'pft_area')
      IF (varid LT 0) THEN BEGIN
          varid = NCDF_VARID(ncid,'lu_area')
      ENDIF
      IF (varid LT 0) THEN BEGIN
         string1='Area Fraction Information Unavailable'
         EZ4_Alert,string1
      ENDIF ELSE BEGIN
         NCDF_VARGET, ncid, varid, larea
      ENDELSE
   ENDIF 
ENDIF


;...variable dimensions
nDims = varinfo.nDims
dimsize = lonarr(nDims)
FOR d=0,nDims-1 DO BEGIN
    NCDF_DIMINQ, ncid, varinfo.dim(d), name, size
    dimsize(d) = size
ENDFOR

;...variable lat/lon/time locations
dreflon = (where(dimsize EQ nlon))[0]
dreflat = (where(dimsize EQ nlat))[0]
dreflu  = (where(dimsize EQ nlu))[0]
dreftime = (where(dimsize EQ ntime))[0]

IF ((dreftime EQ dreflon) OR $
   (dreftime EQ dreflat)) THEN BEGIN
    dreftime = (where(dimsize EQ ntime))[1]
ENDIF

IF ((dreflat LT 0) OR (dreflon LT 0)) THEN BEGIN
    message1 = 'Error With File:'
    message2 = 'Expecting Gridded Data.'
    EZ4_Alert, [message1,message2]
    RETURN
ENDIF

;...variable extras
missingvalue = missing
FOR att=0,varinfo.natts-1 DO BEGIN
    attname = NCDF_ATTNAME(ncid,varid,att)
    IF (attname EQ 'scale_factor') THEN $
        NCDF_ATTGET, ncid, varid, attname, scalefactor
    IF (attname EQ 'add_offset') THEN $
        NCDF_ATTGET, ncid, varid, attname, addoffset
    IF (attname EQ 'missing_value') THEN $
        NCDF_ATTGET, ncid, varid, attname, missingvalue
ENDFOR

;....data
IF (ncinfo.mapsib EQ 0) THEN BEGIN  ;Site
    ncinfo.nsib = 1
    IF (reftime GE 0) THEN BEGIN ;Specific Time
        count = lonarr(ndims) + 1
        offset = lonarr(ndims)
        offset(dreflat) = reflat
        offset(dreflon) = reflon
        offset(dreftime) = reftime

        NCDF_VARGET, ncid, varname, ftempdata, $
            COUNT=count, OFFSET=offset

    ENDIF ELSE $
    IF (reftime EQ -1) THEN BEGIN ;Time Mean
        count = dimsize
        count(dreflon) = 1
        count(dreflat) = 1
        count(dreftime) = 1

        offset = lonarr(ndims) 
        offset(dreflon) = reflon
        offset(dreflat) = reflat

        ftempdata = fltarr(count)
        ftempcount = intarr(count)
        FOR t=0,ntime-1 DO BEGIN
            NCDF_VARGET, ncid, varname, ttempdata, $
                 COUNT=count, OFFSET=offset
            goodref = where(ttempdata ne missing)
            IF (goodref[0] GT -1) THEN BEGIN
                ftempdata(goodref) += ttempdata(goodref)
                ftempcount(goodref)++
            ENDIF

            offset(dreftime)++
        ENDFOR

        goodref = where(ftempcount GT 0)
        IF (goodref[0] GT -1) THEN BEGIN
            ftempdata(goodref) /= ftempcount(goodref)
        ENDIF ELSE BEGIN
             allref = where(ftempcount EQ 0)
             ftempdata(allref) = missing
        ENDELSE

    ENDIF ELSE $
    IF (reftime EQ -2) THEN BEGIN ;Time Series
       count = dimsize
       count(dreflon) = 1
       count(dreflat) = 1
       
       offset = lonarr(ndims)
       offset(dreflon) = reflon
       offset(dreflat) = reflat

       NCDF_VARGET, ncid, varname, ftempdata, $
             COUNT=count, OFFSET=offset
    ENDIF ELSE $
    IF (reftime EQ -3) THEN BEGIN ;Time Total
        count = dimsize
        count(dreflon) = 1
        count(dreflat) = 1
        count(dreftime) = 1

        offset = lonarr(ndims)
        offset(dreflon) = reflon
        offset(dreflat) = reflat

        ftempdata = fltarr(count)
        ftempcount = intarr(count)
        FOR t=0,ntime-1 DO BEGIN
            NCDF_VARGET, ncid, varname, ttempdata, $
                 COUNT=count, OFFSET=offset
            goodref = where(ttempdata ne missing)
            ftempdata(goodref) += ttempdata(goodref)
            ftempcount(goodref)++

            offset(dreftime)++
        ENDFOR

        badref = where(ftempcount EQ 0)
        IF (badref[0] NE -1) THEN $
           ftempdata(badref) = missing

     ENDIF ELSE $
     IF (reftime EQ -4) THEN BEGIN ;No Time
        count = dimsize
        count(dreflon) = 1
        count(dreflat) = 1

        offset = lonarr(ndims)
        offset(dreflon) = reflon
        offset(dreflat) = reflat

         NCDF_VARGET, ncid, varname, ftempdata, $
              COUNT=count,OFFSET=offset 

     ENDIF ELSE BEGIN
        string1='Unknown Time Reference: ' + reftime
        EZ4_Alert,string1
        RETURN
     ENDELSE
ENDIF ELSE BEGIN  ;Map
    IF (reftime GE 0) THEN BEGIN ;Specific Time
        count = dimsize
        count(dreftime) = 1

        offset = lonarr(ndims)
        offset(dreftime) = reftime 

        NCDF_VARGET, ncid, varname, ftempdata, $
             COUNT=count, OFFSET=offset
    ENDIF ELSE $
    IF (reftime EQ -1) THEN BEGIN ;Time Mean
        count = dimsize
        count(dreftime) = 1
        offset = lonarr(ndims)

        atempdata = fltarr(count)
        atempcount = fltarr(count)
        FOR t=0,ntime-1 DO BEGIN
            NCDF_VARGET, ncid, varname, ttempdata, $
                  COUNT=count, OFFSET=offset 
            goodref = WHERE(ttempdata NE missingvalue)
            IF (goodref[0] GE 0) THEN BEGIN
                atempdata(goodref) += ttempdata(goodref)
                atempcount(goodref) ++
            ENDIF
            
            offset(dreftime) ++
         ENDFOR

         ftempdata = fltarr(count) + missingvalue
         goodref = WHERE(atempcount GT 0)
         IF (goodref[0] GE 0) THEN BEGIN
             ftempdata(goodref) = atempdata(goodref) / atempcount(goodref)
         ENDIF 

    ENDIF ELSE $
    IF (reftime EQ -3) THEN BEGIN ;Time Total
       count = dimsize
       count(dreftime) = 1
       offset = lonarr(ndims)

       atempdata = fltarr(count)
       atempcount = fltarr(count)
       FOR t=0,ntime-1 DO BEGIN
           NCDF_VARGET, ncid, varname, ttempdata, $
                COUNT=count, OFFSET=offset
           goodref = WHERE(ttempdata NE missingvalue)
           IF (goodref[0] GE 0) THEN BEGIN
               atempdata(goodref) += ttempdata(goodref)
               atempcount(goodref) ++
           ENDIF
           offset(dreftime)++
        ENDFOR

       ftempdata = fltarr(count) + missingvalue
       goodref = WHERE(atempcount GT 0)
       IF (goodref[0] GE 0) THEN BEGIN
          ftempdata(goodref) = atempdata(goodref)
       ENDIF

   ENDIF ELSE $
   IF (reftime EQ -4) THEN BEGIN ;No Time
       NCDF_VARGET, ncid, varname, ftempdata
   ENDIF ELSE BEGIN 

        string1 = 'Unknown Time Reference: ' + string(reftime,format='(i4)')
        EZ4_Alert,string1
        RETURN

   ENDELSE
ENDELSE

NCDF_CLOSE, ncid

;...modify data if necessary
badref = where(ftempdata EQ missingvalue)
ftempdata = temporary(float(ftempdata))
IF (badref[0] GE 0) THEN $
    ftempdata(badref) = missing

IF (n_elements(scalefactor) GT 0) THEN BEGIN
   goodref = where(ftempdata GT missing)
   IF (goodref[0] GE 0) THEN $
        ftempdata(goodref) *= scalefactor
ENDIF

IF (n_elements(addoffset) GT 0) THEN BEGIN
   goodref = where(ftempdata GT missing)
   IF (goodref[0] GE 0) THEN $
        ftempdata(goodref) += addoffset
ENDIF


;...Parse the data
NC_Parse_Data, ncinfo, $
        ftempdata, larea, lpref


END
