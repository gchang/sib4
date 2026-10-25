;Program to get vector data from a single file
PRO NC_GetData_Vector1, ncinfo

@ez4_BasicParams.com
WIDGET_CONTROL, /HOURGLASS

;;Set up basic variables
npts = ncinfo.nsib
ntime = ncinfo.ntime

mapdata = ncinfo.mapsib
reftime = ncinfo.reftime
refv = ncinfo.refvar
varname = (ncinfo.vname[refv])[0]
fileName = ncinfo.fileNames

;;-------------------------------------------
;;If a point is selected, get lat/lon references
IF ((npts GT 1) AND (mapdata EQ 0)) THEN BEGIN
   IF (n_elements(ncinfo.lonref) GT 1) THEN BEGIN
       lonmatch = ncinfo.vallon
       latmatch = ncinfo.vallat
    ENDIF ELSE BEGIN
       lonmatch = ncinfo.lonref
       latmatch = ncinfo.latref
    ENDELSE

   refmatch = -1
   sib4_PickLonLat, ncinfo.nsib, $
       ncinfo.lonsib, ncinfo.latsib, $
       lonmatch, latmatch, $
       refmatch, 1
   ncinfo.refpt = refmatch
ENDIF

;;-----------------------------
;;Get the data
ncid = EZ4_OPENNCDF(fileName)
IF (ncid LT 0) THEN BEGIN
    EZ4_ALERT,['File Not Found:',strtrim(fileName)]
    RETURN
ENDIF

IF (ncinfo.nlu EQ nlu) THEN BEGIN
    NCDF_VARGET, ncid, 'lu_area', larea

    luprefname = 'lu_pref'
    varid = NCDF_VARID(ncid,luprefname)
    IF (varid LT 0) THEN BEGIN
       luprefname='lu_pftref'
       varid = NCDF_VARID(ncid,luprefname)
    ENDIF
    IF (varid LT 0) THEN BEGIN
       print,'Missing PFT Reference array.'
       print,'Tried lu_pref and lu_pftref.'
       print,'Stopping in nc_getdata_vector1.pro'
       STOP
    ENDIF
    NCDF_VARGET, ncid, luprefname, lpref
ENDIF ELSE $
IF (ncinfo.nlevs EQ npft) THEN BEGIN
    NCDF_VARGET, ncid, 'pft_area', larea
ENDIF ELSE BEGIN
    larea=-1
    lpref=-1
ENDELSE

;...variable information
varid = NCDF_VARID(ncid,varname)
varinfo = NCDF_VARINQ(ncid,varid)
nDims = varinfo.nDims
dimsize = lonarr(nDims)
FOR d=0,nDims-1 DO BEGIN
   NCDF_DIMINQ, ncid, varinfo.dim(d), name, size
   dimsize(d) = size
ENDFOR

;...data
IF (mapdata EQ 0) THEN BEGIN ;Site
    ncinfo.nsib = 1
    ;ncinfo.lon = ncinfo.lonsib(ncinfo.refpt)
    ;ncinfo.lat = ncinfo.latsib(ncinfo.refpt)

    IF (ncinfo.refpt GE 0) THEN BEGIN
       count = dimsize
       count(0) = 1
       offset = lonarr(nDims)
       offset(0) = MAX([ncinfo.refpt,1])

       NCDF_VARGET, ncid, varname, ftempdata, $
             COUNT=count, OFFSET=offset

       IF (N_ELEMENTS(dimsize) EQ 1) THEN BEGIN
          larea=-1
          lpref=-1
       ENDIF
       IF (N_ELEMENTS(larea) GT 1) THEN BEGIN
           nlu = (size(larea))[2]
           IF (dimsize(1) EQ nlu) THEN BEGIN
               larea = REFORM(larea(ncinfo.refpt,*))
               lpref = REFORM(lpref(ncinfo.refpt,*))
            ENDIF
       ENDIF

    ENDIF ELSE $
    IF ((nDims EQ 1) AND (ntime GE 1)) THEN BEGIN
       NCDF_VARGET, ncid, varname, ftempdatatt
       ftempdata=fltarr(1,ntime)
       ftempdata(0,*) = ftempdatatt
    ENDIF ELSE $
    IF ((nDims EQ 2) AND (ntime GE 1)) THEN BEGIN
       NCDF_VARGET, ncid, varname, ftempdata
    ENDIF ELSE BEGIN
       EZ4_Alert,'Unknown Site Data.'
       RETURN
    ENDELSE

ENDIF ELSE BEGIN ;Map
    IF (ntime EQ 1) THEN BEGIN ;Single-Time Files
        NCDF_VARGET, ncid, varname, ftempdata
    ENDIF ELSE $ 
    IF (reftime GE 0) THEN BEGIN ;Specific Time
        count = dimsize
        count(ndims-1) = 1
        offset = lonarr(nDims)
        offset(ndims-1) = reftime
        NCDF_VARGET, ncid, varname, ftempdata, $
             COUNT=count, OFFSET=offset

    ENDIF ELSE $
    IF (reftime EQ -1) THEN BEGIN ;Time Mean
        count = dimsize
        count(ndims-1) = 1
        offset = lonarr(nDims)
        ftempdata = fltarr(dimsize(0:ndims-2))
        FOR t=0,ntime-1 DO BEGIN
            NCDF_VARGET, ncid, varname, ttempdata, $
                COUNT=count, OFFSET=offset
            ftempdata += ttempdata
            offset(ndims-1)++
        ENDFOR
        ftempdata /= ntime
    ENDIF ELSE $
    IF (reftime EQ -3) THEN BEGIN ;Time Total
        count = dimsize
        count(ndims-1) = 1
        offset = lonarr(nDims)
        ftempdata = fltarr(dimsize(0:ndims-2))
        FOR t=0,ntime-1 DO BEGIN
            NCDF_VARGET, ncid, varname, ttempdata, $
                COUNT=count, OFFSET=offset
            ftempdata += ttempdata
            offset(ndims-1)++
        ENDFOR
    ENDIF ELSE BEGIN
         mystring = 'Unknown Time Reference ' + string(reftime,format='(i4)')
         EZ4_Alert,mystring
         RETURN
      ENDELSE

ENDELSE ;Site vs Map

NCDF_CLOSE, ncid

IF (ncinfo.nlu EQ nlu) THEN BEGIN
   sibdata = { data:ftempdata, $
               larea:larea, $
               lpref:lpref}

   SiB4_Parse_Data, ncinfo, $
          ftempdata, larea, lpref
ENDIF ELSE BEGIN
   NC_Parse_Data, ncinfo, ftempdata
ENDELSE


RETURN

END
