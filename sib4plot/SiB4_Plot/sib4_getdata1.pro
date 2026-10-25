;Program to get data from a single file
PRO SiB4_GetData1, sibinfo

WIDGET_CONTROL, /HOURGLASS

;;Set up basic variables
fileName = sibinfo.fileNames
ntime = sibinfo.ntime

mapsib = sibinfo.mapsib
refpt = sibinfo.refpt
reftime = sibinfo.reftime
refv = sibinfo.refvar

siblutype = STRMATCH(fileName,'*.lu.*')
varname = (sibinfo.vname[refv])[0]

;;Get the data
ncid = EZ4_OPENNCDF(fileName)
IF (ncid LT 0) THEN BEGIN
    EZ4_ALERT,'SiB4 File Not Found.'
    RETURN
ENDIF

IF (siblutype) THEN BEGIN
    NCDF_VARGET, ncid, 'lu_area', larea
    NCDF_VARGET, ncid, 'lu_pref', lpref
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

IF (mapsib EQ 0) THEN BEGIN ;Site
    count = dimsize
    count(0) = 1
    offset = lonarr(nDims)
    offset(0) = refpt
    NCDF_VARGET, ncid, varname, ftempdata, $
          COUNT=count, OFFSET=offset
    sibinfo.nsib = 1

    IF (N_ELEMENTS(larea) GT 1) THEN BEGIN
        nlu = (size(larea))[2]
        IF (dimsize(1) EQ nlu) THEN BEGIN
            larea = REFORM(larea(refpt,*))
            lpref = REFORM(lpref(refpt,*))
         ENDIF
    ENDIF

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
    IF (reftime EQ -2) THEN BEGIN ;Time Series
        string1 = 'Time Series Not Possible For Maps'
        string2 = 'Please Select Another Time Choice'
        EZ4_Alert, [string1,string2]
        RETURN
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

SiB4_Parse_Data, sibinfo, $
     ftempdata, larea, lpref


END
