;Program to get vector data from
;  multiple files
PRO NC_GetData_Vector2, ncinfo

@ez4_BasicParams.com
WIDGET_CONTROL, /HOURGLASS

;;Set up basic variables
npts = ncinfo.nsib
ntime = ncinfo.ntime

mapdata = ncinfo.mapsib
refpt = ncinfo.refpt
reftime = ncinfo.reftime
refv = ncinfo.refvar
varname = (ncinfo.vname[refv])[0]
fileName = ncinfo.fileNames

;;-------------------------------------------
;;If a point is selected, get lat/lon references
IF ((npts GT 1) AND (mapdata EQ 0)) THEN BEGIN
   sib4_PickLonLat, ncinfo.nsib, $
       ncinfo.lonsib, ncinfo.latsib, $
       ncinfo.lonref, ncinfo.latref, $
       ncinfo.refpt, 0
ENDIF

string1='Requesting Viewing Of Unstacked Vector Output.'
string2='This should be done in nc_getdata_vector2.pro'
string3='Currently Work In Progress.'
EZ4_Alert,[string1,string2,string3]
RETURN

;;-----------------------------
;;Get the data
ncid = EZ4_OPENNCDF(fileName)
IF (ncid LT 0) THEN BEGIN
    EZ4_ALERT,['File Not Found:',strtrim(fileName)]
    RETURN
ENDIF

IF (ncinfo.nlu EQ nlu) THEN BEGIN
    NCDF_VARGET, ncid, 'lu_area', larea
    NCDF_VARGET, ncid, 'lu_pref', lpref
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
    count = dimsize
    count(0) = 1
    offset = lonarr(nDims)
    offset(0) = refpt
    NCDF_VARGET, ncid, varname, ftempdata, $
          COUNT=count, OFFSET=offset

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
    IF (reftime EQ -2) THEN BEGIN ;Time Total
        EZ4_Alert,'TO DO!'
        RETURN
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
