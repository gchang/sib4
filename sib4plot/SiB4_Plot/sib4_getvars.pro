;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO SiB4_GETVARS, filename, sitesib, $
    nsib, lonsib, latsib, mapsib, $
    nlon, nlat, lon, lat, lonref, latref, $
    nvarsib, namesib, titlesib, unitsib, $
    ncerror, site=site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@ez4_BasicParams.com

WIDGET_CONTROL, /HOURGLASS

;;Set defaults
nsib=-1
lonsib=-1. & latsib=-1.
lonref=-1 & latref=-1
nlon=-1 & nlat=-1
lon=-1. & lat=-1.
lonref=-1 & latref=-1
nvarsib=-1 & namesib=''
titlesib='' & unitsib=''
IF (N_ELEMENTS(site) EQ 0) THEN site=0 $
ELSE site=1

;;------------------------
;;Open the file
ncid = EZ4_OPENNCDF(filename)

;;Get the number of points
ndims = (NCDF_INQUIRE(ncid)).NDIMS
FOR id=0,ndims-1 DO BEGIN
    NCDF_DIMINQ,ncid,id,name,size
    IF ((name EQ 'lon') OR $
        (name EQ 'nlon') OR $
        (name EQ 'longitude')) THEN $
        nlon=size
    IF ((name EQ 'lat') OR $
        (name EQ 'nlat') OR $
        (name EQ 'latitude')) THEN $
        nlat=size
    IF ((name EQ 'nsib') OR $
        (name EQ 'landpoints') OR $
        (name EQ 'nsites')) THEN $
        nsib = size
ENDFOR

;;----------------------------
;;Get the variable information
nvars = (NCDF_INQUIRE(ncid)).NVARS
nvarsib = 0
sitesib = ''
dovar = INTARR(nvars)
FOR id = 0, nvars-1 DO BEGIN
    var = NCDF_VARINQ(ncid,id)

    IF (var.name EQ 'site_names') THEN BEGIN
        sitesib = STRARR(nsib)
        NCDF_VARGET, ncid, var.name, tempsites
        FOR n=0, nsib-1 DO BEGIN
            sitesib(n) = string(tempsites(*,n))
        ENDFOR
    ENDIF 

    IF ((var.name NE 'lat') AND $
        (var.name NE 'latitude') AND $
        (var.name NE 'lon') AND $
        (var.name NE 'longitude') AND $
        (var.name NE 'lonsib') AND $
        (var.name NE 'latsib') AND $
        (var.name NE 'lu_area') AND $
        (var.name NE 'lu_pref') AND $
        (var.name NE 'pft_area') AND $
        (var.name NE 'pft_names') AND $
        (var.name NE 'crop_names') AND $
        (var.name NE 'pft_refnums') AND $
        (var.name NE 'site_names') AND $
        (var.name NE 'time') AND $
        (var.name NE 'year') AND $
        (var.name NE 'month') AND $
        (var.name NE 'hour') AND $
        (var.name NE 'doy') AND $
        (var.name NE 'day') AND $
        (var.name NE 'jday')) THEN BEGIN
         dovar[id] = 1
         nvarsib++
    ENDIF
ENDFOR

IF (nvarsib LT 1) THEN BEGIN
   ez4_alert,'No Variables Available'
   ncerror=1
   RETURN
ENDIF

namesib = STRARR(nvarsib)
titlesib = STRARR(nvarsib)
unitsib = STRARR(nvarsib)

iv = 0
FOR id = 0, nvars-1 DO BEGIN
    IF (dovar(id) EQ 1) THEN BEGIN
       var = NCDF_VARINQ(ncid,id)
       namesib[iv] = STRTRIM(var.name,2)
       FOR att=0,var.natts-1 DO BEGIN
           attname = NCDF_ATTNAME(ncid,id,att)
           IF (attname EQ 'long_name') THEN BEGIN
               NCDF_ATTGET, ncid, id, attname, ttitle
               checkcolon = STRPOS(ttitle,':')
               IF (checkcolon LT 0) THEN $
                   titlesib[iv] = string(ttitle)
           ENDIF ELSE $
           IF ((attname EQ 'title') AND $
               (titlesib[iv] EQ '')) THEN BEGIN
               NCDF_ATTGET, ncid, id, attname, ttitle
               titlesib[iv] = string(ttitle)
           ENDIF ELSE $
           IF (attname EQ 'units') THEN BEGIN
               NCDF_ATTGET, ncid, id, attname, tunits
               unitsib[iv] = string(tunits)
            ENDIF
        ENDFOR
        iv++
    ENDIF  ;dovar
ENDFOR ;nvars id

;...Reorder all variables based on 
;...alphabetical listing of names
IF (sortChoice EQ 1) THEN BEGIN
   order = SORT(namesib)
   namesib = namesib(order)
   titlesib = titlesib(order)
   unitsib = unitsib(order)
ENDIF

;;----------------------------
;;Set mapsib
IF ((nlon EQ -1) AND (nlat EQ -1) $
    AND (nsib EQ -1)) THEN BEGIN
    nlon = 1
    nlat = 1
    nsib = 1
ENDIF

npts = MAX([nsib,nlon*nlat])
IF ((site EQ 1) OR (npts EQ 1) OR $
    (sitesib[0] NE '')) THEN BEGIN
    mapsib=0
ENDIF ELSE mapsib=1


;;----------------------------
;;Get the lat/lon information
IF (nsib LT 1) THEN BEGIN ;Grids

    ltest = NCDF_VARID(ncid,'lon')
    IF (ltest GE 0) THEN lonname='lon' $
    ELSE BEGIN $
        ltest = NCDF_VARID(ncid,'longitude')
        IF (ltest GE 0) THEN lonname='longitude'
   ENDELSE
   IF (ltest GE 0) THEN BEGIN
      NCDF_VARGET, ncid, lonname, lon
   ENDIF ELSE BEGIN
      lon=fltarr(nlon,nlat) - 999.
   ENDELSE

    ltest = NCDF_VARID(ncid,'lat')
    IF (ltest GE 0) THEN latname='lat' $
    ELSE BEGIN $
        ltest = NCDF_VARID(ncid,'latitude')
        IF (ltest GE 0) THEN Latname='latitude'
    ENDELSE
    IF (ltest GE 0) THEN BEGIN
       NCDF_VARGET, ncid, latname, lat
    ENDIF ELSE BEGIN
       lat=fltarr(nlon,nlat) - 999.
    ENDELSE

ENDIF ELSE BEGIN ;Vectors

   ltest = NCDF_VARID(ncid,'lonsib')
   IF (ltest LT 0) THEN ltest = NCDF_VARID(ncid,'lon')
   IF (ltest LT 0) THEN BEGIN
      lonsib = fltarr(nsib) - 999.
      ;ez4_Alert,'Vector Longitude Not Found.'
      ;RETURN
   ENDIF ELSE $
      NCDF_VARGET, ncid, ltest, lonsib

   ltest = NCDF_VARID(ncid,'latsib')
   IF (ltest LT 0) THEN ltest = NCDF_VARID(ncid,'lat')
   IF (ltest LT 0) THEN BEGIN
      latsib = fltarr(nsib) - 999.
      ;ez4_Alert,'Vector Latitude Not Found.'
      ;RETURN
   ENDIF ELSE $
      NCDF_VARGET, ncid, ltest, latsib

   ;;Get gridded lat/lon and references
   IF (mapsib EQ 1) THEN BEGIN
      sib4_gridlatlon, gridInfoFile, $
          nsib, lonsib, latsib, $
          nlon, nlat, lon, lat, lonref, latref
   ENDIF
ENDELSE


NCDF_CLOSE, ncid

END 
