PRO NC_GetData_Grid2, ncinfo

@ez4_BasicParams.com

WIDGET_CONTROL, /HOURGLASS


;;Set up basic variables
missingvalue = missing

nlon = ncinfo.nlon
nlat = ncinfo.nlat
nlev = ncinfo.nlevs
npts = nlon*nlat
ntime = ncinfo.ntime

lon  = ncinfo.lon
lat  = ncinfo.lat

refv = ncinfo.refvar
varname = (ncinfo.vname[refv])[0]

fileNames = ncinfo.fileNames
nfiles=n_elements(fileNames)

reftime = ncinfo.reftime
time = ncinfo.time
timeperday = ncinfo.ntperday
timeflag = ncinfo.timeflag
timeunits = ncinfo.timeunit

dreflat = -1
dreflon = -1
dreflev = -1
dreftime = -1
tsize = -1

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
;;;;Use fflag to determine type of array being saved:
;;;;  fflag = 1 site specific time (single value) or (1,1,nlu)
;;;;  fflag = 2 site mean (single value) or (1,1,nlu)
;;;;  fflag = 3 site timeseries (1,1,ntime) or (1,1,nlu,ntime)
;;;;  fflag = 4 site time total (single value) or (1,1,nlu)
;;;;  fflag = 5 site no time (single value) or (1,1,nlu)
;;;;  fflag = 11 map specific time (nlon,nlat,1) or (nlon,nlat,nlu,1)
;;;;  fflag = 12 map time mean (nlon,nlat,1) or (nlon,nlat,nlu,1)
;;;;  fflag = 14 map time total (nlon,nlat,1) or (nlon,nlat,nlu,1)
;;;;  fflag = 15 map no time (nlon,nlat) or (nlon,nlat,nlu)

fflag = -1
FOR f=0,nfiles-1 DO BEGIN
    ncid = EZ4_OPENNCDF(fileNames[f])

    IF (f EQ 0) THEN BEGIN
        larea = -1
        lpref = -1
        IF (ncinfo.nlu EQ nlu) THEN BEGIN
           NCDF_VARGET, ncid, 'lu_area', larea
           NCDF_VARGET, ncid, 'lu_pref', lpref
        ENDIF ELSE $
        IF (nlev GT 0) THEN BEGIN
           IF (varname NE 'resp_crop') THEN BEGIN
              NCDF_VARGET, ncid, 'pft_area', larea
           ENDIF 
        ENDIF

        ;...variable dimensions
        varid = NCDF_VARID(ncid,varname)
        varinfo = NCDF_VARINQ(ncid,varid)
        nDims = varinfo.nDims
        dimref = intarr(nDims)
        dimsize = lonarr(nDims)
        FOR d=0,nDims-1 DO BEGIN
            NCDF_DIMINQ, ncid, varinfo.dim(d), name, size
            dimref(d) = d  ;varinfo.dim(d)
            dimsize(d) = size
            IF (name EQ 'time') THEN BEGIN
               trefid=varinfo.dim(d)
               tsize=size
            ENDIF
        ENDFOR

        ;...variable lat/lon/time locations
        tempi = (where(dimsize EQ nlon))[0]
        IF (tempi GE 0) THEN dreflon = dimref(tempi)
  
        tempi = (where(dimsize EQ nlat))[0]
        IF (tempi GE 0) THEN dreflat = dimref(tempi)

        IF (ncinfo.nlu EQ nlu) THEN BEGIN
           tempi = (where(dimsize EQ nlu))[0]
           IF (tempi GE 0) THEN dreflev = dimref(tempi)
        ENDIF ELSE BEGIN
           tempi = (where(dimsize EQ nlev))[0]
           IF (tempi GE 0) THEN dreflev = dimref(tempi)
        ENDELSE

        tempi = (where(dimsize EQ tsize))[0]
        IF (tempi GE 0) THEN dreftime = dimref(tempi)

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

        ;...determine variable size to save
        IF (ncinfo.mapsib EQ 0) THEN BEGIN ;Site
           IF (reftime GE 0) THEN BEGIN ;Specific Time
              fflag = 1
              ncinfo.nsib = 1

              tbcount = 0L
              tecount = 0L

              count = dimsize
              count(dreflon) = 1
              count(dreflat) = 1
              count(dreftime) = 1
              ftempdata = fltarr(count)

              offset = lonarr(ndims)
              offset(dreflon) = reflon
              offset(dreflat) = reflat

           ENDIF ELSE $
           IF (reftime EQ -1) THEN BEGIN ;Time Mean
               fflag = 2
               ncinfo.nsib = 1

               offset = lonarr(ndims)
               offset(dreflon) = reflon
               offset(dreflat) = reflat
               
               count = dimsize
               count(dreflon) = 1
               count(dreflat) = 1
               count(dreftime) = 1
               ftempdata = fltarr(count)
               ftempcount = intarr(count)

            ENDIF ELSE $
            IF (reftime EQ -2) THEN BEGIN ;Time Series
               fflag = 3
               ncinfo.nsib = 1

               tbcount = 0L
               tecount = 0L
 
               IF (dreftime EQ -1) THEN BEGIN
                   EZ4_Alert,'Expecting Time As A Dimension!'
                   RETURN
               ENDIF

               offset = lonarr(ndims)
               offset(dreflon) = reflon
               offset(dreflat) = reflat
               
               ftempsize = dimsize
               ftempsize(dreflon) = 1
               ftempsize(dreflat) = 1
               ftempsize(dreftime) = ntime
               ftempdata = fltarr(ftempsize)
            ENDIF ELSE $
            IF (reftime EQ -3) THEN BEGIN ;Time Total
                fflag = 4
                ncinfo.nsib = 1

                offset = lonarr(ndims)
                offset(dreflon) = reflon
                offset(dreflat) = reflat
  
                count = dimsize
                count(dreflon) = 1
                count(dreflat) = 1
                count(dreftime) = 1
                ftempdata = fltarr(count)
                ftempcount = intarr(count)

             ENDIF ELSE $
             IF (reftime EQ -4) THEN BEGIN ;No Time
                fflag = 5
                ncinfo.nsib = 1

                count = dimsize
                count(dreflon) = 1
                count(dreflat) = 1
                ftempdata = fltarr(ndims+1)
                tbcount = 0

                offset = lonarr(ndims)
                offset(dreflon) = reflon
                offset(dreflat) = reflat

              ENDIF ELSE BEGIN
                string1 = 'SiB4 Site Error:'
                string2 = 'Unknown Time Reference'
                EZ4_Alert,[string1,string2]
                RETURN
             ENDELSE
          ENDIF ELSE BEGIN ;Map
             IF (reftime GE 0) THEN BEGIN ;Specific Time
                 fflag = 11
                 tbcount = 0L
                 tecount = 0L

                 count = dimsize
                 count(dreftime) = 1
                 ftempdata = fltarr(dimsize(0:ndims-2))

             ENDIF ELSE $
             IF (reftime EQ -1) THEN BEGIN ;Time Mean
                fflag = 12

                count = dimsize
                count(dreftime) = 1
                ftempdata = fltarr(dimsize(0:ndims-2))
                ftempcount = fltarr(dimsize(0:ndims-2))

             ENDIF ELSE $
             IF (reftime EQ -3) THEN BEGIN ;Time Total
                 fflag = 14

                 count = dimsize
                 count(dreftime) = 1
                 ftempdata = fltarr(dimsize(0:ndims-2))
                 ftempcount = fltarr(dimsize(0:ndims-2))

             ENDIF ELSE BEGIN
                 string1 = 'Map Still Needs Work!'
                 string2 = 'Unknown reftime: ' + $
                             string(reftime,format='(i4)')
                 EZ4_Alert,[string1,string2]
                 RETURN
             ENDELSE
          ENDELSE
     ENDIF ;f==0

     ;....number of times per file
     IF (dreftime GT -1) THEN BEGIN
        NCDF_DIMINQ, ncid, trefid, tname, tsize
     ENDIF

     ;....data
     CASE fflag OF
       1: BEGIN ;Site Specific Time
          tecount = tbcount + tsize - 1
          IF ((reftime GE tbcount) AND $
              (reftime LE tecount)) THEN BEGIN
              offset(dreftime) = reftime - tbcount
              NCDF_VARGET, ncid, varname, ftempdata, $
                   COUNT=count, OFFSET=offset

           ENDIF
           tbcount += tsize
        END

       2: BEGIN ;Site Mean Value
          FOR t=0,tsize-1 DO BEGIN
              offset(dreftime) = t
              NCDF_VARGET, ncid, varname, ttempdata, $
                   COUNT=count, OFFSET=offset
              goodref = where(ttempdata ne missing)
              IF (goodref[0] gt -1) THEN BEGIN
                  ftempdata(goodref) += ttempdata(goodref)
                  ftempcount(goodref)++
              ENDIF
           ENDFOR

          IF (f EQ nfiles-1) THEN BEGIN
             goodref = where(ftempcount GT 0)
             IF (goodref[0] GT -1) THEN BEGIN
                 ftempdata(goodref) /= ftempcount(goodref)
             ENDIF ELSE BEGIN
                 allref = where(ftempcount EQ 0)
                 ftempdata(allref) = missing
             ENDELSE
          ENDIF
       END

      3: BEGIN ;Site Time-Series
         count = dimsize
         count(dreflon) = 1
         count(dreflat) = 1
         count(dreftime) = tsize
         tecount = tbcount + tsize - 1
         NCDF_VARGET, ncid, varname, ttempdata, $
               COUNT=count, OFFSET=offset

         IF (dreftime EQ 1) THEN BEGIN
            ftempdata(*,tbcount:tecount) = ttempdata
         ENDIF ELSE $
         IF (dreftime EQ 2) THEN BEGIN
            ftempdata(*,*,tbcount:tecount) = ttempdata
         ENDIF ELSE $
         IF (dreftime EQ 3) THEN BEGIN
            ftempdata(*,*,*,tbcount:tecount) = ttempdata
         ENDIF ELSE BEGIN
            EZ4_Alert,'Unexpected Dimensions of Site Data.'
            RETURN
         ENDELSE
         tbcount += tsize
      END

      4: BEGIN ;Site Total Value
         FOR t=0,tsize-1 DO BEGIN
             offset(dreftime) = t
             NCDF_VARGET, ncid, varname, ttempdata, $
                  COUNT=count, OFFSET=offset
             goodref = where(ttempdata ne missing)
             IF (goodref[0] GT -1) THEN BEGIN
                ftempdata(goodref) += ttempdata(goodref)
                ftempcount(goodref)++
             ENDIF
          ENDFOR

          IF (f EQ nfiles-1) THEN BEGIN
             badref = where(ftempcount EQ 0)
             IF (badref[0] GT -1) THEN $
                 ftempdata(badref) = missing
          ENDIF
       END

       5: BEGIN  ;Site No Time
           NCDF_VARGET, ncid, varname, ttempdata, $
                COUNT=count, OFFSET=offset
           IF (ndims EQ 2) THEN BEGIN
              ftempdata(*,*,tbcount) = ttempdata(*,*)
           ENDIF ELSE $
           IF (ndims EQ 3) THEN BEGIN
              ftempdata(*,*,*,tbcount) = ttempdata(*,*,*)
           ENDIF ELSE BEGIN
              EZ4_Alert,'Unexpected Dimensions on Non-Time Data.'
              RETURN
           ENDELSE
           tbcount++
       END

       11: BEGIN  ;Map Specific Time
           tecount = tbcount + tsize - 1
           IF ((reftime GE tbcount) AND $
               (reftime LE tecount)) THEN BEGIN
                offset = lonarr(nDims)
                offset(dreftime) = reftime - tbcount

                NCDF_VARGET, ncid, varname, ftempdata, $
                       COUNT=count, OFFSET=offset
           ENDIF
           tbcount += tsize
       END

       12: BEGIN  ;Map Time Mean
           offset = lonarr(nDims)
           FOR t=0,tsize-1 DO BEGIN
               NCDF_VARGET,ncid,varname,ttempdata, $
                    COUNT=count, OFFSET=offset
               goodref = where(ttempdata gt missing)
               IF (goodref[0] GT -1) THEN BEGIN
                   ftempdata(goodref) += ttempdata(goodref)
                   ftempcount(goodref) ++
               ENDIF
               offset(dreftime)++
            ENDFOR

           IF (f EQ nfiles-1) THEN BEGIN
              goodref = where(ftempcount GT 0)
              IF (goodref[0] GT -1) THEN BEGIN
                 ftempdata(goodref) /= ftempcount(goodref)
              ENDIF

              badref = where(ftempcount EQ 0)
              IF (badref[0] GT -1) THEN BEGIN
                 ftempdata(badref) = missing
              ENDIF
           ENDIF
        END

       14: BEGIN  ;Map Time Total
           offset = lonarr(nDims)
           FOR t=0,tsize-1 DO BEGIN
               NCDF_VARGET,ncid,varname,ttempdata, $
                    COUNT=count, OFFSET=offset
               goodref = where(ttempdata gt missing)
               IF (goodref[0] GT -1) THEN BEGIN
                   ftempdata(goodref) += ttempdata(goodref)
                   ftempcount(goodref)++
               ENDIF
               offset(dreftime)++
            ENDFOR

           IF (f EQ nfiles-1) THEN BEGIN
              badref = where(ftempcount EQ 0)
              IF (badref[0] GT -1) THEN BEGIN
                 ftempdata(badref) = missing
              ENDIF
           ENDIF
        END

       15: BEGIN  ;Map No Time
           NCDF_VARGET,ncid,varname,ttempdata
           IF (ndims EQ 2) THEN BEGIN
              ftempdata(*,*,tbcount) = ttempdata(*,*)
           ENDIF ELSE $
           IF (ndims EQ 3) THEN BEGIN
              ftempdata(*,*,*,tbcount) = ttempdata(*,*,*)
           ENDIF ELSE BEGIN
              EZ4_Alert,'Unexpected Dimensions on Non-Time Data.'
              RETURN
           ENDELSE
           tbcount++
       END
     
       ELSE: BEGIN
           string1 = 'Unknown Variable Array'
           string2 = '  Flag: ' + string(fflag,format='(i3)')
           EZ4_Alert,[string1,string2]
           RETURN
        END
    ENDCASE
           
     NCDF_CLOSE, ncid
ENDFOR ;f=0,nfiles-1

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

;...parse data
NC_Parse_Data, ncinfo, $
       ftempdata, larea, lpref

END
