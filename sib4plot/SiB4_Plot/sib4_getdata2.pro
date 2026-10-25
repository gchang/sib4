;Program to get data from multiple files
PRO SiB4_GetData2, sibinfo

COMMON MISSING_COM

WIDGET_CONTROL, /HOURGLASS

;;Set up basic variables
fileNames = sibinfo.fileNames
nfiles = N_ELEMENTS(fileNames)

mapsib = sibinfo.mapsib
refpt = sibinfo.refpt
reftime = sibinfo.reftime
refv = sibinfo.refvar

siblutype = STRMATCH(filenames[0],'*.lu.*')
varname = (sibinfo.vname[refv])[0]

;;Get the data
;;;;Use fflag to determine type of array being saved:
;;;;  fflag = 1 site specific time (single value) or (1,nlu)
;;;;  fflag = 2 site mean (single value) or (1,nlu)
;;;;  fflag = 3 site timeseries (1,ntime) or (1,nlu,ntime)
;;;;  fflag = 4 site time total (single value) or (1,nlu)
;;;;  fflag = 11 map specific time (nsib,1)
;;;;  fflag = 12 map time mean (nsib,1)
;;;;  fflag = 14 map time total (nsib,1)

fflag=-1
FOR f=0,nfiles-1 DO BEGIN
    ncid = EZ4_OPENNCDF(fileNames[f])
    IF (ncid LT 0) THEN BEGIN
        EZ4_Alert,'SiB4 File Not Found.'
        RETURN
    ENDIF

    ;Get the number of times in the file
    tid = ncdf_dimid(ncid,'time')
    NCDF_DIMINQ, ncid, tid, tname, tsize

    ;Get one-time information necessary
    IF (f EQ 0) THEN BEGIN
        ;...PFT information
        IF (siblutype EQ 1) THEN BEGIN
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
        tref = where(dimsize EQ tsize)
        sizetref = N_ELEMENTS(tref)
        IF (sizetref GT 1) THEN BEGIN
           tref = tref[sizetref-1]
        ENDIF

        ;...determine variable size to save
        IF (mapsib EQ 0) THEN BEGIN ;Site
            IF (N_ELEMENTS(larea) GT 1) THEN BEGIN
               nlu = (size(larea))[2]
               IF (dimsize(1) EQ nlu) THEN BEGIN
                   larea = REFORM(larea(refpt,*))
                   lpref = REFORM(lpref(refpt,*))
                ENDIF
            ENDIF

            IF (reftime GE 0) THEN BEGIN ;Specific Time
               fflag = 1
               sibinfo.nsib = 1

               tbcount = 0L
               tecount = 0L

               ftempsize = dimsize(0:ndims-1)
               ftempsize(0) = 1
               ftempsize(tref) = 1
               ftempdata = fltarr(ftempsize)

            ENDIF ELSE $
            IF (reftime EQ -1) THEN BEGIN ;Time Mean
                fflag = 2
                sibinfo.nsib = 1

                offset = lonarr(ndims)
                offset(0) = refpt

                count = dimsize
                count(0) = 1
                count(tref) = 1
                ftempdata = fltarr(count)
                ftempcount = intarr(count)

            ENDIF ELSE $
            IF (reftime EQ -2) THEN BEGIN ;Time Series
                fflag = 3
                sibinfo.nsib = 1

                tbcount = 0L
                tecount = 0L

                IF (tref[0] EQ -1) THEN BEGIN
                    EZ4_Alert,'Expecting Time As A Dimension!'
                    RETURN
                ENDIF

                offset = lonarr(ndims)
                offset(0) = refpt

                ftempsize = dimsize
                ftempsize(0) = 1
                ftempsize(tref) = sibinfo.ntime
                ftempdata = fltarr(ftempsize)
             ENDIF ELSE $
             IF (reftime EQ -3) THEN BEGIN ;Time Total
                fflag = 4
                sibinfo.nsib = 1

                offset = lonarr(ndims)
                offset(0) = refpt

                count = dimsize
                count(0) = 1
                count(tref) = 1
                ftempdata = fltarr(count)
                ftempcount = intarr(count)

             ENDIF ELSE BEGIN
                string1 = 'SiB4 Site Error:'
                string2 = 'Unknown Time Reference.'
                EZ4_Alert,[string1,string2]
                RETURN
             ENDELSE

        ENDIF ELSE BEGIN ;Map
            IF (reftime GE 0) THEN BEGIN ;Specific Time
               fflag = 11
               tbcount = 0L
               tecount = 0L

               count = dimsize
               count(tref) = 1
               ftempdata = fltarr(dimsize(0:ndims-2))

            ENDIF ELSE $
            IF (reftime EQ -1) THEN BEGIN ;Time Mean
               fflag = 12

               count = dimsize
               count(tref) = 1
               ftempdata = fltarr(dimsize(0:ndims-2))
               ftempcount = fltarr(dimsize(0:ndims-2))
            ENDIF ELSE $
            IF (reftime EQ -3) THEN BEGIN ;Time Total
               fflag = 14

               count = dimsize
               count(tref) = 1

               ftempdata = fltarr(dimsize(0:ndims-2))
               ftempcount = fltarr(dimsize(0:ndims-2))

            ENDIF ELSE BEGIN
               EZ4_Alert,'Unknown Option For SiB4 Map'
               RETURN
            ENDELSE
         ENDELSE ;Site or Map
    ENDIF ;f==0

    ;Get the data
    CASE fflag OF
        1: BEGIN ;Site Specific Time
           tecount = tbcount + tsize -1
           IF ((reftime GE tbcount) AND $
               (reftime LE tecount)) THEN BEGIN
               count = dimsize
               count(0) = 1
               count(tref) = 1

               offset = lonarr(ndims)
               offset(0) = refpt
               offset(tref) = reftime - tbcount

               NCDF_VARGET, ncid, varname, ftempdata, $
                     COUNT=count, OFFSET=offset
           ENDIF
           tbcount += tsize
        END

        2: BEGIN ;Site Time Mean
           FOR t=0,tsize-1 DO BEGIN
               offset(tref) = t
               NCDF_VARGET, ncid, varname, ttempdata, $
                   COUNT=count, OFFSET=offset
               goodref = where(ttempdata gt missing)
               IF (goodref[0] GT -1) THEN BEGIN
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

        3: BEGIN ;Site Time Series
           count = dimsize
           count(0) = 1
           count(tref) = tsize
           tecount = tbcount + tsize -1
           NCDF_VARGET, ncid, varname, ttempdata, $
                COUNT=count, OFFSET=offset

           IF (tref EQ 1) THEN BEGIN
              ftempdata(*,tbcount:tecount) = ttempdata
           ENDIF ELSE $
           IF (tref EQ 2) THEN BEGIN
              ftempdata(*,*,tbcount:tecount) = ttempdata
           ENDIF ELSE BEGIN
              EZ4_Alert,'Unexpected Dimensions Of Site-Expected Data.'
              RETURN
           ENDELSE
           tbcount += tsize
        END

        4: BEGIN ;Site Time Total
           FOR t=0,tsize-1 DO BEGIN
               offset(tref) = t
               NCDF_VARGET, ncid, varname, ttempdata, $
                   COUNT=count, OFFSET=offset
               goodref = where(ttempdata gt missing)
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

        11: BEGIN ;Map Specific Time
           tecount = tbcount + tsize -1
           IF ((reftime GE tbcount) AND $
               (reftime LE tecount)) THEN BEGIN
               offset = lonarr(ndims)
               offset(tref) = reftime - tbcount

               NCDF_VARGET, ncid, varname, ftempdata, $
                     COUNT=count, OFFSET=offset
           ENDIF
           tbcount += tsize
        END

        12: BEGIN ;Map Time Mean
           offset = lonarr(nDims)
           FOR t=0,tsize-1 DO BEGIN
               NCDF_VARGET, ncid, varname, ttempdata, $
                    COUNT=count, OFFSET=offset
               goodref = where(ttempdata GT missing)
               IF (goodref[0] GT -1) THEN BEGIN
                  ftempdata(goodref) += ttempdata(goodref)
                  ftempcount(goodref) ++
               ENDIF
               offset(ndims-1)++
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

        14: BEGIN ;Map Time Total
           offset = lonarr(nDims)
           FOR t=0,tsize-1 DO BEGIN
               NCDF_VARGET, ncid, varname, ttempdata, $
                    COUNT=count, OFFSET=offset
               goodref = where(ttempdata GT missing)
               IF (goodref[0] GT -1) THEN BEGIN
                  ftempdata(goodref) += ttempdata(goodref)
                  ftempcount(goodref)++
               ENDIF
               offset(ndims-1)++
            ENDFOR

            IF (f EQ nfiles-1) THEN BEGIN
               badref = where(ftempcount EQ 0)
               IF (badref[0] GT -1) THEN BEGIN
                  ftempdata(badref) = missing
               ENDIF
            ENDIF
        END

        ELSE: BEGIN
           ez4_alert,'Unknown SiB4 Variable Array.'
           RETURN
        END
    ENDCASE

    NCDF_CLOSE, ncid

ENDFOR ;f=0,nfiles

SiB4_Parse_Data, sibinfo, $
     ftempdata, larea, lpref


END
