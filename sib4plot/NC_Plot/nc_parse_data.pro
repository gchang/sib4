PRO NC_Parse_Data, ncinfo, $
    ftempdata, larea, lpref

@ez4_BasicParams.com

;;------------------------------------
;;Set local variables
plottype = -1
dsize = size(ftempdata)
ndims = dsize(0)

nsib = ncinfo.nsib
nlon = ncinfo.nlon
nlat = ncinfo.nlat
lon = ncinfo.lon
lat = ncinfo.lat
nlevs = ncinfo.nlevs
ntime = ncinfo.ntime
time = ncinfo.time

mapdata = ncinfo.mapsib
reflon = ncinfo.reflon
reflat = ncinfo.reflat
refpt = ncinfo.refpt
refpft = ncinfo.refpft
refpftt = ncinfo.refpftt
refpfta = ncinfo.refpfta

reftime = ncinfo.reftime
refv = ncinfo.refvar
varname = (ncinfo.vname[refv])[0]
vartitle = ncinfo.vtitle[refv]
varunits = ncinfo.vunits[refv]
nchars = strlen(varname) + strlen(vartitle)
windowtitle = ncinfo.windowTitle $
    + ' ' + STRUPCASE(varname)

;;Set the Font
EZ4_SetFont

;;Set the Title
IF (nchars gt defaultTitleLength) THEN BEGIN 
   title = STRUPCASE(varname)
ENDIF ELSE BEGIN
   IF (vartitle EQ '') THEN BEGIN
       title = STRUPCASE(varname)
    ENDIF ELSE BEGIN
       title = STRUPCASE(varname) + ': ' + STRUPCASE(vartitle)
    ENDELSE
ENDELSE

;;Set the Comment
comment = ''
IF (mapdata EQ 0) THEN BEGIN
   IF ((ncinfo.vallon GT missing) AND $
       (ncinfo.vallat GT missing)) THEN BEGIN
       comment = 'Lon: ' + string(ncinfo.vallon,format='(F7.1)') $
               + '  Lat: ' + string(ncinfo.vallat,format='(F6.1)')
   ENDIF
ENDIF

pcomment = ''
IF (refpft EQ 0) THEN BEGIN
   IF (refpftt EQ 0) THEN BEGIN
      pcomment = 'ALL PFTS'
   ENDIF
ENDIF ELSE $
IF (refpft GT 0) THEN BEGIN
   pcomment = STRUPCASE(string(ncinfo.levnames(refpft)))
ENDIF

tcomment=''
IF (ntime GT 1) THEN BEGIN
   IF (reftime EQ -3) THEN BEGIN
      tcomment = 'TIME TOTAL'
   ENDIF ELSE $
   IF (reftime EQ -1) THEN BEGIN
       tcomment = 'TIME MEAN'
   ENDIF ELSE $
   IF (reftime GE 0) THEN BEGIN
       IF (ntime EQ 12) THEN BEGIN
           tcomment = monname(reftime)
       ENDIF ELSE $
       IF (ntime GT 1) THEN BEGIN
           timenow = time(reftime)
           tformat = ez4_niceformat(timenow)
           tcomment = 'Time: ' + string(timenow,format=tformat)

           tformat = ez4_niceformat(reftime,/INT)
           tcomment2 = '  (Rec#: ' + string(reftime+1,format=tformat) + ')'
       ENDIF
   ENDIF
ENDIF
tnstring=tcomment

vstring=varunits
IF (vstring EQ '-') THEN vstring=''

IF (comment EQ '') THEN comment = tcomment ELSE $
IF (tcomment NE '') THEN comment += '  ' + tcomment

IF (comment EQ '') THEN BEGIN
    comment = pcomment 
ENDIF ELSE BEGIN
   IF (pcomment NE '') THEN $
       comment = comment + '  ' + pcomment
ENDELSE

;;Set the filename and simlabel
fileName = ncinfo.fileNames
simlabel = ncinfo.simlabel

;;Check the units to see if they need to be changed
EZ4_setUnits, varunits, units


;;;-----------------------------------
;;;Select plot type based on data size
;;; 1=Line Plot
;;; 2=Gridded Map
;;; 3=Vector Map

;;;-----0-DIMENSION VARIABLES------
IF (ndims EQ 0) THEN BEGIN
    string1=STRUPCASE(varname)
    string2= 'Lon: ' + string(lon(reflon),format='(F7.2)') $
           + '   Lat: ' + string(lat(reflat),format='(F6.2)')
    string3=tnstring

    messageformat=EZ4_niceformat(ftempdata)
    string4=string(ftempdata,format=messageformat) $
             + ' ' + vstring

    message = [string1,string2,string3,string4]
    EZ4_MESSAGE, message

    RETURN

ENDIF


;;;-----1-DIMENSION VARIABLES------
IF (ndims EQ 1) THEN BEGIN
   IF (dsize(1) EQ 1) THEN BEGIN
       string1=STRUPCASE(varname)

       string2= 'Lon: ' + string(lon(reflon),format='(F7.2)') $
           + '   Lat: ' + string(lat(reflat),format='(F6.2)')
       string3=tnstring

       messageformat=EZ4_niceformat(ftempdata)
       string4=string(ftempdata,format=messageformat) $
             + ' ' + vstring

       message = [string1,string2,string3,string4]
       EZ4_MESSAGE, message

       RETURN
   ENDIF ELSE BEGIN
      plottype = 3
      gtempdata = ftempdata
   ENDELSE
ENDIF ;1-D Variables

;;;-----2-DIMENSION VARIABLES------
IF (ndims EQ 2) THEN BEGIN
   IF ((dsize(1) EQ nlon) AND $
       (dsize(2) EQ nlat)) THEN BEGIN ;Grid Map
        plottype = 2
        data = ftempdata
   ENDIF
   IF ((dsize(1) EQ nsib) AND $
       (dsize(2) EQ nlevs) AND $
       (mapdata EQ 1)) THEN BEGIN  ;Vector Map
       gtempdata = fltarr(nsib)
       FOR i=0,nsib-1 DO BEGIN
           IF (refpft EQ 0) THEN BEGIN
               gtempdata(i) = TOTAL(ftempdata(i,*))
            ENDIF ELSE BEGIN
               gtempdata(i) = ftempdata(i,refpft-1)
            ENDELSE
       ENDFOR
       plottype = 3
   ENDIF

  IF ((dsize(1) EQ 1) AND $
      (dsize(2) EQ ntime)) THEN BEGIN ;Site Timeseries
      plottype = 1
      xdata = fltarr(1,ntime)
      xdata(0,*) = time
      ydata = fltarr(1,ntime)
      ydata(0,*) = ftempdata(0,*)
      linename = STRUPCASE(varname)

      IF (ncinfo.sitesib(0) NE '') THEN BEGIN
         comment=ncinfo.sitesib(refpt)
      ENDIF
  ENDIF
ENDIF ;2-D Variables

;;;-----3-DIMENSION VARIABLES------
IF (ndims EQ 3) THEN BEGIN
   IF ((dsize(1) EQ 1) AND $
       (dsize(2) EQ 1) AND $
       (dsize(3) EQ ntime)) THEN BEGIN ;Site Timeseries
       plottype = 1

       xdata = fltarr(1,ntime)
       xdata(0,*) = time
       ydata = fltarr(1,ntime)
       ydata(0,*) = ftempdata(0,0,*)
       lonsib = lon(reflon)
    ENDIF ELSE $
    IF ((dsize(1) EQ 1) AND $
        (dsize(2) EQ 1) AND $
        (dsize(3) EQ nlu)) THEN BEGIN ;Site PFT Values
        string1 = STRUPCASE(varname) $
                  + ' (' + varunits + ')'
        mylon = lon(reflon)
        mylat = lat(reflat)
        string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                + '   Lat: ' + string(mylat,format='(F7.2)')
        string4 = tnstring

        mylpref = REFORM(lpref(reflon,reflat,*))
        mylarea = REFORM(larea(reflon,reflat,*))
        npfts = N_ELEMENTS(where(mylpref GT 0))
        tvalue = 0.0
        strings = strarr(npfts + 1)
        FOR i=0,npfts-1 DO BEGIN
             tpref = where(pftRefs EQ mylpref(i))
             tvalue += ftempdata(0,0,i) * mylarea(i)
             strings(i+1) = pftNames(tpref) + '  ' $
                        + string(ftempdata(0,0,i),format='(f10.3)') $
                        + '   ' + string(mylarea(i),format='(f7.3)')
        ENDFOR
        string5 = ''
        string6 = 'PFT      Value      Area'
        strings(0) = pftNames(0) + '  ' $
                     + string(tvalue,format='(f10.3)') + '   ' $
                     + string(1.0,format='(f7.3)')
       IF (string4 EQ '') THEN BEGIN
           message = [string1,string3,string5,string6,strings]
       ENDIF ELSE BEGIN
           message = [string1,string3,string4,string5,string6,strings]
       ENDELSE

       EZ4_MESSAGE, message
       RETURN

    ENDIF ELSE $
    IF ((dsize(1) EQ 1) AND $
        (dsize(2) EQ 1) AND $
        (dsize(3) EQ nlevs)) THEN BEGIN ;Site Levels Values
        string1 = STRUPCASE(varname) $
                  + ' (' + varunits + ')'
        mylon = lon(reflon)
        mylat = lat(reflat)
        string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                + '   Lat: ' + string(mylat,format='(F7.2)')
        string4 = tnstring
        string5 = ''
        string6 = ncinfo.levlabel + '       Value       Area'

        mylarea = REFORM(larea(reflon,reflat,*))
        mynlevs = N_ELEMENTS(where(mylarea GT 0))
        strings = strarr(mynlevs + 1)
        tvalue = 0.0
        tcount = 1
        FOR i=0,nlevs-1 DO BEGIN
            IF (mylarea(i) GT 0) THEN BEGIN
                tvalue += ftempdata(0,0,i)
                strings(tcount) = ncinfo.levNames(i+1) + '  ' $
                        + string(ftempdata(0,0,i),format='(f12.5)') $
                  + ' ' + string(mylarea(i),format='(f7.3)')
                tcount++
            ENDIF
        ENDFOR
        strings(0) = ncinfo.levNames(0) + string(tvalue,format='(f12.5)') $
                     + string(TOTAL(mylarea),format='(f7.3)')

        IF (string4 EQ '') THEN BEGIN
           message = [string1,string3,string5,string6,strings]
        ENDIF ELSE BEGIN
           message = [string1,string3,string4,string5,string6,strings]
        ENDELSE
        EZ4_MESSAGE, message
        RETURN

    ENDIF ELSE $
    IF ((dsize(1) EQ nlon) AND $
        (dsize(2) EQ nlat) AND $
        (dsize(3) EQ nlu)) THEN BEGIN
        gtempdata = fltarr(nlon,nlat)
        FOR i=0,nlon-1 DO BEGIN
            FOR j=0,nlat-1 DO BEGIN
                ltempdata = REFORM(ftempdata(i,j,*))
                ltemparea = REFORM(larea(i,j,*))
                ltemppft = REFORM(lpref(i,j,*))

                IF (ncinfo.refpft LT 0) THEN mypft=0 $
                ELSE mypft=pftRefs(ncinfo.refpft)
                sib4_getpft, missing, refpfta, mypft, $
                    ltempdata, ltemparea, ltemppft, value
                gtempdata(i,j) = value
             ENDFOR
         ENDFOR

        data = gtempdata
        plottype = 2

    ENDIF ELSE $
    IF ((dsize(1) EQ nlon) AND $
        (dsize(2) EQ nlat) AND $
        (dsize(3) EQ ntime)) THEN BEGIN
         message1='Unknown How To Process Data:'
         message2='   nlon/nlat/ntime:'
         message3='  '+string(nlon,format='(i7)') $
                + '  '+string(nlat,format='(i6)') $
                + '  '+string(ntime,format='(i6)')
         EZ4_Message,[message1,message2,message3]
         RETURN
    ENDIF ELSE $
    IF ((dsize(1) EQ nlon) AND $
        (dsize(2) EQ nlat) AND $
        (dsize(3) EQ nlevs)) THEN BEGIN
         gtempdata = fltarr(nlon,nlat)
         IF (refpft EQ 0) THEN BEGIN
             FOR i=0,nlon-1 DO BEGIN
                 FOR j=0,nlat-1 DO BEGIN
                     IF (refpfta EQ 0) THEN BEGIN
                        gtempdata(i,j) = TOTAL(ftempdata(i,j,*))
                     ENDIF ELSE BEGIN
                        mylarea = fltarr(nlevs)
                        mylarea(*) = larea(i,j,*)
                        totlarea = TOTAL(larea(i,j,*))
                        IF (totlarea LT 1.0) THEN BEGIN
                            mylarea(0) += (1.0 - totlarea)
                        ENDIF
                        IF (refpfta EQ 1) THEN BEGIN
                            FOR k=0, nlevs-1 DO BEGIN
                                gtempdata(i,j) += ftempdata(i,j,k) * $
                                                   mylarea(k)
                            ENDFOR
                        ENDIF ELSE BEGIN
                            FOR k=1, nlevs-1 DO BEGIN
                                IF (larea(i,j,k) GT 0.) THEN BEGIN
                                     gtempdata(i,j) += ftempdata(i,j,k) $
                                               / mylarea(k)
                                ENDIF
                             ENDFOR
                         ENDELSE
                      ENDELSE
                  ENDFOR
              ENDFOR
         ENDIF ELSE $
         IF ((refpft GE 1) AND (refpft LE nlevs)) THEN BEGIN
             mylarea = 1.
             IF (refpfta EQ 1) THEN BEGIN
                 mylarea = larea(*,*,refpft-1)
             ENDIF ELSE $  
             IF (refpfta EQ 2) THEN BEGIN
                 mylarea = fltarr(nlon,nlat) + 1.
                 mylareat = larea(*,*,refpft-1)
                 aref = where(mylareat GT 0)
                 mylarea(aref) = 1./mylareat(aref)
                 ;mylarea = 1./larea(*,*,refpft-1)
             ENDIF
             gtempdata(*,*) = ftempdata(*,*,refpft-1) * mylarea

             mylareat = larea(*,*,refpft-1)
             aref = where(mylareat EQ 0.)
             gtempdata(aref) = missing
         ENDIF ELSE BEGIN
             EZ4_Alert,'Error Processing 3-D Data.'
             RETURN
         ENDELSE

        data = gtempdata
        plottype = 2

    ENDIF ELSE BEGIN
       message1='Error Reading 3-D Data:'
       message2='   Dimension 1: '+string(dsize(1),format='(i6)')
       message3='   Dimension 2: '+string(dsize(2),format='(i5)')
       message4='   Dimension 3: '+string(dsize(3),format='(i5)')
       EZ4_Message,[message1,message2,message3,message4]

      RETURN
    ENDELSE
ENDIF ;3-D Variables


;;;-----4-DIMENSION VARIABLES------
IF (ndims EQ 4) THEN BEGIN
   IF ((dsize(1) EQ 1) AND $
       (dsize(2) EQ 1) AND $
       (dsize(3) EQ nlu) AND $
       (dsize(4) EQ ntime)) THEN BEGIN

       data2d = fltarr(nlu,ntime)
       data2d(*,*) = ftempdata(0,0,*,*)
       
       tlarea = REFORM(larea(reflon,reflat,*))
       tlpref = REFORM(lpref(reflon,reflat,*))

       nx = 1
       IF (refpftt EQ 0) THEN BEGIN
           pref = pftRefs(refpft)
           showlabel = 0
           sib4_getpftt, missing, pftrefa, pref, data2d, $
               tlarea, tlpref, values
       ENDIF ELSE BEGIN
           pref = where(tlpref GT 1)
           showlabel = 1
           IF (pref[0] GT -1) THEN nx+=n_elements(pref)
           sib4_getpftt, missing, pftrefa, 0, data2d, $
               tlarea, tlpref, values
        ENDELSE

        IF (values[0] EQ missing) THEN BEGIN
           EZ4_Alert,'Specified PFT Not Simulated.'
           RETURN
        ENDIF

        mypftname=''
        IF (refpft EQ 0) THEN BEGIN
           mypftName = 'ALL PFTS'
        ENDIF ELSE $
        IF (refpft LT npft-1) THEN BEGIN
           mypftname=pftNames(refpft)
        ENDIF

        xdata = fltarr(nx,ntime)
        ydata = fltarr(nx,ntime)
        linecolor = fltarr(nx)
        linestyle = fltarr(nx)
        linename = strarr(nx)
        drawLines = fltarr(nx)

        xdata(0,*) = ncinfo.time
        ydata(0,*) = values(*)
        showLineLabel = showlabel 
        linename(0) = STRUPCASE(varname)  ;mypftName
        drawLines(*) = 1
        FOR i=1,nx-1 DO BEGIN
            pref = tlpref(i-1)
            pnum = where(pftRefs eq pref)
            xdata(i,*) = ncinfo.time
            ydata(i,*) = data2d(i-1,*)
            linecolor(i) = i
            linename(i) = pftNames(pnum)
        ENDFOR
        plottype = 1

   ENDIF ELSE $
   IF ((dsize(1) EQ 1) AND $
       (dsize(2) EQ 1) AND $
       (dsize(3) EQ nlevs) AND $
       (dsize(4) EQ ntime)) THEN BEGIN ;Site Timeseries
       plottype = 1

       ez4_alert,'Needs Work!'
       RETURN

    ENDIF ELSE BEGIN
       message1='Error Reading 4-D Data:'
       message2='   Dimension 1: '+string(dsize(1),format='(i6)')
       message3='   Dimension 2: '+string(dsize(2),format='(i5)')
       message4='   Dimension 3: '+string(dsize(3),format='(i5)')
       message5='   Dimension 4: '+string(dsize(4),format='(i6)')
       EZ4_Message,[message1,message2,message3,message4,message5]
       RETURN
    ENDELSE
 ENDIF


;;;-----------------------------------
;;;Plot appropriate plot type
;;;1=Line Plots
;;;2=Grid Maps
;;;3=Vector Maps
CASE plottype OF
     1: BEGIN ;Line Plots
        ;...create line plot structure
        Line_Struct_Data, geoplot, $
           xdata=xdata, ydata=ydata, $
           xtitle=timeunits, ytitle=units, $
           timeflag=timeflag, timeperday=timeperday, $
           linecolor=linecolor, linestyle=linestyle, $
           drawLines=drawLines, $
           linename=linename, showlinelabel=showlinelabel, $
           lon=lonsib, units=units, $
           comment=comment, title=title, $
           fileName=fileName, simlabel=simlabel

        ;...create a window structure
        Line_Struct_Window, geowindow, windowTitle=windowTitle

        ;...combine information into single structure
        plot = CREATE_STRUCT(geoplot, geowindow)

        ;...plot the data
        Line_Plot, plot
     END

     2: BEGIN ;Maps
        ;...create gridded plot structure
        geo4_struct_data, sibplot, $
            data=data, $
            comment=comment, title=title, $
            fileName=fileName, simlabel=simLabel, $
            cbartitle=units, whiteOut='low', $
            nlon=nlon, nlat=nlat, $
            lon=lon, lat=lat, $
            varname=varname, units=varunits

        ;...create a window structure
        GEO4_Struct_Window, sibwindow, windowTitle=windowTitle

        ;...combine information into single structure
        plot = CREATE_STRUCT(sibplot, sibwindow)

        ;...plot the data
        GEO4_Plot, plot

        RETURN
     END

     3: BEGIN ;Vector Maps
         ;...grid sib4 data
         sib4_griddata, nsib, gtempdata, $
             ncinfo.nlon, ncinfo.nlat, $
             ncinfo.lonref, ncinfo.latref, data

         ;...create gridded plot structure
         geo4_struct_data, geoplot, $
              data=data, $
              comment=comment, title=title, $
              fileName=fileName, simlabel=simLabel, $
              cbartitle=units, whiteOut='low', $
              nlon=ncinfo.nlon, nlat=ncinfo.nlat, $
              lon=ncinfo.lon, lat=ncinfo.lat, $
              units=varunits

          ;...create a window structure
          GEO4_Struct_Window, geowindow, windowTitle=windowTitle

          ;...combine information into single structure
          plot = CREATE_STRUCT(geoplot, geowindow)

          ;...plot the data
          GEO4_Plot, plot

          RETURN
     END

     ELSE: BEGIN
       string1 = 'Unknown NetCDF Plot Type.'
       string2 = '   Variable NumDim: ' + string(ndims,format='(i4)')
       string3 = '   Dimension 1: ' + string(dsize(1),format='(i4)')
       IF (ndims GE 2) THEN BEGIN
           string4 = '   Dimension 2: ' + string(dsize(2),format='(i4)')
       ENDIF ELSE string4 = ''
       IF (ndims GE 3) THEN BEGIN
           string5 = '   Dimension 3: ' + string(dsize(3),format='(i4)')
       ENDIF ELSE string5 = ''
       IF (ndims GE 4) THEN BEGIN
           string6 = '   Dimension 4: ' + string(dsize(4),format='(i4)')
       ENDIF ELSE string6 = ''

       ez4_alert,[string1,string2,string3,string4,string5,string6]
       
        RETURN
     END
ENDCASE



END
