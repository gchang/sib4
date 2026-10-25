;;Select plot type based on data size
;;; 1=Line Plot
;;; 2=Vector -> Gridded Map

PRO SiB4_Parse_Data, sibinfo, $
     ftempdata, larea, lpref

@ez4_BasicParams.com

;;------------
;;Set values
nsib = sibinfo.nsib
ntime = sibinfo.ntime

dsize = size(ftempdata)
ndims = dsize(0)

mapsib = sibinfo.mapsib
refpft = sibinfo.refpft
refpftt = sibinfo.refpftt
refpfta = sibinfo.refpfta
reftime = sibinfo.reftime

refpt = sibinfo.refpt
refv = sibinfo.refvar

mapsib = sibinfo.mapsib
IF (refpt GE 0) THEN $
   lonsib = sibinfo.lonsib(refpt)

time = sibinfo.time
timeperday = sibinfo.ntperday
timeflag = sibinfo.timeflag
timeunits = sibinfo.timeunit
varname = (sibinfo.vname[refv])[0]
vartitle = sibinfo.vtitle[refv]
varunits = sibinfo.vunits[refv]
nchars = strlen(varname) + strlen(vartitle)
windowTitle = sibinfo.windowTitle $
               + ' ' + STRUPCASE(varname)

;;;Set the Font
EZ4_SetFont

;;;Set the Title
IF (nchars gt defaultTitleLength) THEN BEGIN
    title = STRUPCASE(varname)
    ;comment = STRUPCASE(sibinfo.vtitle[refv])
ENDIF ELSE BEGIN
   IF (vartitle EQ '') THEN BEGIN
      title = STRUPCASE(varname)
   ENDIF ELSE BEGIN
      title = STRUPCASE(varname) + ': ' + STRUPCASE(vartitle)
   ENDELSE
ENDELSE

;;;Set the Comment
comment = ''
IF (mapsib EQ 0) THEN BEGIN
   IF (refpt EQ -1) THEN BEGIN
      IF (n_elements(sibinfo.reflon) GT 0) THEN BEGIN
         reflon = sibinfo.reflon
         reflat = sibinfo.reflat
         IF (reflon GE 0) AND (reflat GE 0) THEN BEGIN
             comment = 'Lon: ' $
                     +  string(sibinfo.lon(reflon),format='(f7.2)') $
                     + ', Lat: ' $
                     + string(sibinfo.lat(reflat),format='(f6.2)')
         ENDIF
      ENDIF
   ENDIF ELSE $
   IF (N_ELEMENTS(sibinfo.sitesib) GT refpt) THEN BEGIN
       comment = sibinfo.sitesib(refpt)
   ENDIF ELSE BEGIN
       myformatt = EZ4_niceformat(refpt+1,/INT)
       ;comment = 'SITE' + string(refpt+1,format=myformatt)
       stringll = '   Lon: ' $
            + string(sibinfo.lonsib(refpt),format='(F7.2)') $
            + '  Lat: ' $
            + string(sibinfo.latsib(refpt),format='(F6.2)')
       comment = stringll
   ENDELSE
ENDIF

pcomment=''
IF (refpft LT 0) THEN BEGIN
   mypft=-1
   mypftname=''
ENDIF ELSE $
IF (refpft EQ 0) THEN BEGIN
   mypft=0
   mypftName = 'ALL PFTS'
ENDIF ELSE $
IF (refpft LT npft-1) THEN BEGIN
   mypft=pftRefs(refpft)
   mypftname=pftNames(refpft)
   pcomment = mypftName
ENDIF

tcomment=''
tcomment2=''
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
           tformat = ez4_niceformat(timenow,/YRS)
           tcomment = 'Time: ' + string(timenow,format=tformat)
           tformat = ez4_niceformat(reftime,/INT)
           tcomment2 = '  (Rec#: ' + string(reftime+1,format=tformat) + ')'
        ENDIF
   ENDIF
ENDIF

vstring=varunits
IF (vstring EQ '-') THEN vstring=''  ;(-)'

IF (pcomment EQ '') THEN ptcomment=tcomment ELSE $
IF (tcomment EQ '') THEN ptcomment = pcomment $
ELSE ptcomment = pcomment + '  ' + tcomment

IF (comment EQ '') THEN comment = ptcomment ELSE $
IF (ptcomment NE '') THEN comment += '  ' + ptcomment

    
;;;Set the Filename and SimLabel
fileName = sibinfo.FileNames
simLabel = sibinfo.simLabel

;;;Check the units to see if they need to be changed
EZ4_SetUnits, varunits, units

;;------------------------------------
;;Select plot type based on data size
;;; 1=Line Plot
;;; 2=Vector -> Gridded Map

plottype = -1

;;;-----0-DIMENSION VARIABLES------
IF (ndims EQ 0) THEN BEGIN
    string1=STRUPCASE(varname)

    myformat = EZ4_niceformat(refpt+1,/INT)
    string2= 'Site' + string(refpt+1,myformat)
    string3= 'Lon: ' + string(sibinfo.lonsib(refpt),format='(F7.2)') $
           + '   Lat: ' + string(sibinfo.latsib(refpt),format='(F6.2)')

    messageformat=EZ4_niceformat(ftempdata)
    string5=string(ftempdata,format=messageformat) $
             + ' ' + vstring

    IF (tcomment2 NE '') THEN BEGIN
        message = [string1,string2,string3,tcomment,tcomment2,string5]
    ENDIF ELSE $
        message = [string1,string2,string3,tcomment,string5]
    EZ4_MESSAGE, message

    RETURN
ENDIF

;;;-----1-DIMENSION VARIABLES------
IF (ndims EQ 1) THEN BEGIN

   IF ((dsize(1) EQ 1) AND $
       (mapsib EQ 0)) THEN BEGIN ;Single Site Value
       string1=STRUPCASE(varname)

       myformat = ez4_niceformat(refpt+1,/INT)
       string2 = 'Site' + string(refpt+1,myformat)
       string3= 'Lon: ' + string(sibinfo.lonsib(refpt),format='(F7.2)') $
              + '  Lat: ' + string(sibinfo.latsib(refpt),format='(F6.2)')

       messageformat=EZ4_niceformat(ftempdata)
       string5=string(ftempdata,format=messageformat) $
               + ' ' + vstring

       IF (tcomment2 NE '') THEN BEGIN
           message = [string1,string2,string3,tcomment, $
                      tcomment2,string5]
       ENDIF ELSE $
           message = [string1,string2,string3,tcomment,string5]
       EZ4_MESSAGE, message

       RETURN
   ENDIF

   IF ((dsize(1) EQ nsib) AND $  ;Single Map
       (mapsib EQ 1)) THEN BEGIN
       gtempdata = ftempdata
       plottype=2
   ENDIF
ENDIF

;;;-----2-DIMENSION VARIABLES------
IF (ndims EQ 2) THEN BEGIN
   ;Selected Site Time Series
   IF ((dsize(1) EQ 1) AND $
       (dsize(2) EQ ntime)) THEN BEGIN
        IF (reftime EQ -2) THEN BEGIN
            xdata = fltarr(1,ntime)
            xdata(0,*) = sibinfo.time
            ydata = ftempdata(0,*)

            drawLines = fltarr(1) + 1
            linecolor = fltarr(1)
            linestyle = fltarr(1)
            linename = strarr(1)
            linename(0) = STRUPCASE(varname)
            showLineLabel = 0

            plottype = 1
       ENDIF

       IF (reftime EQ -1) THEN BEGIN
           value = mean(ftempdata)
           myformat = ez4_niceformat(refpt,/INT)
           string1 = STRUPCASE(varname)
           string2 = 'Site' + $
                     string(refpt+1,format=myformat)
           mylon = sibinfo.lonsib(refpt)
           mylat = sibinfo.latsib(refpt)
           string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                   + '   Lat: ' + string(mylat,format='(F7.2)')

           messageformat=EZ4_niceformat(value)
           string5=string(value,format=messageformat)  $
                   + ' ' + vstring

           IF (tcomment2 NE '') THEN BEGIN
              message = [string1,string2,string3, $
                         tcomment,tcomment2,string5]
           ENDIF ELSE $
              message = [string1,string2,string3,tcomment,string5]
           EZ4_MESSAGE, message
           RETURN
        ENDIF

        IF (reftime GE 0) THEN BEGIN
           value = ftempdata(0,reftime)
           myformat = ez4_niceformat(refpt,/INT)

           string1 = STRUPCASE(varname)
           string2 = 'Site' + string(refpt+1,format=myformat)
           mylon = sibinfo.lonsib(refpt)
           mylat = sibinfo.latsib(refpt)
           string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                      + '   Lat: ' + string(mylat,format='(F7.2)')

           messageformat=EZ4_niceformat(value)
           string5=string(value,format=messageformat) $
                      + ' ' + vstring

           IF (tcomment2 NE '') THEN BEGIN
               message = [string1,string2,string3,tcomment,tcomment2,string5]
            ENDIF ELSE $
               message = [string1,string2,string3,tcomment,string5]
           EZ4_MESSAGE, message
           RETURN
      ENDIF
   ENDIF ELSE $
   IF ((dsize(1) EQ nsib) AND $
       (dsize(2) EQ ntime)) THEN BEGIN
       xdata = fltarr(1,ntime)
       xdata(0,*) = sibinfo.time
       ydata = ftempdata(refpt,*)

       drawLines = fltarr(1) + 1
       linecolor = fltarr(1)
       linestyle = fltarr(1)
       linename = strarr(1)
       linename(0) = STRUPCASE(varname)
       showLineLabel = 0

       plottype = 1
    ENDIF

   ;Selected Site Value
   IF ((dsize(1) EQ 1) AND $
       (dsize(2) EQ nlu) AND $
       (mapsib EQ 0)) THEN BEGIN
       myformat = ez4_niceformat(refpt,/INT)
       string1 = STRUPCASE(varname) $
                  + ' (' + varunits + ')'
       string2 = 'Site ' $
                  + string(refpt+1,format=myformat)
       mylon = sibinfo.lonsib(refpt)
       mylat = sibinfo.latsib(refpt)
       string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
               + '   Lat: ' + string(mylat,format='(F7.2)')

       npfts = N_ELEMENTS(where(lpref GT 0))
       tvalue = 0.0
       strings = strarr(npfts + 1)
       FOR i=0,npfts-1 DO BEGIN
            tpref = where(pftRefs EQ lpref(i))
            tvalue += ftempdata(0,i) * larea(i)
            strings(i+1) = pftNames(tpref) + '  ' $
                        + string(ftempdata(0,i),format='(f8.2)') $
                        + '   ' + string(larea(i),format='(f6.2)')
        ENDFOR
        string5 = ''
        string6 = 'PFT      Value      Area'
        strings(0) = pftNames(0) + '  ' $
                     + string(tvalue,format='(f8.2)') + '   ' $
                     + string(1.0,format='(f6.2)')

        IF (tcomment2 NE '') THEN BEGIN
            message = [string1,string2,string3,tcomment, $
                       tcomment2,string5,string6,strings]
        ENDIF ELSE $
            message = [string1,string2,string3,tcomment, $
                       string5,string6,strings]
        EZ4_MESSAGE, message
        RETURN

   ENDIF

   ;Map
   IF ((dsize(1) EQ nsib) AND $
       (dsize(2) EQ nlu) AND $
       (mapsib EQ 1)) THEN BEGIN
       gtempdata = fltarr(nsib)
       FOR i=0,nsib-1 DO BEGIN
           ltempdata = REFORM(ftempdata(i,*))
           ltemparea = REFORM(larea(i,*))
           ltemppft  = REFORM(lpref(i,*))
           sib4_getpft, missing, refpfta, mypft, $
                 ltempdata, ltemparea, ltemppft, $
                 sib4value
           gtempdata(i) = sib4value
       ENDFOR
       plottype = 2

   ENDIF

ENDIF ;2-D Variables
   
;;;-----3-DIMENSION VARIABLES------
IF (ndims EQ 3) THEN BEGIN
   IF ((dsize(1) EQ 1) AND $
       (dsize(2) EQ nlu) AND $
       (dsize(3) EQ 5)) THEN BEGIN

       IF (varunits EQ '') THEN BEGIN
           string1 = STRUPCASE(varname)
      ENDIF ELSE BEGIN
           string1 = STRUPCASE(varname) $
                 + ' (' + varunits + ')'
       ENDELSE
       string2 = 'Site ' $
                 + string(refpt+1,format=myformat)
       mylon = sibinfo.lonsib(refpt)
       mylat = sibinfo.latsib(refpt)
       string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
               + '   Lat: ' + string(mylat,format='(F7.2)')
       string4 = 'PFT: ' + pftNames(refpft)
       string5 = ''

       stringsnow = strarr(5)
       mydata = fltarr(5)
       IF (refpft EQ 0) THEN BEGIN
           FOR k=0,4 DO BEGIN
               FOR l=0,nlu-1 DO BEGIN
                    mydata(k) += ftempdata(0,l,k)*larea(l)
                 ENDFOR
            ENDFOR
       ENDIF ELSE BEGIN
           mydata(*) = ftempdata(0,refpft,*)
       ENDELSE

       snowlayer=-5               
       FOR k=0,4 DO BEGIN
           stringsnow(k) = 'Value: ' + string(mydata(k),format='(F10.3)') + $
                           '    Snow Layer: ' + string(snowlayer,format='(I4)')
           snowlayer++
       ENDFOR

       message = [string1,string2,string3, $
                  string4,string5,stringsnow]
       EZ4_Message,message

       RETURN
   ENDIF

   IF ((dsize(1) EQ nsib) AND $
       (dsize(2) EQ nlu) AND $
       (dsize(3) EQ ntime)) THEN BEGIN

       IF (mapsib EQ 0) THEN BEGIN ;Site
          data2d = fltarr(nlu,ntime)
          IF (nsib EQ 1) THEN BEGIN
             data2d(*,*) = ftempdata(0,*,*)
             tlarea = larea
             tlpref = lpref
          ENDIF ELSE BEGIN
             data2d(*,*) = ftempdata(refpt,*,*)
             tlarea = REFORM(larea(refpt,*))
             tlpref = REFORM(lpref(refpt,*))
          ENDELSE

          nx = 1
          IF (refpftt EQ 0) THEN BEGIN
              pref = pftRefs(refpft)
              showlabel = 0
              sib4_getpftt, missing, refpfta, pref, data2d, $
                  tlarea, tlpref, values
          ENDIF ELSE BEGIN
              pref = where(tlpref GT 1)
              showlabel = 1
              IF (pref[0] GT -1) THEN nx+=n_elements(pref)
              sib4_getpftt, missing, refpfta, 0, data2d, $
                  tlarea, tlpref, values
          ENDELSE    

          IF (values[0] EQ missing) THEN BEGIN
              EZ4_Alert, 'Specified PFT Not Simulated.'
              RETURN
          ENDIF 

          IF (reftime GE 0) THEN BEGIN
               value = values(reftime)
               string1 = STRUPCASE(varname) $
                          + ' (' + varunits + ')'
               string2 = 'Site ' $
                          + string(refpt+1,format=myformat)
               mylon = sibinfo.lonsib(refpt)
               mylat = sibinfo.latsib(refpt)
               string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                          + '   Lat: ' + string(mylat,format='(F7.2)')

               string5 = ''
               string6 = 'Value For PFT ' + pftNames(refpft) + ': '
               myformat = ez4_niceformat(value)
               string7 = string(value,format=myformat)
            
              IF (tcomment2 NE '') THEN BEGIN
                  message=[string1,string2,string3,tcomment, $
                           tcomment2, string5,string6,string7]
              ENDIF ELSE $
                  message = [string1,string2,string3,tcomment, $
                            string5,string6,string7]
              EZ4_Message,message
              RETURN
            ENDIF ELSE $
            IF (reftime EQ -1) THEN BEGIN ;mean value
               value = mean(values)
               string1 = STRUPCASE(varname) + ' (' + varunits + ')'
               string2 = 'Site ' $
                          + string(refpt+1,format=myformat)
               mylon = sibinfo.lonsib(refpt)
               mylat = sibinfo.latsib(refpt)
               string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                          + '   Lat: ' + string(mylat,format='(F7.2)')

               string5 = ''
               string6 = 'Value For PFT ' + pftNames(refpft) + ': '
               myformat = ez4_niceformat(value)
               string7 = string(value,format=myformat)
            
               IF (tcomment2 NE '') THEN BEGIN
                   message=[string1,string2,string3,tcomment, $
                            tcomment2,string5,string6]
               ENDIF ELSE $
                   message=[string1,string2,string3,tcomment, $
                            string5,string6]
               EZ4_Message,message
               RETURN
            ENDIF ELSE $
            IF (reftime EQ -2) THEN BEGIN ;timeseries
               xdata = fltarr(nx,ntime)
               ydata = fltarr(nx,ntime)
               linecolor = fltarr(nx)
               linestyle = fltarr(nx)
               linename = strarr(nx)
               drawLines = fltarr(nx)

               xdata(0,*) = sibinfo.time
               ydata(0,*) = values(*)
               showLineLabel = showlabel 
               linename(0) = STRUPCASE(varname)  ;mypftName
               drawLines(*) = 1
               FOR i=1,nx-1 DO BEGIN
                   pref = tlpref(i-1)
                   pnum = where(pftRefs eq pref)
                   xdata(i,*) = sibinfo.time
                   ydata(i,*) = data2d(i-1,*)
                   linecolor(i) = i
                   linename(i) = pftNames(pnum)
               ENDFOR
               plottype = 1

            ENDIF ELSE $
            IF (reftime EQ -3) THEN BEGIN ;time total
               value = total(values)
               string1 = STRUPCASE(varname) + ' (' + varunits + ')'
               string2 = 'Site ' $
                          + string(refpt+1,format=myformat)
               mylon = sibinfo.lonsib(refpt)
               mylat = sibinfo.latsib(refpt)
               string3 = 'Lon: ' + string(mylon,format='(F7.2)') $
                          + '   Lat: ' + string(mylat,format='(F7.2)')

               string5 = ''
               string6 = 'Value For PFT ' + pftNames(refpft) + ': '
               myformat = ez4_niceformat(value)
               string7 = string(value,format=myformat)
            
               IF (tcomment2 NE '') THEN BEGIN
                   message=[string1,string2,string3,tcomment, $
                            tcomment2,string5,string6]
               ENDIF ELSE $
                   message=[string1,string2,string3,tcomment, $
                            string5,string6]
               EZ4_Message,message
               RETURN
            ENDIF ELSE BEGIN
                EZ4_Alert,'Error With Site Data',$
                          'Unknown Time References.'
                RETURN
             ENDELSE

       ENDIF ELSE BEGIN ;Map

            gtempdata = ftempdata(*,0,0)
            plottype = 0

       ENDELSE
   ENDIF
ENDIF ;3-D Variables 

;;;Plot appropriate plot type
CASE plottype OF
     1: BEGIN ;Line Plots
        ;...create line plot structure
        Line_Struct_Data, sibplot, $
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
        Line_Struct_Window, sibwindow, windowTitle=windowTitle

        ;...combine information into single structure
        plot = CREATE_STRUCT(sibplot, sibwindow)

        ;...plot the data
        Line_Plot, plot

     END

     2: BEGIN ;Maps
        ;...grid sib4 data
        sib4_griddata, nsib, gtempdata, $
            sibinfo.nlon, sibinfo.nlat, $
            sibinfo.lonref, sibinfo.latref, data

        ;...create gridded plot structure
        geo4_struct_data, sibplot, $
            data=data, $
            comment=comment, title=title, $
            fileName=fileName, simlabel=simLabel, $
            cbartitle=units, whiteOut='low', $
            nlon=sibinfo.nlon, nlat=sibinfo.nlat, $
            lon=sibinfo.lon, lat=sibinfo.lat, $
            units=varunits

        ;...create a window structure
        GEO4_Struct_Window, sibwindow, windowTitle=windowTitle

        ;...combine information into single structure
        plot = CREATE_STRUCT(sibplot, sibwindow)

        ;...plot the data
        GEO4_Plot, plot

        RETURN
     END

     ELSE: BEGIN
       string1 = 'Unknown SiB4 Plot Type.'
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
