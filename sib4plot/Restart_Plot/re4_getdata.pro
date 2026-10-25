PRO RE4_GetData, reinfo

@ez4_BasicParams.com

WIDGET_CONTROL, /HOURGLASS

;;Set up basic variables
myaflag = 0
messageformat='(F14.8)'
fileName = reinfo.file

npts = reinfo.nsib
refpt = reinfo.refpt

refpft = reinfo.refpft
mypft=pftRefs(refpft)
mypftname=pftNames(refpft)

reflp = reinfo.refpftpool
refdp = reinfo.reflupool
refsnow = reinfo.refsnow
refsoil = reinfo.refsoil
reftot = reinfo.reftot
refv = reinfo.refvar

simlabel = reinfo.simlabel
varname = (reinfo.vname[refv])[0]
vartitle = reinfo.vtitle[refv]
varunits = reinfo.vunit[refv]
nchars = strlen(varname) + strlen(vartitle)

IF (nchars gt defaultTitleLength) THEN BEGIN
    title = STRUPCASE(varname)
    comment = STRUPCASE(reinfo.vtitle[refv])
ENDIF ELSE BEGIN
   title = STRUPCASE(varname) + ': ' + STRUPCASE(vartitle)
   comment = ''
ENDELSE

IF (refpft GT 0) THEN tcomment = mypftname $
ELSE tcomment = 'All PFTs'

IF (comment EQ '') THEN comment=tcomment $
ELSE comment += ' (' + tcomment + ')'


;;Check the units to see if they need to be changed
EZ4_SetFont
EZ4_SetUnits, varunits, units


;;Get the data
ncid = EZ4_OPENNCDF(fileName)
NCDF_VARGET, ncid, varname, ftempdata
NCDF_VARGET, ncid, 'lu_area', lu_area
NCDF_VARGET, ncid, 'lu_pftref', lu_pftref
NCDF_CLOSE, ncid

;;Change any specialty variables
if (varname EQ 'nsl') then ftempdata=abs(ftempdata)

;;Select plot type based on data size
;;; 1=Line Plot
;;; 2=Gridded Map

plottype = -1
dsize = size(ftempdata)
ndims = dsize(0)

;;;-----SINGLE VALUE VARIABLES-----
IF (ndims EQ 0) THEN BEGIN
   string1='Value of '+ STRUPCASE(varname)
   string2=string(ftempdata,format=messageformat) $
            + ' ' + varunits
   message = [string1,string2]
   EZ4_MESSAGE, message
   RETURN
ENDIF

;;;-----1-DIMENSION VARIABLES------
IF (ndims EQ 1) THEN BEGIN
   nels = dsize(1)
   IF (nels EQ npts) THEN BEGIN
       IF (refpt GE 0) THEN BEGIN ;Site
           string1='Value of '+ STRUPCASE(varname)
           string2=string(ftempdata(refpt),format=messageformat) $
                    + ' ' + varunits
           message = [string1,string2]
           EZ4_MESSAGE, message

           RETURN
        ENDIF ELSE BEGIN ;Map
           gtempdata=ftempdata 
           plottype = 2

        ENDELSE
    ENDIF
ENDIF

;;;-----2-DIMENSION VARIABLES------
IF (ndims EQ 2) THEN BEGIN
   nels = dsize(1)
   nlevs = dsize(2)

   IF (nels EQ npts) THEN BEGIN
      IF (refpt GE 0) THEN BEGIN ;Site
          sib4_getpft, missing, myaflag, mypft, $
                reform(ftempdata(refpt,*)), $
                reform(lu_area(refpt,*)), $
                reform(lu_pftref(refpt,*)), $
                sib4value

          IF (sib4value EQ -999) THEN BEGIN
             string1='Value of ' + STRUPCASE(varname)
             string2='not reported for ' + mypftname + '.'
             message = [string1,string2]
          ENDIF ELSE BEGIN
             string1='Value of '+ STRUPCASE(varname) + ':'
             string2=string(sib4value,format=messageformat) $
                    + ' ' + varunits
             string3='(PFT = ' + mypftname + ')'
             message = [string1,string2,string3]
          ENDELSE
          EZ4_MESSAGE, message

          RETURN
       ENDIF ELSE BEGIN ;Map
          gtempdata = FLTARR(npts)
          FOR i=0,npts-1 DO BEGIN
              ltempdata = ftempdata(i,*)
              ltemparea = lu_area(i,*)
              ltemppft = lu_pftref(i,*)
              sib4_getpft, missing, myaflag, mypft, $
                  reform(ltempdata), $
                  reform(ltemparea), $
                  reform(ltemppft), $
                  sib4value
               gtempdata(i) = sib4value
          ENDFOR
          plottype = 2
       ENDELSE
   ENDIF ;1st Dimension Number of Points
ENDIF ;2-D Restart Variables
   
;;;-----3-DIMENSION VARIABLES------
IF (ndims EQ 3) THEN BEGIN
   nels = dsize(1)
   nlevs = dsize(2)
   nother = dsize(3)

   IF (nels NE npts) THEN BEGIN
       sib4_alert,'Mismatching Points in Array.'
       RETURN
   ENDIF
   IF (nlevs NE nlu) THEN BEGIN
       sib4_alert,'Mismatching Land Units in Array.'
       RETURN
   ENDIF

   ;;;3rd Dimension == npoolcan
   IF (nother EQ npoolcan) THEN BEGIN
       IF (refpt GE 0) THEN BEGIN ;Site
          templeaf = ftempdata(refpt,*,0)
          tempstem = ftempdata(refpt,*,1)
          tempprod = ftempdata(refpt,*,2)

          sib4_getpft, missing, myaflag, mypft, $
                reform(templeaf), $
                reform(lu_area(refpt,*)), $
                reform(lu_pftref(refpt,*)), $
                leafvalue

          sib4_getpft, missing, myaflag, mypft, $
                reform(tempstem), $
                reform(lu_area(refpt,*)), $
                reform(lu_pftref(refpt,*)), $
                stemvalue

          sib4_getpft, missing, myaflag, mypft, $
                reform(tempprod), $
                reform(lu_area(refpt,*)), $
                reform(lu_pftref(refpt,*)), $
                prodvalue

          IF (leafvalue EQ -999) THEN BEGIN
             string1='VALUE of ' + STRUPCASE(varname)
             string2='not reported for ' + mypftname + '.'
             message = [string1,string2]
          ENDIF ELSE BEGIN
             string1=STRUPCASE(poolcanNames(1)) + $
                  ' '+ STRUPCASE(varname) + ':' $
                  + string(leafvalue,format=messageformat) $
                  + ' ' + varunits
             string2=STRUPCASE(poolcanNames(2)) + $
                  ' '+ STRUPCASE(varname) + ':' $
                  + string(stemvalue,format=messageformat) $
                  + ' ' + varunits
             string3=STRUPCASE(poolcanNames(3)) + $
                  ' '+ STRUPCASE(varname) + ':' $
                  + string(prodvalue,format=messageformat) $
                  + ' ' + varunits
             string4='(PFT = ' + mypftname + ')'
             message = [string1,string2,string3,string4]
          ENDELSE

          EZ4_MESSAGE, message
          RETURN
       ENDIF ELSE BEGIN ;Map
          EZ4_MESSAGE, 'Working on Grazing. MultiPlot Required.'
          RETURN
       ENDELSE
    ENDIF ;3rd Dim == npoolcan

   ;;;3rd Dimension == nsnow
   ;IF (nother EQ nsnow) THEN BEGIN
   IF (varname EQ 'dzsnow') THEN BEGIN
       IF (refpt GE 0) THEN BEGIN ;Site
          snowvalue = fltarr(nsnow)         

          FOR i=0,nsnow-1 DO BEGIN
               tempsnow = ftempdata(refpt,*,i)
               sib4_getpft, missing, myaflag, mypft, $
                   reform(tempsnow), $
                   reform(lu_area(refpt,*)), $
                   reform(lu_pftref(refpt,*)), $
                   tempvalue
                snowvalue(i) = tempvalue
          ENDFOR

          IF (snowvalue(0) EQ -999) THEN BEGIN
             string1='VALUE of ' + STRUPCASE(varname)
             string2='not reported for ' + mypftname + '.'
             message = [string1,string2]
          ENDIF ELSE BEGIN
             snowstring = STRARR(nsnow)
             layerval = 0
             FOR i=nsnow-1,0,-1 DO BEGIN
                 snowstring(i)='Layer' $
                           + string(layerval,format='(i2)') $
                           + ' '+ STRUPCASE(varname) + ':' $
                           + string(snowvalue(i),format=messageformat) $
                           + ' ' + varunits
                 layerval++
             ENDFOR
             stringpft='(PFT = ' + mypftname + ')'
             message = [snowstring,stringpft]
          ENDELSE
          EZ4_MESSAGE, message
          RETURN
       ENDIF ELSE BEGIN ;Map
          ltempdata = FLTARR(nlevs)
          gtempdata = FLTARR(npts)
          IF (refsnow EQ 0) THEN BEGIN
              tcomment = 'All Snow Layers'
          ENDIF ELSE BEGIN
              tcomment='Snow Layer ' + $
                     string(refsnow,format='(I2)')
          ENDELSE
          IF (comment EQ '') THEN comment=tcomment $
          ELSE comment+= ', ' + tcomment

          FOR i=0,npts-1 DO BEGIN
              ltemparea = lu_area(i,*)
              ltemppft = lu_pftref(i,*)
              IF (refsnow EQ 0) THEN BEGIN
                 ltempdata(*) = 0.0
                 FOR j=0,nsnow-1 DO BEGIN
                     ltempdata(*) += ftempdata(i,*,j)
                  ENDFOR
              ENDIF ELSE BEGIN
                 ltempdata(*) = ftempdata(i,*,refsnow-1)
              ENDELSE

              sib4_getpft, missing, myaflag, mypft, $
                  ltempdata, $
                  reform(ltemparea), $
                  reform(ltemppft), $
                  sib4value
               gtempdata(i) = sib4value
          ENDFOR

           plottype = 2

       ENDELSE
    ENDIF ;3rd Dim == nsnow

   ;;;3rd Dimension == ntot
   IF (nother EQ ntot) THEN BEGIN
       IF (refpt GE 0) THEN BEGIN ;Site
          totvalue = fltarr(ntot)         

          FOR i=0,ntot-1 DO BEGIN
               temptot = ftempdata(refpt,*,i)
               sib4_getpft, missing, myaflag, mypft, $
                   reform(temptot), $
                   reform(lu_area(refpt,*)), $
                   reform(lu_pftref(refpt,*)), $
                   tempvalue
                totvalue(i) = tempvalue
          ENDFOR

          IF (totvalue(0) EQ -999) THEN BEGIN
             string1='VALUE of ' + STRUPCASE(varname)
             string2='not reported for ' + mypftname + '.'
             message = [string1,string2]
          ENDIF ELSE BEGIN
             totstring = STRARR(ntot)
             layerval = -nsoil
             FOR i=ntot-1,0,-1 DO BEGIN
                 totstring(i)='Layer ' $
                           + string(layerval,format='(i3)') $
                           + ' '+ STRUPCASE(varname) + ':' $
                           + string(totvalue(i),format=messageformat) $
                           + ' ' + varunits
                 layerval++
             ENDFOR
             stringpft='(PFT = ' + mypftname + ')'
             message = [totstring,stringpft]
          ENDELSE
          EZ4_MESSAGE, message
          RETURN
       ENDIF ELSE BEGIN ;Map
          ltempdata = FLTARR(nlevs)
          gtempdata = FLTARR(npts)
          snowval = [5,4,3,2,1]

          IF (reftot EQ 0) THEN tcomment='All Layers' $
          ELSE BEGIN
              IF (reftot LE nsnow) THEN BEGIN
                  tcomment='Snow Layer ' + $
                        string(snowval(reftot-1),format='(I2)')
              ENDIF ELSE BEGIN
                  tcomment='Soil Layer ' + string(reftot-nsnow,format='(I2)')
              ENDELSE
          ENDELSE 
          IF (comment EQ '') THEN comment=tcomment $
          ELSE comment+=', ' + tcomment

          FOR i=0,npts-1 DO BEGIN
              ltemparea = lu_area(i,*)
              ltemppft = lu_pftref(i,*)
              IF (reftot EQ 0) THEN BEGIN
                 ltempdata(*) = 0.0
                 IF (varname EQ 'td') THEN BEGIN
                     FOR l=0,nlevs-1 DO BEGIN
                         stempdata = ftempdata(i,l,*)
                         sref = where(stempdata GT 1.)
                         ltempdata(l) = mean(stempdata(sref))
                      ENDFOR
                  ENDIF ELSE BEGIN
                      FOR l=0,nlevs-1 DO BEGIN
                          ltempdata(l) = total(ftempdata(i,l,*))
                      ENDFOR
                  ENDELSE     
               ENDIF ELSE BEGIN
                 IF (varname EQ 'td') THEN BEGIN
                    ltempdata(*) = ftempdata(i,*,reftot-1)
                    sref = where(ltempdata LT 1.)
                    ltempdata(sref) = missing
                 ENDIF ELSE BEGIN
                    ltempdata(*) = ftempdata(i,*,reftot-1)
                 ENDELSE
              ENDELSE

              sib4_getpft, missing, myaflag, $
                  mypft, ltempdata, $
                  reform(ltemparea), $
                  reform(ltemppft), $
                  sib4value
               gtempdata(i) = sib4value

          ENDFOR

           plottype = 2

       ENDELSE
    ENDIF ;3rd Dim == ntot

   ;;;3rd Dimension == npoolpft
   IF (nother EQ npoolpft) THEN BEGIN
       IF (refpt GE 0) THEN BEGIN ;Site
          totvalue = fltarr(npoolpft)         

          FOR i=0,npoolpft-1 DO BEGIN
               temptot = ftempdata(refpt,*,i)
               sib4_getpft, missing, myaflag, mypft, $
                   reform(temptot), $
                   reform(lu_area(refpt,*)), $
                   reform(lu_pftref(refpt,*)), $
                   tempvalue
                totvalue(i) = tempvalue
          ENDFOR

          IF (totvalue(0) EQ -999) THEN BEGIN
             string1='VALUE of ' + STRUPCASE(varname)
             string2='not reported for ' + mypftname + '.'
             message = [string1,string2]
          ENDIF ELSE BEGIN
             poolstring = STRARR(npoolpft)
             FOR i=0,npoolpft-1 DO BEGIN
                 poolstring(i)= STRUPCASE(poolpftNames(i+1)) $
                           + ' '+ STRUPCASE(varname) + ':' $
                           + string(totvalue(i),format=messageformat) $
                           + ' ' + varunits
             ENDFOR
             stringpft='(PFT = ' + mypftname + ')'
             message = [poolstring,stringpft]
          ENDELSE
          EZ4_MESSAGE, message
          RETURN
       ENDIF ELSE BEGIN ;Map
          ltempdata = FLTARR(nlevs)
          gtempdata = FLTARR(npts)

          FOR i=0,npts-1 DO BEGIN
              ltemparea = lu_area(i,*)
              ltemppft = lu_pftref(i,*)
              IF (reftot EQ 0) THEN BEGIN
                 ltempdata(*) = 0.0
                 FOR l=0,nlevs-1 DO BEGIN
                     ltempdata(l) = total(ftempdata(i,l,*))
                 ENDFOR
              ENDIF ELSE BEGIN
                 ltempdata(*) = ftempdata(i,*,reftot-1)
              ENDELSE

              sib4_getpft, missing, myaflag, $
                  mypft, ltempdata, $
                  reform(ltemparea), $
                  reform(ltemppft), $
                  sib4value
               gtempdata(i) = sib4value
          ENDFOR
           plottype = 2
       ENDELSE
    ENDIF ;3rd Dim == npoolpft

   ;;;3rd Dimension == npoollu
   IF (nother EQ npoollu) THEN BEGIN
       IF (refpt GE 0) THEN BEGIN ;Site
          totvalue = fltarr(npoollu)         

          FOR i=0,npoollu-1 DO BEGIN
               temptot = ftempdata(refpt,*,i)
               sib4_getpft, missing, myaflag, mypft, $
                   reform(temptot), $
                   reform(lu_area(refpt,*)), $
                   reform(lu_pftref(refpt,*)), $
                   tempvalue
                totvalue(i) = tempvalue
          ENDFOR

          IF (totvalue(0) EQ -999) THEN BEGIN
             string1='VALUE of ' + STRUPCASE(varname)
             string2='not reported for ' + mypftname + '.'
             message = [string1,string2]
          ENDIF ELSE BEGIN
             poolstring = STRARR(npoollu)
             FOR i=0,npoollu-1 DO BEGIN
                 poolstring(i)= STRUPCASE(poolluNames(i+1)) $
                           + ' '+ STRUPCASE(varname) + ':' $
                           + string(totvalue(i),format=messageformat) $
                           + ' ' + varunits
             ENDFOR
             stringpft='(PFT = ' + mypftname + ')'
             message = [poolstring,stringpft]
          ENDELSE
          EZ4_MESSAGE, message
          RETURN
       ENDIF ELSE BEGIN ;Map
          ltempdata = FLTARR(nlevs)
          gtempdata = FLTARR(npts)

          FOR i=0,npts-1 DO BEGIN
              ltemparea = lu_area(i,*)
              ltemppft = lu_pftref(i,*)
              IF (reftot EQ 0) THEN BEGIN
                 ltempdata(*) = 0.0
                 FOR l=0,nlevs-1 DO BEGIN
                     ltempdata(l) = total(ftempdata(i,l,*))
                 ENDFOR
              ENDIF ELSE BEGIN
                 ltempdata(*) = ftempdata(i,*,reftot-1)
              ENDELSE

              sib4_getpft, missing, myaflag, $
                  mypft, ltempdata, $
                  reform(ltemparea), $
                  reform(ltemppft), $
                  sib4value
               gtempdata(i) = sib4value
          ENDFOR
           plottype = 2
       ENDELSE
    ENDIF ;3rd Dim == npoollu

ENDIF ;3-D Restart Variables

;;;-----4-DIMENSION VARIABLES------
IF (ndims EQ 4) THEN BEGIN
   nels = dsize(1)
   nlus = dsize(2)
   nother = dsize(3)
   nlevs = dsize(4)

   IF (nels NE npts) THEN BEGIN
       sib4_alert,'Mismatching Points in Array.'
       RETURN
   ENDIF
   IF (nlus NE nlu) THEN BEGIN
       sib4_alert,'Mismatching Land Units in Array.'
       RETURN
   ENDIF
   IF (nlevs NE nsoil) THEN BEGIN
      sib4_alert,'Mismatching Soil Layers in Array.'
      RETURN
   ENDIF

   npool = -1
   IF (nother EQ npoolpft) THEN BEGIN
       npool = npoolpft
       refpool = reflp
       stringpool = poolpftNames(reflp)
   ENDIF
   IF (nother EQ npoollu) THEN BEGIN
       npool = npoollu
       refpool = refdp
       stringpool = poolluNames(refdp)
   ENDIF
   stringpool += ' Pool'

   IF (refpt GE 0) THEN BEGIN ;Site
      IF (npool GE 1) THEN BEGIN
          temppool = fltarr(nlu)
          temppool2 = fltarr(nlu,npool)
          
          FOR l=0,nlu-1 DO BEGIN
              FOR i=0,npool-1 DO BEGIN
                   IF (refsoil EQ 0) THEN BEGIN
                       temppool2(l,i) = TOTAL(ftempdata(refpt,l,i,*))
                       stringsoil = 'All Layers'
                   ENDIF ELSE BEGIN
                       temppool2(l,i) = ftempdata(refpt,l,i,refsoil-1)
                       stringsoil = 'Layer ' + string(refsoil,format='(i2)')
                   ENDELSE
               ENDFOR ;npool

               IF (refpool EQ 0) THEN BEGIN
                   temppool(l) = TOTAL(temppool2(l,*))
               ENDIF ELSE BEGIN
                   temppool(l) = temppool2(l,refpool-1)
               ENDELSE
          ENDFOR ;l=nlu

           sib4_getpft, missing, myaflag, mypft, temppool, $
               reform(lu_area(refpt,*)), $
               reform(lu_pftref(refpt,*)), $
               poolvalue

           IF (poolvalue EQ -999) THEN BEGIN
               string1='Value of ' + STRUPCASE(varname)
               string2='not reported for ' + mypftname + '.'
               message = [string1,string2]
           ENDIF ELSE BEGIN
               poolstring=STRARR(6)
               poolstring(0)='Value of '+STRUPCASE(varname) + ':'
               poolstring(1)=string(poolvalue,format=messageformat) $
                            + ' ' + varunits
               poolstring(2)=''
               poolstring(3)='Pool = ' + stringpool
               poolstring(4)='Soil = ' + stringsoil
               poolstring(5)='PFT = ' + mypftname
               message = poolstring
          ENDELSE
          EZ4_MESSAGE, message
          RETURN
      ENDIF ;npool >= 1

   ENDIF ELSE BEGIN ;Map
      IF (npool NE -1) THEN BEGIN
          IF (refpool GT 0) THEN title=stringpool
          IF (refsoil EQ 0) THEN tcomment='All Layers' $
          ELSE tcomment='Soil Layer ' + string(refsoil,format='(I2)')
          IF (comment EQ '') THEN comment=tcomment $
          ELSE comment+=', ' + tcomment

          gtempdata = FLTARR(npts)
          ltempdata = FLTARR(nlus)
          ttempdata = FLTARR(nlus,nsoil)

          FOR i=0,npts-1 DO BEGIN
              ltemparea = lu_area(i,*)
              ltemppft = lu_pftref(i,*)

              IF (refpool GE 1) THEN BEGIN
                 ttempdata(*,*) = ftempdata(i,*,refpool-1,*)
              ENDIF ELSE BEGIN
                 ttempdata(*,*) = 0.0
                 FOR l=0, nlus-1 DO BEGIN
                     IF (lu_area(i,l) GT 0.001) THEN BEGIN
                        FOR p=0, npool-1 DO BEGIN
                            ttempdata(l,*) += ftempdata(i,l,p,*)
                        ENDFOR
                     ENDIF
                  ENDFOR
              ENDELSE

              IF (refsoil EQ 0) THEN BEGIN
                  ltempdata(*) = 0.0
                  FOR l=0,nlus-1 DO BEGIN
                      ltempdata(l) = total(ttempdata(l,*))
                  ENDFOR
              ENDIF ELSE BEGIN
                  ltempdata(*) = ttempdata(*,refsoil-1)
              ENDELSE

              sib4_getpft, missing, myaflag, $
                  mypft, ltempdata, $
                  reform(ltemparea), $
                  reform(ltemppft), $
                  sib4value
              gtempdata(i) = sib4value
          ENDFOR
          plottype = 2
      ENDIF ;npool >= 1

   ENDELSE

ENDIF ;4D Variables

;;;Plot appropriate plot type
CASE plottype OF
     2: BEGIN
        ;;Get grid information
        sib4_gridlatlon, gridInfoFile, $
            npts, lonsib, latsib, $
            nlon, nlat, lon, lat, lonref, latref

        sib4_griddata, npts, gtempdata, $
            nlon, nlat, lonref, latref, data

        geo4_struct_data, re4plot, $
            nlon=nlon, nlat=nlat, $
            data=data, lon=lon, lat=lat, $
            cbartitle=units, whiteOut='low', $
            comment=comment, title=title, $
            fileName=fileName, simlabel=simlabel

        ;;Create a window structure
        EZ4_FILEBREAK, filename, FILE=fNameOnly, DIR=fDir
        windowTitle = fNameOnly + ' ' + STRUPCASE(varname)

        GEO4_Struct_Window, re4window, windowTitle=windowTitle

        ;;Combine all the information into a single structure
        plot = CREATE_STRUCT(re4plot, re4window)

        ;;Plot the data
        GEO4_Plot, plot

        RETURN
     END

     ELSE: BEGIN
        ez4_alert,'Unknown Plot Type for Restart Files.'
        RETURN
     END
ENDCASE

END
