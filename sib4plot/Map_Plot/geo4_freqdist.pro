PRO Geo4_FreqDist, plot

COMMON Missing_COM

WIDGET_CONTROL, /HOURGLASS

;Save all data in vector form
ntot = FLOAT(plot.nlon) * FLOAT(plot.nlat)
alldata = FLTARR(ntot) + missing

;Get the bins
nbins=18.
interval=(plot.maxdata-plot.mindata)/nbins
binval = FINDGEN(nbins+1.)*interval + plot.mindata
binnum = FLTARR(nbins)

;Put data into bins
count=0L
FOR i=0,plot.nlon-1 DO BEGIN
   FOR j=0,plot.nlat-1 DO BEGIN
       dataval = plot.data(i,j)
       IF ((dataval GT missing) AND $
           (plot.lon(i) GE plot.limits[1]) AND $
           (plot.lon(i) LE plot.limits[3]) AND $
           (plot.lat(j) GE plot.limits[0]) AND $
           (plot.lat(j) LE plot.limits[2])) THEN BEGIN
           alldata(count) = dataval
           count++

           aref = (where(binval GT dataval))[0] - 1.
           IF (aref GE 0) THEN binnum(aref)++ ELSE $
           IF (binval[0] EQ dataval) THEN binnum(0)++ ELSE $
           IF (binval[nbins] LE dataval) THEN binnum(nbins-1)++ $
           ELSE BEGIN
                string1 = 'Lat: ' $
                       + string(plot.lon(i),format='(f7.2)') $
                       + '  Lon: ' $
                       + string(plot.lat(i),format='(f6.2)')

                myformat = ez4_niceformat(dataval,/FLT) 
                string2 = 'Value: ' + string(dataval,format=myformat)
 
                string3 = 'Min Bin: ' + string(min(binval),format=myformat) $
                        + '  Max Bin: ' + string(max(binval),format=myformat) 
                EZ4_Alert,['Error Computing Frequency Distribution.', $
                           string1,string2,string3]
                RETURN
           ENDELSE
      ENDIF
  ENDFOR
ENDFOR


;Get data in correct format
numplots = 1 

goodref = where(alldata GT missing)
IF (goodref[0] EQ -1) THEN BEGIN
   EZ4_Alert,'No Valid Data In Distribution.'
   RETURN
ENDIF

;check the number of points in the distribution;
norig = n_elements(goodref)
nbinnum = TOTAL(binnum)
IF (norig NE nbinnum) THEN BEGIN
   myformat = ez4_niceformat(norig,/INT)
   string1 = 'Original Number Of Data Points: ' $
             + string(norig,format=myformat)
   string2 = 'Frequency Distribution Number:  ' $
             + string(nbinnum,format=myformat)
   ez4_Alert,['Error Calculating Frequency Distribution.', $
              string1,string2]
   RETURN
ENDIF

origdata = FLTARR(numplots, norig)
origdata(0,*) = alldata(goodref)

ydata = FLTARR(numplots, nbins)
ydata(0,*) = binnum(*)

varname = EZ4_GetVarName(plot.title)
title = varname + ': Frequency Distribution'
xtitle = varname
IF ((plot.units NE '') AND (plot.units NE '-')) THEN $
    xtitle += ' (' + plot.units + ')'
ytitle= 'Number'

linename = strarr(numplots)
linename(0) = varname

;Put data into proper structure
hperror=0
Hist_Struct_Data, plotnew, hperror, $
   origdata=origdata, $
   xdata=binval, ydata=ydata, $
   numbins=numdata, numplots=numplots, $
   units=plot.units, $
   title=title, xtitle=xtitle, ytitle=ytitle, $
   comment=plot.comment, filename=plot.filename, $
   simlabel=plot.simlabel, linename=linename
IF (hperror EQ 1) THEN RETURN

Hist_Struct_Window, histwindow, $
    windowTitle=title

histplot = CREATE_STRUCT(plotnew, histwindow)

;Create histogram plot
Hist_Plot, histplot

END
