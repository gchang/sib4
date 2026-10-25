PRO GEO4_ZonalMean, plot

COMMON Missing_COM

zonalmean = FLTARR(plot.nlat)
FOR j=0,plot.nlat-1 DO BEGIN
   tempdata = plot.data(*,j)
   gooddata = where(tempdata GT missing)
   zonalmean(j) = MEAN(tempdata(gooddata))
ENDFOR

numdata = plot.nlat
numlevs = 1
xdata = FLTARR(numlevs, numdata)
xdata(0,*) = plot.lat
ydata = FLTARR(numlevs, numdata)
ydata(0,*) = zonalmean(*)
units = plot.units
title = 'Zonal Mean ' + plot.title
ytitle= units
xtitle= 'Latitude'

;Create a line plot
Line_Struct_Data, plotnew, $
   xdata=xdata, ydata=ydata, $
   xtitle=xtitle, ytitle=ytitle, $
   title=title

Line_Struct_Window, windownew, windowTitle=title

plot = CREATE_STRUCT(plotnew, windownew)

;Draw the line plot
Line_Plot, plot


END
