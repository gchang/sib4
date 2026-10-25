PRO Line_SwitchTime, plot

;set local variables
plref = STRPOS(plot.xtitle,'(')
time = REFORM(plot.xdata(0,*))

;change time according to timeflag
IF (plot.timeflag EQ 4) THEN BEGIN
   IF (plot.timelocal EQ 0) THEN BEGIN
       newtime = EZ4_LocalTime(plot.lon,plot.timeflag,time)
       plot.timelocal = 1
        IF (plref GE 0) THEN $
             plot.xtitle=STRMID(plot.xtitle,0,plref-1) + ' (LST)'

   ENDIF ELSE BEGIN
       newtime = EZ4_LocalTime(plot.lon,plot.timeflag, $
                               time, /GMT)
        plot.timelocal = 0
        IF (plref GE 0) THEN $
             plot.xtitle=STRMID(plot.xtitle,0,plref-1) + ' (GMT)'

   ENDELSE

ENDIF ELSE BEGIN
   EZ4_Alert, 'Not Available Yet'
   RETURN
ENDELSE

;set new time back to plot
sortref = sort(newtime)
FOR l=0,plot.numlevs-1 DO BEGIN
    plot.xdata(l,*) = newtime(sortref)
    plot.ydata(l,*) = plot.ydata(l,sortref)
    ltref = STRPOS(plot.linename(l),' (LST)')
    IF (ltref GE 0) THEN BEGIN
        plot.linename(l)=STRMID(plot.linename(l),0,ltref) + ' (GMT)'
    ENDIF ELSE BEGIN
       ltref = STRPOS(plot.linename(l),' (GMT)')
       IF (ltref GE 0) THEN $
          plot.linename(l)=STRMID(plot.linename(l),0,ltref) + ' (LST)'
   ENDELSE
ENDFOR

plot.minx = min(plot.xdata)
plot.maxx = max(plot.xdata) 
WIDGET_CONTROL, plot.xMinField, SET_VALUE=plot.minx
WIDGET_CONTROL, plot.xMaxField, SET_VALUE=plot.maxx

;draw new plot
Line_DrawPlot, plot

END
