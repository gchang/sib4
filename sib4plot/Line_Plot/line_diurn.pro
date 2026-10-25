;===================================================
PRO Line_Diurn, plot, GROUP_LEADER=GROUP_LEADER
;===================================================
; compute diurnal cycle values

;;Time Flags:
;;;    0= hours
;;;    1= days
;;;    2= months
;;;    3= years
;;;    4= diurnal composite

WIDGET_CONTROL, /HOURGLASS

badval=-900.

;Set local variables
varname = EZ4_GetVarName(plot.title)
title = varname + ' Diurnal Composite'
linename = varname + ' (GMT)'
xtitle = 'Hour (GMT)'
timeflag=4

;Process the data
IF ((plot.timeperday mod 24) EQ 0) THEN BEGIN
    time = plot.xdata
    dt = time(3) - time(2)
    ncomp = plot.timeperday
    minrec = ROUND((plot.minx-plot.savminx)/dt)
    maxrec = ROUND((plot.maxx-plot.savminx)/dt)
ENDIF ELSE BEGIN
    string1 = 'Timesteps Per Day: ' $
               + string(plot.timeperday,format='(I6)')
    string2 = 'Only Hourly Works For Sure.'
    string3 = 'Please Check/Modify Line_Diurn.'
    EZ4_Alert,[string1,string2,string3]
ENDELSE

; compute the diurnal cycle and set the x data
newdata = FLTARR(plot.numlevs,ncomp)
ngood = INTARR(ncomp)
FOR i=0,plot.numlevs-1 DO BEGIN
    numdays = FLOOR((maxrec(i)-minrec(i))/ncomp)
    newnumrec = ncomp * numdays
    count = LONG(minrec(i))
    ngood(*) = 0

    ; compute the diurnal cycle
    FOR day = 0L, numdays-1 DO BEGIN
        FOR hr = 0, ncomp-1 DO BEGIN
            IF (plot.ydata(i,count) GT badval) THEN BEGIN
               newdata(i,hr) += plot.ydata(i,count)
               ngood(hr) ++
            ENDIF
            count++
        ENDFOR
    ENDFOR

    FOR it = 0, ncomp-1 DO BEGIN
        IF (ngood(it) GT 0) THEN $
             newdata(i,it)/=FLOAT(ngood(it))
    ENDFOR

    ; set the data in order
    xcomp = time[i,minrec(i):minrec(i)+ncomp-1]
    cons=FLOOR(xcomp)
    xcomp -= cons
    xcomp *= 24.
    sortref=sort(xcomp)
    newdata(i,*) = newdata(i,sortref)
ENDFOR

; Create new diurnal hourly values
numdiurn=ncomp/24.
newxcomp = findgen(ncomp)/numdiurn
newdt = newxcomp(1)-newxcomp(0)
newxcomp += newdt*0.5

newtime = fltarr(plot.numlevs,ncomp)
FOR i=0,plot.numlevs-1 DO BEGIN
    newtime(i,*) = newxcomp(*)
ENDFOR

;;;Create a new plot
;...line plot structure
Line_Struct_Data, newsibplot, $
        xdata=newtime, ydata=newdata, $
        xtitle=xtitle, ytitle=plot.ytitle, $
        timeflag=timeflag, timelocal=0, timeperday=24, $
        linecolor=plot.linecolor, linestyle=plot.linestyle, $
        linethick=plot.linethick, drawlines=plot.drawlines, $
        linename=linename, showlinelabel=plot.showlinelabel, $
        lon=plot.lon, units=plot.units, $ 
        title=title, comment=plot.comment, $
        fileName=plot.fileName, simlabel=plot.simlabel

;...window structure
windowtitle=title
Line_Struct_Window, newsibwindow, windowTitle=windowTitle

;...combine into single structure
newplot = CREATE_STRUCT(newsibplot, newsibwindow)

;...plot the data
Line_Plot, newplot

END
