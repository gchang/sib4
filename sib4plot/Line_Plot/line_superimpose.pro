;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Superimpose a line onto an existing line plot
;;
PRO Line_Superimpose, plot1, plot2, nmaxColors=nmaxColors

;; If second plot is not provided, ask for a file to read
IF (N_ELEMENTS(plot2) EQ 0) THEN BEGIN
    EZ4_Alert,['Please copy a plot.']
    RETURN
ENDIF

;; IF no specific maximum number of colors is provided,
;;   set to generic value
IF (N_ELEMENTS(nmaxColors) EQ 0) THEN nmaxColors=24

;; Set local variables
showLineLabel = 1
showmean = -1
meanValue=-999
meanFormat='(I4)'
xtitle=plot1.xtitle

IF (plot1.timeflag EQ plot2.timeflag) THEN $
    timeflag=plot1.timeflag $
ELSE BEGIN
    string1='Error With Super Imposing:'
    string2='Expecting Same Time Units.'
    EZ4_Alert,[string1,string2]
    RETURN
ENDELSE

timeperday=plot1.timeperday
simlabel=plot1.simlabel

IF (plot1.lon EQ plot2.lon) THEN lon=plot1.lon $
ELSE lon=-1

IF (plot1.filename[0] EQ plot2.filename[0]) THEN BEGIN
   filename = plot1.filename[0]
ENDIF ELSE filename=''


;; Set a new title
title1 = ez4_getvarname(plot1.title)
title2 = ez4_getvarname(plot2.title)
title = title1
IF (title1 NE title2) THEN BEGIN
   IF (STRPOS(title1,title2) EQ -1) THEN $
       title = title1 + ' and ' + title2
ENDIF

;; Set comments
IF (plot1.comment EQ plot2.comment) THEN comment=plot1.comment $
ELSE comment = ''

;; Set a new window title
windowTitle = 'SuperImposed ' + title

;; Set the x data
time1 = plot1.xdata
time2 = plot2.xdata

;; Set the y data
data1 = plot1.ydata
data2 = plot2.ydata

num1levs = plot1.numlevs
num2levs = plot2.numlevs
numlevs = num1levs+num2levs

num1recs = plot1.numdata
num2recs = plot2.numdata
numrecs = MAX([num1recs,num2recs])

;; Figure out y range - we assume the x-axis is the same
minx = MIN([plot1.minx,plot2.minx])
maxx = MAX([plot1.maxx,plot2.maxx])

miny = MIN([plot1.miny,plot2.miny])
maxy = MAX([plot1.maxy,plot2.maxy])

;; Set units
IF (plot1.units EQ plot2.units) THEN units=plot1.units
ytitle=plot1.ytitle
IF (ytitle NE plot2.ytitle) THEN BEGIN
   IF ((ytitle EQ '') OR (ytitle EQ '-') AND $
       (plot2.ytitle EQ '') OR (plot2.ytitle EQ '-')) THEN BEGIN
        ytitle='-'
   ENDIF ELSE BEGIN
      IF (STRPOS(ytitle,plot2.ytitle) EQ -1) THEN $
           ytitle=ytitle + ' and ' + plot2.ytitle
   ENDELSE
ENDIF

;; Combine data and setup new information
ydata = FLTARR(numlevs,numrecs)
xdata = FLTARR(numlevs,numrecs)
linecolor = FLTARR(numlevs)
linestyle = FLTARR(numlevs)
linethick = FLTARR(numlevs)
drawlines = FLTARR(numlevs)
linename = STRARR(numlevs)

ref=0
FOR i=0,plot1.numlevs-1 DO BEGIN
     ydata(ref,0:num1recs-1) = data1(i,*)
     xdata(ref,0:num1recs-1) = time1(i,*)  
     IF (num1recs lt numrecs) THEN BEGIN
        ydata(ref,num1recs:numrecs-1) = data1(i,num1recs-1)
        xdata(ref,num1recs:numrecs-1) = xdata(ref,num1recs-1)
     ENDIF
     linecolor(ref) = ref
     linename(ref) = plot1.linename(i)
     linestyle(ref) = plot1.linestyle(i)   
     linethick(ref) = plot1.linethick(i)
     drawlines(ref) = plot1.drawlines(i)

     ref++
ENDFOR

FOR i=0,plot2.numlevs-1 DO BEGIN
     ydata(ref,0:num2recs-1) = data2(i,*)
     xdata(ref,0:num2recs-1) = time2(i,*)
     IF (num2recs lt numrecs) THEN BEGIN
        ydata(ref,num2recs:numrecs-1) = ydata(ref,num2recs-1)
        xdata(ref,num2recs:numrecs-1) = xdata(ref,num2recs-1)
     ENDIF
     linecolor(ref) = ref
     linename(ref)  = plot2.linename(i)
     linestyle(ref) = plot2.linestyle(i)   
     linethick(ref) = plot2.linethick(i)
     drawlines(ref) = plot2.drawlines(i)

     ref++
ENDFOR

IF (ref GE nmaxColors) THEN BEGIN
   mystring = 'Maximum Colors Available: ' $
               + string(nmaxColors,format='(I3)')
   EZ4_ALERT, ['Superimpose Error - Too Many Colors', $
               mystring]
   RETURN
ENDIF

minData=MIN(ydata)
maxData=MAX(ydata)

;; Create a new plot structures
Line_Struct_Data, sibplot, $
     xdata=xdata, ydata=ydata, $
     numdata=numrecs, numlevs=numlevs, $
     minx=minx, maxx=maxx, miny=miny, maxy=maxy, $
     xtitle=xtitle, ytitle=ytitle, $
     timeflag=timeflag, timeperday=timeperday, $
     linename=linename, showlinelabel=showlinelabel, $
     linecolor=linecolor, linestyle=linestyle, $
     linethick=linethick, drawLines=drawLines, $
     units=units, comment=comment, title=title, $
     fileName=fileName, simlabel=simlabel, $
     lon=lon, showMean=showMean

windowTitle = title
Line_Struct_Window, sibwindow, windowTitle=windowTitle

;...combine information into single structure
plot = CREATE_STRUCT(sibplot, sibwindow)

;...plot the data
Line_Plot, plot


END
