;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Superimpose a histogram onto an existing hist plot
;;
PRO Hist_Superimpose, plot1, plot2, nmaxColors=nmaxColors

COMMON Colors_COM
COMMON Missing_COM

;; If second plot is not provided, return
IF (N_ELEMENTS(plot2) EQ 0) THEN BEGIN
    EZ4_Alert,['Please copy a plot.']
    RETURN
ENDIF

;; IF no specific maximum number of colors is provided,
;;   set to generic value
IF (N_ELEMENTS(nmaxColors) EQ 0) THEN nmaxColors=specNColors

;; Set local variables
IF (plot1.filename[0] EQ plot2.filename[0]) THEN BEGIN
   filename = plot1.filename[0]
ENDIF ELSE filename=''

showLineLabel = 1
showmean = -1
meanValue=-999
meanFormat='(I4)'
simlabel=plot1.simlabel
xtitle=plot1.xtitle
ytitle=plot1.ytitle

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


;; Check bins and data
binval = plot1.xdata
data1  = plot1.ydata

nbins = plot1.numbins
nbins1 = nbins+1
xequal = TOTAL(binval EQ plot2.xdata)

;redo plot2 bins to match plot
IF (xequal NE nbins1) THEN BEGIN
   odata = plot2.origdata
   ntot = (size(odata))[2]
   data2 = fltarr(plot2.numplots,nbins)
   FOR p=0,plot2.numplots-1 DO BEGIN
       for i=0,ntot-1 DO BEGIN
           dataval = odata(p,i)
           IF (dataval GT missing) THEN BEGIN
               aref = (where(binval GT dataval))[0] - 1.
               IF (aref GE 0) THEN data2(p,aref)++ ELSE $
               IF (binval[0] EQ dataval) THEN data2(p,0)++ ELSE $
               IF (binval[nbins] EQ dataval) THEN data2(p,nbins-1)++
            ENDIF
        ENDFOR
    ENDFOR
ENDIF ELSE data2 = plot2.ydata

;find maximum size of origdata:
osize1 = (size(plot1.origdata))[2]
osize2 = (size(plot2.origdata))[2]
newosize = max([osize1,osize2])


;; Combine data and setup new information
xdata = binval
newnplots = plot1.numplots + plot2.numplots
odata = FLTARR(newnplots,newosize)
ydata = FLTARR(newnplots,nbins)
linecolor = FLTARR(newnplots)
linestyle = FLTARR(newnplots)
linethick = FLTARR(newnplots)
drawlines = FLTARR(newnplots)
linename = STRARR(newnplots)

ref=0
FOR i=0,plot1.numplots-1 DO BEGIN
     odata(ref,0:osize1-1) = plot1.origdata(i,*)
     ydata(ref,0:nbins-1) = data1(i,*)
     linecolor(ref) = ref
     linename(ref) = plot1.linename(i)
     linestyle(ref) = plot1.linestyle(i)   
     linethick(ref) = plot1.linethick(i)
     drawlines(ref) = plot1.drawlines(i)

     ref++
ENDFOR

FOR i=0,plot2.numplots-1 DO BEGIN
     odata(ref,0:osize2-1) = plot2.origdata(i,*)
     ydata(ref,0:nbins-1) = data2(i,*)
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

;; Create a new plot structures
Hist_Struct_Data, sibplot, $
     origdata=odata, $
     xdata=xdata, ydata=ydata, $
     numbins=nbins, numplots=newnplots, $
     units=units, $
     title=title, xtitle=xtitle, ytitle=ytitle, $
     comment=comment, filename=filename, $
     simlabel=simlabel, $
     linename=linename, showlinelabel=showlinelabel, $
     linecolor=linecolor, linestyle=linestyle, $
     linethick=linethick, drawLines=drawLines

windowTitle = title
Hist_Struct_Window, sibwindow, windowTitle=windowTitle

;...combine information into single structure
plot = CREATE_STRUCT(sibplot, sibwindow)

;...plot the data
Hist_Plot, plot


END
