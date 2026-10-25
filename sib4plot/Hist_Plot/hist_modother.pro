;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Create new data from plot and plot2 using the requested operation
;;  (add, divide, ..)
;;
PRO Hist_ModOther, oldPlot, plot2, operation

;; If plot2 not given, read it from a file
IF N_ELEMENTS(plot2) EQ 0 THEN BEGIN
   EZ4_Alert,'No Other Plot Specified.'
   RETURN
ENDIF

plot = oldPlot

;; Define the operation to be performed
CASE operation OF
  'add':      BEGIN & operator = ' + ' & extra = ' (Addition)' & END
  'subtract': BEGIN & operator = ' - ' & extra = ' (Difference)' & END
  'multiply': BEGIN & operator = ' * ' & extra = ' (Multiplied)' & END
  'divide':   BEGIN & operator = ' / ' & extra = ' (Divided)' & END
ENDCASE

;; Set up arrays
binval = plot.xdata
data  = plot.ydata

nbins = plot.numbins
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

;; If division, check for divide by zero
;; and perform operation on non-zero points
nZeroData   = 0

IF (operation EQ 'divide') THEN BEGIN
  ZeroData = WHERE(data2 EQ 0, nZeroData)
  goodData = WHERE(data2 NE 0, nGood)

  IF (nGood GT 0) THEN BEGIN
     a = EXECUTE('data[goodData] = data[goodData]' $
           + operator + 'data2[goodData]')
  ENDIF ELSE BEGIN
     EZ4_ALERT,['Error in Line_ModOther', $
                'Cannot Divide By All Zero Data.']
     RETURN
  ENDELSE
ENDIF ELSE BEGIN
  a = EXECUTE('data = data ' + operator + ' data2')
ENDELSE

;; Put modified data into plot
plot.ydata = data

numl=total(plot.drawlines)
datatemp=fltarr(numl,nbins)
count=0
FOR i=0,plot.numplots-1 DO BEGIN
    IF (plot.drawlines(i) GT 0) THEN BEGIN
       datatemp(count,*) = data(i,*)
       count++
    ENDIF
ENDFOR

minyval = min(datatemp)
maxyval = max(datatemp)
plot.miny = minyval
plot.maxy = maxyval

IF (plot.ytitle NE plot2.ytitle) THEN plot.ytitle='Bin'
IF (plot.title NE plot2.title) THEN $
    plot.title = 'Frequency Distribution'
plot.windowTitle = 'Frequency Distribution' $
     + ' (' + operator + ')'

tcomment1 = plot.comment
tcomment2 = plot2.comment
IF (tcomment1 NE tcomment2) THEN BEGIN
   plot.comment = ''
   comment1 = strsplit(tcomment1,',',/extract)
   comment2 = strsplit(tcomment2,',',/extract)

   IF (comment1[0] EQ comment2[0]) THEN plot.comment=comment1[0]
   IF (comment1[1] EQ comment2[1]) THEN plot.comment=comment1[1]
ENDIF

Hist_Plot, plot

END

