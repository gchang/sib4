;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Create new data from plot and plot2 using the requested operation
;;  (add, divide, ..)
;;
PRO Line_ModOther, oldPlot, plot2, operation

COMMON Missing_COM

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
num1x=size(plot.xdata,/n_elements)
num2x=size(plot2.xdata,/n_elements)

IF (num1x NE num2x) THEN BEGIN
   EZ4_ALERT,['Error in Line_ModOther', $
              'Expecting same number of data points.']
   RETURN
ENDIF

data  = plot.ydata
data2 = plot2.ydata

;; If division, check for divide by zero
;; and perform operation on non-zero points
IF (operation EQ 'divide') THEN BEGIN
  goodData = WHERE((data2 NE 0) AND $
                   (data2 NE missing) AND $
                   (data NE missing))
ENDIF ELSE BEGIN
  goodData = WHERE((data2 NE missing) AND $
                   (data NE missing))
ENDELSE

IF (goodData[0] GE 0) THEN BEGIN
     a = EXECUTE('data[goodData] = data[goodData]' $
           + operator + 'data2[goodData]')
ENDIF ELSE BEGIN
     EZ4_ALERT,['Error in Line_ModOther:', $
                'Cannot Modify All Bad/Missing Data.']
     RETURN
ENDELSE


;; Put modified data into plot
plot.ydata = data
plot.miny = MIN(data[goodData])
plot.maxy = MAX(data[goodData])
plot.meanValue = mean(data[goodData])

mmdiff = (plot.maxy-plot.miny)
IF ((mmdiff LT 1.E-4) AND $
    (abs(plot.meanValue) GT 0.1)) THEN BEGIN
    mmval = (plot.maxy + plot.miny)*0.5
    plot.miny = mmval
    plot.maxy = mmval
ENDIF

;; Change text
varname1 = EZ4_GetVarName(plot.title[0])
varname2 = EZ4_GetVarName(plot2.title[0])

IF ((operation EQ 'multiply') OR $
    (operation EQ 'divide')) THEN BEGIN
    space = STRPOS(varname1, ' ')
    IF (space GE 0) THEN varname1 = '(' + varname1 + ')'
 ENDIF
plot.title = varname1 + operator + varname2

IF (plot.title EQ plot2.title) THEN BEGIN
   plot.windowTitle = varname1 + ' (' + operator + ')'
ENDIF ELSE BEGIN
   plot.title = varname1 + operator + varname2
   plot.windowTitle = plot.title
ENDELSE

tcomment1 = plot.comment
tcomment2 = plot2.comment
IF (tcomment1 NE tcomment2) THEN BEGIN
   plot.comment = ''
   comment1 = strsplit(tcomment1,',',/extract)
   comment2 = strsplit(tcomment2,',',/extract)
   IF ((N_ELEMENTS(comment1) GT 1) AND $
       (N_ELEMENTS(comment2) GT 1)) THEN BEGIN
       IF (comment1[0] EQ comment2[0]) THEN plot.comment=comment1[0]
       IF (comment1[1] EQ comment2[1]) THEN plot.comment=comment1[1]
   ENDIF
ENDIF

Line_Plot, plot

END

