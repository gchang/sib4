;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Create new data from plot and plot2 using the requested operation
;;  (add, divide, ..)
;;
PRO GEO4_ModOther, oldPlot, plot2, operation

@ez4_BasicParams.com

;; Return if plot2 is empty
IF N_ELEMENTS(plot2) EQ 0 THEN BEGIN
   EZ4_Alert,['No Other PLot Specified.', $
              'Please Copy A Plot For This Operation.']
   RETURN
ENDIF

plot = oldPlot

;; Check for correct data types
p1type = SIZE(plot,/TYPE)
p2type = SIZE(plot2,/TYPE)

IF (p1type NE p2type) THEN BEGIN
   EZ4_Alert, 'Incompatable Plots.'
   RETURN
ENDIF

IF ((plot.nlon NE plot2.nlon) OR (plot.nlat NE plot2.nlat)) THEN BEGIN
   EZ4_ALERT,['Error in GEO4_ModOther', $
              'Expecting same lat/lon.']
   RETURN
ENDIF

;; Define the operation to be performed
CASE operation OF
  'add':      BEGIN & operator = ' + ' & extra = ' (Addition)' & END
  'subtract': BEGIN & operator = ' - ' & extra = ' (Difference)' & END
  'multiply': BEGIN & operator = ' * ' & extra = ' (Multiplied)' & END
  'divide':   BEGIN & operator = ' / ' & extra = ' (Divided)' & END
ENDCASE

;; Set up arrays
data  = plot.data
data2 = plot2.data

badref = WHERE(data LE missing)
goodref = WHERE(data GT missing)

;; If division, check for divide by zero
;; and perform operation on non-zero points
IF (operation EQ 'divide') THEN BEGIN
    GoodDataRef = WHERE((data2 NE 0) AND $
                        (data2 NE missing) AND $
                        (data NE missing))
ENDIF ELSE BEGIN
    GoodDataRef = WHERE((data2 NE missing) AND $
                        (data NE missing))
ENDELSE

IF (GoodDataRef[0] GE 0) THEN BEGIN
     a = EXECUTE('data[goodDataRef] = data[goodDataRef]'+operator+'data2[goodDataRef]')
ENDIF ELSE BEGIN
     EZ4_ALERT,['Error in GEO4_ModOther:', $
                'Cannot Modify All Bad/Missing Data.']
     RETURN
ENDELSE

mindata = MIN(data[GoodDataRef])
maxdata = MAX(data[GoodDataRef])
meanval = MEAN(data[GoodDataRef])

;; Put modified data into plot
plot.data = data
plot.mindata = mindata
plot.maxdata = maxdata
plot.meanValue = meanval

IF (plot.whiteOut NE plot2.whiteOut) THEN plot.whiteOut=''

nContours=coltabNColors(plot.colorChoice)-1
GEO4_Set_Contours, nContours, plot.mindata, plot.maxdata, plot.meanValue, dcontours

plot.ncontours = ncontours
plot.contours = [missing+1, dcontours]
plot.minContour = dcontours(0)
plot.contourInterval = (dcontours(3)-dcontours(2))
plot.fillColors = GEO4_Set_FillColors(plot.colorChoice, plot.whiteOut)

contour_range=(MAX(dcontours)-MIN(dcontours))
plot.cbarLFormat=EZ4_NiceFormat(contour_range)

varname1 = EZ4_GetVarName(plot.title[0])
varname2 = EZ4_GetVarName(plot2.title[0])

IF ((operation EQ 'multiply') OR $
    (operation EQ 'divide')) THEN BEGIN
    space = STRPOS(varname1, ' ')
    IF (space GE 0) THEN varname1 = '(' + varname1 + ')'
ENDIF

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
      IF ((N_ELEMENTS(comment1) GE 2) AND $
          (N_ELEMENTS(comment2) GE 2)) THEN $
          IF (comment1[1] EQ comment2[1]) THEN plot.comment=comment1[1]
   ENDIF
ENDIF

GEO4_PLOT, plot

END

