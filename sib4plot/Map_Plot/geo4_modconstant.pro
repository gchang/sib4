;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Modify data from plot with constant using the requested operation
;;  (add, divide, ..)
;;
PRO GEO4_ModConstant, oldPlot, operation

@ez4_BasicParams.com

plot = oldPlot
data = plot.data

;; Define the operation to be performed
CASE operation OF
  'add':      BEGIN & operator = ' + ' & extra = ' (Addition)' & 
                      prompt = 'Add a scalar to all values'
              END
  'subtract': BEGIN & operator = ' - ' & extra = ' (Difference)' & 
                      prompt = 'Subtract from all values'
              END
  'multiply': BEGIN & operator = ' * ' & extra = ' (Multiplied)' & 
                      prompt = 'Multiply all values by'
              END
  'divide':   BEGIN & operator = ' / ' & extra = ' (Divided)' & 
                      prompt = 'Divide all values by'
              END
ENDCASE

;;Get a constant and return if cancelled
ok = EZ4_GetField(constant, VALUE=1.0, /FLOATING, PROMPT=prompt, $
                   TITLE='Constant Modify', XSIZE=10)
IF (ok EQ 0) THEN RETURN

;; Cannot divide by zero
IF (operation EQ 'divide') AND (ABS(constant) LE 1.e-15) THEN BEGIN
   EZ4_Alert, 'Cannot divide by a very small number or zero!'
   RETURN
ENDIF

goodref = WHERE(data NE missing)
a = EXECUTE('data(goodref) = data(goodref) ' + operator + STRING(constant))

mindata = plot.mindata
maxdata = plot.maxdata
meanval = plot.meanValue

a = EXECUTE('mindata = mindata ' + operator + STRING(constant))
a = EXECUTE('maxdata = maxdata ' + operator + STRING(constant))
a = EXECUTE('meanval = meanval ' + operator + STRING(constant))

;; Put modified data into plot
plot.data = data
plot.mindata = mindata
plot.maxdata = maxdata
plot.meanValue = meanval

nContours=coltabNColors(plot.colorChoice)-1
GEO4_Set_Contours, nContours, plot.mindata, plot.maxdata, meanValue, dcontours
plot.contours = [missing+1, dcontours]
plot.minContour = dcontours(0)
plot.contourInterval = (dcontours(3)-dcontours(2))
plot.fillColors = GEO4_Set_FillColors(plot.colorChoice, plot.whiteOut)

contour_range=(MAX(dcontours)-MIN(dcontours))
plot.cbarLFormat=EZ4_NiceFormat(contour_range)

varname = EZ4_GetVarName(plot.title[0])
constantstring = STRTRIM(STRING(constant, FORMAT='(F8.2)'))
plot.title = varname + '  (' + operator + constantstring + ')'
plot.windowTitle = plot.title

GEO4_PLOT, plot

END

