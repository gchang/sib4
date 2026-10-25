;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Modify data from plot with constant using the requested operation 
;;  (add, divide, ..)
;;
PRO Line_ModConstant, oldPlot, operation

@ez4_BasicParams.com

plot = oldPlot

CASE operation OF
  'add':      BEGIN & operator = '+' & comment = 'added '
                      prompt = 'Add To All Values: '
              END
  'subtract': BEGIN & operator = '-' & comment = 'subtracted '
                      prompt = 'Subtract From All Values: '
              END
  'multiply': BEGIN & operator = '*' & comment = 'multiplied by '
                      prompt = 'Multiply All Value By: '
              END
  'divide':   BEGIN & operator = '/' & comment = 'divided by '
                      prompt = 'Divide All Values By: '
              END
ENDCASE

;; Get a constant and return if cancelled
ok = EZ4_GetField(constant, VALUE=1.0, /FLOATING, PROMPT=prompt, $
              TITLE='Constant Modify', XSIZE=10)
IF ok EQ 0 THEN RETURN

;; Cannot divide by zero
IF operation EQ 'divide' AND ABS(constant) LE 1.e-15 THEN BEGIN
  EZ4_ALERT, 'Cannot divide by zero!' & RETURN
ENDIF

modifier = operator + 'constant'
comment = '  (' + comment + STRCOMPRESS(STRING(constant, format='(F6.2)')) + ')'

;; Modify data with the constant
goodref = WHERE(plot.ydata GT missing)
a = EXECUTE('plot.ydata(goodref) = plot.ydata(goodref)'+modifier)
a = EXECUTE('plot.meanValue = plot.meanValue'+modifier)
a = EXECUTE('plot.miny = plot.miny'+modifier)
a = EXECUTE('plot.maxy = plot.maxy'+modifier)

;; Edit the comment, only save latest parenthetical
parenpos = STRPOS(plot.comment,'(')
IF (parenpos GE 1) THEN BEGIN
    plot.comment = STRMID(plot.comment,0,parenpos-1) + comment
ENDIF ELSE plot.comment=comment

plot.savminy = plot.miny
plot.savmaxy = plot.maxy
plot.savminx = plot.minx
plot.savmaxx = plot.maxx

;; Edit the window title
title=plot.title[0]
varname = EZ4_GetVarName(title)
plot.windowTitle = varname + comment

;; Draw the plot
Line_Plot, plot

END
