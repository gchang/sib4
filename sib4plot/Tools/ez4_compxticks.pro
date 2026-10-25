PRO EZ4_COMPXTICKS, xrmin, xrmax, numxticks, xtv
; xrmin, xrmax   -> min/max time

@ez4_BasicParams.com

xrange = xrmax - xrmin

nIntervals = 0L

IF (xrange LT 0.1) THEN tickint = 0.01 $
ELSE IF (xrange LT 1) THEN tickint = 0.1 $
ELSE IF (xrange LT 4) THEN tickint = 0.5 $
ELSE IF (xrange LT 10) THEN tickint = 1 $
ELSE IF (xrange LT 16) THEN tickint = 2 $
ELSE IF (xrange LT 25) THEN tickint = 4 $
ELSE IF (xrange LT 50) THEN tickint = 5 $
ELSE IF (xrange LT 100) THEN tickint = 10 $
ELSE IF (xrange LT 150) THEN tickint = 15 $
ELSE IF (xrange LT 200) THEN tickint = 20 $
ELSE tickint = 30

nIntervals = ROUND(xrange / tickint)

; Figure out x ticks
xrmax_new = xrmin + nIntervals*tickint
diff = xrmax - xrmax_new
IF (diff GE tickint/2.0) THEN nIntervals = nIntervals+1
numxticks = nIntervals+1
xtv = LONARR(numxticks)
FOR i=0, numxticks-2 DO xtv[i] = xrmin + i*tickint
xtv[numxticks-1] = xrmax

END
