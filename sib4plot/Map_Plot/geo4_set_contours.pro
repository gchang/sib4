PRO GEO4_SET_CONTOURS, nContours, dataMin, dataMax, dataMean, contourLevel, $
         Option=Option

IF (N_ELEMENTS(Option) LT 1) THEN Option=0
CASE Option OF
  0:    centralValue = 0.5 * (dataMin + dataMax)
  1:    centralValue = dataMean
  2:    centralValue = 0.0
  ELSE: centralValue = 0.5 * (dataMin + dataMax)
ENDCASE

halfRange = (ABS(dataMax-centralValue) > ABS(dataMin-centralValue))
interval  = halfRange / (0.5 * nContours)
cMin      = centralValue - halfRange - 0.5 * interval

;.....................................................................

IF Option EQ 0 AND interval NE 0 THEN BEGIN
  ; Get a nice contour interval
  magInterval = 10.0 ^ FLOOR(ALOG10(ABS(interval)))
  mantissa    = interval / magInterval
  integerPart = FLOOR(mantissa)
  decimalPart = mantissa - integerPart
  ;print, 'set_contours: mantissa = ',mantissa, integerPart, decimalPart
  IF decimalPart LT 0.1 AND magInterval GT 1. THEN $
    interval = ROUND(interval) $
  ELSE BEGIN  
    interval = magInterval * $
                      (integerPart + EZ4_NiceNumber(decimalPart))
    magInterval     = 10.0 ^ FLOOR(ALOG10(ABS(interval)))
  ENDELSE

  ; Get a nice minimum contour value
  mantissa = cMin / magInterval
  ;magCMin = 10.0 ^ FLOOR(ALOG10(ABS(cMin)))
  cMin = ROUND(mantissa) * magInterval 
ENDIF

;.....................................................................

contourLevel = cMin + (FINDGEN(nContours)+1) * interval


END 
