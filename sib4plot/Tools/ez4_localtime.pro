FUNCTION EZ4_LocalTime, lon, timeflag, time, GMT=GMT

;;Time Flags:
;;;    0= hours
;;;    1= days
;;;    2= months
;;;    3= years
;;;    4= diurnal composite

CASE timeflag OF
   0: tmult=1.
   1: tmult=1./24.
   2: tmult=1./(365.*24.)
   3: tmult=0.
   4: tmult=1.
END

timezone=ROUND(lon/15.)
IF (N_ELEMENTS(GMT) NE 0) THEN BEGIN
   newtime = time - timezone*tmult
ENDIF ELSE BEGIN
   newtime = time + timezone*tmult
ENDELSE

IF (MIN(newtime) LT 0) THEN BEGIN
   IF (timeflag EQ 1 OR timeflag EQ 2) THEN newtime(*)+=1.
   IF (timeflag EQ 4) THEN BEGIN
      tref = WHERE(newtime LT 0)
      newtime(tref) = newtime(tref) + 24.
   ENDIF
ENDIF

IF ((MAX(newtime) GT 24) AND (timeflag EQ 4)) THEN BEGIN
   tref = WHERE(newtime GT 24)
   newtime(tref) = newtime(tref) - 24.
ENDIF

RETURN, newtime
END
