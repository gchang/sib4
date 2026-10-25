;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Return a string array of the formatted 'data' (lon/lat) formatted to
;; have only one decimal digit
;;
FUNCTION EZ4_FormatLonLat, data, lon=lon, lat=lat

; Determine number of integer digits plus the minus sign (if negative)
np      = N_ELEMENTS(data)
mx      = MAX(data, MIN=mn)
range   = mx - mn
IF (range GT np) THEN format = '(I4)' ELSE $
IF (range GT 1) THEN format = '(F5.1)' ELSE $
IF (range GT 0.1) THEN format = '(F6.2)' ELSE format='(F7.3)'
labels  = STRTRIM(STRING(data, FORMAT=format),2)

;;Add on N/S if requested
IF (N_ELEMENTS(lat) NE 0) THEN BEGIN
   FOR i=0, np-1 DO BEGIN
       IF (data(i) GT 0) THEN BEGIN
          labels(i) = labels(i) + ' N'
       ENDIF
       IF (data(i) LT 0) THEN BEGIN
          a=STRLEN(labels(i))
          labels(i) = STRMID(labels(i),1,a-1) + ' S'
       ENDIF
    ENDFOR

    a=WHERE(data EQ 0)
    IF (a GE 0) THEN labels(a) = 'EQ'
ENDIF

IF (N_ELEMENTS(lon) NE 0) THEN BEGIN
   FOR i=0, np-1 DO BEGIN
       IF (data(i) LT 0) THEN BEGIN
          a=STRLEN(labels(i))
          labels(i) = STRMID(labels(i),1,a-1) + ' W'
       ENDIF ELSE BEGIN
          labels(i) = labels(i) + ' E'
       ENDELSE
       IF (data(i) EQ 0) THEN labels(i) = '0'
   ENDFOR
ENDIF

RETURN, labels

END
