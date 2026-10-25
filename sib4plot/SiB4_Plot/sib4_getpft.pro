pro sib4_getpft, missing, aflag, mypft, data, larea, lpftref, value

nlu=(size(lpftref))[1]
hitpft=0
value=0.
IF (mypft LE 0) THEN BEGIN
   goodref = where(data GT missing)
   IF (goodref[0] GE 0) THEN BEGIN
      hitpft=1
      IF (n_elements(goodref) EQ nlu) THEN BEGIN
          FOR l=0,nlu-1 DO BEGIN
              value+= data(l)*larea(l)
           ENDFOR
      ENDIF ELSE BEGIN
          areatot = total(larea(goodref))
          FOR l=0,nlu-1 DO BEGIN
              IF (data(l) GT missing) THEN BEGIN
                 value+= data(l)*larea(l)/areatot
              ENDIF
           ENDFOR
       ENDELSE
   ENDIF
ENDIF ELSE BEGIN
   FOR l=0,nlu-1 DO BEGIN
       IF (mypft EQ lpftref(l)) THEN BEGIN
          IF (aflag EQ 0) THEN BEGIN
             mylarea = 1.
          ENDIF ELSE $
          IF (aflag EQ 1) THEN BEGIN
             mylarea = larea(l)
          ENDIF ELSE $
          IF (aflag EQ 2) THEN BEGIN
             IF (larea(l) GT 0.) THEN BEGIN
                 mylarea = 1./larea(l)
              ENDIF ELSE mylarea=1.
          ENDIF
          value = data(l) * mylarea
          hitpft = 1
       ENDIF     
   ENDFOR
ENDELSE

IF (hitpft EQ 0) THEN value=missing

end
