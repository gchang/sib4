pro sib4_getpftt, missing, aflag, mypft, data, larea, lpftref, values

nlu=(size(data))[1]
ntime=(size(data))[2]

hitpft=0
values=fltarr(ntime)
IF (mypft EQ 0) THEN BEGIN
   hitpft=1
   FOR l=0,nlu-1 DO BEGIN
       values(*) += data(l,*)*larea(l)
   ENDFOR
ENDIF ELSE BEGIN
   lref = where(lpftref EQ mypft)
   IF (lref[0] GE 0) THEN BEGIN
       IF (aflag EQ 0) THEN BEGIN
          mylarea = 1.
       ENDIF ELSE $
       IF (aflag EQ 1) THEN BEGIN
          mylarea = larea(lref)
       ENDIF ELSE $
       IF (aflag EQ 2) THEN BEGIN
          IF (larea(lref) GT 0.) THEN BEGIN
             mylarea = 1./larea(lref)
          ENDIF ELSE mylarea=1.
       ENDIF

       hitpft = 1
       values(*) = data(lref,*)*mylarea
   ENDIF
ENDELSE

IF (hitpft EQ 0) THEN values(*) = missing

end
