;------------------------------------------------------------------------------ 
;       procedure GETFILES
;------------------------------------------------------------------------------ 
; This routine finds the files at the current directory level. 
;------------------------------------------------------------------------------ 
 
FUNCTION EZ4_GetFiles, filter 
;; Check for more than one filter
filters = STRSPLIT(filter, /EXTRACT)
n       = 0
list    = ''
FOR i=0, N_ELEMENTS(filters)-1 DO BEGIN
  files = FILE_SEARCH(filters[i], COUNT=n1)
  IF n1 GT 0 THEN BEGIN
    n    = n + n1
    list = [list, files]
  ENDIF
ENDFOR
IF n GT 0 THEN BEGIN
  list = list[1:*]
  ;; Check for those that are 
  good = WHERE(FILE_TEST(list, /REGULAR) EQ 1, n)
  IF n GT 0 THEN RETURN, list[good]
ENDIF
RETURN, ""
END 
