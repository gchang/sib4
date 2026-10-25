FUNCTION EZ4_GETVARNAME, title

IF (N_ELEMENTS(title) EQ 0) THEN title = ''

;Test for explanation
p = STRPOS(title,':')
IF (p GE 0) THEN BEGIN
   title = STRMID(title,0,p)
ENDIF

;Test for parentheticals
p = STRPOS(title,'(')
IF (p GE 0) THEN BEGIN
   title = STRMID(title,0,p)
ENDIF

varname = title
RETURN, varname
END
