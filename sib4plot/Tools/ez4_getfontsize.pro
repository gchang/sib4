FUNCTION EZ4_getFontSize, infont

IF (N_ELEMENTS(infont) EQ 0) THEN BEGIN
   EZ4_Alert,'Incorrect Font Specified in EZ4_GetFontSize.'
   RETURN, ''
ENDIF

sizeref1 = STRPOS(infont,'--')
sizeref2 = STRPOS(infont,'-*')
sizeref = sizeref1+2
sizelen = sizeref2-sizeref

result = STRMID(infont,sizeref,sizelen)

RETURN, result

END
