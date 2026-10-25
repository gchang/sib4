FUNCTION EZ4_setFontSize, infont, newsize

IF (N_ELEMENTS(infont) EQ 0 OR N_ELEMENTS(newSize) EQ 0) THEN BEGIN
   EZ4_Alert,'Incorrect Font Specified in EZ4_SetFontSize.'
   RETURN,''
ENDIF

sizeref1 = STRPOS(infont,'--')
sizeref2 = STRPOS(infont,'-*')
sizeref = sizeref1+2
sizelen = sizeref2-sizeref
fontlen = STRLEN(infont)

mainfont = STRMID(infont,0,sizeref)
endfont = STRMID(infont,sizeref+sizelen,fontlen)

newSizeType = SIZE(newSize,/TYPE)
IF (newSizeType NE 7) THEN BEGIN
   IF (newSize LT 10) THEN $
       newSizeString = STRING(newSize,FORMAT='(I1)') $
   ELSE newSizeString = STRING(newSize,FORMAT='(I2)')
ENDIF ELSE newSizeString=newSize

result = mainfont + newSizeString + endfont

RETURN, result
END
