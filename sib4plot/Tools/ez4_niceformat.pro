
FUNCTION EZ4_NiceFormat, value, $
         INT=INT, FLT=FLT, YRS=YRS

IF (N_ELEMENTS(INT) GT 0) THEN BEGIN
    valueformat='(I7)'
    IF (value LT 100000) THEN valueFormat='(I6)'
    IF (value LT 10000) THEN valueFormat='(I5)'
    IF (value LT 1000) THEN valueFormat='(I4)'
    IF (value LT 100) THEN valueformat='(I3)'
    IF (value LT 10) THEN valueformat='(I2)'
    IF (value LT 0) THEN valueformat='(I3)'
    IF (value LT -10) THEN valueformat='(I4)'
    IF (value LT -100) THEN valueformat='(I5)'
    IF (value LT -1000) THEN valueformat='(I6)'
    IF (value LT -10000) THEN valueformat='(I7)'

    RETURN, valueFormat
ENDIF

IF (N_ELEMENTS(FLT) GT 0) THEN BEGIN
    IF (abs(value) GT 1000) THEN valueFormat='(e9.1)'  ELSE $
    IF (abs(value) GT 1)    THEN valueFormat='(f10.2)' ELSE $
    IF (abs(value) GT 0.1)  THEN valueFormat='(f10.2)' ELSE $
    if (abs(value) GT 0.01) THEN valueFormat='(f10.3)' ELSE $
    if (abs(value) GT 1.0E-3) THEN valueFormat='(f12.4)' ELSE $
    if (abs(value) GT 1.0E-4) THEN valueFormat='(f12.5)' ELSE $
    if (abs(value) GT 1.0E-5) THEN valueFormat='(f12.6)' ELSE $
    if (abs(value) GT 1.0E-6) THEN valueFormat='(f12.7)' ELSE $
    valueformat='(f18.10)'

    RETURN, valueFormat
ENDIF

IF (N_ELEMENTS(YRS) GT 0) THEN valueFormat='(f10.4)'

;default code from EZPLOT
IF (value EQ 0) THEN BEGIN
    valueFormat = '(F6.1)'
    RETURN, valueFormat
ENDIF

avalue = ABS(value)
IF (avalue LT 1) THEN BEGIN
    decimals=1
    myfloor=0
    myavalue=avalue
    WHILE (myfloor EQ 0) DO BEGIN
          decimals++
          myavalue*= 10.
          myfloor = FLOOR(myavalue)
    ENDWHILE

    IF (decimals GE 6) THEN valueFormat = '(e9.2)' $
    ELSE BEGIN
       width = decimals+3
       valueFormat = STRCOMPRESS('(f'+STRING(width)+'.' $
                       +STRING(decimals)+')')
    ENDELSE
ENDIF ELSE BEGIN
    integers = FLOOR(ALOG10(ABS(value))) + 1
    IF integers LT 3 THEN BEGIN
        valueFormat='(F6.2)'
    ENDIF ELSE $
    IF integers EQ 3 THEN BEGIN
        valueFormat='(F7.1)'
    ENDIF ELSE BEGIN
        IF (value LT 0) THEN BEGIN
            width = integers + 1
            decimal = 0
        ENDIF ELSE BEGIN
            width = integers
            decimal = 0
        ENDELSE
        IF (decimal GT 0) THEN BEGIN
            valueFormat = STRCOMPRESS('(f'+STRING(width) $
                             + '.' + STRING(decimal) + ')', /REMOVE_ALL)
        ENDIF ELSE BEGIN
            valueFormat = STRCOMPRESS('(i'+STRING(width)+')', /REMOVE_ALL)
        ENDELSE
    ENDELSE
ENDELSE

RETURN, valueFormat

END
