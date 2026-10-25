FUNCTION GEO4_Set_FillColors, colorChoice, whiteOut

COMMON Colors_COM

nBottom = coltabStartIndex[colorChoice]
nFillColors = coltabnColors[colorChoice]
fillColors = INDGEN(nFillColors) + nBottom

; White out specified colors
IF N_ELEMENTS(whiteOut) GT 0 THEN BEGIN
  midpt = nFillColors/2
  IF nFillColors MOD 2 EQ 0 THEN BEGIN
    CASE whiteOut OF
      'low'    : fillColors[0] = white
      'middle' : fillColors[midpt-1:midpt] = white
      'high'   : fillColors[nFillColors-1] = white
      ELSE :
    ENDCASE
  ENDIF ELSE BEGIN
    CASE whiteOut OF
      'low'    : fillColors[0] = white
      'center' : fillColors[midpt] = white
      'middle' : fillColors[midpt-1:midpt+1] = white
      'high'   : fillColors[nFillColors-1] = white
      ELSE :
    ENDCASE
  ENDELSE
ENDIF


RETURN, fillColors

END
