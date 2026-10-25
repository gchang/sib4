;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sets the device font to the appropriate font
;;
PRO EZ4_SetFont, NameFont=NameFont, SizeFont=SizeFont, $
    xlmargin=xlmargin, xrmargin=xrmargin, $
    ybmargin=ybmargin, ytmargin=ytmargin, $
    xsize=xsize, ysize=ysize, $
    trueType=trueType

@ez4_BasicParams.com

; Set font
IF (N_Elements(NameFont) EQ 0) THEN BEGIN
   !P.Font=-1
ENDIF ELSE BEGIN
   IF (N_ELEMENTS(TrueType) EQ 0) THEN BEGIN
      !P.FONT = 0
      DEVICE, SET_FONT=NameFont
   ENDIF ELSE BEGIN
      !P.FONT = 1
      DEVICE, SET_FONT=NameFont,/TT_FONT
   ENDELSE
ENDELSE

; Make sure we get a white background and black lines:
!P.BACKGROUND = white
!P.COLOR      = black

; Set margins
IF (N_ELEMENTS(xlmargin) EQ 0) THEN xlmargin=xLeftMargin
IF (N_ELEMENTS(xrmargin) EQ 0) THEN xrmargin=xRightMargin
!X.Margin = [xlmargin,xrmargin]

IF (N_ELEMENTS(ybmargin) EQ 0) THEN ybmargin=yBotMargin
IF (N_ELEMENTS(ytmargin) EQ 0) THEN ytmargin=yTopMargin
!Y.Margin = [ybmargin,ytmargin]

; Set character size
IF (N_ELEMENTS(sizeFont) EQ 0) THEN sizeFont=defaultSize
!P.CharSize = sizeFont


RETURN

END

