;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The main drawing routine.
;;
PRO GEO4_drawPlot, plot

@ez4_BasicParams.com

IF n_elements(nBottom) EQ 0 THEN nBottom = coltabStartIndex[plot.ColorChoice]
IF n_elements(nFillColors) EQ 0 THEN nFillColors = coltabnColors[plot.ColorChoice]

; Make sure we get a white background and black lines:
!P.BACKGROUND = white
!P.COLOR      = black

;; Set the font 
EZ4_SetFont
IF (NOT dopsplot) THEN BEGIN
   EZ4_SetFont, NameFont=labelFont, SizeFont=labelSize
ENDIF

;; Draw the map
IF (plot.region EQ 'Globe') THEN GEO4_DrawMap_Globe, plot $
ELSE GEO4_DrawMap_Region, plot


;; Add a nice color bar to color plots
IF plot.colorBar THEN BEGIN

    EZ4_SetFont

    ncontours = plot.ncontours
    IF (nContours EQ maxNColors) THEN BEGIN
        contours = plot.contours
        fillColors = plot.fillColors
    ENDIF ELSE BEGIN
        contours = FLTARR(ncontours)
        fillColors = FLTARR(ncontours)
        contours(*) = plot.contours(0:ncontours-1)
        fillColors(*) = plot.fillColors(0:ncontours-1)
    ENDELSE

    cBarTSize = plot.cBarTSize
    cBarLSize = plot.cBarLSize
    cBarPos = plot.cBarPos
    cBarLThick = plot.cBarLThick
    IF (dopsplot) THEN BEGIN
       ;cBarTSize *= 0.7
       ;cBarLSize *= 0.7
       cBarPos(1) += 0.03
       cBarPos(3) += 0.03
       cBarLThick = 2.
    ENDIF

    GEO4_Color_Bar, contours, fillColors, $
      position=cBarPos, $
      cbar_title=plot.cBarTitle, cbar_showTitle=plot.showcBarTitle, $
      cbar_tcolor=plot.cBarTColor, cbar_tsize=cBarTSize, $
      cbar_tyoff=plot.cBarTYOff,                              $
      cbar_format=plot.cBarLFormat, cbar_size=cBarLSize, $
      cbar_thick=cBarLThick, cbar_color=plot.cBarLColor, $
      cBar_lbotoff=plot.cBarLBotOff, /cbar_alttb

ENDIF

;; Add the text
GEO4_addtext, plot

END
