;:::::::::::::::::::::::::::::::::::::::::::;
pro GEO4_Color_Bar, contour, fillColors, $
               position=position, cbar_showtitle=cbar_showtitle, $
               cbar_title=cbar_title, cbar_tyoff=cbar_tyoff, $
               cbar_tcolor=cbar_tcolor, cbar_tsize=cbar_tsize, $
               cbar_noline=cbar_noline, cbar_label=cbar_label, $
               cbar_format=cbar_format, cbar_size=cbar_size, $
               cbar_color=cbar_color, cbar_thick=cbar_thick, $
               cbar_alltop=cbar_alltop, cbar_alttb=cbar_alttb,   $
               cbar_ltopoff=cbar_ltopoff, cbar_lbotoff=cbar_lbotoff
;:::::::::::

; Set defaults
nContours = n_elements(contour)

if (n_elements(position) eq 0) then begin
  leftEdge = 0.05
  rightEdge = 0.95
  bottom = 0.1
  top = 0.15
endif else begin
  leftEdge = position[0]
  rightEdge = position[2]
  bottom = position[1]
  top = position[3]
endelse

IF (N_ELEMENTS(cbar_showtitle) EQ 0) THEN cbar_showtitle=1
IF (N_ELEMENTS(cbar_title) EQ 0) THEN cbar_title=''
IF (N_ELEMENTS(cbar_tyoff) EQ 0) THEN cbar_tyoff=0.08
title=bottom-cbar_tyoff

IF (N_ELEMENTS(cbar_tcolor) EQ 0) THEN cbar_tcolor=0
IF (N_ELEMENTS(cbar_tsize) EQ 0) THEN cbar_tsize=2.0
IF (N_ELEMENTS(cbar_thick) EQ 0) THEN cbar_thick=2.0
IF (N_ELEMENTS(cbar_border) EQ 0) THEN cbar_border=1
IF (N_ELEMENTS(cbar_noline) EQ 0) THEN cbar_noline=0
IF (N_ELEMENTS(cbar_label) EQ 0) THEN cbar_label=1
IF (N_ELEMENTS(cbar_size) EQ 0) THEN cbar_size=2.0
IF (N_ELEMENTS(cbar_color) EQ 0) THEN cbar_color=0

if (n_elements(cbar_ltopoff) eq 0) then cbar_ltopoff=0.01
if (n_elements(cbar_lbotoff) eq 0) then cbar_lbotoff=0.03
if (n_elements(cbar_alltop) eq 0) then cbar_alltop=0
if (n_elements(cbar_alttb) eq 0) then cbar_alttb=0

center = leftEdge + ((rightEdge - leftEdge) / 2.)
width  = (rightEdge - leftEdge) / nContours

; Draw the color bar
extra = 0.0
IF (cbar_noline) THEN extra = 0.007

left	= leftEdge
right = left+width
y_coord = [bottom, bottom, top, top]
mid=(bottom+top)/2

IF (cbar_noline EQ 0) THEN BEGIN
  POLYFILL,COLOR=fillColors[0],/normal,[left,right,right,left], $
     [mid,top,bottom,mid]
  left=right-extra
endif

FOR i=1, nContours-2 DO BEGIN
  right = left + width + extra 
  POLYFILL, COLOR = fillColors[i], /NORMAL, [left,right,right,left], y_coord
  left=right - extra
ENDFOR

IF (cbar_noline EQ 0) then begin
  left+=extra
  right=left+width
  POLYFILL,COLOR=fillColors[nContours-1],/NORMAL,[left,right,right,left], $
        [top,mid,mid,bottom]
  left=right
endif

; Draw line separators between distinct colors
IF (cbar_noline EQ 0) THEN BEGIN
   right = leftEdge + width
   FOR i=0, nContours-2 DO BEGIN
       PLOTS, [right, right], [top, bottom], /NORMAL,COLOR=cbar_color,thick=cbar_thick
       right = right + width
   ENDFOR
ENDIF

; Draw border around the color bar
IF (cbar_border EQ 1) THEN BEGIN
   IF (cbar_noline EQ 0) THEN BEGIN
       left=leftEdge+width
       right=right-width
       PLOTS, [left, right, right, left, left], $
              [top, top, bottom, bottom, top], /NORMAL, COLOR=cbar_color, $
              thick=cbar_thick

       left=leftEdge
       right=left+width
       PLOTS,[left,right,right,left],[mid,top,bottom,mid], $
             /NORMAL,color=cbar_color, $
             thick=cbar_thick

       left=rightEdge-width
       right=rightEdge
       PLOTS,[left,right,right,left],[top,mid,mid,bottom], $
       /NORMAL,color=cbar_color, $
       thick=cbar_thick
   ENDIF ELSE BEGIN ;cbar_noline == 0
       left=leftEdge
       right=right
       PLOTS, [left, right, right, left, left], $
              [top, top, bottom, bottom, top], /NORMAL, COLOR=cbar_color, $
              thick=cbar_thick
   ENDELSE
ENDIF
;...................

; Label the contour levels
contournm=contour(ncontours-1)
IF (N_ELEMENTS(cbar_format) EQ 0) THEN $
    cbar_format=EZ4_NiceFormat(contournm)
IF (cbar_format EQ '') THEN $
    cbar_format=EZ4_NiceFormat(contournm)
contourString = strtrim((string(contour,format=cbar_format)),2)

lpybot = bottom-cbar_lbotoff
lpytop = top+cbar_ltopoff

lpyPos = fltarr(ncontours-1)
IF (cbar_alttb EQ 1) THEN BEGIN
   FOR i=0,ncontours-2 DO BEGIN
       IF ((i MOD 2) EQ 0) THEN $
              lpypos(i)=lpybot ELSE lpypos(i)=lpytop
   ENDFOR
ENDIF ELSE BEGIN
   IF (cbar_alltop EQ 0) THEN lpypos(*)=lpybot $
   ELSE lpypos(*)=lpytop
ENDELSE
lpxPos = leftEdge+width

IF (cbar_label) THEN BEGIN
   FOR i=1, nContours-1 DO BEGIN
       XYOUTS,lpxPos,lpyPos(i-1),contourString[i], $
            ALIGNMENT=0.5, /NORMAL, COLOR=cbar_color, $
            charsize=cbar_size, charthick=cbar_thick
       lpxPos += width
   ENDFOR
ENDIF

; Draw title
IF (cbar_showTitle EQ 1) THEN $
   XYOUTS, center, title, cbar_title, ALIGNMENT=0.5, /NORMAL, $
     COLOR=cbar_tcolor, charsize=cbar_tsize, charthick=cbar_thick

END
