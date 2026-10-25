;======================================
PRO EZ4_AddLineLabel, plot, POSITION=pos
;======================================

@ez4_BasicParams.com

IF (dopsplot) THEN EZ4_SetFont, NameFont=psFontL, /TrueType $
ELSE EZ4_SetFont, NameFont=labelFontL, SizeFont=labelSizeL

IF (N_ELEMENTS(pos) EQ 0) THEN pos = plot.pos
numlabel = TOTAL(plot.drawlines)
PLOT,[0,1],/NODATA,/NOERASE,XSTYLE=4,YSTYLE=4, $
     POSITION=[0,0,1,1]

xxdel = 0.1
IF (numlabel LE 6) THEN BEGIN
   xoff=0.08
   dx=0.38
ENDIF ELSE $
IF (numlabel LE 9) THEN BEGIN
   xoff=0.02 
   dx = 0.3
ENDIF ELSE $
IF (numlabel LE 12) THEN BEGIN
   xoff=-0.08
   dx = 0.24
ENDIF 

xx1s = MAX([0.03,pos[0] + xoff])
xx1e = xx1s + xxdel
xx2s = xx1s + dx
xx2e = xx2s + xxdel
xx3s = xx2s + dx
xx3e = xx3s + xxdel
xx4s = xx3s + dx
xx4e = xx4s + xxdel

IF (numlabel LE 6) THEN BEGIN
   xPos1 = [xx1s,xx2s,xx1s,xx2s,xx1s,xx2s,xx1s]
   xPos2 = [xx1e,xx2e,xx1e,xx2e,xx1e,xx2e,xx1e]
ENDIF ELSE $
IF (numlabel LE 9) THEN BEGIN
   xPos1 = [xx1s,xx2s,xx3s,xx1s,xx2s,xx3s,xx1s,xx2s,xx3s]
   xPos2 = [xx1e,xx2e,xx3e,xx1e,xx2e,xx3e,xx1e,xx2e,xx3e]
ENDIF ELSE $
IF (numlabel LE 12) THEN BEGIN
   xPos1 = [xx1s,xx2s,xx3s,xx4s,xx1s,xx2s,xx3s,xx4s,xx1s,xx2s,xx3s,xx4s]
   xPos2 = [xx1e,xx2e,xx3e,xx4e,xx1e,xx2e,xx3e,xx4e,xx1e,xx2e,xx3e,xx4e]
ENDIF ELSE BEGIN
   string1 = 'Need more line label positions!'
   string2 = 'Stopping in ez4_addlinelabel.'
   EZ_Alert,[string1,string2]
   RETURN
ENDELSE

origdelta = 0.13
xyodelta = 0.14
ydelta = 0.07
xyydelta = 0.07
IF (numlabel GT 9) THEN BEGIN
   origdelta = 0.10
   xyodelta = 0.11
   ydelta = 0.06
   xyydelta = 0.06
ENDIF

yy1 = MAX([0.035,pos[1]-origdelta])
yy2 = yy1-ydelta
yy3 = yy2-ydelta
yy4 = yy3-ydelta

IF (numlabel LE 6) THEN BEGIN
   yPos = [yy1,yy1,yy2,yy2,yy3,yy3,yy4]
ENDIF ELSE $
IF (numlabel LE 9) THEN BEGIN
   yPos = [yy1,yy1,yy1,yy2,yy2,yy2,yy3,yy3,yy3]
ENDIF ELSE $
IF (numlabel LE 12) THEN BEGIN
   ypos = [yy1,yy1,yy1,yy1,yy2,yy2,yy2,yy2, $
           yy3,yy3,yy3,yy3,yy4,yy4,yy4,yy4]
ENDIF

xyxoffset = 0.11
xyxPos=xPos1+xyxoffset
xyy1 = MAX([0.032,pos[1]-xyodelta])
xyy2 = xyy1-xyydelta
xyy3 = xyy2-xyydelta
xyy4 = xyy3-xyydelta

IF (numlabel LE 6) THEN BEGIN
   xyyPos=[xyy1,xyy1,xyy2,xyy2,xyy3,xyy3,xyy4]
ENDIF ELSE $
IF (numlabel LE 9) THEN BEGIN
   xyyPos=[xyy1,xyy1,xyy1,xyy2,xyy2,xyy2,xyy3,xyy3,xyy3]
ENDIF ELSE $
IF (numlabel LE 12) THEN BEGIN
   xyyPos=[xyy1,xyy1,xyy1,xyy1,xyy2,xyy2,xyy2,xyy2, $
           xyy3,xyy3,xyy3,xyy3,xyy4,xyy4,xyy4,xyy4]
ENDIF

IF (plot.showLineLabel EQ 1) THEN BEGIN
   FOR i=0,numlabel-1 DO BEGIN
       IF (plot.drawlines(i) EQ 1) THEN BEGIN
           myx=[xpos1(i),xPos2(i)]
           myy=REPLICATE(yPos(i),2)
           oplot,myx,myy, $
             COLOR=specColors[plot.linecolor(i)], $
             LINESTYLE=plot.linestyle(i), THICK=plot.linethick(i)
           XYOUTS,xyxPos(i),xyyPos(i),plot.linename(i),/NORMAL
       ENDIF
   ENDFOR
ENDIF ELSE BEGIN
   ;..white-out labels
   FOR i=0,numlabel-1 DO BEGIN
       myx=[xPos1(i),xPos2(i)]
       myy=REPLICATE(yPos(i),2)
       labxleft = myx - 0.10
       labxright = myx + 0.20
       labytop = myy + 0.03
       labybot = myy - 0.02

       POLYFILL, [labXleft,labXright,labXright,labXleft], $
                 [labYtop,labYtop,labYbot,labYbot], $
                  COLOR=white, /NORMAL
    ENDFOR
ENDELSE 

RETURN

END
