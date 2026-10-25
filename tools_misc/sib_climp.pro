;Program to plot the climatological stress.
;
;Depends on climatological water availability (x = clim_wa)
;varying this using an equation of the form:  y=A(B)^x + C*(x-D)
;
;kdhaynes, 07/17

pro sib_climp

;Parameters
ntry=9
cwa_type=4
A=[0.04, 0.06, 0.03, 0.01, 0.03, 0.08, 0.00, 0.00, 0.00]
B=[800., 800.0, 60.0, 400., 450., 800., 0.00, 0.00, 0.00]
C=[0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.2, 0.80, 2.20]
D=[0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.10, 0.00, 0.10]
CMIN=[0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.10, 0.1, 0.05]
CMAX=[ 2.0,  2.0,  1.4, 1.6,  1.8, 2.0, 1.7,  2.0, 1.6]
;COL=[10,30,50,80,100,150,200,240]
COL=['black','midnight blue','red','royal blue','green','dark orange','gold','indigo','deep pink']

if (cwa_type EQ 1) OR (cwa_type EQ 2) THEN BEGIN
   xmax=6.2
   xval=[1,2,3,4,5]
ENDIF ELSE BEGIN
   xmax=1.2
   xval=[0.2,0.4,0.6,0.8,1.]
ENDELSE
xlabel=string(xval,format='(f4.1)')
xmin=0.
ymin=0.
ymax=2.2
thick=2.4

spdim=[800,600]
spmargin=[0.12,0.10,0.03,0.03]

;Arrays
num=101
climw=findgen(num)/100.*xmax
climp=fltarr(num,ntry)

;Calculate climp
for i=0,num-1 do begin
    for nt=0,ntry-1 do begin
        climp(i,nt) = A(nt)*(B(nt)^climw(i)) + C(nt)*(climw(i)-D(nt))
        if (climp(i,nt) LT cmin(nt)) THEN climp(i,nt) = cmin(nt)
        if (climp(i,nt) GT cmax(nt)) THEN climp(i,nt) = cmax(nt)
     endfor
endfor

;Plot
xtitle='ClimW'
ytitle='ClimP'
tsize=18

close_windows
myp = plot(climw, climp(*,0), $
     dimension=spdim, margin=spmargin, $
     xtitle=xtitle, ytitle=ytitle, $
     xrange=[xmin,xmax], xmajor=4, $
     xtickv=xval, xtickname=xlabel, $
     yrange=[ymin,ymax], $
     /xstyle, /ystyle, $
     font_name='Hershey',font_size=tsize, thick=thick)

for nt=0,ntry-1 do begin
    mypo = plot(climw, climp(*,nt), $
                color=col(nt), /overplot, thick=thick)
endfor

scol='crimson'
wcol='blue'
fsize=14

xval1=0.16
yval1=0.07
mytd2 = text(xval1,yval1,'Dry',/normal, $
             font_color=scol,font_size=fsize, $
              alignment=0.5, vertical_alignment=0.5)

xval2=0.92
yval2=0.07
mytd2 = text(xval2,yval2,'Wet',/normal, $
             font_color=wcol,font_size=fsize, $
              alignment=0.5, vertical_alignment=0.5)

xval3=0.06
yval3=0.84
mytd3 = text(xval3,yval3,'Productive',/normal, $
             font_color=wcol, font_size=ssize, $
              alignment=0.5, vertical_alignment=0.5)

xval4=0.06
yval4=0.14
mytd4 = text(xval4,yval4,'Stressed',/normal, $
             font_color=scol,font_size=ssize, $
              alignment=0.5, vertical_alignment=0.5)


end

