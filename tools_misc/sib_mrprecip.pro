;========================================================
;Program to plot the precipitation respiration inhibition
;
;kdhaynes, 2019/03

pro sib_mrprecip

;Parameters
xtitle='Seasonal Precipitation (mm/day)'
ytitle='MH$_{Sfc,Precip}$'

ntry=5.
COL=['crimson','dark orange','green','blue','indigo']

clim_precip = [4.0, 3.0, 2.0, 1.0, 3.0]
pml = [1.2, 1.1, 1.0, 0.9, 0.8]
pmin = [0.1, 0.15, 0.2, 0.25, 0.3]

precipmax=6.
spmargin=[0.13,0.13,0.03,0.01]
spdim=[900,600]
spsize=20
spthick=2.

lsize=18
pxvall=0.84
pxval=0.84
pyval=0.58
pyint=.052
pcsize=2.
lcsize=2.

;Arrays
num=101
precip=findgen(num)/100.*precipmax
mrprecip=fltarr(num,ntry)+1.0

;Calculate mrprecip
for nt=0,ntry-1 do begin
    slope = (1.0 - pmin(nt))/(clim_precip(nt)*pml(nt))
    yint = pmin(nt)
    for i=0,num-1 do begin
        if (precip(i) LT clim_precip(nt)*pml(nt)) THEN BEGIN
           mrprecip(i,nt) = MAX([pmin(nt), $
                slope*precip(i) + yint])
        endif
     endfor
endfor

;Plot
minstress=0.
maxstress=1.1

close_windows
myplot = plot(precip,mrprecip(*,0), $
          dimension=spdim, margin=spmargin, $
          xtitle=xtitle, ytitle=ytitle, $
          font_name='Hershey', font_size=spsize, $
          yrange=[minstress,maxstress])

xval1=0.66
xint=0.06
yval1=0.30
yval2=0.22
yval3=0.20
mystring=textoidl('HRT_{PML}=')
mytext = text(xval1-xint,  yval1, mystring, /normal, $
              font_size=14, alignment=0.5, vertical_alignment=0.5)
mystring2=textoidl('Pr_{Clim}=')
mytext = text(xval1-xint, yval2, mystring2, /normal, $
              font_size=14, alignment=0.5, vertical_alignment=0.5)
for nt=0,ntry-1 do begin
    myploto = plot(precip,mrprecip(*,nt), $
          color=col(nt), /overplot, thick=spthick)

   mytext = text(xval1+nt*xint, yval1, string(pml(nt),format='(F4.1)'), /normal, $
               font_color=COL(nt), font_size=14, alignment=0.5, $
                  vertical_alignment=0.5)
    mytext = text(xval1+nt*xint, yval2, string(clim_precip(nt),format='(F4.1)'), /normal, $
               font_color=COL(nt), font_size=14, alignment=0.5, $
                  vertical_alignment=0.5)
    ;myploto2 = plot([clim_precip(nt)*pml(nt),  $
    ;                 clim_precip(nt)*pml(nt)], $
    ;       [minstress,maxstress],color=col(nt), $
    ;       linestyle=2,/overplot)
endfor


end



