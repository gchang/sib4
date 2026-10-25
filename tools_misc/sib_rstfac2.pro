;Program to plot the root zone water stress.
;
;kdhaynes, 01/17

PRO sib_rstfac2, psFile=psFile

;Parameters
nchoices=5
wssp = [0.01, 0.20, 0.40, 0.60, 0.80]
pcolor = [30, 70, 110, 200, 240]

;Plotting locations
xval = 0.80
yval = 0.40
yint = 0.06
ytitle='rstfac2'
xtitle='pawrw_frac'

;Soil Properties
poros = 0.44
phsat = -0.36
bee = 9.3
fphi = -15.0
wphi = -1500.0

fc =  poros*((fphi/9.8)/phsat)^(-1.0/bee)
wp =  poros*((wphi/9.8)/phsat)^(-1.0/bee)
range = fc - wp

;Arrays
nvals=101
lp = fix(100*wp)
hp = fix(100*fc)
paw = findgen(nvals)
rstfac2 = fltarr(nchoices,nvals)
rstfac2(*,0:lp) = 0.0
rstfac2(*,hp+1:nvals-1) = 1.0 

;Water Stress
for j = 0, nchoices-1 do begin
    for i = lp+1, hp do begin
        wfrac = i/100.
        rstfac2(j,i) = ( (1.0 + wssp(j)) * ((wfrac - wp)/range)) / $
                         (wssp(j) + ((wfrac - wp)/range))
     endfor
endfor


;Plot
set_pdisplay,psfile=psFile

plot, paw, rstfac2(0,*), $
   /xstyle, /ystyle, $
   xtitle=xtitle, ytitle=ytitle

for j = 0, nchoices-1 do begin
    oplot, paw, rstfac2(j,*), color=pcolor(j)
endfor

str_wssp=string(wssp,format='(F5.2)')
for j = 0, nchoices-1 do begin
    xyouts,xval,yval+j*yint,str_wssp(j),color=pcolor(j)
endfor


if keyword_set(psFile) THEN BEGIN
   Device,/CLOSE
   Set_Plot, 'X'
   !P.Font=-1
endif

end
