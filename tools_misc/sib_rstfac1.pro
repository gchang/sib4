;Program to plot the relative humidity stress
;   soft landing.
;
;kdhaynes, 01/17

pro sib_rstfac1, psFile=psFile

;Parameters
rhfac_astart = 0.8
rhfac_exp = 1.1

rhfac_cropstart = 0.8
rhfac_cropexp = 0.7

;Plotting values
ytitle='rstfac1'

;Arrays
mynum=100.
h2osrh = findgen(mynum)/100.
newh2osrh = h2osrh
croph2osrh = h2osrh

;Humidity Stress Modification
for i=0,mynum-1 do begin
    if (h2osrh(i) lt rhfac_astart) then newh2osrh(i) = h2osrh(i) + (rhfac_astart - h2osrh(i))^rhfac_exp
    croph2osrh(i) = h2osrh(i)^rhfac_cropexp
endfor

;Plot
set_pdisplay, psFile=psFile
plot,h2osrh,h2osrh, $
   /xstyle,/ystyle, $
   xrange=[-0.1,1.1], yrange=[-0.1,1.1], $
   ytitle=ytitle
oplot,h2osrh,newh2osrh,color=240
oplot,h2osrh,croph2osrh,color=50

if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end




