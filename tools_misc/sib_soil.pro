pro sib_soil


np=100.
wpotfc = -15.0
wpotwp = -1500.0
silt = fltarr(np)
clay = (findgen(np)+0.5)/100.
cfrac = clay*100.

sand = 1-clay
sfrac = sand*100.

poros = fltarr(np)
satco = fltarr(np)
fieldcap = fltarr(np)
wilt = fltarr(np)
wopt = fltarr(np)
wsat = fltarr(np)

for i=0,np-1 do begin
    poros(i) = 0.489-0.00126*sfrac(i)
    satco(i) = 0.0070556*10^(-0.884+0.0153*sfrac(i))/1000.

    bee = 2.91+0.159*cfrac(i)
    phsat = -10.*10.^(1.88-0.0131*sfrac(i))/1000.
    fieldcap(i) = poros(i) * ((wpotfc/9.8)/phsat)^(-1./bee)
    wilt(i) = poros(i) * ((wpotwp/9.8)/phsat)^(-1./bee)

    wopt(i) = (-0.08*clay(i)^2 + 0.22*clay(i)+0.59) ;*100.
    wsat(i) = 0.25*clay(i)+0.5
endfor


set_display ;,/tall
!p.multi=[0,2,2]
plot,clay,poros,/xstyle,/ystyle, $
    xtitle='Clay Fraction',ytitle='', $
    title='Porosity'

!x.margin=[10,2]
plot,clay,satco,/xstyle,/ystyle, $
    xtitle='Clay Fraction',title='Conductivity',ytitle=''

!x.margin=[6,2]
plot,clay,fieldcap,/xstyle,/ystyle,yrange=[0,0.5], $
    xtitle='Clay Fraction',ytitle='', $
    title='FC (black) and WP (red)'
oplot,clay,wilt,color=240

plot,clay,wopt,/xstyle,/ystyle, $
    yrange=[0,1], $
    xtitle='Clay Fraction',ytitle='', $
    title='WOPT and WSat'
oplot,clay,wsat,color=240

end
