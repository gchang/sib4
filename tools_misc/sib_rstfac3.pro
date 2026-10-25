;Program to plot the temperature stress.
;
;kdhaynes, 01/17

pro sib_rstfac3, psFile=psFile

;Parameters
slti=0.2
shti=0.3
hlti=272.
hhti=308.
sfti=0.35
hfti=270.

;Plotting Values
xtitle='Temperature (K)'
ytitle=textoidl('F_{T}')

;Arrays
trange=60
tmin=260
tc=findgen(trange)+tmin

;Temperature Stresses
temph = (0.98+exp(shti * (tc-hhti)))
tempc = (0.98+exp(slti * (hlti-tc)))
tempf = 1.+exp(sfti * (hfti - tc))
templ = tempc * tempf

rstfac3 = 1./(templ*temph)
rstfac3(where(rstfac3 gt 1.)) = 1.0

;Plot
set_pdisplay, psFile=psFile
!y.margin=[3,1]
plot,tc,rstfac3, $
    /xstyle,/ystyle, $
    yrange=[-0.1,1.1], $
    xtitle=xtitle, ytitle=ytitle, thick=5.

ptempl = 1./templ
ptempl(where(ptempl gt 1.0)) = 1.0
oplot,tc,ptempl,color=80

mystring=textoidl('1/T_{Low}')
xval=0.6
yval=0.34
yint=0.08
xyouts,xval,yval,/normal,mystring,color=80

ptemph = 1./temph
ptemph(where(ptemph gt 1.0)) = 1.0
oplot,tc,ptemph,color=240

mystring=textoidl('1/T_{High}')
xyouts,xval,yval-yint,/normal,mystring,color=240

ptempf = 1./tempf
ptempf(where(ptempf gt 1.0)) = 1.0
oplot,tc,ptempf,color=30

mystring=textoidl('1/T_{Frost}')
xyouts,xval,yval-2*yint,/normal,mystring,color=30

oplot,tc,rstfac3

if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif


end




