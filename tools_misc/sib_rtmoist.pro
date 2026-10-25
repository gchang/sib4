PRO sib_rtmoist, psFile=psFile

num=101
satfrac = findgen(num)/100.
satfrac(0)=0.001

numtry=6
clay=[0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
sand=[0.7, 0.7, 0.5, 0.5, 0.1, 0.0]
coltry=[240,200,130,100,60,30]

fphi  = -15.0
wphi  = -1500.0
toprange=0.98  
botrange=0.02  

xtitle='Saturation Fraction'
ytitle=textoidl('MH_{Soil,Moist}')
xval=0.8
yval=0.34
yint=0.08
csize=2.0

;...calculate soil variables from %sand, %clay
;...these relationships are from:
;...Clapp and Hornberger, 1978; Cosby et al., 1984; and
;...Lawrence and Slater, 2008
moistfac=fltarr(numtry,num)

for nt=0,numtry-1 do begin
    clayfrac=clay(nt)
    sandfrac=sand(nt)
    cfrac=clayfrac*100.
    sfrac=sandfrac*100.

    bee   = 2.91+0.159*cfrac
    phsat = -10.*10.^(1.88-0.0131*sfrac)/1000.
    poros = 0.489-0.00126*sfrac
    wopt  = (-0.08*clayfrac^2 + 0.22*clayfrac+0.59)*100.
    zm    = -2*clayfrac^3 - 0.4491*clayfrac^2 + $
             0.2101*clayfrac+0.3478
    woptzm= (wopt/100.)^zm
    wsat  = 0.25*clayfrac+0.5

    fc =  poros*((fphi/9.8)/phsat)^(-1.0/bee)
    wp =  poros*((wphi/9.8)/phsat)^(-1.0/bee)

    for i=0,num-1 do begin
        wet_exp = ((satfrac(i)^zm-woptzm)/(1.-woptzm))^2.
        moistfac(nt,i) = toprange*wsat^wet_exp+botrange
    endfor
 endfor ;nt=0,numtry

;Plot
set_artmdisplay, psFile=psFile

PLOT,satfrac,moistfac(*,0), /nodata, $
   /XSTYLE, /YSTYLE, $
   xtitle=xtitle, ytitle=ytitle, $
   yrange=[-0.1,1.1]

xyouts,xval,yval+yint,'Clay:', $
    alignment=0.0, size=csize
for nt=0,numtry-1 do begin
    oplot,satfrac,moistfac(nt,*), color=coltry(nt)
    xyouts, xval, yval-yint*nt, $
        string(clay(nt),format='(F5.1)'), $
        color=coltry(nt), $
        alignment=0.0, size=csize
endfor



if keyword_set(psFile) THEN BEGIN
   Device,/CLOSE
   Set_Plot, 'X'
   !P.Font=-1
endif

end


pro set_artmdisplay, psFile=psFile


if (n_elements(psFile) EQ 0) THEN BEGIN

   myheight=500
   myratio=1.4
   mywidth=myheight*myratio 
  
   mycsize=2.5
   mycthick=2.0
   myxyzthick=3.
   mypthick=2.

   myxmargin=[7,2]
   myymargin=[3.5,1]

   set_plot,'x'
   !p.font=-1
   device,decompose=0,true_color=24,retain=2
   window,0,xsize=mywidth,ysize=myheight

ENDIF ELSE BEGIN
   myheight=5.5
   myratio=1.4
   mywidth=myheight*myratio
   myfont=8

   mycsize=2.7
   mycthick=2.7
   myxyzthick=3.
   mypthick=4.

   myxmargin=[7,2]
   myymargin=[3,1]

   set_plot,'PS'
   !p.font=0
   device,filename=psFile,xsize=mywidth,ysize=myheight,/inches,  $
         color=8,font_size=myfont,/encapsulated,bits_per_pixel=8

ENDELSE

!p.charsize=mycsize
!p.charthick=mycthick
!x.thick=myxyzthick
!y.thick=myxyzthick
!z.thick=myxyzthick
!p.thick=mypthick

!x.margin=myxmargin
!y.margin=myymargin
!p.multi=[0,1,1]

;Set colors:
loadct,39
!p.background=255  
!p.color=0

return

end

