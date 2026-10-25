;Program to plot the leaf growth index
;
;Depends on LAI and climatological suitability
;
;kdhaynes, 2019/03

pro sib_psgrowth, psFile=psFile

;Parameters
laiplot=7.0
lctbmax=1.0
lctbmin=0.25
laicoef=2.0
laioffl=0.0
laioffg=0.6

;Values to show for cstress
numcstress=7
cstress=[0.2,0.4, 0.6, 1.0, 1.4, 1.6, 2.0]
colcstress=[240,220,200,180,150,120,90,60,30,1]

;Plotting Information
if (n_elements(psFile) eq 0) then begin
   xvalc=0.2
   xvalv=0.24
   xint=0.09
   yvalc=0.24
   yvalv=0.16
   csize=1.6
endif else begin
   xvalc=0.44
   xvalv=0.32
   xint=0.06
   yvalc=0.28
   yvalv=0.21
   csize=2.0
endelse
xtitle='LAI'
ytitle=textoidl('PS_{GI}')

;Arrays
numpts=101
lai=(findgen(numpts)+1)/numpts*laiplot
lctb=fltarr(numcstress,numpts)

;Calculate LCTB
for c=0,numcstress-1 do begin
     laimin = cstress(c)*laicoef + laioffl
     laimax = cstress(c)*laicoef + laioffg

     slope = (lctbmax-lctbmin)/(laimin-laimax)
     yint  = lctbmin - slope*laimax
 
     for i=0,numpts-1 do begin 
        if (lai(i) lt laimin) then begin
           lctb(c,i) = lctbmax
        endif else begin
           if (lai(i) lt laimax) then begin
              lctb(c,i) = slope*lai(i) + yint
           endif else begin
                lctb(c,i) = lctbmin
             endelse
        endelse
      endfor
endfor

;Plot
set_pdisplay,psFile=psFile
!x.margin=[7,2]
!y.margin=[3,1]
ymax=max(lctb)*1.1
xmax=laiplot

plot,lai,lctb[0,*], $
     xtitle=xtitle, ytitle=ytitle, $
     yrange=[0,ymax], xrange=[0,xmax], $
     /ystyle, /xstyle, /nodata

;for j=0,numcstress-1 do begin
for j=numcstress-1,0,-1 do begin
    oplot,lai,lctb[j,*],color=colcstress[j]
endfor

sname='ClimP='
xyouts,xvalc,yvalc,sname,charsize=csize,/normal
for j=0,numcstress-1 do begin
    xyouts,xvalv+j*xint,yvalv, $
         string(cstress(j),format='(f5.1)'), $
         color=colcstress(j),charsize=csize,/normal
endfor

if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif


end

