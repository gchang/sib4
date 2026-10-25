;==========================================================================
;Program to plot the assimilation rate respiration scaling factor.
;
;Depends on clim_assim, varying this using
;an equation of the form:
;    y = (amax-amin)/(climp*amh - climp*aml)
;
;kdhaynes, 2019/03

pro sib_crassim

;Parameters
xtitle='AssimD ($\mu$mol C m$^{-2}$ s$^{-1}$)'
ytitle='MCR$_{Assim}$'

ntry=5.
clim_assim = [1.0, 1.5, 2.0, 2.5, 3.0]
COL=['crimson','dark orange','green','blue','indigo']

assimmax = 6.
aml = [0.4, 0.5, 0.6, 0.7, 0.8]
amh = [4.0, 3.0, 2.0, 1.8, 1.5]
amin = [0.2, 0.4, 0.5, 0.6, 0.8]
amax = [1.1, 1.2, 1.3, 1.4, 1.4]

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
assim=findgen(num)/100.*assimmax
crassim=fltarr(num,ntry)

;Calculate artassim
for nt=0,ntry-1 do begin
    slope = (amax(nt) - amin(nt)) $
       /(clim_assim(nt)*amh(nt) - clim_assim(nt)*aml(nt))
    yint = amin(nt) - slope*clim_assim(nt)*aml(nt)

    for i=0,num-1 do begin
        if (assim(i) LT clim_assim(nt)*aml(nt)) then begin
            crassim(i,nt) = amin(nt)
        endif else $
        if (assim(i) LT clim_assim(nt)*amh(nt)) then begin
            crassim(i,nt) = slope*assim(i) + yint
        endif else $
            crassim(i,nt) = amax(nt)  
     endfor
endfor

;Plot
minstress=0.
maxstress=MAX(crassim)*1.05

close_windows
myplot = plot(assim,crassim(*,0), $
          dimension=spdim, margin=spmargin, $
          xtitle=xtitle, ytitle=ytitle, $
          font_name='Hershey', font_size=spsize, $
          yrange=[minstress,maxstress])

xval1=0.66
xint=0.06
yval1=0.36
yval2=0.28
yval3=0.20
mystring=textoidl('CR_{AML} =')
mytext = text(xval1-xint, yval1, mystring, /normal, $
              font_size=14, alignment=0.5, vertical_alignment=0.5)
mystring=textoidl('CR_{AMH} = ')
mytext = text(xval1-xint, yval2, mystring, /normal, $
              font_size=14, alignment=0.5, vertical_alignment=0.5)
mystring=textoidl('Clim_{Assim} =')
mytext = text(xval1-xint*1.4, yval3, mystring, /normal, $
              font_size=14, alignment=0.5, vertical_alignment=0.5)
for nt=0,ntry-1 do begin
    myploto = plot(assim,crassim(*,nt), $
          color=col(nt), /overplot, thick=spthick)

    mytext = text(xval1+nt*xint, yval1, string(aml(nt),format='(F4.1)'), /normal, $
               font_color=COL(nt), font_size=14, alignment=0.5, $
                  vertical_alignment=0.5)
    mytext = text(xval1+nt*xint, yval2, string(amh(nt),format='(F4.1)'), /normal, $
               font_color=COL(nt), font_size=14, alignment=0.5, $
                  vertical_alignment=0.5)

    mytext = text(xval1+nt*xint, yval3, string(clim_assim(nt),format='(F4.1)'), /normal, $
               font_color=COL(nt), font_size=14, alignment=0.5, $
               vertical_alignment=0.5)
    ;myploto2 = plot([clim_assim(nt),clim_assim(nt)], $
    ;       [minstress,maxstress],color=col(nt), $
    ;       linestyle=2,/overplot)
endfor


end



