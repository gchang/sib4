;Program to plot the leaf transfer from lack of water.
;
;kdhaynes, 2019/03

pro sib_ltdry, psFile=psFile

;Parameters
ntry=5
lt_nwref = [0.12,0.1,0.08,0.06,0.14]
lt_nwcoef = [0.005,0.01,0.008,0.02,0.002]
lt_nwbase = [0.07,0.1,0.03,0.05,0.08]
lt_nwmax = [0.10,0.08,0.08,0.07,0.09]
col=[80,200,240,30,160]

;Plotting information
if (n_elements(psFile) gt 0) then begin
    csize=2.0
endif else begin
    csize=1.8
endelse

;Arrays
num=40
pawmax=0.15
pawfrw = findgen(num)/num*pawmax
ltdry=fltarr(num,ntry)

;Calculate ltdry
for nt=0,ntry-1 do begin
    for i=0,num-1 do begin
        if (pawfrw(i) lt lt_nwref(nt)) then begin
           ltdry(i,nt) = MIN([lt_nwmax(nt),lt_nwcoef(nt) * (lt_nwbase(nt)^(10*(pawfrw(i)-lt_nwref(nt))) - 1.)])
        endif else begin
           ltdry(i,nt) = 0.0
        endelse
    endfor
endfor

;Plot
set_pdisplay,psFile=psFile
!x.margin=[8,3]
!y.margin=[3,1]
mytitle='' ;'Water Deficit Transfer'
xtitle='PAWFRW'
ytitle=textoidl('T_W (Fraction Per Day)')

minstress=min(ltdry)*0.95
maxstress=max(ltdry)*1.05
plot,pawfrw,ltdry(*,0),title=mytitle, $
     xtitle=xtitle, ytitle=ytitle, $
     xrange=[min(pawfrw),max(pawfrw)], $
     yrange=[minstress,maxstress], $
     charsize=csize

xval1=0.38
xint=0.11
yint=0.08
yval1=0.88
yval2=yval1-yint
yval3=yval1-2*yint
yval4=yval1-3*yint
csize=1.6

mystring1=textoidl('LT_{WB}=')
mystring2=textoidl('LT_{WC}=')
mystring3=textoidl('LT_{WR}=')
mystring4=textoidl('LT_{WMax}=')
xyouts,xval1-0.9*xint,yval1,mystring1,charsize=csize,/normal
xyouts,xval1-0.9*xint,yval2,mystring2,charsize=csize,/normal
xyouts,xval1-0.9*xint,yval3,mystring3,charsize=csize,/normal
xyouts,xval1-0.9*xint,yval4,mystring4,charsize=csize,/normal
for nt=0,ntry-1 do begin
   oplot,pawfrw,ltdry(*,nt),color=col(nt)

    mystring=string(lt_nwbase(nt),format='(F4.2)')
    xyouts,xval1+xint*nt,yval1,mystring,/normal,color=col(nt),charsize=csize
    mystring=string(lt_nwcoef(nt),format='(F5.3)')
    xyouts,xval1+xint*nt,yval2,mystring,/normal,color=col(nt),charsize=csize
    mystring=string(lt_nwref(nt),format='(F5.3)')
    xyouts,xval1+xint*nt,yval3,mystring,/normal,color=col(nt),charsize=csize
    mystring=string(lt_nwmax(nt),format='(F5.3)')
    xyouts,xval1+xint*nt,yval4,mystring,/normal,color=col(nt),charsize=csize
endfor


if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end
