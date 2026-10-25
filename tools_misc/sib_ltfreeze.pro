;Program to plot the cold/freeze transfer scaling factor.
;
;Depends on temperature using a Q10 relationship.
;
;kdhaynes, 2019/03

pro sib_ltfreeze, psFile=psFile

;Parameters
ntry=5
cq10 = [2.2,2.2,2.0,1.8,1.6]
ctref= [278.,274.,270.,268.,272.]
col = [240,200,100,40,140]
cmax = 0.1

;Plotting information
if (n_elements(psFile) gt 0) then begin
    pcsize=2.0
endif else begin
    pcsize=1.6
endelse

;Arrays
num=20.
temp=findgen(num) + 260.
atfreeze=fltarr(num,ntry)

;Calculate atfreeze
for nt=0,ntry-1 do begin
    for i=0,num-1 do begin
        if (temp(i) lt ctref(nt)) then begin
            qt = 0.01*(ctref(nt)-temp(i))
            atfreeze(i,nt) = min([cmax, cq10(nt)^qt - 1.])
        endif else begin
            atfreeze(i,nt) = 0.
        endelse           
    endfor
endfor

;Plot
set_pdisplay,psFile=psFile
mytitle='' ;'Leaf Freeze Transfer'
xtitle='Temperature (K)'
ytitle=textoidl('T_F (Fraction Per Day)')

!x.margin=[8,2]
!y.margin=[3,1]
minstress=min(atfreeze)*0.95
maxstress=max(atfreeze)*1.05
plot,temp,atfreeze(*,0), title=mytitle, $
     xtitle=xtitle, ytitle=ytitle, $
     yrange=[minstress,maxstress]


xval1=0.54
xint=0.08
yval1=0.88
yval2=0.80
csize=2

mystring1=textoidl('LT_{FQ10} =')
mystring2=textoidl('LT_{FR} =')
xyouts,xval1-1.4*xint,yval1,mystring1,charsize=csize,/normal
xyouts,xval1-1.4*xint,yval2,mystring2,charsize=csize,/normal
for nt=0,ntry-1 do begin
    oplot,temp,atfreeze(*,nt), $
          color=col(nt)
    mystring=string(cq10(nt),format='(F4.1)')
    xyouts,xval1+xint*nt,yval1,mystring,/normal,color=col(nt),charsize=csize
    mystring=string(ctref(nt),format='(F4.0)')
    xyouts,xval1+xint*nt,yval2,mystring,/normal,color=col(nt),charsize=csize
endfor

if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end
