;Program to plot the respiration freeze inhibition.
;
;kdhaynes, 2019/03

pro sib_mrfreeze, psFile=psFile

;Parameters
numtry=4
fmul=[0.1,1.0,0.3,0.4]
fref=[274.,272.,270.,268.]
fmin=[0.3,0.4,0.4,0.5]
col=[240,200,100,30]

;Plotting information
tmin=250.

;Arrays
num=101
temp=findgen(num)/3. + tmin
tmax=max(temp)
mrfreeze=fltarr(num,numtry)

;Calculate mrfreeze
for nt=0,numtry-1 do begin
    for i=0,num-1 do begin
         mrfreeze(i,nt) = MAX([fmin(nt), MIN([1.0, $
              exp(fmul(nt)*(temp(i) - fref(nt)))])])
    endfor
endfor

;Plot
set_mrfreeze,psFile=psFile
xtitle=textoidl('T_C (K)')
ytitle=textoidl('MCR_{Freeze}')

minstress=0.
maxstress=1.1
plot,temp,mrfreeze(*,0), $
     xtitle=xtitle, ytitle=ytitle, $
     xrange=[255,280], $
     yrange=[minstress,maxstress]

xval1=0.6
xint=0.08
yval1=0.27
yval2=0.19
csize=2
mystring1=textoidl('CR_{FMul} =')
mystring2=textoidl('CR_{FRef} =')
xyouts,xval1-1.4*xint,yval1,mystring1,charsize=csize,/normal
xyouts,xval1-1.4*xint,yval2,mystring2,charsize=csize,/normal
for nt=0,numtry-1 do begin
    oplot,temp,mrfreeze(*,nt), $
          color=col(nt),thick=5
    mystring=string(fmul(nt),format='(F4.1)')
    xyouts,xval1+xint*nt,yval1,mystring,/normal,color=col(nt),charsize=csize
    mystring=string(fref(nt),format='(F4.0)')
    xyouts,xval1+xint*nt,yval2,mystring,/normal,color=col(nt),charsize=csize
endfor

if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end


pro set_mrfreeze, psFile=psFile


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
   myymargin=[3.4,1]

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



