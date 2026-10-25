;Program to plot the hot temperature respiration exponential.
;
;Depends on temperature using a Q10 relationship.
;
;kdhaynes, 2019/03

pro sib_mrhot, psFile=psFile

;Parameters
ntry=6
hq10=[  2.0,  2.4,  2.8,  2.0,  2.4, 1.6]
htref=[300., 304., 308., 308., 311., 293.]
hmax1=3.
hmax=fltarr(ntry)+hmax1
col=[240,200,120,90,55,20]


;Plotting information
tmin=260.
tmax=330.

pxval=0.80
pyval=0.40
pyint=0.04
if (n_elements(psFile) gt 0) then begin
    pcsize=2.0
endif else begin
    pcsize=1.0
endelse

;Arrays
num=101
temp=findgen(num) + 240.
mrhot=fltarr(num,ntry) + 1.

;Calculate
for nt=0,ntry-1 do begin
     for i=0,num-1 do begin
         qt = 0.1*(temp(i) - htref(nt))
         hot_temp = hq10(nt)^qt

         if (temp(i) gt htref(nt)) then begin
             mrhot(i,nt) = MAX([0.0, MIN([hmax(nt), $
                     hot_temp])])
         endif
    endfor
endfor

;Plot
set_mrdisplay,psFile=psFile
xtitle=textoidl('T_C (K)')
ytitle=textoidl('MCR_{Hot}')

minstress=0.
maxstress=max(hmax)*1.1
plot,temp,mrhot(*,0), $
     xtitle=xtitle, ytitle=ytitle, $
     xrange=[290,330], $
     yrange=[0.6,3.8], /xstyle, /ystyle, $
     yminor=1

xval1=0.3
xint=0.08
yval1=0.88
yval2=0.80
csize=2
mystring1=textoidl('CR_{HQ10} =')
mystring2=textoidl('CR_{HRef} =')
xyouts,xval1-1.4*xint,yval1,mystring1,charsize=csize,/normal
xyouts,xval1-1.4*xint,yval2,mystring2,charsize=csize,/normal
for nt=0,ntry-1 do begin
    oplot,temp,mrhot(*,nt), $
          color=col(nt)
    mystring=string(hq10(nt),format='(F4.1)')
    xyouts,xval1+xint*nt,yval1,mystring,/normal,color=col(nt),charsize=csize
    mystring=string(htref(nt),format='(F4.0)')
    xyouts,xval1+xint*nt,yval2,mystring,/normal,color=col(nt),charsize=csize
endfor


if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end


pro set_mrdisplay, psFile=psFile


if (n_elements(psFile) EQ 0) THEN BEGIN

   myheight=500
   myratio=1.4
   mywidth=myheight*myratio 
  
   mycsize=2.5
   mycthick=2.0
   myxyzthick=3.
   mypthick=2.

   myxmargin=[6,2]
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
   mypthick=8.

   myxmargin=[6.2,2]
   myymargin=[3.5,1]

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


