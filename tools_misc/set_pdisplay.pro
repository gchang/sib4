;Basic program to set up graphics for IDL.
;
;kdhaynes, 01/17

pro set_pdisplay, psFile=psFile


if (n_elements(psFile) EQ 0) THEN BEGIN

   myheight=500
   myratio=1.2
   mywidth=myheight*myratio 
  
   mycsize=2.5
   mycthick=2.0
   myxyzthick=3.
   mypthick=2.

   myxmargin=[7,3]
   myymargin=[3.5,2]

   set_plot,'x'
   !p.font=-1
   device,decompose=0,true_color=24,retain=2
   window,0,xsize=mywidth,ysize=myheight

ENDIF ELSE BEGIN
   myheight=5.5
   myratio=1.4
   mywidth=myheight*myratio
   myfont=8

   mycsize=2.4
   mycthick=2.4
   myxyzthick=3.
   mypthick=3.

   myxmargin=[7,3]
   myymargin=[4,3]

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
