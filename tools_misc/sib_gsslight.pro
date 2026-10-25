;Program to plot the daylength phenology stage potential.
;
;Depends on daylength and daylenmax, 
;   the maximum daylength at the site
;   (depends on latitude)
;
;kdhaynes, 07/17

pro set_pfddisplay, psFile=psFile


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
   mypthick=4.

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

;--------------------
function rt_asc, ang

eccn = 0.016715  ;eccentricity
perhl = 102.7    ;factor for zenith angle

reccn = 1./(1.-eccn*eccn) ^ 1.5
perhlr = perhl * (3.15159/180.)

rt_asc = reccn * (1-eccn * cos(ang - perhlr))^2

return, rt_asc
end

;--------------------------------
pro sib_gsslight, psFile=psFile

;Parameters
nlat=1
lat=[40,20,50,72]
LSTYLE=[0,2,4]

ntry=1
daymini=[24.0]
dayoffd=[-13.0]
COL=[240,200,120,80,40]
plotdlength=1  ;1=plot day length

;Constants
pi180 = 3.14159/180.
pidaypy = 0.0172142
eqnx=80
decmax=23.441

;Arrays
numd=365
doy=findgen(numd)
dlength=fltarr(numd,nlat)
dlengthdt=fltarr(numd,nlat)
phengss_light=fltarr(numd,ntry,nlat)


;Calculate day lengths-----
;...find initial solar declination
iday = doy(0) - eqnx
if (iday lt 0) then iday=iday+numd
lonearth = 0.

if (iday ne 0) then begin
   while (iday gt 0) do begin
      iday = iday - 1
      t1 = rt_asc(lonearth) * pidaypy
      t2 = rt_asc(lonearth+t1*0.5) * pidaypy
      t3 = rt_asc(lonearth+t2*0.5) * pidaypy
      t4 = rt_asc(lonearth+t3) * pidaypy
      lonearth = lonearth + (t1+2.*(t2+t3)+t4) / 6.
   endwhile
endif

;...loop over all days
dsave=-999.
for t=0,numd-1 do begin
   ;increment longitude of Earth from equinox
   if (doy(t) eq eqnx) then lonearth = 0.0
   t1 = rt_asc(lonearth) * pidaypy
   t2 = rt_asc(lonearth+t1*0.5) * pidaypy
   t3 = rt_asc(lonearth+t2*0.5) * pidaypy
   t4 = rt_asc(lonearth+t3) * pidaypy
   lonearth = lonearth + (t1+2.*(t2+t3)+t4) / 6.

   ;calculate sin and cos of solar declination
   sin_dec = sin(decmax*pi180)*sin(lonearth)
   cos_dec = sqrt(1.-sin_dec*sin_dec)
   if (cos_dec ne 0.) then begin
       tan_dec = sin_dec / cos_dec
   endif else begin
      tan_dec = 0.
   endelse

   ;calculate length of day
   for l=0,nlat-1 do begin
      if ((lat(l) gt -90) and (lat(l) lt 90)) then begin
           tan_lat = tan(pi180*lat(l))
      endif else begin
           tan_lat = 0.
      endelse

      cos_hr = -tan_lat * tan_dec
      if (cos_hr lt -1.) then begin
          dlength(t,l) = 24.
      endif else begin
         if (cos_hr gt 1.0) then begin
             dlength(t,l) = 0.
         endif else begin
             if (t gt 0) then begin
                  pdlength = dlength(t-1,l)
             endif else begin
                  pdlength = (acos(cos_hr)/pi180*24.)/180.
             endelse
             dlength(t,l) = (acos(cos_hr)/pi180*24.)/180.
         endelse
      endelse

      if (t gt 0) then begin
         dlengthdt(t-1,l) = dlength(t,l)-dlength(t-1,l)
      endif
   endfor
endfor

maxdaylen=fltarr(nlat)
for l=0,nlat-1 do begin
   maxdaylen(l) = max(dlength(*,l))
   print,''
   print,format='(a,f6.2)','Daylength For Latitude: ',lat(l)
   print,format='(a,f6.2)','Maximum Daylength     : ',maxdaylen(l)
endfor

;if (plotdlength eq 1) then begin
;   set_pdisplay
;   plot,doy,dlength, $
;      /xstyle, /ystyle, $
;       xtitle='DOY', ytitle='Daylength (hrs)'

;   print,'Press any key to continue...'
;   ok = get_kbrd()
;endif


;Calculate phengss_light-------
for nl=0,nlat-1 do begin
    for nt=0,ntry-1 do begin
       for i=0,numd-1 do begin
          if (dlengthdt(i,nl) ge 0.) then begin
             phengss_light(i,nt,nl) = min([1.0, max([0., $
                                dlength(i,nl)-daymini(nt)])])
          endif else begin
             if (dayoffd(nt) gt 0.) then begin
                if ((maxdaylen(nl) - dlength(i,nl)) lt dayoffd(nt)) then $
                   phengss_light(i,nt,nl) = 1
             endif else begin
                if (dlength(i,nl) lt abs(dayoffd(nt))) then $
                   phengss_light(i,nt,nl) = 1
             endelse
          endelse
      endfor
  endfor
endfor

;Plot
set_pfddisplay,psFile=psFile
xtitle='DOY'
ytitle='Day Length (hr)'
ytitle2=textoidl('GSS Light')

!y.margin=[3.3,1]
!x.margin=[7,5]
if (n_elements(psFile) ne 0) then begin
   csize=2.5
endif else csize=2.0
cthick=2.

doystart=1
doystop=360

minstress=0.0
maxstress=1.1
plot, doy, phengss_light(*,0,0), $
     yrange=[minstress,maxstress], $
     xrange=[doystart,doystop], $
     xstyle=1, ystyle=8, $
     xtitle=xtitle, ytitle=ytitle2, $
     charsize=csize, /nodata

for nl=0,nlat-1 do begin
    for nt=0,ntry-1 do begin
         oplot, doy, phengss_light(*,nt,nl), $
             color=col(nt),linestyle=lstyle(nl)
      endfor
endfor

if (plotdlength eq 1) then begin
   dlengthf = dlength(*,0)
   mymin=min(dlength)*0.9
   mymax=max(dlength)*1.05
   plot, doy, dlengthf, /noerase, /nodata, $
         yrange=[mymin,mymax], $
         xrange=[doystart,doystop], $
         xstyle=1,ystyle=4, charsize=csize
   axis, yaxis=1, yrange=[mymin,mymax],ytitle=ytitle,charsize=csize*0.8

   for nl=0,nlat-1 do begin
       oplot,doy, dlength(*,nl), linestyle=lstyle(nl)
   endfor
endif

if (n_elements(psFile) eq 0) THEN BEGIN
   xvalc=0.2
   xvalv=0.29
   xint=0.09
   yvalc=0.24
   yvalv=0.24
   csize=1.6

   xvald=0.2
   xvalw=0.29
   yvald=0.20
   yvalw=0.20
endif ELSE BEGIN
   xvalc=0.26
   xvalv=0.36
   xint=0.08
   yvalc=0.28
   yvalv=0.28
   csize=1.8

   xvald=0.26
   xvalw=0.36
   yvald=0.20
   yvalw=0.20
ENDELSE

if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end



