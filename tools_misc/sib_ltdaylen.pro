;Program to plot the daylength transfer scaling factor.
;
;Depends on daylength, 
;using an equation of the form:
;    y=MAX(dlmin, MIN(0.0, 
;           dlcoef(daylenmax-daylen)*(daylenmax-dlref)
;
;where daylenmax is the maximum daylength at the site
;   (depends on latitude)
;
;kdhaynes, 2019/03

function rt_asc, ang

eccn = 0.016715  ;eccentricity
perhl = 102.7    ;factor for zenith angle

reccn = 1./(1.-eccn*eccn) ^ 1.5
perhlr = perhl * (3.15159/180.)

rt_asc = reccn * (1-eccn * cos(ang - perhlr))^2

return, rt_asc

end

pro sib_ltdaylen, psFile=psFile

;Parameters
lat=52.

ntry=5
dlmax=[0.02, 0.01, 0.03, 0.05, 0.08]
dlcoef=[0.0003, 0.001, 0.0006, 0.0005, 0.0008]
dlref=[-3., -1.4, 14.0, -6., -0.4]
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
dlength=fltarr(numd)
dlengthdt=fltarr(numd)
atdaylen=fltarr(numd,ntry)


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
   if ((lat gt -90) and (lat lt 90)) then begin
       tan_lat = tan(pi180*lat)
   endif else begin
       tan_lat = 0.
   endelse

   cos_hr = -tan_lat * tan_dec
   if (cos_hr lt -1.) then begin
       dlength(t) = 24.
   endif else begin
      if (cos_hr gt 1.0) then begin
          dlength(t) = 0.
      endif else begin
          if (t gt 0) then begin
               pdlength = dlength(t-1)
          endif else begin
               pdlength = (acos(cos_hr)/pi180*24.)/180.
          endelse
          dlength(t) = (acos(cos_hr)/pi180*24.)/180.
       endelse
   endelse

   if (t gt 0) then begin
      dlengthdt(t-1) = dlength(t)-dlength(t-1)
   endif
endfor
maxdaylen = max(dlength)
print,format='(a,f6.2)','Daylength For Latitude: ',lat
print,format='(a,f6.2)','Maximum Daylength     : ',maxdaylen

;if (plotdlength eq 1) then begin
;   set_pdisplay
;   plot,doy,dlength, $
;      /xstyle, /ystyle, $
;       xtitle='DOY', ytitle='Daylength (hrs)'

;   print,'Press any key to continue...'
;   ok = get_kbrd()
;endif


;Calculate atdaylen-------
for nt=0,ntry-1 do begin
    for i=0,numd-1 do begin
        if (dlengthdt(i) lt 0.) then begin
            atdaylen(i,nt) = MAX([0.0, MIN([dlmax(nt), $
                    dlcoef(nt)*(maxdaylen-dlength(i))*(maxdaylen-dlref(nt))])])
        endif else begin
            atdaylen(i,nt) = 0.0
        endelse
     endfor
endfor

;Plot
set_pdisplay,psFile=psFile
mytitle=''  ;Day Length Leaf Transfer'
xtitle='DOY'
ytitle='Day Length (hr)'
ytitle2=textoidl('T_D (Fraction Per Day)')  ;textoidl('Fraction Per Day')

!y.margin=[3,1]
!x.margin=[8,7]
if (n_elements(psFile) ne 0) then begin
   csize=2.5
endif else csize=1.6

minstress=0.0
maxstress=MAX(dlmax)*1.1
plot, doy, atdaylen(*,0), $
     yrange=[minstress,maxstress], $
     xstyle=1, ystyle=8, title=mytitle, $
     xtitle=xtitle, ytitle=ytitle2, charsize=csize

xval1=0.27
xint=0.11
yint=0.044
yval1=0.92
yval2=yval1-yint
yval3=yval2-yint
ccsize=1.7
mystring1=textoidl('LT_{DC}=')
mystring2=textoidl('LT_{DMax}= ')
mystring3=textoidl('LT_{DR}=')
xyouts,xval1-0.9*xint,yval1,mystring1,charsize=ccsize,/normal
xyouts,xval1-0.9*xint,yval2,mystring2,charsize=ccsize,/normal
xyouts,xval1-0.9*xint,yval3,mystring3,charsize=ccsize,/normal

lstring='Lat= ' + string(lat,format='(I2)') + 'N'
xyouts,xval1-0.9*xint,0.15,lstring,charsize=ccsize,/normal
for nt=0,ntry-1 do begin
    oplot,doy,atdaylen(*,nt), $
          color=col(nt)

    mystring=string(dlcoef(nt),format='(F7.4)')
    xyouts,xval1+xint*nt,yval1,mystring,/normal,color=col(nt),charsize=ccsize

    mystring=string(dlmax(nt),format='(F4.2)')
    xyouts,xval1+xint*nt,yval2,mystring,/normal,color=col(nt),charsize=ccsize

    mystring=string(dlref(nt),format='(F4.1)')
    xyouts,xval1+xint*nt,yval3,mystring,/normal,color=col(nt),charsize=ccsize
endfor

if (plotdlength eq 1) then begin
   mymin=min(dlength)*0.9
   mymax=max(dlength)*1.05
   plot, doy, dlength, /noerase, $
         yrange=[mymin,mymax], $
         xstyle=1,ystyle=4, charsize=csize
   axis, yaxis=1, yrange=[mymin,mymax],ytitle=ytitle,charsize=csize
endif


if (n_elements(psFile) ne 0) then begin
   device,/close
   set_plot,'X'
   !p.font=-1
endif

end


