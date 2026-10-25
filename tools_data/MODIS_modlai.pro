pro MODIS_modlai

mminval=0
badval=-9999.9

;---
;get input information
sitename=''
dooffset=0
dooverwrite=0
offset=0.0
thalf=0.0
numdaysz=0.0
pyrstart=0
pyrstop=0

specfile = 'MODIS_modlai.txt'
openr,11,specfile
readf,11,sitename,format=('(a6)')
readf,11,dooffset
readf,11,dooverwrite
readf,11,offset
readf,11,thalf
readf,11,numdaysz
readf,11,pyrstart
readf,11,pyrstop
close,11

print,format=('(2a)'),   'Processing Site: ',sitename
print,format=('(a,2i6)'),'  Years: ',pyrstart,pyrstop

filename1='/Users/kdhaynes/Dropbox/datas/lai_data/proc/MOD15A2_'+sitename+'.nc'
filename2='/Users/kdhaynes/Dropbox/datas/lai_data/proc/MYD15A2_'+sitename+'.nc'
;filename1='/Users/kdhaynes/Desktop/MOD15A2_'+sitename+'.nc'
;filename2='/Users/kdhaynes/Desktop/MYD15A2_'+sitename+'.nc'


;---------------------------------
;open files, get LAI
nid1 = ncdf_open(filename1,/write)
nid2 = ncdf_open(filename2,/write)

varid1 = ncdf_varid(nid1,'lai')
ncdf_varget,nid1,varid1,data1

varid2 = ncdf_varid(nid2,'lai')
ncdf_varget,nid2,varid2,data2

;--------------
if (dooffset eq 3) then begin
   tid1 = ncdf_varid(nid1,'time')
   ncdf_varget,nid1,tid1,time1
   time1dec=time1-floor(time1)
   href=where(time1dec gt offset)
   ;gref=where(data1 lt mminval)
   ;data1(gref) = 0.

   data1temp=data1
   data1(href)=0.

   tid2 = ncdf_varid(nid2,'time')
   ncdf_varget,nid2,tid2,time2
   time2dec=time2-floor(time2)
   href=where(time2dec gt offset)
   ;gref=where(data2 lt mminval)
   ;data2(gref) = 0.

   data2temp=data2
   data2(href)=0.

   set_display
   !p.multi=[0,1,2]

   plot,time1,data1temp,min_val=mminval,/xstyle,/ystyle
   oplot,time1,data1, min_val=mminval, color=240

   plot,time2,data2temp,min_val=mminval,/xstyle,/ystyle
   oplot,time2,data2, min_val=mminval, color=240

   if (dooverwrite eq 1) then begin
      ncdf_varput,nid1,varid1,data1
      ncdf_varput,nid2,varid2,data2
      print,'Values Replaced.'
   endif

endif 

if (dooffset eq 2) then begin
   data1temp=data1
   data1=data1temp*offset
   data1(where(data1temp le mminval)) = badval

   data2temp=data2
   data2=data2temp*offset
   data2(where(data2temp le mminval)) = badval

   set_display
   !p.multi=[0,1,2]

   tid1 = ncdf_varid(nid1,'time')
   ncdf_varget,nid1,tid1,time1
   gref=where(data1temp gt mminval)
   plot,time1(gref),data1temp(gref), $
       /xstyle,/ystyle
   oplot,time1,data1, min_val=mminval, color=240

   tid2 = ncdf_varid(nid2,'time')
   ncdf_varget,nid2,tid2,time2
   gref=where(data2temp gt mminval)
   plot,time2(gref),data2temp(gref), $
       /xstyle,/ystyle
   oplot,time2,data2, min_val=mminval, color=240


   if (dooverwrite eq 1) then begin
      ncdf_varput,nid1,varid1,data1
      ncdf_varput,nid2,varid2,data2
      print,'Values Replaced.'
   endif

endif 

if (dooffset eq 1) then begin
   ;remove offset
   data1temp=data1
   data1 -= offset
   data1(where(data1 le mminval)) = badval

   data2temp=data2
   data2 -= offset
   data2(where(data2 le mminval)) = badval

   set_display
   !p.multi=[0,1,2]

   tid1 = ncdf_varid(nid1,'time')
   ncdf_varget,nid1,tid1,time1
   gref=where(data1temp gt mminval)
   plot,time1(gref),data1temp(gref), $
       /xstyle,/ystyle
   oplot,time1,data1, min_val=mminval, color=240

   tid2 = ncdf_varid(nid2,'time')
   ncdf_varget,nid2,tid2,time2
   gref=where(data2temp gt mminval)
   plot,time2(gref),data2temp(gref), $
       /xstyle,/ystyle
   oplot,time2,data2, min_val=mminval, color=240


   if (dooverwrite eq 1) then begin
      ncdf_varput,nid1,varid1,data1
      ncdf_varput,nid2,varid2,data2
      print,'Values Replaced.'
   endif
endif

if (dooffset eq 0) then begin
   ;remove jumpy points
   tid1 = ncdf_varid(nid1,'time')
   ncdf_varget,nid1,tid1,time1
   numt=n_elements(time1)

   zerocount=0
   crefstart=-999
   crefstop=-999
   for t=0,numt-1 do begin
       if (data1(t) eq 0.) then begin
          zerocount++
          if (crefstart lt 0) then crefstart=t
          crefstop=t
       endif else begin
          if ((zerocount gt 0) and (zerocount lt numdaysz)) then begin
             data1(crefstart:crefstop) = badval
          endif
          zerocount=0
          crefstart=-999
          crefstop=-999
       endelse
    endfor

   gref=where(data1 gt mminval)
   numt=n_elements(gref)

   mttime=time1(gref)
   mttime(numt-1) += 0.001
   mtdata=data1(gref)
   pull_spline, mttime, mtdata, timefit, datafit, $
       THALF=thalf, NC=2
   set_display
   !p.multi=[0,1,2]

   plot,mttime,mtdata,min_val=badval, $
       yrange=[min(mtdata),max(mtdata)], $
       xrange=[pyrstart,pyrstop+1], $
       /xstyle,/ystyle,title=sitename
   oplot,timefit,datafit,color=240
   data1(gref) = datafit(*)

   tid2 = ncdf_varid(nid2,'time')
   ncdf_varget,nid2,tid2,time2
   numt=n_elements(time2)

   zerocount=0
   crefstart=-999
   crefstop=-999
   for t=0,numt-1 do begin
       if (data2(t) eq 0.) then begin
          zerocount++
          if (crefstart lt 0) then crefstart=t
          crefstop=t
       endif else begin
          if ((zerocount gt 0) and (zerocount lt 10)) then begin
             data2(crefstart:crefstop) = badval
          endif
          zerocount=0
          crefstart=-999
          crefstop=-999
       endelse
    endfor
   gref=where(data2 gt mminval)
   numt=n_elements(gref)

   mttime=time2(gref)
   mttime(numt-1)+=0.01
   mtdata=data2(gref)
   pull_spline, mttime, mtdata, timefit, datafit, $
       THALF=thalf, NC=2

   plot,mttime,mtdata,min_val=badval, $
       yrange=[0.,max(mtdata)], $
       xrange=[pyrstart,pyrstop+1],/xstyle,/ystyle
   oplot,timefit,datafit,color=240
   data2(gref) = datafit(*)

   if (dooverwrite eq 1) then begin
       ncdf_varput,nid1,varid1,data1
       ncdf_varput,nid2,varid2,data2
       print,'Values Replaced.'
    endif
endif


ncdf_close,nid1
ncdf_close,nid2

end
