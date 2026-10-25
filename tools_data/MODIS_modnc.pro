pro MODIS_modnc

sitename='BR-Ji1'
dooverwrite=0  ;1=overwrite values

timermstart=2004.8
timermstop=2005.2

pyrstart=2000
pyrstop=2014
mminval=0
badval=-9999.9

filename1='/Users/kdhaynes/Dropbox/datas/lai_data/proc/MOD15A2_'+sitename+'.nc'
filename2='/Users/kdhaynes/Dropbox/datas/lai_data/proc/MYD15A2_'+sitename+'.nc'
;filename1='/Users/kdhaynes/Desktop/MOD15A2_'+sitename+'.nc'
;filename2='/Users/kdhaynes/Desktop/MYD15A2_'+sitename+'.nc'


;---------------------------------
;open files, get time and LAI
nid1 = ncdf_open(filename1,/write)
nid2 = ncdf_open(filename2,/write)

varid1 = ncdf_varid(nid1,'time')
ncdf_varget,nid1,varid1,time1
varid1 = ncdf_varid(nid1,'lai')
ncdf_varget,nid1,varid1,data1

varid2 = ncdf_varid(nid2,'time')
ncdf_varget,nid2,varid2,time2
varid2 = ncdf_varid(nid2,'lai')
ncdf_varget,nid2,varid2,data2

;--------------
;remove specific chunk
dref=where((time1 ge timermstart) and $
           (time1 le timermstop)) 
data1(dref) = badval

dref=where(data1 gt 3.4)
data1(dref) = badval

dref=where((time2 ge timermstart) and $
           (time2 le timermstop))
data2(dref) = badval

dref=where(data2 gt 3.4)
data2(dref) = badval

;plot changes
mtref=where(data1 gt mminval)
mymin=min(data1(mtref))
mymax=max(data1(mtref))*1.1

set_display
plot,time1,data1, min_val=-900, $
       yrange=[0.,mymax], $
       xrange=[pyrstart,pyrstop+1],/xstyle,/ystyle

goodtref=where(time2 gt mminval)
gt2=time2(goodtref)
gd2=data2(goodtref)
oplot,gt2,gd2,min_val=-900,color=240


;replace values in file
if (dooverwrite eq 1) then begin
       ncdf_varput,nid1,varid1,data1
       ncdf_varput,nid2,varid2,data2
       print,'Values Replaced.'
endif


ncdf_close,nid1
ncdf_close,nid2

end
