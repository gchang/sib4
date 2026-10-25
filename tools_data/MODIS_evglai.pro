pro MODIS_evglai

mminval=0
badval=-9999.9

;---
;get input information
sitename=''
pyrstart=0
pyrstop=0
varallow=0.0
dooverwrite=0

specfile = 'MODIS_evglai.txt'
openr,11,specfile
readf,11,sitename,format=('(a6)')
readf,11,pyrstart
readf,11,pyrstop
readf,11,dooverwrite
readf,11,varallow
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

tid1 = ncdf_varid(nid1,'time')
ncdf_varget,nid1,tid1,time1
varid1 = ncdf_varid(nid1,'lai')
ncdf_varget,nid1,varid1,data1

tid2 = ncdf_varid(nid2,'time')
ncdf_varget,nid2,tid2,time2
varid2 = ncdf_varid(nid2,'lai')
ncdf_varget,nid2,varid2,data2

;--------------

;calculate mean annual maximum
dmax1=0.
dmax2=0.
count1=0
count2=0
for yr=pyrstart,pyrstop do begin
    yrref = where(floor(time1) eq yr)
    amax = max(data1(yrref))
    if (amax gt 0.) then begin
       dmax1 += amax
       count1++
    endif

    yrref = where(floor(time2) eq yr)
    amax = max(data2(yrref))
    if (amax gt 0.) then begin
        dmax2 += amax
        count2++
    endif
endfor
if (count1 gt 0) then datamax1 = dmax1/count1 $
    else datamax1=0.
if (count2 gt 0) then datamax2 = dmax2/count2 $
    else datamax2=0.
print,'  Mean Annual Maximums: ',datamax1, datamax2

;save values above threshold
valsave1 = datamax1*(1.-varallow)
chgref = where(data1 lt valsave1)
data1(chgref) = valsave1

valsave2 = datamax2*(1.-varallow)
chgref = where(data2 lt valsave2)
data2(chgref) = valsave2

;plot values
set_display
!p.multi=[0,1,2]

mymin = 0.
mymax = max([max(data1),max(data2)])
plot,time1,data1,min_val=badval, $
    yrange=[mymin,mymax], $
    xrange=[pyrstart,pyrstop+1], $
    /xstyle,/ystyle,title=sitename

plot,time2,data2,min_val=badval, $
    yrange=[mymin,mymax], $
    xrange=[pyrstart,pyrstop+1], $
    /xstyle,/ystyle,title=sitename

;replace, if requested
if (dooverwrite eq 1) then begin
     ncdf_varput,nid1,varid1,data1
     ncdf_varput,nid2,varid2,data2
     print,'Values Replaced.'
endif

;close files
ncdf_close,nid1
ncdf_close,nid2

end
