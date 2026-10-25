pro fluxnet_modnc

sitename='DK-Ris'
varname='FC'
suffix='_0408_h.nc'

myminval=-50.
mymaxval=50.

mminval=-900
badval=-9999.

varname=''
myminval=-9999.
mymaxval=9999.
myreplace=0
filename=''
trash=''

specfile='fluxnet_modnc.txt'
openr,11,specfile
readf,11,trash
readf,11,varname
readf,11,trash
readf,11,myminval
readf,11,trash
readf,11,mymaxval
readf,11,trash
readf,11,myreplace
readf,11,trash
readf,11,filename
close,11

;Open and read file
nid = ncdf_open(filename,/write)
varid = ncdf_varid(nid,varname)
ncdf_varget,nid,varid,data
newdata=data

timeid = ncdf_varid(nid,'time')
ncdf_varget,nid,timeid,time

;Alter data
ref=where(data lt myminval)
if (ref(0) ge 0) then newdata(ref) = badval
ref=where(data gt mymaxval)
if (ref(0) ge 0) then newdata(ref) = badval

;Plot
indx = STRPOS(filename,'-',/reverse_search)
siteuc=strmid(filename,indx-2,6)
mytitleo='Original ' + siteuc + ' ' + varname
mytitlen='New ' + siteuc + ' ' + varname

set_display
!p.multi=[0,1,2]
plot,time,data,title=mytitleo,min_val=mminval
plot,time,newdata,title=mytitlen,min_val=mminval

;Overwrite
if (myreplace eq 1) then begin
   ncdf_varput,nid,varid,newdata
   print,''
   print,'Replaced Values.'
   print,''
endif

;Close file
ncdf_close,nid

end
