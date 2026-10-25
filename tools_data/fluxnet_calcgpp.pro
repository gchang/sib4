;Program to calculate annual GPP from Fluxnet data.
;
;kdhaynes, 04/16

pro fluxnet_calcgpp

;Variable information
gppname='GPP_DT_VUT_REF'
reconame='RECO_DT_VUT_REF'
neename='NEE_VUT_REF'

;Misc information
dayspermon=[31,28,31,30,31,30,31,31,30,31,30,31]
specfile='fluxnet_calcgpp.txt'

;Read input information
trash=''
filename=''
yrstart=0
yrstop=0

close,11
openr,11,specfile
readf,11,trash
readf,11,filename
close,11

;;;Site Name:
namepos=strpos(filename,'/',/reverse_search)-6
sitename = strmid(filename,namepos,6)
print,''
print,'Processing Site: ',sitename

yrpos=strpos(filename,'/',/reverse_search)+5
lyrstart=fix(strmid(filename,yrpos,2))
IF (lyrstart LT 16) THEN yrstart=lyrstart+2000. $
   ELSE yrstart=lyrstart+1900.
lyrstop=fix(strmid(filename,yrpos+2,2))
IF (lyrstop LT 16) THEN yrstop=lyrstop+2000. $
   ELSE yrstop=lyrstop+1900.
print,'  Years Available: ',yrstart,yrstop

ftpos=strpos(filename,'_',/reverse_search)+1
sitetres = strmid(filename,ftpos,1)

;Set up SiB file-specific information
CASE sitetres of
   'h': BEGIN
      convert=36.*12./1.E6  ;convert from umol/m2/s to Mg/ha/yr
      dconvert=0.12/24.    ;convert from mol/m2/day to Mg/ha/yr
      numtperyr=365.*24.
   END
   'm': BEGIN
      convert=864.*12./1.E6 ;convert from umol/m2/s to Mg/ha/yr
      dconvert=0.12    ;convert from mol/m2/day to Mg/ha/yr
      numtperyr=12.
   END
   'd': BEGIN
     convert=864.*12./1.E6 ;convert from umol/m2/s to Mg/ha/yr
     dconvert=0.12  ;convert from mol/m2/day to Mg/ha/yr
     numtperyr=365.
   END
   ELSE: BEGIN
     print,'Unexpected File Type.'
     stop
  END
ENDCASE

;Read data
nid = ncdf_open(filename,/nowrite)

varid = ncdf_varid(nid,gppname)
IF (varid LT 0) THEN BEGIN
   print,'GPP not available. Stopping.'
   RETURN
ENDIF
ncdf_varget,nid,varid,gpp

varid=ncdf_varid(nid,reconame)
IF (varid LT 0) THEN BEGIN
   print,'RECO not available.  Stopping.'
   RETURN
ENDIF
ncdf_varget,nid,varid,re

ncdf_close,nid

;Process data
count=0L
numyrs=yrstop-yrstart+1
mynumtperyr=numtperyr

countstart=0L
countstop=0L

gpptot=fltarr(numyrs)
retot=fltarr(numyrs)
for yr=yrstart,yrstop do begin
    mynumtperyr=numtperyr
    mydayspermon=dayspermon
    if ((yr mod 4) eq 0) then begin
        mydayspermon(1) += 1
        if (numtperyr eq 365.) then mynumtperyr+=1
        if (numtperyr eq 365.*24.) then mynumtperyr+=24.
    endif
    countstop=countstart+mynumtperyr 
    for it=countstart+1,countstop do begin
         myconvert = convert
         mydconvert = dconvert
         IF (mynumtperyr EQ 12) THEN BEGIN
             myconvert = convert * mydayspermon(it-(countstart+1))
             mydconvert = dconvert * mydayspermon(it-(countstart+1))
         ENDIF
         gpptot(yr-yrstart) += MAX([0.,gpp(count)*myconvert])
         retot(yr-yrstart)  += MAX([0.,re(count)*myconvert])
         count++
    endfor
    countstart=countstop+1
endfor ;yr=yrstart,yrstop

;Print the stocks:
print,''
print,'---------------------------------------'
print,'         GPP       RE      (Mg C/ha/yr)'
print,format='(a,2f10.3)','Mean', $
     total(gpptot)/numyrs,total(retot)/numyrs
print,''
print,'Year     GPP       RE         NEE'
for t=0,numyrs-1 do begin
   print,format='(i4,3f10.3)',yrstart+t,gpptot(t),retot(t),retot(t)-gpptot(t)
endfor


end
