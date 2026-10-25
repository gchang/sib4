PRO NC_GETLINFO, filename, mynlu, $
    mynlevs, levnames, levlabel

COMMON SiBInfo_COM
WIDGET_CONTROL, /HOURGLASS

;;Open the file
ncid = EZ4_OPENNCDF(filename)

ndims = (NCDF_INQUIRE(ncid)).NDIMS
mynlevs=-1
mynlu=-1
FOR id=0,ndims-1 DO BEGIN
    NCDF_DIMINQ,ncid,id,name,size
    IF (name EQ 'nlu') THEN mynlu=size
    IF ((name EQ 'ncrop') OR $
        (name EQ 'npft')  OR $
        (name EQ 'nlev') OR $
        (name EQ 'nlevs') OR $
        (name EQ 'level')) THEN BEGIN
       mynlevs=size
     ENDIF
ENDFOR

IF (mynlu GT 1) THEN BEGIN
   levnames = strarr(mynlevs + 1)
   levnames(0) = 'ALL'
   levlabel='PFT'

   varid = NCDF_VARID(ncid,'pft_names')
   IF (varid ge 0) THEN BEGIN
       NCDF_VARGET, ncid, 'pft_names', templevnames

       FOR id=1,mynlevs DO BEGIN
           levnames(id) = string(templevnames(*,id-1))
       ENDFOR
   ENDIF ELSE BEGIN
      FOR id=1,mynlevs DO BEGIN
           levnames(id) = string(id)
       ENDFOR
   ENDELSE

ENDIF ELSE $
IF (mynlevs EQ npft) THEN BEGIN
   NCDF_VARGET, ncid, 'pft_names', templevnames
    levnames = strarr(mynlevs + 1)
    levnames(0) = 'ALL'
    FOR id=1,mynlevs DO BEGIN
        levnames(id) = string(templevnames(*,id-1))
    ENDFOR
    levlabel='PFT'
ENDIF ELSE $
IF (mynlevs EQ ncrop) THEN BEGIN
  NCDF_VARGET, ncid, 'crop_names', templevnames
  levnames = strarr(mynlevs + 1)
  levnames(0) = 'ALL'
  FOR id=1,mynlevs DO BEGIN
      levnames(id) = string(templevnames(*,id-1))
  ENDFOR
  levlabel='CROP'
ENDIF ELSE $
IF (mynlevs GT 1) THEN BEGIN
   levnames = sindgen(mynlevs+1)
   levnames(0) = 'ALL'
   levlabel='LEVEL'
ENDIF ELSE BEGIN
   levnames=''
   levlabel=''
ENDELSE

NCDF_CLOSE,ncid

END
