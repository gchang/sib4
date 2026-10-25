PRO SiB4_PickLonLat, nsib, lonsib, latsib, lonref, latref, ptref, printinfo

;Find the nearest point:
mydiff=999.

FOR i=0,nsib-1 DO BEGIN
    diff = ABS(lonref-lonsib(i)) + ABS(latref-latsib(i))
    IF (diff LT mydiff) THEN BEGIN
        mydiff = diff
        ptref=i
    ENDIF
ENDFOR

IF (n_elements(printinfo) EQ 0) THEN printinfo=1
IF (printinfo) THEN BEGIN
   string1='Specified Lon/Lat:   ' + string(lonref,FORMAT='(F8.3)') + $
       ' ' + string(latref,FORMAT='(F8.3)')
   string2='Model Lon/Lat:   ' + string(lonsib(ptref),FORMAT='(F8.3)') + $
       ' ' + string(latsib(ptref),FORMAT='(F8.3)')
   string3='Reference SiBPT:   ' + string(ptref+1,FORMAT='(I8)')
   EZ4_Message, [string1,string2,string3], TITLE='Specific Lat/Lon Value'
ENDIF

RETURN
END
