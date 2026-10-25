PRO GEO4_PickLonLat, plot

ok = GEO4_GetLonLat(newlon,newlat)

;Find the nearest longitude:
londiff=999.
lonref=-999
FOR i=0,plot.nlon-1 DO BEGIN
    diff = ABS(plot.lon(i) - newlon)
    IF (diff LT londiff) THEN BEGIN
       londiff = diff
       lonref = i
    ENDIF
ENDFOR

;Find the nearest latitude:
latdiff=999.
latref=-999
FOR i=0,plot.nlat-1 DO BEGIN
    diff = ABS(plot.lat(i) - newlat)
    IF (diff LT latdiff) THEN BEGIN
       latdiff = diff
       latref = i
    ENDIF
ENDFOR

string1='Specified Lon/Lat:   ' + string(newlon,FORMAT='(F8.3)') + $
    ' ' + string(newlat,FORMAT='(F8.3)')
string2='Model Lon/Lat:   ' + string(plot.lon(lonref),FORMAT='(F8.3)') + $
    ' ' + string(plot.lat(latref),FORMAT='(F8.3)')
string4=''
string5='Model Value: ' + $
    string(plot.data(lonref,latref),FORMAT='(F10.3)')

EZ4_Message, [string1,string2,string4,string5], TITLE='Specific Lat/Lon Value'


END
