pro sib4_getgrid, numpts, lonsib, latsib, $
                  nlon, nlat, lon, lat, lonref, latref

  IF ((n_elements(lonsib) NE numpts) OR $
      (n_elements(latsib) NE numpts)) THEN BEGIN
     ez4_alert,'Error Gridding SiB4 Lat/Lon.'
     RETURN
  ENDIF

  ;;Latitude grid information
  lattest=latsib
  latmin=MIN(lattest)
  latmax=MAX(lattest)
  deltalat=999.

  IF (numpts gt 3) THEN BEGIN
     deltalat = ABS(latsib(3)-latsib(2))
  ENDIF ELSE BEGIN
     ez4_alert,'Insufficient Lat Points For Grid.'
     RETURN
  ENDELSE
  
  truelatmin=latmin
  truelatmax=latmax
  nlat = (latmax-latmin)/deltalat + 1
  
  lat = FLTARR(nlat)
  FOR i=1,nlat DO BEGIN
     lat(i-1) = latmin + deltalat*(i-1)
  ENDFOR

  ;;Longitude grid information
  lontest=lonsib
  lonmin=MIN(lontest)
  lonmax=MAX(lontest)
  deltalon=999.

  IF (numpts gt 3) THEN BEGIN
     minref = where(lontest eq lonmin)
     lontest(minref) = 999.
     newlonmin = min(lontest)
     deltalon = ABS(newlonmin - lonmin)
     lontest = lonsib
  ENDIF ELSE BEGIN
     ez4_alert,'Insufficient Lon Points For Grid.'
     RETURN
  ENDELSE

  truelonmin=lonmin
  truelonmax=lonmax
  nlon = ABS(lonmax-lonmin)/deltalon+1

  lon = FLTARR(nlon)
  FOR i=1,nlon DO BEGIN
     lon(i-1) = lonmin+ deltalon*(i-1)
  ENDFOR

  ;;Create the gridded longitude and latitude
  latref=fltarr(numpts)-999
  lonref=fltarr(numpts)-999
  for i=0,numpts-1 do begin
     testlat=0
     x=0
     WHILE(testlat eq 0) DO BEGIN
        IF (x eq nlat-1) THEN BEGIN
           testlat=1
           IF (latsib(i) ge lat(x)) then begin
              latref(i) = x
           ENDIF ELSE BEGIN
              string1='Error Gridding SiB4'
              string2='Latitude: '+string(latsib(i),FORMAT='(F8.3)')
              EZ4_ALERT,[string1,string2]
           ENDELSE
        ENDIF ELSE $
        IF ((latsib(i) GE lat(x) AND $
                (latsib(i) LT lat(x+1)))) THEN BEGIN
               latref(i) = x
               testlat=1
        ENDIF

        x++
     ENDWHILE

     testlon=0
     x=0
     WHILE(testlon EQ 0) DO BEGIN
        IF (x EQ nlon-1) THEN BEGIN
           testlon=1
           IF (lonsib(i) GE lon(x)) THEN BEGIN
              lonref(i) = x
           ENDIF ELSE BEGIN
              string1='Error Gridding SiB4'
              string2='Longitude: '+string(lonsib(i),FORMAT='(F8.3)')
              ez4_alert,[string1,string2]
           ENDELSE
        ENDIF ELSE $
           IF ((lonsib(i) GE lon(x)) AND $
               (lonsib(i) LT lon(x+1))) THEN BEGIN
           lonref(i) = x
           testlon=1
        ENDIF

        x++
     ENDWHILE
     
  ENDFOR                        ;i=1,numpts

  
end 
