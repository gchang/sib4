pro sib4_gridlatlon, $
    gridInfoFile, $
    nsib, lonsib, latsib, $
    nlon, nlat, lon, lat, $
    lonref, latref


;;;First check grid information file
result = routine_info('sib4plot',/source)
index = strpos(result.path,'/',/reverse_search)
sib4dir = strmid(result.path,0,index+1)
gridfile = sib4dir + 'Example_Files/' + gridInfoFile
myinfo = FILE_INFO(gridfile)

IF (myinfo.exists) THEN BEGIN
    RESTORE, gridfile
    IF (nsib EQ gridinfo.nsib) THEN BEGIN
        nlon=gridinfo.nlon
        nlat=gridinfo.nlat
        lon =gridinfo.lon
        lat =gridinfo.lat
        lonref=gridinfo.lonref
        latref=gridinfo.latref
     ENDIF ELSE BEGIN
        sib4_getgrid, nsib, lonsib, latsib, $
             nlon, nlat, lon, lat, lonref, latref
     ENDELSE
ENDIF ELSE BEGIN
    ez4_alert, 'Missing Grid Information.'
    RETURN
ENDELSE


end
