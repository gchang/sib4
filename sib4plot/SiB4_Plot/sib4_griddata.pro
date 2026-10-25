pro sib4_griddata, nsib, sibdata, $
    nlon, nlat, lonref, latref, data

@ez4_BasicParams.com

data=FLTARR(nlon,nlat) + missing
FOR i=0,nsib-1 DO BEGIN
    myxr=lonref(i)
    myyr=latref(i)
    data(myxr,myyr) = sibdata(i)
ENDFOR

end
