PRO SiB4_SiteName, fileName, sitename=sitename

;;Setup basic variables
sitename=''
sitelat=-999.
sitelon=-999.

loc = strpos(fileName,'-')
IF (loc GT 0) THEN BEGIN
   sitename = strmid(fileName,loc-2,6)
ENDIF

END
