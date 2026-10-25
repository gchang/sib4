;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO NC_PLOT, DRIVER=DRIVER, $
    SITE=SITE

@ez4_BasicParams.com

filter = '*.nc'
IF (N_ELEMENTS(driver) GT 0) THEN filter = 'merra*.nc'

;Get the netcdf file
ncFileName = SiB4_PickOpen(FILTER=filter, PATH=inputDir, GET_PATH=inputDir)

IF (ncFileName[0] EQ '') THEN BEGIN
   IF (quitPrompt EQ 1) THEN EZ4_Alert,'No Files Selected.'
   RETURN
ENDIF

WIDGET_CONTROL, /HOURGLASS

NC_NCDF_PLOT, ncFileName, SITE=SITE

END
