;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO SiB4_PLOT, FILTER=FILTER

   ;Get the SiB4 output, in netcdf
   IF (FILTER EQ 'HR') THEN filter = 'hsib_*.nc' ELSE $
   IF (FILTER EQ 'PBP') THEN filter = 'psib_*.nc' ELSE $
   IF (FILTER EQ 'QP') THEN filter = 'qsib_*.nc' ELSE $
   filter = '?sib_*.nc'

   sibFileName = SiB4_PickOpen(FILTER=filter, PATH=inputDir, GET_PATH=inputDir)

   IF (sibFileName[0] EQ '') THEN BEGIN
      ;IF (quitPrompt EQ 1) THEN EZ4_Alert,'No Files Selected.'
      RETURN
   ENDIF

   WIDGET_CONTROL, /HOURGLASS

   SiB4_NCDF_PLOT, sibFileName

END
