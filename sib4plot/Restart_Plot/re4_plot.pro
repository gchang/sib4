;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO RE4_PLOT

@ez4_BasicParams.com

   ;Get the SiB4 output, in netcdf
   filter = 'sib_r*.nc'
   reFileName = SiB4_PickOpen(FILTER=filter, PATH=inputDir, GET_PATH=inputDir)

   IF (reFileName[0] EQ '') THEN BEGIN
      IF (quitPrompt EQ 1) THEN EZ4_Alert,'No Files Selected.'
      RETURN
   ENDIF

   WIDGET_CONTROL, /HOURGLASS

   RE4_NCDF_PLOT, reFileName 

END
