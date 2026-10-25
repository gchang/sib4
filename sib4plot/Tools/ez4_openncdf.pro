;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Open a file 'filename' as a netCDF file and retruns its netCDF ID
;;  Return -1 if the file is not in netCDF format or does not exist
;;  
FUNCTION EZ4_OpenNCDF, filename
  CATCH, ncErr
  IF ncErr EQ 0 THEN id = NCDF_OPEN(filename) ELSE id = -1
  CATCH, /CANCEL
  RETURN, id
END

