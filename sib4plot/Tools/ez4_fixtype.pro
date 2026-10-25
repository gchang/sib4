;------------------------------------------------------------------------------ 
PRO EZ4_FixType, typesChoice, fileTypeNum, selecttxt, type, fname

  WIDGET_CONTROL, typesChoice, GET_UVALUE=extTypes
  type = extTypes[fileTypeNum]
  IF type EQ 'data' OR type EQ 'params' THEN type = ''
  WIDGET_CONTROL, selecttxt, GET_VALUE=fname
  fname = fname[0]

  ;; find position of last extension
  dotChar = RSTRPOS(fname,'.')
  IF dotChar EQ -1 THEN ext = '' ELSE ext = STRMID(fname, dotChar+1)

  IF (ext NE type) THEN BEGIN
     fname = STRMID(fname,0,dotChar) 
     IF type NE '' THEN fname = fname + '.' + type
  ENDIF
  WIDGET_CONTROL, selecttxt, SET_VALUE=fname
END
