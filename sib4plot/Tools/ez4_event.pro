pro ez4_event, event

@ez4_BasicParams.com

WIDGET_CONTROL, event.top, GET_UVALUE=state

CASE event.id OF
   state.quitButton: BEGIN
      IF quitPrompt THEN BEGIN
         IF ez4_Confirm(GROUP_LEADER=event.top, 'Quit?') EQ 0 THEN RETURN
      ENDIF
      print,''
      print,'  !!Thank you for using EZPLOT for SiB4!!'
      print,'                 Bye Bye'
      print,''
      EXIT
      RETURN
   END
 
   state.hrbutton: BEGIN
     SiB4_Plot, FILTER='HR'
     RETURN
   END

   state.pbpbutton: BEGIN
     SiB4_Plot, FILTER='PBP'
     RETURN
   END

   state.qpbutton: BEGIN
     SiB4_Plot, FILTER='QP'
     RETURN
  END

  state.restartbutton: BEGIN
     RE4_Plot
     RETURN
  END

  state.driverbutton: BEGIN
    NC_Plot, /DRIVER
    RETURN
  END

  state.mapbutton: BEGIN
    NC_Plot
    RETURN
  END

  state.linebutton: BEGIN
    NC_Plot, /SITE
    RETURN
  END

  state.linesavebutton: BEGIN
     ;Open a saved line plot
     fileName = SiB4_PickOpen(FILTER='*.lp', PATH=inputDir, $
              GET_PATH=inputDir)
     exist = FILE_TEST(fileName)
     IF (exist EQ 1) THEN BEGIN
         RESTORE, fileName
         Line_Plot, plot
     ENDIF
     RETURN
  END

  state.mapsavebutton: BEGIN
   ;Open a saved map plot
   fileName = SiB4_PickOpen(FILTER='*.map', PATH=inputDir, $
              GET_PATH=inputDir)
   exist = FILE_TEST(fileName)
   IF (exist EQ 1) THEN BEGIN
       RESTORE, fileName
       Geo4_Plot, plot
   ENDIF
   RETURN
  END

  state.histsavebutton: BEGIN
    ;Open a saved histogram plot
    fileName = SiB4_PickOpen(FILTER='*.hp', PATH=inputDir, $
               GET_PATH=inputDir)
    exist = FILE_TEST(fileName)
    IF (exist EQ 1) THEN BEGIN
        RESTORE, fileName
        Hist_Plot, plot
    ENDIF
    RETURN
  END

   ELSE:
ENDCASE
      
end
