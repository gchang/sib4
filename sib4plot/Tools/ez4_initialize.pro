;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO EZ4_INITIALIZE

; Get the parameters
@ez4_BasicParams.com

; Get the preference file path and name
@sib4plot_preferences

; --- Set BasicParameters_COMS
ez4_dead = 0
ez4_initialized = 1
domultiplot = 0
dopsplot = 0

; --- Set Colors_COM
ez4_definecolors

; --- Set File_COM
; Custom directory buttons for file selection widget
IF N_ELEMENTS(dirButton) NE 0 THEN BEGIN
  ncdtest = (SIZE(dirButton))[0]
  IF (ncdtest EQ 1) THEN BEGIN
     nCustomDirs = 1
     dirButtonName = dirButton[0]
     dirName = dirButton[1]
  ENDIF ELSE BEGIN
     nCustomDirs   = (SIZE(dirButton))[2]
     dirButtonName = REFORM(dirButton[0,*],nCustomDirs)
     dirName       = REFORM(dirButton[1,*],nCustomDirs)
  ENDELSE
ENDIF ELSE nCustomDirs = 0

; set directory
IF N_ELEMENTS(dirInput) EQ 0 THEN CD, CURRENT = dirInput       
IF N_ELEMENTS(dirOutput) EQ 0 THEN CD, CURRENT = dirOutput       

; --- Set File_COM
IF (N_ELEMENTS(mainFont) NE 0) THEN $
    WIDGET_CONTROL, DEFAULT_FONT=mainFont

; --- Set SiBInfo_COM
; Set Pft Names
IF (n_elements(luNames) LT 1) THEN $
   luNames=''
nlu = n_elements(luNames)-1

IF (n_elements(pftNames) LT 1) THEN BEGIN
   pftNames=''
ENDIF ELSE BEGIN
   pftNames=STRUPCASE(pftNames)
ENDELSE
npft = n_elements(pftNames)-1

IF (n_elements(pftRefs) LT 1) THEN $
    pftRefs=''
npftref = n_elements(pftRefs)-1
IF (npftref NE npft) THEN BEGIN
   string1 = 'PFT names do not match references.'
   string2 = 'Please change SIB4PLOT_PREFERENCES.'
   string3 = 'CONTINUE AT YOUR OWN RISK!'
   message = [string1,string2,string3]
   ez4_message, message
ENDIF

IF (n_elements(cropNames) LT 1) THEN $
    cropNames=''
ncrop = n_elements(cropNames)-1

IF (n_elements(poolcanNames) LT 1) THEN $
    poolcanNames=''
npoolcan = n_elements(poolcanNames)-1

IF (n_elements(poolpftNames) LT 1) THEN $
   poolpftNames=''
npoolpft = n_elements(poolpftNames)-1

IF (n_elements(poollUNames) LT 1) THEN $
   poolluNames=''
npoollu = n_elements(poolluNames)-1

IF (n_elements(snowNames) LT 1) THEN $
   snowNames=''
nsnow = n_elements(snowNames)-1

IF (n_elements(soilNames) LT 1) THEN $
   soilNames=''
nsoil = n_elements(soilNames)-1

IF (n_elements(totNames) LT 1) THEN $
   totNames=''   
ntot = n_elements(totNames)-1

; --- Set Window_COM
ezBase = -1
struct = { WINDOW_LIST_S, ID:0L, name:'', plot:0B, index:0, menuBar:0 }
ezWindowList = { WINDOW_LIST_S, ezBase, 'SiB4PLOT Main Menu', 0, -1, -1 }


END
