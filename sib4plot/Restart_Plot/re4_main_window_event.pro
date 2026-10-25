;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO RE4_MAIN_WINDOW_EVENT, event


WIDGET_CONTROL, event.top, GET_UVALUE=reinfo, /NO_COPY

; Handle the current event depending on which action (widget) was 
; selected by the user
menuName = WIDGET_INFO(event.id, /UNAME)

; read the simulation label
WIDGET_CONTROL, reinfo.simlabelField, GET_VALUE=simlabel
reinfo.simlabel = simlabel

CASE menuName OF

  'DONE' : WIDGET_CONTROL, event.top, /DESTROY
  'OK': BEGIN

      ; Grab the point if necessary
      IF (reinfo.wdlpt GE 0) THEN BEGIN
         ptref = WIDGET_INFO(reinfo.wdlpt, /List_Select)
         reinfo.refpt = ptref
      ENDIF

      ; Grab the pft selection
      pftref = WIDGET_INFO(reinfo.wdlpft, /DropList_Select)
      reinfo.refpft = pftref

      ; Grab the pft pool selection
      pftpoolref = WIDGET_INFO(reinfo.wdlpftpool,/DropList_Select)
      reinfo.refpftpool = pftpoolref

      ; Grab the lu pool selection
      lupoolref = WIDGET_INFO(reinfo.wdllupool,/DropList_Select)
      reinfo.reflupool = lupoolref

      ; Grab the snow selection
      snowref = WIDGET_INFO(reinfo.wdlsnow,/DropList_Select)
      reinfo.refsnow = snowref

      ; Grab the soil selection
      soilref = WIDGET_INFO(reinfo.wdlsoil,/DropList_Select)
      reinfo.refsoil = soilref

      ; Grab the total layer selection
      totref = WIDGET_INFO(reinfo.wdltot,/DropList_Select)
      reinfo.reftot = totref

      ; Grab the variable name
      varref = WIDGET_INFO(reinfo.wdlvar, /List_Select)
      reinfo.refvar = varref

      ; Create the requested plot
      RE4_GetData, reinfo
   END ;OK

  'POINT NUMBER': reinfo.refpt = event.index
  'PFT NUMBER'  : reinfo.refpft = event.index
  'PFT POOL VALUE': reinfo.refpftpool = event.index
  'LU POOL VALUE': reinfo.reflupool = event.index
  'SNOW VALUE': reinfo.refsnow = event.index
  'SOIL VALUE': reinfo.refsoil = event.index
  'TOT VALUE': reinfo.reftot = event.index
  'VAR REF': reinfo.refvar = event.index

ENDCASE ;menuName

; Copy the revised state information back into the original structure,
; and return control to the XMANAGER to await more widget events
DONE:
IF WIDGET_INFO(event.top, /VALID_ID) THEN $
   WIDGET_CONTROL, event.top, SET_UVALUE=reinfo, /NO_COPY

END

