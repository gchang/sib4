;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO RE4_NCDF_PLOT, refile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set window file name
  EZ4_FILEBREAK, refile[0], FILE=fNameOnly, DIR=fDir
  windowTitle = fNameOnly

  ;;Get basic data information
  RE4_GETVARS, refile[0], $
       nsib, lonsib, latsib, sitesib, $
       nvarsre, namere, titlere, unitre, $
       reerror

  ;;Return if error opening file...
  IF (reerror eq 1) THEN RETURN

  ;;Set up a data structure to hold the information
  reinfo = { file:refile[0], $
             nsib:nsib, sitesib:sitesib, $
             lonsib:lonsib, latsib:latsib, $
             nvars:nvarsre, $
             vtitle:titlere, vname:namere, $
             vunit:unitre, $
             wdlpt:-1, wdllu:-1, $
             wdlpft:-1, wdlpftpool:-1, wdllupool:-1, $
             wdlsnow:-1, wdlsoil:-1, wdltot:-1, $
             wdlvar:-1, $
             refpt:-1, reflu:-1, $
             refpft:-1, refpftpool:-1, reflupool:-1, $
             refsnow:-1, refsoil:-1, reftot:-1, $
             refvar:-1, $
             simlabel:'', simlabelField:-1, $
             windowTitle:windowTitle}

   RE4_MAIN_WINDOW, reinfo

END

