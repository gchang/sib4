;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO SiB4_MAIN_WINDOW, sibinfo

@ez4_BasicParams.com

xsizef = 6
xsizel = 20
xsizep = 10
xsizet = 300
xsizev = MIN([40,MAX([18,STRLEN(sibinfo.vname)*2.6])])
sibtypelu = STRMATCH(sibinfo.fileNames[0],'*.lu.*')

ysizemin = 30

; set up the window
windowTitle = sibinfo.windowTitle
tlb = WIDGET_BASE(TITLE=windowTitle, /COLUMN, _EXTRA=extra, SPACE=20)

; simulation label
sub1 = WIDGET_BASE(tlb, /ALIGN_CENTER)
sibinfo.simlabelField = CW_FIELD(sub1, TITLE='Label:', $
     XSIZE=xsizel, VALUE=sibinfo.simlabel)

; point/map choices
IF (sibinfo.nsib GT 1) THEN BEGIN
   IF (sibinfo.sitesib[0] NE '') THEN BEGIN
      IF (sibtypelu EQ 1) THEN BEGIN
         sub2 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER)
         sub2a = WIDGET_BASE(sub2, /COLUMN, /FRAME, /ALIGN_CENTER)
         item = WIDGET_LABEL(sub2a, VALUE='PFT')
         sibinfo.wdlpft = WIDGET_DROPLIST(sub2a, $
                           /ALIGN_CENTER, UNAME='PFT REF', VALUE=pftNames)

         pType = ['Combined PFTs','Individual PFTs']
         sibinfo.wdlpftt = WIDGET_DROPLIST(sub2a, $
                          /ALIGN_CENTER, UNAME='PFT TYPE', VALUE=pType)

         string1 = 'As Is'
         string2 = 'Area Weight (*)'
         string3 = 'Un-Area Weight (/)'
         paType = [string1,string2,string3]
         sibinfo.wdlpfta = WIDGET_DROPLIST(sub2a, $
              /ALIGN_CENTER, UNAME='PFT AREA', VALUE=paType)
         WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=0
      ENDIF ELSE BEGIN
         sub2 = WIDGET_BASE(tlb, /COLUMN, /ALIGN_CENTER)
      ENDELSE

      sub2b  = WIDGET_BASE(sub2, /COLUMN, /FRAME, /ALIGN_CENTER, SPACE=5)
      item = WIDGET_LABEL(sub2b, VALUE='SITE')
      ptNames = STRARR(sibinfo.nsib)
      FOR n=0,sibinfo.nsib-1 DO BEGIN
          ptNames(n) = 'Lat: ' + string(sibinfo.latsib(n),format='(f6.2)') $
                    + ', Lon:' $
                    + string(sibinfo.lonsib(n),format='(f7.2)') $
                    + ', ' + sibinfo.sitesib(n)
      ENDFOR
      ysize = MIN([102, MAX([ysizemin,sibinfo.nvars*25])])
      ncpt = WIDGET_LIST(sub2b, SCR_YSIZE=ysize, $
                UNAME='POINT REF', VALUE=ptNames)
      WIDGET_CONTROL, ncpt, SET_LIST_SELECT=0
      sibinfo.wdlpt = ncpt

   ENDIF ELSE BEGIN
      IF (sibtypelu EQ 1) THEN BEGIN
         sub2 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER)
         sub2a = WIDGET_BASE(sub2, /COLUMN, /FRAME, /ALIGN_CENTER)
         item = WIDGET_LABEL(sub2a, VALUE='PFT')
         sibinfo.wdlpft = WIDGET_DROPLIST(sub2a, $
                           /ALIGN_CENTER, UNAME='PFT REF', VALUE=pftNames)

         pType = ['Combined PFTs','Individual PFTs']
         sibinfo.wdlpftt = WIDGET_DROPLIST(sub2a, $
                          /ALIGN_CENTER, UNAME='PFT TYPE', VALUE=pType)
         WIDGET_CONTROL, sibinfo.wdlpftt, SENSITIVE=0

         string1 = 'As Is'
         string2 = 'Area Weight (*)'
         string3 = 'Un-Area Weight (/)'
         paType = [string1,string2,string3]
         sibinfo.wdlpfta = WIDGET_DROPLIST(sub2a, $
              /ALIGN_CENTER, UNAME='PFT AREA', VALUE=paType)
         WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=0
      ENDIF ELSE BEGIN
         sub2 = WIDGET_BASE(tlb, /COLUMN, /ALIGN_CENTER)
      ENDELSE

      sub2b = WIDGET_BASE(sub2, /COLUMN, /FRAME, /ALIGN_CENTER)
      sorg = ['Global','Site']
      wdlgs = WIDGET_DROPLIST(sub2b, VALUE=sorg, $
                /ALIGN_CENTER, UNAME='POINT OR MAP')

      sub2ba = WIDGET_BASE(sub2b, /ROW)
      sitelon = 'Site Lon'
      sibinfo.wdllon = CW_FIELD(sub2ba, TITLE=sitelon, $
                        XSIZE=xsizef, /FLOATING, /COLUMN)
      WIDGET_CONTROL, sibinfo.wdllon, SENSITIVE=0

      sitelat = 'Site Lat'
      sibinfo.wdllat = CW_FIELD(sub2ba, TITLE=sitelat, $
                       XSIZE=xsizef, /FLOATING, /COLUMN)
      WIDGET_CONTROL, sibinfo.wdllat, SENSITIVE=0

      ;sub2bb = WIDGET_BASE(sub2b, /ALIGN_CENTER)
      ;siteinfo = '(Sites 1 thru ' $
      ;    + string(sibinfo.nsib,format='(I5)') $
      ;    + ')'
      ;sibinfo.wdlpt = CW_FIELD(sub2bb, TITLE=siteinfo, $
      ;          XSIZE=xsizep, /COLUMN, /LONG)
      ;WIDGET_CONTROL, sibinfo.wdlpt, SENSITIVE=0

   ENDELSE
ENDIF ELSE BEGIN
   sibinfo.refpt = 0

   IF (sibtypelu EQ 1) THEN BEGIN
      sub2 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER)
      item = WIDGET_LABEL(sub2, VALUE='PFT:')
      sibinfo.wdlpft = WIDGET_DROPLIST(sub2, UNAME='PFT REF', VALUE=pftNames)

      pType = ['Combined PFTs','Individual PFTs']
      sibinfo.wdlpftt = WIDGET_DROPLIST(sub2, UNAME='PFT TYPE', VALUE=pType)

      string1 = 'As Is'
      string2 = 'Area Weight (*)'
      string3 = 'Un-Area Weight (/)'
      paType = [string1,string2,string3]
      sibinfo.wdlpfta = WIDGET_DROPLIST(sub2, $
              /ALIGN_CENTER, UNAME='PFT AREA', VALUE=paType)
      WIDGET_CONTROL, sibinfo.wdlpfta, SENSITIVE=0
   ENDIF
ENDELSE

; time choices
timeid = -1
timesid = -1
timemid = -1
timetid = -1

IF (sibinfo.ntime GT 1) THEN BEGIN
  sub3 = WIDGET_BASE(tlb,/COLUMN,/FRAME,/ALIGN_CENTER)
  ;item = WIDGET_LABEL(sub3, VALUE='TIME')

  ; ...time mean button
  sub4 = WIDGET_BASE(sub3, XSIZE=xsizet, /ROW, $
               /ALIGN_CENTER, /NONEXCLUSIVE)
  timemid = WIDGET_BUTTON(sub4, /ALIGN_CENTER, $
               VALUE='MEAN   ', UNAME='TIME MEAN')

  ;...time series button
  timesid = WIDGET_BUTTON(sub4, /ALIGN_CENTER, $
               VALUE='TIME SERIES  ', UNAME='TIME SERIES')

  ;...time total button
  timetid = WIDGET_BUTTON(sub4, /ALIGN_RIGHT, $
               VALUE='TOTAL', UNAME='TIME TOTAL')

  ; time selector (slider)
  timeid = WIDGET_SLIDER(sub3, $
            TITLE='Record #', VALUE=1, MIN=1, $
            MAX=sibinfo.ntime, UNAME='TIME VALUE')

  IF (sibinfo.mapsib EQ 1) THEN BEGIN
    sibinfo.reftime = -1
    WIDGET_CONTROL, timemid, SET_BUTTON=1 
    WIDGET_CONTROL, timeid, SENSITIVE=0
    WIDGET_CONTROL, timesid, SET_BUTTON=0
    WIDGET_CONTROL, timesid, SENSITIVE=0
    WIDGET_CONTROL, timetid, SET_BUTTON=0
    WIDGET_CONTROL, timetid, SENSITIVE=1
  ENDIF ELSE BEGIN
    sibinfo.reftime = -2
    WIDGET_CONTROL, timesid, SET_BUTTON=1
    WIDGET_CONTROL, timeid, SENSITIVE=0
    WIDGET_CONTROL, timemid, SET_BUTTON=0
    WIDGET_CONTROL, timemid, SENSITIVE=1
    WIDGET_CONTROL, timetid, SET_BUTTON=0
    WIDGET_CONTROL, timetid, SENSITIVE=1
  ENDELSE

  sibinfo.wdltime = timeid
  sibinfo.wdltimem = timemid
  sibinfo.wdltimes = timesid
  sibinfo.wdltimet = timetid
ENDIF


; variables available
sub5  = WIDGET_BASE(tlb, /COLUMN, /FRAME, /ALIGN_CENTER, SPACE=5)
item = WIDGET_LABEL(sub5, VALUE='SAVED FIELDS')
row  = WIDGET_BASE(sub5, /ROW)

ncfulltitle=strarr(sibinfo.nvars)
FOR v=0,sibinfo.nvars-1 DO BEGIN
    IF (sibinfo.vtitle(v) NE '') THEN BEGIN
        ncfulltitle(v) = STRUPCASE(sibinfo.vname(v)) $
              + ': ' + STRTRIM(sibinfo.vtitle(v))
    ENDIF ELSE $
    IF (sibinfo.vunits(v) NE '') THEN BEGIN
        ncfulltitle(v) = STRUPCASE(sibinfo.vname(v)) $
              + ': ' + STRTRIM(sibinfo.vunits(v))
    ENDIF ELSE $
        ncfulltitle(v) = STRUPCASE(sibinfo.vname(v))
ENDFOR

sub6 = WIDGET_BASE(sub5, /ROW, /ALIGN_CENTER)

ysize = MIN([105, MAX([ysizemin,sibinfo.nvars*25.])])
nclist = WIDGET_LIST(sub6, VALUE=ncfulltitle, $
            UNAME='VAR REF', SCR_YSIZE=ysize, XSIZE=xsizev)
WIDGET_CONTROL, nclist, SET_LIST_SELECT=0
sibinfo.wdlvar = nclist

;;;;ok and close
sub7 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER, /FRAME)
ok = WIDGET_BUTTON(sub7, VALUE='OK', UNAME='OK')
close = WIDGET_BUTTON(sub7, VALUE='CLOSE', UNAME='DONE')

;  Realize the widgets
WIDGET_CONTROL, tlb, /REALIZE, SET_UVALUE=sibinfo, /NO_COPY

; Form the name of the submenu in 'Windows' menu
p = STRPOS(windowTitle, ' - ')
IF p EQ -1 THEN title = windowTitle ELSE title = STRMID(windowTitle,0,p)
EZ4_WindowAdd, id=tlb, title=title

XMANAGER, 'SiB4_MAIN_WINDOW', tlb, CLEANUP = 'EZ4_WindowDelete'

END

