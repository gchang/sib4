;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO NC_MAIN_WINDOW, ncinfo

@ez4_BasicParams.com

xsizef = 6
xsizel = 20
xsizep = 10
xsizet = 300
xsizev = MIN([32,MAX([18,STRLEN(ncinfo.vname)*2.4])])
ysizemax = 105
ysizemin = 60
ysize = MIN([ysizemax, MAX([ysizemin,ncinfo.nvars*25])])

; set up the window
windowTitle = ncinfo.windowTitle
tlb = WIDGET_BASE(TITLE=windowTitle, /COLUMN, _EXTRA=extra, SPACE=20)

; label
ncinfo.simlabelField = CW_FIELD(tlb, TITLE='Label:', $
                 XSIZE=xsizel, VALUE=ncinfo.simlabel)

; PFT options
IF ((ncinfo.nlevs GT 1) OR (ncinfo.nlu GT 1)) THEN BEGIN
   sub1 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER)
   sub1a = WIDGET_BASE(sub1, /COLUMN, /FRAME, /ALIGN_CENTER)
   item = WIDGET_LABEL(sub1a, VALUE=ncinfo.levlabel)
   ncinfo.wdlpft = WIDGET_DROPLIST(sub1a, $
          /ALIGN_CENTER, UNAME='PFT REF', VALUE=ncinfo.levnames)
   string1 = 'Combined ' + ncinfo.levlabel
   string2 = 'Individual ' + ncinfo.levlabel
   pType = [string1,string2]
   ncinfo.wdlpftt = WIDGET_DROPLIST(sub1a, $
                    /ALIGN_CENTER, UNAME='PFT TYPE', VALUE=pType)
   WIDGET_CONTROL, ncinfo.wdlpftt, SENSITIVE=0

   string1 = 'As Is'
   string2 = 'Area Weight (*)'
   string3 = 'Un-Area Weight (/)'
   paType = [string1,string2,string3]
   ncinfo.wdlpfta = WIDGET_DROPLIST(sub1a, $
              /ALIGN_CENTER, UNAME='PFT AREA', VALUE=paType)
   WIDGET_CONTROL, ncinfo.wdlpfta, SENSITIVE=1

ENDIF ELSE BEGIN
   sub1 = WIDGET_BASE(tlb, /COLUMN, /FRAME, /ALIGN_CENTER)
ENDELSE

; point/map choices
npts = MAX([ncinfo.nsib,ncinfo.nlon*ncinfo.nlat])
IF (ncinfo.mapsib EQ 1) THEN BEGIN ;Maps/Global
   sub1b = WIDGET_BASE(sub1, /COLUMN, /FRAME, /ALIGN_CENTER)
   sorg = ['Global','Site']
   wdlgs = WIDGET_DROPLIST(sub1b, VALUE=sorg, $
            /ALIGN_CENTER, UNAME='POINT OR MAP')

   sub1c = WIDGET_BASE(sub1b, /ROW)
   ;loninfo = 'Lon: ' + string(MIN(ncinfo.lon),format='(F7.2)') $
   ;         + ' to ' + string(MAX(ncinfo.lon),format='(F7.2)')
   loninfo = 'Site Lon'
   ncinfo.wdllon = CW_FIELD(sub1c, TITLE=loninfo, $
            XSIZE=xsizef, /COLUMN, /FLOATING)
    WIDGET_CONTROL, ncinfo.wdllon, SENSITIVE=0

   ;latinfo = 'Lat: ' + string(MIN(ncinfo.lat),format='(F6.2)') $
   ;           + ' to ' + string(MAX(ncinfo.lat),format='(F6.2)')
   latinfo = 'Site Lat'
   ncinfo.wdllat = CW_FIELD(sub1c, TITLE=latinfo, $
            XSIZE=xsizef, /COLUMN, /FLOATING)
   WIDGET_CONTROL, ncinfo.wdllat, SENSITIVE=0
ENDIF ELSE BEGIN ;Sites
   IF (ncinfo.nsib GT 1) THEN BEGIN
      sub1b = WIDGET_BASE(sub1, /COLUMN, /FRAME, /ALIGN_CENTER)
      item = WIDGET_LABEL(sub1b, VALUE='SITE')
      ptNames = STRARR(ncinfo.nsib)
      FOR n=0,ncinfo.nsib-1 DO BEGIN
          ptNames(n) = 'Lat: ' + string(ncinfo.latsib(n),format='(f6.2)') $
                    + ', Lon:' $
                    + string(ncinfo.lonsib(n),format='(f7.2)')
          IF (ncinfo.sitesib(n) NE '') THEN $
             ptNames(n) += ', ' + ncinfo.sitesib(n)
      ENDFOR
      ncpt = WIDGET_LIST(sub1b, SCR_YSIZE=ysize, $
             UNAME='POINT REF', VALUE=ptNames)
      WIDGET_CONTROL, ncpt, SET_LIST_SELECT=0
      ncinfo.wdlpt = ncpt
   ENDIF

ENDELSE

; time choices
timeid = -1
timemid = -1
timesid = -1
timetid = -1
IF (ncinfo.ntime GT 1) THEN BEGIN

  sub3 = WIDGET_BASE(tlb,/COLUMN,/FRAME,/ALIGN_CENTER)

  ; ...time mean button
  sub4 = WIDGET_BASE(sub3, XSIZE=xsizet, /ROW, $
          /ALIGN_CENTER, /NONEXCLUSIVE)
  timemid = WIDGET_BUTTON(sub4, /ALIGN_CENTER, $
          VALUE='MEAN', UNAME='TIME MEAN')

  ;...time series button
  timesid = WIDGET_BUTTON(sub4, /ALIGN_RIGHT, $
          VALUE='TIME SERIES', UNAME='TIME SERIES')

  ;...time total button
  timetid = WIDGET_BUTTON(sub4, /ALIGN_RIGHT, $
               VALUE='TOTAL', UNAME='TIME TOTAL')

  ; ...time selector (slider)
  timeid = WIDGET_SLIDER(sub3, TITLE='Time Record #', VALUE=1, $
              MIN=1, MAX=ncinfo.ntime, UNAME='TIME VALUE')

  IF (ncinfo.mapsib EQ 1) THEN BEGIN
      ncinfo.reftime = -1
      WIDGET_CONTROL, timemid, SET_BUTTON=1 
      WIDGET_CONTROL, timeid, SENSITIVE=0
      WIDGET_CONTROL, timesid, SET_BUTTON=0
      WIDGET_CONTROL, timesid, SENSITIVE=0
      WIDGET_CONTROL, timetid, SET_BUTTON=0
      WIDGET_CONTROL, timetid, SENSITIVE=1
  ENDIF ELSE BEGIN
      ncinfo.reftime = -2
      WIDGET_CONTROL, timesid, SET_BUTTON=1
      WIDGET_CONTROL, timeid, SENSITIVE=0
      WIDGET_CONTROL, timemid, SET_BUTTON=0
      WIDGET_CONTROL, timemid, SENSITIVE=1
      WIDGET_CONTROL, timetid, SET_BUTTON=0
      WIDGET_CONTROL, timetid, SENSITIVE=1
  ENDELSE

  ncinfo.wdltime = timeid
  ncinfo.wdltimem = timemid
  ncinfo.wdltimes = timesid
  ncinfo.wdltimet = timetid
ENDIF


; variables available
sub5  = WIDGET_BASE(tlb, /COLUMN, /FRAME, /ALIGN_CENTER, SPACE=5)
item = WIDGET_LABEL(sub5, VALUE='SAVED FIELDS')
row  = WIDGET_BASE(sub5, /ROW)

ncfulltitle = strarr(ncinfo.nvars)
FOR v=0,ncinfo.nvars-1 DO BEGIN
    IF (ncinfo.vtitle(v) NE '') THEN BEGIN
        ncfulltitle(v) = STRUPCASE(ncinfo.vname(v)) $
              + ': ' + STRTRIM(ncinfo.vtitle(v))
    ENDIF ELSE $
    IF (ncinfo.vunits(v) NE '') THEN BEGIN
        ncfulltitle(v) = STRUPCASE(ncinfo.vname(v)) $
              + ' (' + STRTRIM(ncinfo.vunits(v)) + ')'
    ENDIF ELSE $
        ncfulltitle(v) = STRUPCASE(ncinfo.vname(v))
ENDFOR

nclist = WIDGET_LIST(sub5, VALUE=ncfulltitle, $
            UNAME='VAR REF', $
            SCR_YSIZE=ysize, XSIZE=xsizev)
WIDGET_CONTROL, nclist, SET_LIST_SELECT=0
ncinfo.wdlvar = nclist


;;;;ok and close
sub6 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER, /FRAME)
ok = WIDGET_BUTTON(sub6, VALUE='OK', UNAME='OK')
close = WIDGET_BUTTON(sub6, VALUE='CLOSE', UNAME='DONE')


;  Realize the widgets
WIDGET_CONTROL, tlb, /REALIZE, SET_UVALUE=ncinfo, /NO_COPY

; Form the name of the submenu in 'Windows' menu
p = STRPOS(windowTitle, ' - ')
IF p EQ -1 THEN title = windowTitle ELSE title = STRMID(windowTitle,0,p)
EZ4_WindowAdd, id=tlb, title=title

XMANAGER, 'NC_MAIN_WINDOW', tlb, CLEANUP = 'EZ4_WindowDelete'

END

