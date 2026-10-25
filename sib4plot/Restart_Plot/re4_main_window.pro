;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
PRO RE4_MAIN_WINDOW, reinfo

@ez4_BasicParams.com

; Set up the variable and plot type widget once the data has been read
windowTitle = reinfo.windowTitle
tlb = WIDGET_BASE(TITLE=windowTitle, /COLUMN, _EXTRA=extra, SPACE=20)

reinfo.simlabelField = CW_FIELD(tlb, TITLE='Simulation Label:', $
                 XSIZE=40, VALUE=reinfo.simlabel)

; variables available
sub  = WIDGET_BASE(tlb, /COLUMN, /FRAME, /ALIGN_CENTER, SPACE=5)
item = WIDGET_LABEL(sub, VALUE='SAVED FIELDS')
row  = WIDGET_BASE(sub, /ROW)

IF (N_ELEMENTS(reinfo.sitesib) EQ reinfo.nsib) THEN BEGIN
   sub1 = WIDGET_BASE(sub, /ROW, /ALIGN_CENTER)
   item = WIDGET_LABEL(sub1, VALUE='SITE: ')
   ptNames = STRARR(reinfo.nsib)
   FOR n=0,reinfo.nsib-1 DO BEGIN
       ptNames(n) = 'Lat: ' + string(reinfo.latsib(n),format='(f6.2)') $
                    + ', Lon:' $
                    + string(reinfo.lonsib(n),format='(f7.2)') $
                    + ', ' + reinfo.sitesib(n)
    ENDFOR
   if (reinfo.nsib EQ 1) THEN ysize=26 $
       ELSE ysize = MIN([102, reinfo.nsib*23])
   rept = WIDGET_LIST(sub1, SCR_YSIZE=ysize, UNAME='POINT NUMBER', VALUE=ptNames)
   WIDGET_CONTROL, rept, SET_LIST_SELECT=0
   reinfo.wdlpt = rept
ENDIF

sub2 = WIDGET_BASE(sub, /ROW, /ALIGN_CENTER)
item = WIDGET_LABEL(sub2, VALUE='PFT:')
repft = WIDGET_DROPLIST(sub2, UNAME='PFT NUMBER', VALUE=pftNames)
reinfo.wdlpft = repft

item = WIDGET_LABEL(sub2, VALUE='LIVE POOL:')
repftpool = WIDGET_DROPLIST(sub2, UNAME='PFT POOL VALUE', VALUE=poolPFTNames)
reinfo.wdlpftpool = repftpool

item = WIDGET_LABEL(sub2, VALUE='DEAD POOL:')
relupool = WIDGET_DROPLIST(sub2, UNAME='LU POOL VALUE', VALUE=poolLUNames)
reinfo.wdllupool = relupool

sub3 = WIDGET_BASE(sub, /ROW, /ALIGN_CENTER)
item = WIDGET_LABEL(sub3, VALUE='SNOW LAYER:')
resnow = WIDGET_DROPLIST(sub3, UNAME='SNOW VALUE', VALUE=snowNames)
reinfo.wdlsnow = resnow

item = WIDGET_LABEL(sub3, VALUE='SOIL LAYER:')
resoil = WIDGET_DROPLIST(sub3, UNAME='SOIL VALUE', VALUE=soilNames)
reinfo.wdlsoil = resoil

item = WIDGET_LABEL(sub3, VALUE='TOT LAYER:')
retot = WIDGET_DROPLIST(sub3, UNAME='TOT VALUE', VALUE=totNames)
;WIDGET_CONTROL, retot, SET_DROPLIST_SELECT=5
reinfo.wdltot = retot


sub4 = WIDGET_BASE(sub, /ROW, /ALIGN_CENTER)
refulltitle = STRUPCASE(reinfo.vname) + ': ' + STRTRIM(reinfo.vtitle)
relist = WIDGET_LIST(sub4, VALUE=refulltitle, $
            UNAME='VAR REF', $
            YSIZE=(reinfo.nvars < 10), XSIZE=40)
WIDGET_CONTROL, relist, SET_LIST_SELECT=0
reinfo.wdlvar = relist

;;;;ok and close
sub5 = WIDGET_BASE(tlb, /ROW, /ALIGN_CENTER, /FRAME)
ok = WIDGET_BUTTON(sub5, VALUE='OK', UNAME='OK')
close = WIDGET_BUTTON(sub5, VALUE='CLOSE', UNAME='DONE')

;  Realize the widgets
WIDGET_CONTROL, tlb, /REALIZE, SET_UVALUE=reinfo, /NO_COPY

; Form the name of the submenu in 'Windows' menu
p = STRPOS(windowTitle, ' - ')
IF p EQ -1 THEN title = windowTitle ELSE title = STRMID(windowTitle,0,p)
EZ4_WindowAdd, id=tlb, title=title

XMANAGER, 'RE4_MAIN_WINDOW', tlb, CLEANUP = 'EZ4_WindowDelete'

END

