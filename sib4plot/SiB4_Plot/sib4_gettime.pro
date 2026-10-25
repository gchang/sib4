;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO SiB4_GETTIME, nfiles, sibfiles, $
    numyrs, startyr, stopyr, $
    ntime, time, timenames, $
    timeunit, timeflag, ntperday, ncerror
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Time Flags:
;;;    0= hours
;;;    1= days
;;;    2= months
;;;    3= years
;;;    4= diurnal composite

;;Get the total number of timesteps
ncid=lonarr(nfiles)
ntime=0
FOR f=0,nfiles-1 DO BEGIN
    ncid(f) = EZ4_OPENNCDF(sibfiles(f))
    tid = ncdf_dimid(ncid(f),'time')
    IF (tid LT 0) THEN $
         tid = ncdf_dimid(ncid(f),'numt')
    IF (tid LT 0) THEN $
         tid = ncdf_dimid(ncid(f),'ntime')
    IF (tid GE 0) THEN BEGIN
        NCDF_DIMINQ, ncid(f), tid, tname, tsize
        ntime+=tsize
    ENDIF
ENDFOR

IF (ntime LE 0) THEN BEGIN
   ncerror = 1
   RETURN
ENDIF

;;Get the time values
time=dblarr(ntime)
tcount=0L
FOR f=0,nfiles-1 DO BEGIN
    NCDF_VARGET, ncid(f), 'time', ttime
    tsize = n_elements(ttime)
    time(tcount:tcount+tsize-1) = ttime(*)
    tcount+=tsize
ENDFOR


;;Get the time units
ttimeunit = ''
tid = NCDF_VARID(ncid(0),'time')

;;;time units from attributes
tinfo = NCDF_VARINQ(ncid(0),tid)
FOR att=0,tinfo.natts-1 DO BEGIN
    attname = NCDF_ATTNAME(ncid(0),tid,att)
    IF (attname EQ 'units') THEN BEGIN
        NCDF_ATTGET, ncid(0), tid, attname, tunit
        ttimeunit = strlowcase(string(tunit))
    ENDIF
ENDFOR

;;;time units from sizes - guesses
IF (n_elements(tunit) LT 1) THEN BEGIN
   ;do basic checks
   ;...years??
   timeyr = FLOOR(time[0])
   IF ((timeyr GT 1800) AND $
       (timeyr LT 2100)) THEN BEGIN
       ttimeunit = 'year'
   ENDIF ELSE $

    ;...months??
   IF (ntime EQ 12) THEN $
       ttimeunit = 'mon' ELSE $

   ;...days??
   IF ((ntime EQ 365) OR (ntime EQ 366)) THEN $
       ttimeunit='day' ELSE $

   ;...hours??
   IF (ntime EQ 24) THEN $
       ttimeunit = 'hour' $

   ELSE BEGIN
      EZ4_ALERT,'Unknown Time Units.'
      ncerror = 1
      RETURN
   ENDELSE
ENDIF

;;Close the files
FOR f=0,nfiles-1 DO BEGIN
    NCDF_CLOSE, ncid(f)
ENDFOR

;;Get the time names and time flag
timenames = ''
timeflag = -1

hrpos  = STRPOS(ttimeunit,'hour')
daypos = STRPOS(ttimeunit,'day')
monpos = STRPOS(ttimeunit,'mon')
yrpos  = STRPOS(ttimeunit,'year')

;;;timeflag 0=hours 1: days  2: months  3: years
IF (hrpos[0] NE -1) THEN BEGIN
    timeflag = 0
    timeunit = 'Hour (GMT)'
ENDIF ELSE $
IF (daypos[0] NE -1) THEN BEGIN
    timeflag = 1
    timeunit = 'DOY'
ENDIF ELSE $
IF (monpos[0] NE -1) THEN BEGIN
    timeflag = 2
    timeunit = 'Month'
ENDIF ELSE $
IF (yrpos[0] NE -1) THEN BEGIN
    timeflag = 3
    timeunit = ''
ENDIF

IF (timeflag LT 0) THEN BEGIN
    EZ4_ALERT,['Unknown Time Units', $
               'Not Hours, Days, Months, or Years']
    ncerror = 1
    RETURN
ENDIF

;;;timesteps per day
ntperday = 0
IF (ntime GT 1) THEN BEGIN
   IF (ntime GT 2) THEN BEGIN
       dttime = time(2) - time(1)
   ENDIF ELSE dttime = time(1) - time(0)

   IF (timeflag EQ 0) THEN BEGIN ;hourly time
      IF (dttime LT 24) THEN ntperday=24./dttime
   ENDIF ELSE $
   IF (timeflag EQ 1) THEN BEGIN ;daily time
      IF (dttime LT 1) THEN ntperday=FIX(ROUND(1./dttime))
   ENDIF ELSE $
   IF (timeflag EQ 3) THEN BEGIN ;yearly time
       IF (startyr EQ -1) THEN startyr=floor(time[0])
       IF (stopyr EQ -1) THEN stopyr=floor(time[ntime-1])
       IF (numyrs EQ -1) THEN numyrs=stopyr-startyr+1

      ;dttime method
      ntperyear=365.
      IF (startyr MOD 4) EQ 0 THEN ntperyear++
      tperday=1./ntperyear
      IF (dttime LT tperday) THEN BEGIN
          ntperday1=ROUND(tperday/dttime)
      ENDIF ELSE ntperday1=0.

      ;since dttime is so small, 
      ;  try different method
      IF (ntime GE 24) THEN BEGIN
          mytime = time[0:23]
          mytimeday = (mytime-floor(mytime))*ntperyear - 1.
          daystart = (floor(mytimeday))[0]
          testref = WHERE(floor(mytimeday) EQ daystart)
          IF (testref[0] EQ -1) THEN ntperday2=0 ELSE $
          ntperday2 = N_ELEMENTS(testref)

          IF (ntperday2 EQ 1) THEN BEGIN
              mytime = time[24]
              mytimeday = (mytime-floor(mytime))*ntperyear - 1.
              daysecond = floor(mytimeday)
         
              IF (daysecond NE (daystart+1)) THEN ntperday2=0
           ENDIF
       ENDIF ELSE ntperday2=0.
       ntperday = MAX([ntperday1,ntperday2])
   ENDIF
   ;print,'Number of Timesteps Per Day: ',ntperday
ENDIF ;ntime > 1

;;;time names
;;;;Currently, set time names to time record numbers
timenames = strarr(ntime)

;IF ((timeflag EQ 2) AND (numyrs EQ 1)) THEN BEGIN
;    FOR t=0,ntime-1 DO BEGIN
;        timenames(t) = monname(time(t)-1)
;    ENDFOR
;ENDIF

;;;time conversions
;;;;For units of days, if more than one year convert to years
IF ((timeflag EQ 1) AND (numyrs GT 1)) THEN BEGIN
    timeyr = dblarr(ntime)
    yrref=startyr
    ndaysperyr = 365.
    IF ((yrref MOD 4) EQ 0) THEN ndaysperyr++

    FOR i=0,ntime-2 DO BEGIN
        tval = time(i)
        tvalnext = time(i+1)
        timeyr(i) = tval/ndaysperyr + yrref
        IF (tvalnext LT tval) THEN BEGIN
            yrref++
            ndaysperyr = 365.
            IF ((yrref MOD 4) EQ 0) THEN ndaysperyr++
        ENDIF
    ENDFOR
    timeyr(ntime-1) = time(ntime-1)/ndaysperyr + yrref

    time=timeyr
    timeflag = 3
    timeunit = ''
ENDIF

;;;;For units of years, if less than one year convert to days
IF ((timeflag EQ 3) AND (numyrs EQ 1)) THEN BEGIN
   timeday = dblarr(ntime)
   yrref=startyr
   ndaysperyr = 365.
   IF ((yrref MOD 4) EQ 0) THEN ndaysperyr++

   FOR i=0,ntime-1 DO BEGIN
       timeday(i) = (time(i)-yrref)*ndaysperyr - 1.
   ENDFOR

   time = timeday
   timeflag = 1     
   timeunit = 'DOY'

ENDIF

END

