;=======================================
PRO HIST_PLOT_EVENT, event
;=======================================

COMMON Missing_COM

WIDGET_CONTROL, event.id, GET_UVALUE=button

IF SIZE(button, /TYPE) EQ 8 THEN RETURN

IF ((button EQ 'REDRAW') OR (button EQ 'FULL PLOT') $
    OR (button EQ 'CALCULATE')) THEN BEGIN
  WIDGET_CONTROL, event.top, GET_UVALUE=plot, /NO_COPY
  ; Set the window index to the appropriate value
  WIDGET_CONTROL, plot.plotWindow, GET_VALUE=windowIndex
  WSET, windowIndex
ENDIF

IF button EQ 'REDRAW' THEN BEGIN

   Hist_ReDraw, plot, EVENT=event

ENDIF ELSE $
IF button EQ 'FULL PLOT' THEN BEGIN

  WIDGET_CONTROL, plot.yMinField, SET_VALUE=plot.savminy
  WIDGET_CONTROL, plot.yMaxField, SET_VALUE=plot.savmaxy
  plot.miny = plot.savminy
  plot.maxy = plot.savmaxy

  Hist_DrawPlot, plot
  WIDGET_CONTROL, event.top, SET_UVALUE=plot, /NO_COPY

ENDIF ELSE $
IF button EQ 'CALCULATE' THEN BEGIN

  EZ4_Undo, plot

  WIDGET_CONTROL, plot.xminField, GET_VALUE=minx
  WIDGET_CONTROL, plot.xintField, GET_VALUE=interval
  WIDGET_CONTROL, plot.xnbinField, GET_VALUE=nbins

  ;set new bin values
  binval = FINDGEN(nbins+1.)*interval + minx
  newydata = FLTARR(plot.numplots,nbins)

  ;put data into new bins
  odata = plot.origdata
  ntot = (size(odata))[2]

  FOR p=0,plot.numplots-1 DO BEGIN
      FOR i=0,ntot-1 DO BEGIN
           dataval = plot.origdata(p,i)
           IF (dataval GT missing) THEN BEGIN
               aref = (where(binval GT dataval))[0] - 1.
               IF (aref GE 0) THEN newydata(p,aref)++ ELSE $
               IF (binval[0] EQ dataval) THEN newydata(p,0)++ ELSE $
               IF (binval[nbins] EQ dataval) THEN newydata(p,nbins-1)++
            ENDIF
      ENDFOR
  ENDFOR           

  ;create a new structure
  hperror=0
  Hist_Struct_Data, newplot, hperror, $
      origdata=odata, $
      xdata=binval, ydata=newydata, $
      numbins=nbins, numplots=nlevs, $
      units=plot.units, title=plot.title, $
      xtitle=plot.xtitle, ytitle=plot.ytitle, $
      comment=plot.comment, filename=plot.filename, $
      simlabel=plot.simlabel
  IF (hperror EQ 1) THEN RETURN

  windowTitle=plot.title+' (New Bins)'
  Hist_Struct_Window, histwindow, windowTitle=windowTitle

  histplot = CREATE_STRUCT(newplot, histwindow)

  ;create new histogram plot
  Hist_Plot, histplot

ENDIF ELSE BEGIN
  EZ4_Alert,'***BAD SELECTION - ERROR IN HIST_PLOT***'
ENDELSE

END
