;Program to plot PFT fractions from a SiB vegetation file
;
;kdcorbin, 09/17
 
pro plotsib_pftfrac,psFile=psFile,lba=lba

;;;Input Info:
fileinfo='plotsib_pftfrac.txt'
pftchoice=0
wideortall=0
fname=''

npft=15
minpftarea=0.01

numgroups=11
groupname=['Bare Ground','Needleleaf Forest', 'Evergreen Forest', 'Deciduous Forest', $
           'Shrubs','C3 Grass','C4 Grass','Cropland','Maize','Soybean','Winter Wheat']

;;;crops
cvstart=10
cvstop=14

;;;grasses
gvstart=7
gvstop=9

;;;shrubs
svstart=5
svstop=6

;;;forests
fvstart=1
fvstop=4

mylevs=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
myformat='(F6.1)'
mycscheme=1
mycsize=1.7
mytsize=2.0

if (n_elements(psFile) ne 0) then begin
   mycsize=2.0
   mytsize=2.0
endif

;;;Open Plotting Preferences:
close,11
openr,11,fileinfo
readf,11,pftchoice
readf,11,wideortall
readf,11,fname
readf,11,fname
close,11

;;;Open SiB4 Vegetation:
    nid = ncdf_open(fname,/nowrite)

    ;Get dimensions
    dimid = ncdf_dimid(nid,'nsib')
    ncdf_diminq,nid,dimid,name,nsib
    dimid = ncdf_dimid(nid,'npft')
    ncdf_diminq,nid,dimid,name,npft

    dimid = ncdf_dimid(nid,'nlu')
    ncdf_diminq,nid,dimid,name,nlu
    ;print,'Landpoints/PFTs/LUs: ',nsib,npft,nlu
    
    ;Get PFT names
    varid = ncdf_varid(nid,'pft_names')
    ncdf_varget,nid,varid,tempnames
    pft_names=STRUPCASE(string(tempnames))

    ;Get PFT refs
    varid = ncdf_varid(nid,'pft_refs')
    ncdf_varget,nid,varid,pft_refs
    
    ;Get lat/lon arrays
    varid = ncdf_varid(nid,'lonsib')
    ncdf_varget,nid,varid,lonsib
    varid = ncdf_varid(nid,'latsib')
    ncdf_varget,nid,varid,latsib

    ;Get variables
    pft_frac=fltarr(nsib,nlu)
    varid = ncdf_varid(nid,'larea')
    ncdf_varget,nid,varid,pft_frac

    pft_ref = fltarr(nsib,nlu)
    varid = ncdf_varid(nid,'pftref')
    ncdf_varget,nid,varid,pft_ref

    ncdf_close,nid

    sibfrac=fltarr(nsib,npft)
    for n=0,nsib-1 do begin
        for l=0,nlu-1 do begin
            pref = pft_ref(n,l)
            pnum = where(pft_refs eq pref)
            parea = pft_frac(n,l)
            if (parea gt minpftarea) then begin
                sibfrac(n,pnum) = parea
            endif
         endfor
     endfor

;;;;;Plot Crops
IF (pftchoice EQ -4) THEN BEGIN
      top1=1.00
      bot1=0.50
      top2=0.62
      bot2=0.12

      left1=0.00
      right1=0.34
      left2=0.34
      right2=0.68
      left3=0.68
      right3=1.00

      off2=0.20
      myposition=[[left1+off2,bot1,right1+off2,top1],[left2+off2,bot1,right2+off2,top1], $
                  [left1,bot2,right1,top2],[left2,bot2,right2,top2], $
                  [left3,bot2,right3,top2]]

      center1=0.17
      center2=0.50
      center3=0.83
      col1=0.92
      col2=0.52
      tpos=[[center1+off2,col1],[center2+off2,col1],[center1,col2], $
            [center2,col2],[center3,col2]]
      mycpos=[0.20,0.12,0.80,0.15]

      set_display,psFile=psFile,/fill
      set_plot_colors,scheme=mycscheme

   for v=cvstart,cvstop do begin
       a=sibfrac(*,v)
       mytitle=pft_names(v)
       myctitle='Fractional Coverage'
       if (v eq cvstart) then begin
           plotsib,a,lonsib,latsib,/noerase, /robinson, $ 
              position=myposition(*,v-cvstart), $
              levels=mylevs,whiteout=2, $
              title=mytitle, tsize=mytsize, tposition=tpos(*,v-cvstart), $
              cbar_position=mycpos,cbar_title=myctitle,cbar_csize=mycsize, $
              labelFormat=myformat
        endif else begin
          plotsib,a,lonsib,latsib,/robinson, $
             position=myposition(*,v-cvstart), $
             levels=mylevs, /nocbar, $
             whiteout=2,/noerase,title=mytitle,tposition=tpos(*,v-cvstart),tsize=mytsize
       endelse
   endfor
ENDIF


;;;;;Plot Grasses
IF (pftchoice EQ -3) THEN BEGIN
      top1=0.95
      bot1=0.50
      top2=0.47
      bot2=0.02

      left1=0.00
      right1=0.50
      left2=0.50
      right2=1.00
      myposition=[[left1,bot1,right1,top1],[left2,bot1,right2,top1], $
                  [left1,bot2,right1,top2]]

      center1=0.25
      center2=0.75
      col1=0.95
      col2=0.46
      tpos=[[center1,col1],[center2,col1], $
            [center1,col2]]
      mycpos=[0.55,0.26,0.95,0.29]

      set_display,psFile=psFile,/fill
      set_plot_colors,scheme=mycscheme

   for v=gvstart,gvstop do begin
       vref=v-gvstart
       a=sibfrac(*,v)
       mytitle=pft_names(v)
       myctitle='Fractional Coverage'
       if (vref eq 0) then begin
           plotsib,a,lonsib,latsib,/noerase, /robinson, $
              position=myposition(*,vref), $
              levels=mylevs,whiteout=2, $
              title=mytitle, tsize=mytsize, tposition=tpos(*,vref), $
              cbar_position=mycpos,cbar_title=myctitle,cbar_csize=mycsize, $
              labelFormat=myformat
        endif else begin
          plotsib,a,lonsib,latsib,/robinson, $
             position=myposition(*,vref), $
             levels=mylevs, /nocbar, $
             whiteout=2,/noerase,title=mytitle,tposition=tpos(*,vref),tsize=mytsize
       endelse
   endfor
ENDIF

;;;;;Plot Shrubs
IF (pftchoice EQ -2) THEN BEGIN
      top1=1.00
      bot1=0.20

      left1=0.00
      right1=0.54
      left2=0.50
      right2=1.04

      myposition=[[left1,bot1,right1,top1],[left2,bot1,right2,top1]]

      center1=0.3
      center2=0.7
      col1=0.92
      tpos=[[center1,col1],[center2,col1]]

      mycpos=[0.20,0.15,0.80,0.19]
      set_display,psFile=psFile,/fill,/stripe
      set_plot_colors,scheme=mycscheme

   for v=svstart,svstop do begin
       a=sibfrac(*,v)
       mytitle=pft_names(v)
       myctitle='Fractional Coverage'
       if (v eq svstart) then begin
           plotsib,a,lonsib,latsib,/noerase, /robinson, $
              position=myposition(*,v-svstart), $
              levels=mylevs,whiteout=2, $
              title=mytitle, tsize=mytsize, tposition=tpos(*,v-svstart), $
              cbar_position=mycpos,cbar_title=myctitle,cbar_csize=mycsize, $
              labelFormat=myformat
        endif else begin
          plotsib,a,lonsib,latsib,/robinson, $
             position=myposition(*,v-svstart), $
             levels=mylevs, /nocbar, $
             whiteout=2,/noerase,title=mytitle,tposition=tpos(*,v-svstart),tsize=mytsize
       endelse
   endfor
ENDIF

;;;;;Plot Forests
IF (pftchoice EQ -1) THEN BEGIN
      top1=0.95
      bot1=0.50
      top2=0.47
      bot2=0.00

      left1=0.00
      right1=0.50
      left2=0.50
      right2=1.00
      myposition=[[left1,bot1,right1,top1],[left2,bot1,right2,top1], $
                  [left1,bot2,right1,top2],[left2,bot2,right2,top2]]

      center1=0.25
      center2=0.75
      col1=0.92
      col2=0.05
      tpos=[[center1,col1],[center2,col1], $
            [center1,col2],[center2,col2]]
      mycpos=[0.15,0.49,0.85,0.52]
      set_display,psFile=psFile,/fill
      set_plot_colors,scheme=mycscheme

   for v=fvstart,fvstop do begin
       a=sibfrac(*,v)
       mytitle=pft_names(v)
       myctitle='Fractional Coverage'
       if (v eq fvstart) then begin
           plotsib,a,lonsib,latsib,/noerase, /robinson, $
              position=myposition(*,v-fvstart), $
              levels=mylevs,whiteout=2, $
              title=mytitle, tsize=mytsize, tposition=tpos(*,v-fvstart), $
              cbar_position=mycpos,cbar_title=myctitle,cbar_csize=mycsize, $
              labelFormat=myformat
        endif else begin
          plotsib,a,lonsib,latsib,/robinson, $
             position=myposition(*,v-fvstart), $
             levels=mylevs, /nocbar, $
             whiteout=2,/noerase,title=mytitle,tposition=tpos(*,v-fvstart),tsize=mytsize
       endelse
   endfor
ENDIF


;;;;;Plot Group Sample
IF (pftchoice EQ 0) THEN BEGIN
   ;;;;;Set up plotting stuff
   ;3 rows, 4 columns
   if wideortall eq 0 then begin
      top1=0.98
      bot1=0.64
      top2=0.66
      bot2=0.32
      top3=0.36
      bot3=0.02

      left1=0.02
      right1=0.28 
      left2=0.26
      right2=0.52
      left3=0.5
      right3=0.76
      left4=0.74
      right4=0.98

      myposition=[[left1,bot1,right1,top1],[left2,bot1,right2,top1], $
                    [left3,bot1,right3,top1],[left4,bot1,right4,top1], $
                 [left1,bot2,right1,top2],[left2,bot2,right2,top2], $
                    [left3,bot2,right3,top2],[left4,bot2,right4,top2], $
                 [left1,bot3,right1,top3],[left2,bot3,right2,top3], $
                    [left3,bot3,right3,top3],[left4,bot3,right4,top3]]
      center1=0.15
      center2=0.4
      center3=0.65
      center4=0.86
      col1=0.95
      col2=0.63
      col3=0.33
      tpos=[[center1,col1],[center2,col1],[center3,col1],[center4,col1], $
            [center1,col2],[center2,col2],[center3,col2],[center4,col2], $
            [center1,col3],[center2,col3],[center3,col3],[center4,col3]]
      cpos=[0.70,0.2,0.95,0.23]
      set_display,psFile=psFile,/fill,/wide
   endif else begin

      ;4 rows, 3 columns
      top1=0.99
      bot1=0.73
      top2=0.75
      bot2=0.49
      top3=0.49
      bot3=0.25
      top4=0.25
      bot4=0.0

      left1=0.02
      right1=0.35
      left2=0.33
      right2=0.68
      left3=0.66
      right3=0.98

      myposition=[[left1,bot1,right1,top1],[left2,bot1,right2,top1], $
                      [left3,bot1,right3,top1], $
                  [left1,bot2,right1,top2],[left2,bot2,right2,top2], $
                      [left3,bot2,right3,top2], $
                  [left1,bot3,right1,top3],[left2,bot3,right2,top3], $
                      [left3,bot3,right3,top3], $
                  [left1,bot4,right1,top4],[left2,bot4,right2,top4], $
                      [left3,bot4,right3,top4]]
      center1=0.2
      center2=0.5
      center3=0.82
      col1=0.97
      col2=0.73
      col3=0.48
      col4=0.24
      tpos=[[center1,col1],[center2,col1],[center3,col1], $
            [center1,col2],[center2,col2],[center3,col2], $
            [center1,col3],[center2,col3],[center3,col3], $
            [center1,col4],[center2,col4],[center3,col4]]
      cpos=[0.7,0.15,0.96,0.18]
      cbar_lablev=0.05
      cbar_off=0.02

      set_display,psFile=psFile,/fill
   endelse

   set_plot_colors,scheme=mycscheme
   for v=0,numgroups-1 do begin
       if (v eq 0) then a=sibfrac(*,0)
       if (v eq 1) then a=sibfrac(*,1)+sibfrac(*,2)
       if (v eq 2) then a=sibfrac(*,3)
       if (v eq 3) then a=sibfrac(*,4)
       if (v eq 4) then a=sibfrac(*,5)+sibfrac(*,6)
       if (v eq 5) then a=sibfrac(*,7)+sibfrac(*,8)
       if (v eq 6) then a=sibfrac(*,9)
       if (v eq 7) then a=sibfrac(*,10)+sibfrac(*,11)
       if (v eq 8) then a=sibfrac(*,12)
       if (v eq 9) then a=sibfrac(*,13)
       if (v eq 10) then a=sibfrac(*,14)

       mytitle=groupname(v)
       myctitle='Fractional Coverage'
       if (v eq 0) then begin
           plotsib,a,lonsib,latsib,/robinson, $
              position=myposition(*,v),levels=mylevs, $
              cbar_position=cpos,cbar_title=myctitle,cbar_csize=mycsize, $
              labelFormat=myformat,cbar_lablev=cbar_lablev, $
              cbar_off=cbar_off, whiteout=2,/noerase, $
              title=mytitle,tposition=tpos(*,v),tsize=mytsize
        endif else begin
          plotsib,a,lonsib,latsib,/robinson, $
             position=myposition(*,v),levels=mylevs, /nocbar, $
             whiteout=2,/noerase,title=mytitle,tposition=tpos(*,v),tsize=mytsize
        endelse
   endfor
endif

pftmax=max(pft_refs)
IF ((pftchoice GE 1) AND (pftchoice LE pftmax)) then begin

       pref=where(pft_refs eq pftchoice)
       set_display,psFile=psFile,/small,/fill
       set_plot_colors,scheme=1

       mylevs=[0.05,0.15,0.3,0.45,0.60,0.75,0.9]
       myposition=[0.03,0.2,0.97,.9]
       mycpos=[0.2,0.17,0.8,0.2]   
       myctitle='Fractional Coverage'
       myformat='(F5.2)'
       mytitle=pft_names(pref)
       tpos=[0.5,0.86]
       csize=1.8
       tsize=2.5
       a=sibfrac(*,pref)

       if (n_elements(psFile) ne 0) then begin
           mycpos=[0.15,0.17,0.85,0.22]
           axiscsize=2.0
           csize=3.0
           tsize=4.0
           coff=0.0
       endif

       plotsib,a,lonsib,latsib,/noerase, /robinson, $
              position=myposition, $
              levels=mylevs,whiteout=2, $
              title=mytitle,tsize=tsize, tposition=tpos, $
              cbar_position=mycpos,cbar_title=myctitle,cbar_csize=csize, $
              labelFormat=myformat, lba=lba

endif  ;pft specified

if (n_elements(psFile) ne 0) then device,/close

end
 
