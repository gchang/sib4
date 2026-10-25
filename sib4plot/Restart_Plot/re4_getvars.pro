;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO RE4_GETVARS, filename,  $
    nsib, lonsib, latsib, sitesib, $
    nvarsre, namere, titlere, unitre, $
    reerror
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@ez4_BasicParams.com

WIDGET_CONTROL, /HOURGLASS
reerror = 0

;;Open the file and get dimensions
ncid = EZ4_OPENNCDF(filename)

did = NCDF_DIMID(ncid,'nsib')
IF (did GE 0) THEN NCDF_DIMINQ, ncid, did, x, nsib

NCDF_VARGET, ncid, 'lonsib', lonsib
NCDF_VARGET, ncid, 'latsib', latsib

nludid = NCDF_DIMID(ncid,'nlu')
IF (nludid LT 0) THEN nludid = NCDF_DIMID(ncid,'nlu')
IF (nludid GE 0) THEN NCDF_DIMINQ, ncid, nludid, x, nlure
IF (nlu NE nlure) THEN BEGIN
   ez4_alert,'Restart File Mismatching NLU.'
   reerror = 1
   RETURN
ENDIF

npftdid = NCDF_DIMID(ncid,'npft')
IF (npftdid LT 0) THEN npftdid = NCDF_DIMID(ncid,'npft')
IF (npftdid GE 0) THEN NCDF_DIMINQ, ncid, npftdid, x, npftre
IF (npft NE npftre) THEN BEGIN
   ez4_alert,'Restart File Mismatching NPFT.'
   reerror = 1
   RETURN
ENDIF

npoolpftdid = NCDF_DIMID(ncid,'npoolpft')
IF (npoolpftdid GE 0) THEN $
    NCDF_DIMINQ, ncid, npoolpftdid, x, npoolpftre
IF (npoolpftre NE npoolpft) THEN BEGIN
   ez4_alert,'Restart File Mismatchin NPOOLPFT.'
   reerror = 1
   RETURN
ENDIF

npoolludid = NCDF_DIMID(ncid,'npoollu')
IF (npoolludid GE 0) THEN $
   NCDF_DIMINQ, ncid, npoolludid, x, npoollure
IF (npoollu NE npoollure) THEN BEGIN
   ez4_alert,'Restart File Mismatching NPOOLLU.'
   reerror = 1
   RETURN
ENDIF

nsoildid = NCDF_DIMID(ncid,'nsoil')
IF (nsoildid GE 0) THEN $
   NCDF_DIMINQ, ncid, nsoildid, x, nsoilre
IF (nsoil NE nsoilre) THEN BEGIN
   ez4_alert,'Restart File Mistmatching NSOIL.'
   reerror = 1
   RETURN
ENDIF

nsnowdid = NCDF_DIMID(ncid, 'nsnow')
IF (nsnowdid GE 0) THEN $
   NCDF_DIMINQ, ncid, nsnowdid, x, nsnowre
IF (nsnow NE nsnowre) THEN BEGIN
   ez4_alert,'Restart File Mistmacthing NSNOW.'
   reerror = 1
   RETURN
ENDIF

;;Get the variable information
nvars = (NCDF_INQUIRE(ncid)).NVARS
nvarsre = 0

sitesib = ''
dovar = INTARR(nvars)
FOR id = 0, nvars-1 DO BEGIN
    var = NCDF_VARINQ(ncid,id)

    IF (var.name EQ 'site_names') THEN BEGIN
        sitesib = STRARR(nsib)
        NCDF_VARGET, ncid, var.name, tempsites
        FOR n=0, nsib-1 DO BEGIN
            sitesib(n) = string(tempsites(*,n))
        ENDFOR
    ENDIF 

    IF ((var.name NE 'lonsib') AND $
        (var.name NE 'latsib') AND $
        (var.name NE 'pftnames') AND $
        (var.name NE 'pftrefs') AND $
        (var.name NE 'poolpft_names') AND $
        (var.name NE 'poollu_names') AND $
        (var.name NE 'site_names')) THEN BEGIN
         dovar[id] = 1
         nvarsre++
    ENDIF
ENDFOR

namere = STRARR(nvarsre)
titlere = STRARR(nvarsre)
unitre = STRARR(nvarsre)

iv = 0
FOR id = 0, nvars-1 DO BEGIN
    IF (dovar(id) EQ 1) THEN BEGIN
       var = NCDF_VARINQ(ncid,id)

       namere[iv] = STRTRIM(var.name,2)
       CASE namere[iv] OF
            'lu_area': BEGIN
                titlere[iv] = 'Areal Coverage'
                unitre[iv] = 'Grid Fraction (%)'
            END
            'lu_pftref': BEGIN
                titlere[iv] = 'PFT Reference'
                unitre[iv] = ''
            END

            'capacc_liq': BEGIN
                titlere[iv] = 'Canopy Surface Liquid'
                unitre[iv] = 'kg/m2'
             END

            'capacc_snow': BEGIN
                titlere[iv] = 'Canopy Surface Snow'
                unitre[iv] = 'kg/m2'
             END

            'capacg': BEGIN
                titlere[iv] = 'Ground Surface Liquid'
                unitre[iv] = 'kg/m2'
             END

            'clim_assim': BEGIN
                titlere[iv] = 'Climatological Assimilation'
                unitre[iv] = 'mol C/m2/s'
             END

            'clim_cupr': BEGIN
                titlere[iv] = 'Climatological Daily Convective Precip'
                unitre[iv] = 'mm/day'
             END

            'clim_lai': BEGIN
                titlere[iv] = 'Climatological LAI'
                unitre[iv] = ''
             END

            'clim_pawfrw': BEGIN
                titlere[iv] = 'Climatological Root-Weighted PAW Fraction'
                unitre[iv] = ''
             END

            'clim_precip': BEGIN
                titlere[iv] = 'Climatological Daily Precipitation'
                unitre[iv] = 'mm/day'
             END

            'clim_tawfrw': BEGIN
                titlere[iv] = 'Climatological Root-Weighted TAW Fraction'
                unitre[iv] = ''
             END

            'clim_tm': BEGIN
                titlere[iv] = 'Climatological Daily Temperature'
                unitre[iv] = 'K'
             END

            'dzsnow': BEGIN
                titlere[iv] = 'Snow Layer Thickness'
                unitre[iv] = 'm'
             END

            'eacas': BEGIN
                titlere[iv] = 'CAS Water Vapor Pressure'
                unitre[iv] = 'hPa or mb'
             END

            'nsl': BEGIN
                titlere[iv] = 'Number of Snow Layers'
                unitre[iv] = ''
             END

            'pco2cas': BEGIN
                titlere[iv] = 'CAS CO2 Partial Pressure'
                unitre[iv] = 'Pa'
             END

            'pcoscas': BEGIN
                titlere[iv] = 'CAS COS Partial Pressure'
                unitre[iv] = 'Pa'
             END

            'phenave_assim': BEGIN
                titlere[iv] = 'Phenology Mean Assimilation'
                unitre[iv] = 'mol C/m2/s'
             END

            'phenave_assimsm': BEGIN
               titlere[iv] = 'Phenology Assimilation Seasonal Maximum'
               unitre[iv] = 'mol C/m2/s'
            END

            'phenave_env': BEGIN
                titlere[iv] = 'Phenology Mean Environmental Potential'
                unitre[iv] = ''
             END

            'phenave_tawftop': BEGIN
                titlere[iv] = 'Phenology Mean TAWFTOP Potential'
                unitre[iv] = ''
             END

            'phenave_tm': BEGIN
                titlere[iv] = 'Phenology Mean Temperature'
                unitre[iv] = 'K'
             END

            'phenave_wa': BEGIN
                titlere[iv] = 'Phenology Mean Water Availability Potential'
                unitre[iv] = ''
             END

            'phenave_wacsm': BEGIN
                titlere[iv] = 'Phenology Mean Combined Potential Seasonal Max'
                unitre[iv] = ''
             END

            'phenc_climp': BEGIN
                titlere[iv] = 'Phenology Climatological Suitability'
                unitre[iv] = ''
             END

             'phenc_laimax': BEGIN
                titlere[iv] = 'Phenology Climatological Potential LAI Max'
                unitre[iv] = ''
             END

             'phenc_laimin': BEGIN
                titlere[iv] = 'Phenology Climatological Potential LAI Min'
                unitre[iv] = ''
             END

            'poollu': BEGIN
                titlere[iv] = 'Dead Pools'
                unitre[iv] = 'mol C/m2'
             END

            'poolpft': BEGIN
                titlere[iv] = 'Live Pools'
                unitre[iv] = 'mol C/m2'
             END

            'rst': BEGIN
                titlere[iv] = 'Stomatal Resistance'
                unitre[iv] = 's/m'
             END

            'seas_precip': BEGIN
                titlere[iv] = 'Seasonal Precipitation'
                unitre[iv] = 'mm/day'
             END

            'shcas': BEGIN
                titlere[iv] = 'CAS Water Vapor Pressure Mixing Ratio'
                unitre[iv] = 'kg/kg'
             END

             'tc': BEGIN
                titlere[iv] = 'Canopy Temperature'
                unitre[iv] = 'K'
             END

            'tcas': BEGIN
                titlere[iv] = 'CAS Temperature'
                unitre[iv] = 'K'
             END

            'td': BEGIN
                titlere[iv] = 'Soil/Snow Temperature'
                unitre[iv] = 'K'
             END

            'tkecas': BEGIN
                titlere[iv] = 'CAS Turbulent Kinetic Energy'
                unitre[iv] = 'J/kg'
             END

            'www_ice': BEGIN
                titlere[iv] = 'Soil/Snow Ice'
                unitre[iv] = 'kg/m2'
             END

            'www_liq': BEGIN
                titlere[iv] = 'Soil/Snow Liquid Water'
                unitre[iv] = 'kg/m2'
             END

            else: BEGIN
                titlere[iv] = ''
                unitre[iv] = ''
            END
       ENDCASE
       iv++
    ENDIF  ;dovar
ENDFOR ;nvars id

NCDF_CLOSE, ncid

;;Reorder all variables based on 
;;alphabetical listing of names
IF (sortChoice EQ 1) THEN BEGIN
   order = SORT(namere)
   namere = namere(order)
   titlere = titlere(order)
   unitre = unitre(order)
ENDIF

END 
