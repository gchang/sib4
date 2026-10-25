PRO EZ4_SetUnits, unitin, unitout

unitout=unitin

IF (unitin EQ 'J/kg') THEN $
    unitout=textoidl('J kg^{-1}')
IF (unitin EQ 'kg/m2') THEN $
    unitout=textoidl('kg m^{-2}')
IF (unitin EQ 'micromol C/m2/s') THEN $
    unitout=textoidl('\mumol C m^{-2} s^{-1}')
IF (unitin EQ 'micromoles C/m2/s') THEN $
    unitout=textoidl('\mumol C m^{-2} s^{-1}')
IF (unitin EQ 'mol C/m2') THEN $
    unitout=textoidl('mol C m^{-2}')
IF (unitin EQ 's/m') THEN $
    unitout=textoidl('s m^{-1}')
IF (unitin EQ 'umol C/m2/s') THEN $
    unitout=textoidl('\mumol C m^{-2} s^{-1}')
IF ((unitin EQ 'W/m2') OR $
    (unitin EQ 'W/m^2')) THEN $
    unitout=textoidl('W m^{-2}')
IF (unitin EQ 'W/m2/nm/sr') THEN $
    unitout=textoidl('W m^{-2} nm^{-1} sr^{-1}')

END
