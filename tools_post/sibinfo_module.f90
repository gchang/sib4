module sibinfo_module

! -----------------------------
! SiB4 Output Information
! -----------------------------

implicit none


!Dimension Information
character(len=10), parameter :: nsibnamed = 'landpoints'
character(len=4),  parameter :: nsibnamer = 'nsib'
character(len=3),  parameter :: nluname = 'nlu'
character(len=4),  parameter :: npftname = 'npft'
character(len=5),  parameter :: ncropname = 'ncrop'
character(len=4),  parameter :: ncharname = 'clen'

!Required Variable Information
integer, parameter :: nvarsreq = 8

character(len=11), parameter :: &
    lonname   = 'lonsib     ',  &
    latname   = 'latsib     ',  &
    pftname   = 'pft_names  ',  &
    pftrefs   = 'pft_refnums',  &
    lareaname = 'lu_area    ',  &
    lprefname = 'lu_pref    ',  &
    sitename  = 'site_names ',  &
    timename  = 'time       '
character(len=11), dimension(nvarsreq), parameter :: &
    varsreq = (/lonname, latname, pftname, pftrefs, &
                lareaname, lprefname, sitename, timename/)

!Flux Variable Information
character(len=5), parameter :: gppname   = 'assim'
character(len=8), parameter :: respname  = 'resp_tot'
character(len=8), parameter :: respbname = 'resp_bal'
character(len=120),parameter :: &
    respblong = 'Balanced Respiration (Non-Crops)', &
    respbtitle= 'Balanced Respiration', &
    respbunits= 'micromoles C/m2/s'

character(len=9), parameter :: respcname = 'resp_crop'
character(len=120), parameter :: &
    respclong = 'Distributed Crop Respiration From Harvest', &
    respctitle= 'Crop Respiration From Harvest', &
    respcunits= 'micromoles C/m2/s'

!PFT Information
integer, parameter :: npft = 15
integer, parameter :: ncrop = 5

integer, parameter :: npftstart = 1
integer, parameter :: npftcropstart = 17
integer, parameter :: npftrefmax = 24

character(len=3), dimension(npft) :: &
   pftnames = (/'DBG','ENF','DNF','EBF','DBF', &
                'SHB','SHA','C3A','C3G','C4G', &
                'C3C','C4C','MZE','SOY','WWT'/)
integer, dimension(npft) :: &
   pftref = (/1,2,4,5,8, &
              11,12,13,14,15, &
              17,18,20,22,24/)
integer, dimension(npftrefmax) :: &
   pftnum = (/1,2,0,3,4,0,0,5,0,0, &
              6,7,8,9,10,0, &
              11,12,0,13,0,14,0,15/)

!Biome Information
integer, parameter :: nbiome = 8
character(len=20), dimension(nbiome) :: &
    biomenames = (/'Tropical Forest     ', & !(1)
                   'Temperate Forest    ', & !(2)
                   'Boreal Forest       ', & !(3)
                   'Tropical Sav/Grass  ', & !(4)
                   'Temperate Sav/Grass ', & !(5)
                   'Desert              ', & !(6)
                   'Tundra              ', & !(7)
                   'Crop                '/)  !(8)
integer, dimension(npft) :: &
   biomeref = (/6,2,3,1,2, &
                6,7,7,5,4, &
                8,8,8,8,8/)

!General Information
integer, parameter :: spinup_max=10
real, parameter :: regionfrac=0.90
character(len=3), dimension(12) :: &
     monnames = (/'Jan','Feb','Mar','Apr','May','Jun', &
                  'Jul','Aug','Sep','Oct','Nov','Dec'/)
integer, dimension(12) :: &
     dayspermon = (/31,28,31,30,31,30, &
                    31,31,30,31,30,31/)

!General Format Parameters
!...character only
character(len=20), parameter :: &
   fa = '(a)',      &
   fd = '(a180)',   &
   ff = '(a256)',   &
   fj = '(a45)',    &
   fv = '(a20)',    &
   f2a = '(2a)',    &
   f3a = '(3a)'

!...integer only
character(len=20), parameter :: &
    fi = '(i6)'

!...logical only
character(len=20), parameter :: &
    fl = '(l7)'

!...combos
character(len=20), parameter :: &
   f2ae= '(a,e14.6,a,e14.6)', &
   f2af= '(a,f14.8,a,f14.6)', &
   f2ai= '(a,i6,a,i6)', &
   f3ai= '(a,a,a,i6)',  &
   fae = '(a,e14.6)',  &
   faf = '(a,f14.6)',  &
   fa2e= '(a,2e12.6)', &
   fa2f= '(a,2f14.6)', &
   fa3f= '(a,3f14.6)', &
   fa5f= '(a,5f14.6)', &
   fai = '(a,i6)',   &
   fai8= '(a,i8)',   &
   fa2i= '(a,2i6)',  &
   fja = '(a45,a3)', &
   fji = '(a45,i4)', &
   fjl = '(a45,l7)'
   
!Compression Parameters - Deflate Level
integer, parameter :: deflev=2
logical, parameter :: use_deflate=.true.
!Only used if uncommented with netcdf4
!Valid Values Range From 1-9

!===========================================
!Diagnostic Output Information (SiB4 Vector)
integer, parameter :: ndiag = 6

character(len=4), dimension(ndiag), parameter :: &
    siblabel=(/'hr2 ','hr3 ','qp2 ','qp3 ','pbp1','pbp2'/)
character(len=5), dimension(ndiag), parameter :: &
    sibtype=(/'hsib2','hsib3','qsib2','qsib3','psib1','psib2'/)
character(len=5), dimension(ndiag), parameter :: &
    sibprefix=(/'hsib_','hsib_','qsib_','qsib_','psib_','psib_'/)
character(len=11), dimension(ndiag), parameter :: &
    sibsuffix=(/'.g.hr2.nc  ', '.lu.hr3.nc ', &
                '.g.qp2.nc  ', '.lu.qp3.nc ', &
                '.g.pbp1.nc ', '.lu.pbp2.nc'/)

integer, parameter :: ndiagpft = 3
integer, dimension(ndiagpft), parameter :: &
    ndrefg=(/1,3,5/), ndrefpft=(/2,4,6/)

!Diagnostic Output Information 
!...(crops/grids/pulling/unstacking)
character(len=6), dimension(ndiag), parameter :: &
    climprefix=(/'hclim_','hclim_','qclim_','qclim_', &
                 'pclim_','pclim_'/)

character(len=5), dimension(ndiag), parameter :: &
    cropprefix=(/'hcro_','hcro_','qcro_','qcro_', &
                 'pcro_','pcro_'/)

character(len=6), dimension(ndiag), parameter :: &
    gridprefix=(/'hgrid_','hgrid_','qgrid_','qgrid_', &
                 'pgrid_','pgrid_'/)
character(len=6), dimension(ndiag), parameter :: &
    gridsuffix=(/'.g.nc ','.lu.nc','.g.nc ','.lu.nc', &
                 '.g.nc ','.lu.nc'/)

character(len=6), dimension(ndiag), parameter :: &
    pullprefix=(/'hpull_','hpull_','qpull_','qpull_', &
                 'ppull_','ppull_'/)

character(len=5), dimension(ndiag), parameter :: &
    ratioprefix=(/'hbal_','hbal_','qbal_','qbal_', &
                  'pbal_','pbal_'/)

character(len=6), dimension(ndiag), parameter :: &
    ustackprefix=(/'sib4_','sib4_','sib4_', &
                   'sib4_','sib4_','sib4_'/)
character(len=8), dimension(ndiag), parameter :: &
    ustacksuffix=(/'.g.nc  ', '.pft.nc', &
                   '.g.nc  ', '.pft.nc', &
                   '.g.nc  ', '.pft.nc'/)


end module sibinfo_module
