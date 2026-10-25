!==================================================
subroutine get_site_info( &
     nsites, sitename, &
     fprefix, fneename, fgppname, frename, &
     fprname, flename, fshname, fswname, flstflag)
!==================================================

implicit none

!input variables
integer, intent(in) :: nsites
character(len=6), dimension(nsites), intent(in) :: sitename

character(len=8), dimension(nsites), intent(inout) :: fprefix
character(len=20), dimension(nsites), intent(inout) :: fneename
character(len=20), dimension(nsites), intent(inout) :: fgppname, frename
character(len=20), dimension(nsites), intent(inout) :: flename, fshname
character(len=20), dimension(nsites), intent(inout) :: fprname, fswname
logical, dimension(nsites), intent(inout) :: flstflag

!local variables
integer :: ns

!--------------------
fprefix(:) = ''
fneename(:) = ''
fgppname(:) = ''
frename(:) = ''
fprname(:) = ''
flename(:) = ''
fshname(:) = ''
fswname(:) = ''
flstflag(:) = .FALSE.
do ns=1,nsites
   
   select case (sitename(ns))
   case('AR-Slu')
       fprefix(ns) ='slu_0911'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS' 
       fprname(ns) = 'P'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AR-Vir')
       fprefix(ns) ='vir_0912'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AT-Neu')
       fprefix(ns) ='neu_0212'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS' 
       fprname(ns) = 'P'
       fswname(ns) ='SW_IN_F'
       
    case('AU-Ade')
       fprefix(ns) ='ade_0709'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS' 
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-ASM')
       fprefix(ns) ='asm_1013'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Cpr')
       fprefix(ns) ='cpr_1013'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Cum')
       fprefix(ns) ='cum_1214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-DaP')
       fprefix(ns) ='dap_0813'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS' 
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-DaS')
       fprefix(ns) ='das_0714'
       fneename(ns)='Fc'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='Fe'
       fshname(ns) ='Fh'
       fprname(ns) =''
       fswname(ns) ='FSD'
       flstflag(ns)=.TRUE.

    case('AU-Dry')
       fprefix(ns) ='dry_0813'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Emr')
       fprefix(ns) ='emr_1113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-GWW')
       fprefix(ns) ='gww_1314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) = 'P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Otw')
       fprefix(ns) ='otw_0710'
       fneename(ns)='Fc'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='Fe'
       fshname(ns) ='Fh'
       fprname(ns) ='PRECIP'
       fswname(ns) ='FSD'
       flstflag(ns)=.TRUE.

    case('AU-RDF')
       fprefix(ns) ='rdf_1113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Rig')
       fprefix(ns) ='rig_1113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Rob')
       fprefix(ns) ='rob_14'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Stp')
       fprefix(ns) ='stp_0813'
       fneename(ns)='Fc'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='Fe'
       fshname(ns) ='Fh'
       fprname(ns) =''
       fswname(ns) ='FSD'
       flstflag(ns)=.TRUE.

    case('AU-Tum')
       fprefix(ns) ='tum_0113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Whr')
       fprefix(ns) ='whr_1114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Wom')
       fprefix(ns) ='wom_1014'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('AU-Ync')
       fprefix(ns) ='ync_1214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('BE-Bra')
       fprefix(ns) ='bra_9614'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('BE-Lon')
       fprefix(ns) ='lon_0416'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('BE-Vie')
       fprefix(ns) ='vie_9614'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('BR-Ban')
       fprefix(ns) ='ban_0306'
       fneename(ns)='Fc'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RGS'
       flstflag(ns)=.TRUE.

    case('BR-Cax')
      fprefix(ns) ='cax_9903'
      fneename(ns)='Fc'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

   case('BR-Ji1')
      fprefix(ns) ='ji1_9902'
      fneename(ns)='FCO2'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

    case('BR-Ji2')
      fprefix(ns) ='ji2_9902'
      fneename(ns)='Fc'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

    case('BR-Ma2')
      fprefix(ns) ='ma2_9906'
      fneename(ns)='Fc'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

    case('BR-Sa1')
      fprefix(ns) ='sa1_0205'
      fneename(ns)='Fc'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

    case('BR-Sa2')
      fprefix(ns) ='sa2_0105'
      fneename(ns)='Fc'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

    case('BR-Sa3')
      fprefix(ns) ='sa3_0004'
      fneename(ns)='Fc'
      fgppname(ns)=''
      frename(ns) =''
      flename(ns) ='LH'
      fshname(ns) ='H'
      fprname(ns) ='PREC'
      fswname(ns) ='RGS'
      flstflag(ns)=.TRUE.

    case('BR-Sp1')
       fprefix(ns) ='sp1_0203'
       fneename(ns)='Fc'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RGS'
       flstflag(ns)=.TRUE.

    case('CA-Gro')
       fprefix(ns) ='gro_0314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE. 

    case('CA-Let')
       fprefix(ns) ='let_9907'
       fneename(ns)='NEE'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LatentHeatFlux'
       fshname(ns) ='SensibleHeatFlux'
       fprname(ns) =''
       fswname(ns) =''

    case('CA-Man')
       fprefix(ns) ='man_9408'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CA-NS1')
       fprefix(ns) ='ns1_0205'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CA-NS3')
       fprefix(ns) ='ns3_0105'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CA-NS5')
       fprefix(ns) ='ns5_0105'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CA-Oas')
       fprefix(ns) ='oas_9610'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE. 

    case('CA-Obs')
       fprefix(ns) ='obs_9710'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CA-Qfo')
       fprefix(ns) ='qfo_0310'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE. 

    case('CA-TP4')
       fprefix(ns) ='tp4_0214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE. 

    case('CA-TPD')
       fprefix(ns) ='tpd_1214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE. 

    case('CG-Tch')
       fprefix(ns) ='tch_0609'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CH-Cha')
       fprefix(ns) ='cha_0612'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CH-Dav')
       fprefix(ns) ='dav_9714'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CH-Fru')
       fprefix(ns) ='fru_0612'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CH-Lae')
       fprefix(ns) ='lae_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CH-Oe1')
       fprefix(ns) ='oe1_0208'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CH-Oe2')
       fprefix(ns) ='oe2_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CN-Cha')
       fprefix(ns) ='cha_0305'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Cng')
       fprefix(ns) ='cng_0710'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Dan')
       fprefix(ns) ='dan_0405'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_DT_VUT_REF'
       frename(ns) ='RECO_DT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Din')
       fprefix(ns) ='din_0305'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Du2')
       fprefix(ns) ='du2_0608'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Du3')
       fprefix(ns) ='du3_0910'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE. 

    case('CN-HaM')
       fprefix(ns) ='ham_0204'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_DT_VUT_REF'
       frename(ns) ='RECO_DT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Hab')
       fprefix(ns) ='hab_0305'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns) =''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CN-LSH')
       fprefix(ns) ='lsh_02'
       fneename(ns)='NEE'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PPT'
       fswname(ns) ='RG'

    case('CN-QHB')
        fprefix(ns) ='qhb_0204'
        fneename(ns)='NEE'
        fgppname(ns)=''
        frename(ns) =''
        flename(ns) =''
        fshname(ns) =''
        fprname(ns) =''
        fswname(ns) =''

    case('CN-Qia')
       fprefix(ns) ='qia_0305'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CN-Sw2')
       fprefix(ns) ='sw2_1012'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('CZ-BK1')
       fprefix(ns) ='bk1_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('CZ-BK2')
       fprefix(ns) ='bk2_0411'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Geb')
       fprefix(ns)='geb_0114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Gri')
       fprefix(ns) ='gri_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Hai')
       fprefix(ns) ='hai_0012'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Kli')
       fprefix(ns) ='kli_0415'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('DE-Lkb')
       fprefix(ns) ='lkb_0913'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Lnf')
       fprefix(ns) ='lnf_0212'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Obe')
       fprefix(ns) ='obe_0814'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-RuS')
       fprefix(ns) ='rus_1114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Seh')
       fprefix(ns) ='seh_0710'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Spw')
       fprefix(ns) ='spw_1014'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Tha')
       fprefix(ns) ='tha_9614'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DE-Zrk')
       fprefix(ns) ='zrk_1314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DK-Eng')
       fprefix(ns) ='eng_0508'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DK-Fou')
       fprefix(ns) ='fou_0505'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DK-NuF')
       fprefix(ns) ='nuf_0814'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('DK-Ris')
       fprefix(ns) ='ris_0408'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('DK-Sor')
       fprefix(ns) ='sor_9614'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('DK-ZaH')
       fprefix(ns) ='zah_0014'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('ES-Amo')
       fprefix(ns) ='amo_0712'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('ES-ES2')
       fprefix(ns) ='es2_0408'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('ES-LJu')
       fprefix(ns) ='lju_0413'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
    
    case('ES-LgS')
       fprefix(ns) ='lgs_0709'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns)='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FI-Hyy')
       fprefix(ns) ='hyy_9614'
       fneename(ns)='NEE_VUT_REF' 
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('FI-Jok')
       fprefix(ns) ='jok_0003'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) =''
       fshname(ns) =''
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FI-Let')
       fprefix(ns) ='let_0912'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FI-Lom')
       fprefix(ns) ='lom_0709'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FI-Sod')
       fprefix(ns) ='sod_0114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FR-Aur')
       fprefix(ns) ='aur_0810'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('FR-Avi')
       fprefix(ns) ='avi_0407'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('FR-Fon')
       fprefix(ns) ='fon_0514'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FR-Gri')
       fprefix(ns) ='gri_0414'
       fneename(ns)=''
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('FR-Lam')
       fprefix(ns) ='lam_0410'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('FR-Pue')
       fprefix(ns) ='pue_0014'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('GF-Guy')
       fprefix(ns) ='guy_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('GH-Ank')
       fprefix(ns) ='ank_1114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
    
    case('HU-He1')
       fprefix(ns) ='he1_9710'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IE-Ca1')
       fprefix(ns) ='ca1_0408'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IE-Dri')
       fprefix(ns) ='dri_0305'
       fneename(ns)='NEE_f'
       fgppname(ns)='GPP_f'
       frename(ns) ='Reco'
       flename(ns) ='LE_f'
       fshname(ns) ='H_f'
       fprname(ns) ='PRECIP_F'
       fswname(ns) =''

    case('IT-BCi')
       fprefix(ns) ='bci_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-CA1')
       fprefix(ns) ='ca1_1114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-CA2')
       fprefix(ns) ='ca2_1113'   
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Cas')
       fprefix(ns) ='cas_0610'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IT-Col')
       fprefix(ns) ='col_9614'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Cpz')
       fprefix(ns) ='cpz_9709'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Ctv')
       fprefix(ns) ='ctv_0607'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IT-Isp')
       fprefix(ns) ='isp_1314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Lav')
       fprefix(ns) ='lav_0314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-MBo')
       fprefix(ns) ='mbo_0306'
       fneename(ns)='NEE_f'
       fgppname(ns)='GPP_f'
       frename(ns) ='Reco'
       flename(ns) ='LE_f'
       fshname(ns) ='H_f'
       fprname(ns) ='PRECIP_F'
       fswname(ns) ='SWIN'

    case('IT-Neg')
       fprefix(ns) ='neg_0610'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IT-Noe')
       fprefix(ns) ='noe_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-PT1')
       fprefix(ns) ='pt1_0204'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF' 
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Ren')
       fprefix(ns) ='ren_9813'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS' 
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       
    case('IT-Ro1')
       fprefix(ns) ='ro1_0008'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF' 
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Ro2')
       fprefix(ns) ='ro2_0212'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Ro3')
       fprefix(ns) ='ro3_0713'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IT-Ro4')
       fprefix(ns) ='ro4_0713'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('IT-SR2')
       fprefix(ns) ='sr2_1314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-SRo')
       fprefix(ns) ='sro_9912'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('IT-Tor')
       fprefix(ns) ='tor_0813'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_DT_VUT_REF'
       frename(ns) ='RECO_DT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('JP-MBF')
       fprefix(ns) ='mbf_0305'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('JP-SMF')
       fprefix(ns) ='smf_0206'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('JP-TMK')
       fprefix(ns) ='tmk_0103'
       fneename(ns)='NEE'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('MO-SKT')
       fprefix(ns) ='skt_0305'
       fneename(ns)='Fc'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PPT'
       fswname(ns) ='RG'
    
    case('MX-Lpa')
       fprefix(ns) ='lpa_0508'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('MY-PSO')
       fprefix(ns) ='pso_0309'
       fneename(ns)='NEE_VUT_REF'    
       fgppname(ns)='GPP_NT_VUT_REF' 
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('NL-Hor')
       fprefix(ns) ='hor_0411'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('NL-Lan')
       fprefix(ns) ='lan_0506'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('NL-Loo')
       fprefix(ns) ='loo_9614'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('NL-Lut')
       fprefix(ns) ='lut_0607'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('NL-Mol')
       fprefix(ns) ='mol_0506'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('PA-SPn')
       fprefix(ns) ='spn_0709'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('PA-SPs')
       fprefix(ns) ='sps_0709'
       fneename(ns)='NEE_VUT_REF' 
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('PH-RIf')
       fprefix(ns) ='rif_1214'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('PL-Brd')
       fprefix(ns) ='brd_1112'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('PT-Mi1')
       fprefix(ns) ='mi1_0305'
       fneename(ns)='NEE_st_fMDS'
       fgppname(ns)='GPP_st_ANN'
       frename(ns) ='Reco_st'
       flename(ns) ='LE_f'
       fshname(ns) ='H_f'
       fprname(ns) ='PRECIP'
       fswname(ns) =''

    case('RU-Che')
       fprefix(ns) ='che_0205'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('RU-Cok')
       fprefix(ns) ='cok_0314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.
    
    case('RU-Fyo')
       fprefix(ns) ='fyo_9814'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('RU-Ha1')
       fprefix(ns) ='ha1_0204'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('RU-Sam')
       fprefix(ns) ='sam_0214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_DT_VUT_REF'
       frename(ns) ='RECO_DT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('RU-SkP')
       fprefix(ns) ='skp_1214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)=''
       frename(ns) ='' 
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.
    
    case('RU-Vrk')
       fprefix(ns) ='vrk_08'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('RU-Ylf')
       fprefix(ns) ='ylf_1214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.
    
    case('SD-Dem')
       fprefix(ns) ='dem_0509'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('SE-St1')
       fprefix(ns) ='st1_1214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('UK-ESa')
       fprefix(ns) ='esa_0306'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('UK-Her')
       fprefix(ns) ='her_0606'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'

    case('US-An3')
       fprefix(ns) ='an3_0810'
       fneename(ns)='NEE_PI'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
    
    case('US-AR1')
       fprefix(ns) ='ar1_0912'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-AR2')
       fprefix(ns) ='ar2_0912'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Arc')
       fprefix(ns) ='arc_0506'
       fneename(ns)='NEE'   
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'

    case('US-ARM')
       fprefix(ns) ='arm_0312'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Atq')
       fprefix(ns) ='atq_9906'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PRECIP'
       fswname(ns) ='RG_IN'

    case('US-Bkg')
       fprefix(ns) ='bkg_0410'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) =''
       fswname(ns) ='RG'

    case( 'US-Blo')
       fprefix(ns) ='blo_9807'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Bo1')
       fprefix(ns) ='bo1_9608'
       fneename(ns)='NEE_PI'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-Bo2')
       fprefix(ns) ='bo2_0408'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-Brw')
       fprefix(ns) ='brw_9907'
       fneename(ns)='NEE'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) =''
       flstflag(ns)=.TRUE.

    case('US-CRT')
       fprefix(ns) ='crt_1113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Ctn')
       fprefix(ns) ='ctn_0708'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-Dk2')
       fprefix(ns) ='dk2_0108'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) ='RECO_PI'
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-Dk3')
       fprefix(ns) ='dk3_9808'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) ='RECO_PI'
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-FPe')
       fprefix(ns) ='fpe_0008'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-GBT')
       fprefix(ns) ='gbt_9906'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-GLE')
       fprefix(ns) ='gle_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       fprname(ns) = 'P'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fswname(ns) ='SW_IN_F'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Goo')
       fprefix(ns) ='goo_0206'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Ha1')
       fprefix(ns) ='ha1_9112'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Ho1')
       fprefix(ns) ='ho1_9614'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

   case('US-HVa')
       fprefix(ns) ='hva_9495'
       fneename(ns)='NEE'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-IB1')
       fprefix(ns) ='ib1_0511'
       fneename(ns) ='NEE'
       fgppname(ns) ='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-IB2')
       fprefix(ns) ='ib2_0411'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-ICh')
       fprefix(ns) ='ich_0711'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-ICt')
       fprefix(ns) ='ict_0711'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-Ivo')
       fprefix(ns) ='ivo_0307'
       fneename(ns)='NEE_PI'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.
    
    case('US-KFS')
       fprefix(ns) ='kfs_0812'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

   case('US-Kon')
       fprefix(ns) ='kon_0712'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       
    case('US-KS1')
       fprefix(ns) ='ks1_02'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-KS2')
       fprefix(ns) ='ks2_0306'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Lin')
       fprefix(ns) ='lin_0910'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Los')
       fprefix(ns) ='los_0014'
       fneename(ns)='NEE_VUT_REF' 
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Me2')
       fprefix(ns) ='me2_0214'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns)='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-MMS')
       fprefix(ns) ='mms_9914'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-MOz')
       fprefix(ns) ='moz_0415'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-Ne1')
       fprefix(ns) ='ne1_0113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Ne2')
       fprefix(ns) ='ne2_0113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Ne3')
       fprefix(ns) ='ne3_0113'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-NR1')
       fprefix(ns) ='nr1_9814'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Oho')
       fprefix(ns) ='oho_0413'
       fneename(ns)='NEE_PI'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

   case('US-PFa')
       fprefix(ns) ='pfa_9514'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_DT_VUT_REF'
       frename(ns) ='RECO_DT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)= .TRUE.

    case('US-Pon')
       fprefix(ns) ='pon_9700'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) =''
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-Prr')
       fprefix(ns) ='prr_1014'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Sdh')
       fprefix(ns) ='sdh_0409'
       fneename(ns)='NEE'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-Seg')
       fprefix(ns) ='seg_0712'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RECO'
       flename(ns) ='LH'
       fshname(ns) ='SH'
       fprname(ns) ='PRECIP'
       fswname(ns) =''
       flstflag(ns)=.TRUE.

    case('US-Ses')
       fprefix(ns) ='ses_0714'
       fneename(ns)='NEE_PI'
       fgppname(ns)=''
       frename(ns) ='RECO_PI'
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.
    
    case('US-Shd')
       fprefix(ns) ='shd_9899'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'
       flstflag(ns)=.TRUE.

    case('US-SO2')
       fprefix(ns) ='so2_9706'
       fneename(ns)='FC'
       fgppname(ns)=''
       frename(ns) ='RECO_PI'
       flename(ns) ='LE'
       fshname(ns) ='H'
       fprname(ns) ='P'
       fswname(ns) ='SW_IN'
       flstflag(ns)=.TRUE.

    case('US-SRC')
       fprefix(ns) ='src_0814'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.
    
    case('US-SRM')
       fprefix(ns) ='srm_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.
    
    case('US-Sta')
       fprefix(ns) ='sta_0509'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Syv')
       fprefix(ns) ='syv_0114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Ton')
       fprefix(ns) ='ton_0114'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Tw2')
       fprefix(ns) ='tw3_1213'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Tw3')
       fprefix(ns) ='tw3_1314'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Twt')
       fprefix(ns) ='twt_0914'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-UMB')
       fprefix(ns) ='umb_0014'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Var')
       fprefix(ns) ='var_0014'
       fneename(ns)='NEE_VUT_REF'    
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-WCr')
       fprefix(ns) ='wcr_9914'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Whs')
       fprefix(ns) ='whs_0714'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Wi3')
       fprefix(ns) ='wi3_0204'
       fneename(ns)='NEE_VUT_REF'  
       fgppname(ns)='GPP_NT_CUT_REF'
       frename(ns) ='RECO_NT_CUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Wi4')
       fprefix(ns) ='wi4_0205'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='' 
       frename(ns) =''
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.
    
    case('US-Wkg')
       fprefix(ns) ='wkg_0414'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'
       flstflag(ns)=.TRUE.

    case('US-Wlr')
       fprefix(ns) ='wlr_0104'
       fneename(ns)='FC'
       fgppname(ns)='GPP'
       frename(ns) ='RE'
       flename(ns) ='LH'
       fshname(ns) ='H'
       fprname(ns) ='PREC'
       fswname(ns) ='RG'

    case('ZA-Kru')
       fprefix(ns) ='kru_0013'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_DT_VUT_REF'
       frename(ns) ='RECO_DT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

    case('ZM-Mon')
       fprefix(ns) ='mon_0709'
       fneename(ns)='NEE_VUT_REF'
       fgppname(ns)='GPP_NT_VUT_REF'
       frename(ns) ='RECO_NT_VUT_REF'
       flename(ns) ='LE_F_MDS'
       fshname(ns) ='H_F_MDS'
       fprname(ns) ='P_F'
       fswname(ns) ='SW_IN_F'

   case default
       print*,''
       print*,'Unknown Fluxnet Site: ',sitename(ns)
       !print*,'Add Info To Fluxnet_Tools.f90 File.'
       !print*,'Stopping.'
       !STOP

   end select
enddo

RETURN
end subroutine get_site_info

!==================================================
subroutine get_site_toffset( &
   nsites, sitelon, &
   flstflag,fhrshift)
!==================================================

implicit none

!input variables
integer, intent(in) :: nsites
real, dimension(nsites), intent(in) :: sitelon
logical, dimension(nsites), intent(in) :: flstflag
integer, dimension(nsites), intent(inout) :: fhrshift

!local variables
integer :: ns

fhrshift=0
do ns=1,nsites
   if (flstflag(ns)) then
       fhrshift(ns) = int(24.*sitelon(ns)/360.)
   endif
enddo

RETURN
end subroutine get_site_toffset

!==================================================
subroutine write_fluxnet_outfile( &
   fluxdir, &
   nsites, sitename, prefix, &
   neename, gppname, rename, &
   prname, lhname, shname, swname, &
   fhrshift)
!==================================================

use ncmodule
use netcdf
implicit none

!input variables
character(len=*), intent(in) :: fluxdir
integer, intent(in) :: nsites
character(len=*), dimension(nsites), intent(in) :: sitename
character(len=*), dimension(nsites), intent(in) :: prefix
character(len=*), dimension(nsites), intent(in) :: neename
character(len=*), dimension(nsites), intent(in) :: gppname, rename
character(len=*), dimension(nsites), intent(in) :: lhname, shname
character(len=*), dimension(nsites), intent(in) :: prname, swname
integer, dimension(nsites), intent(in) :: fhrshift

!local variables
integer :: ns, ntimef, ntime
real :: dtime
character(len=5) :: suffix
character(len=256) :: infile
real, dimension(:), allocatable :: gppf, respf, neef
real, dimension(:), allocatable :: prf, lhf, shf, swf
real, dimension(:), allocatable :: gpp, resp, nee
real, dimension(:), allocatable :: pr, lh, sh, sw

integer :: yrlen
real*8 :: tvalstart, diffstart, chngstart
integer, dimension(:), allocatable :: hrshift
real, dimension(:), allocatable :: doy
real*8, dimension(:), allocatable :: time, timef

!------------------------------------

ntime=0
status=nf90_inq_dimid(outid,'time',dimid)
status=nf90_inquire_dimension(outid,dimid,len=ntime)
!if (ntime .le. 1) then
!   print*,'Expecting More Than One Time.'
!   print*,'Stopping.'
!   STOP
!endif

allocate(gpp(ntime),resp(ntime),nee(ntime))
allocate(lh(ntime),sh(ntime))
allocate(pr(ntime),sw(ntime))
allocate(doy(ntime),time(ntime))
status=nf90_inq_varid(outid,'doy',varid)
status=nf90_get_var(outid,varid,doy)
if (status .ne. nf90_noerr) then
   print*,'Error Getting Output DOYs.'
   print*,'Stopping.'
   STOP
endif

status=nf90_inq_varid(outid,'time',varid)
status=nf90_get_var(outid,varid,time)
if (status .ne. nf90_noerr) then
   print*,'Error Getting Output Times.'
   print*,'Stopping.'
   STOP
endif

allocate(hrshift(nsites))
hrshift(:)=0

if (ntime .eq. 1) then
   print('(a)'),'   Time Resolution Monthly'
   suffix='_m.nc'
   dtime = 0
else
   dtime = doy(2) - doy(1)
endif

if (dtime .gt. 31) then
   print*,'Expecting monthly timesteps at most'
   print*,'   to compare SiB4 and Fluxnet.'
   print*,'Stopping.'
   STOP
elseif ((dtime .ge. 28) .and. (dtime .le. 31)) then
   print('(a)'),'   Time Resolution: Monthly'
   !print('(a,f8.3)'),'    dt (days): ', dtime
   suffix='_m.nc'
elseif ((dtime .ge. 0.98) .and. (dtime .le. 1.02)) then
   print('(a)'),'   Time Resolution: Daily'
   !print('(a,f8.3)'),'    dt (days): ', dtime
   suffix='_d.nc'
elseif ((dtime .gt. 0.) .and. (dtime .lt. 0.05)) then
   print('(a)'),'   Time Resolution: Hourly'
   !print('(a,f8.3)'),'    dt (days): ', dtime
   suffix='_h.nc'
   hrshift(:) = fhrshift(:)
endif

print('(a)'),'   Site  (Start Time/Stop Time/Time References)'
do ns=1,nsites
   write(infile,'(5a)') trim(fluxdir), &
         trim(sitename(ns)), '/', &
         trim(prefix(ns)), trim(suffix)
   status=nf90_open(trim(infile),nf90_nowrite,inid)
   if (status .ne. nf90_noerr) then
      print*,'Error Opening Fluxnet File: '
      print*,'  ',trim(infile)
      ntimef = ntime

      allocate(timef(ntimef))
      timef(:) = badval
      
      allocate(gppf(ntimef), respf(ntimef))
      allocate(neef(ntimef), lhf(ntimef), shf(ntimef))
      allocate(prf(ntimef), swf(ntimef))
      gppf(:) = badval
      respf(:) = badval
      neef(:) = badval
      lhf(:) = badval
      shf(:) = badval
      prf(:) = badval
      swf(:) = badval

      !print*,'Stopping.'
      !STOP
   else
      !print*,'Opening Fluxnet File: '
      !print*,'  ',trim(infile)

      status=nf90_inq_dimid(inid,'numt',dimid)
      status=nf90_inquire_dimension(inid,dimid,len=ntimef)
      if (status .ne. nf90_noerr) then
         print*,'Error Getting Fluxnet Time-Steps.'
         print*,'     Site: ',sitename(ns)
         print*,'     Stopping.'
         STOP
      endif

      allocate(timef(ntimef))
      status=nf90_inq_varid(inid,'time',varid)
      status=nf90_get_var(inid,varid,timef)
      if (status .ne. nf90_noerr) then
         print*,'Error Getting Fluxnet Time.'
         print*,'    Site: ',sitename(ns)
         print*,'    Stopping.'
         STOP
      endif
      if (suffix .eq. '_h.nc') then
          diffstart = timef(1) - floor(timef(1))
          yrlen = 8760
          if (mod(floor(timef(1)),4) .eq. 0) yrlen = 8784
          tvalstart = 0.5/dble(yrlen)
          chngstart = diffstart - tvalstart
          if (chngstart .gt. 1.E-4) then
             timef(:) = timef(:) - chngstart
          endif
      endif
      if (suffix .eq. '_d.nc') then
         diffstart = timef(1) - floor(timef(1))
         yrlen = 365
         if (mod(floor(timef(1)),4) .eq. 0) yrlen = 366
         tvalstart = 0.5/dble(yrlen)
         chngstart = diffstart - tvalstart
         if (chngstart .gt.1.E-4) then
            timef(:) = timef(:) - chngstart
         endif
      endif

      allocate(gppf(ntimef))
      gppf(:)=badval
      status=nf90_inq_varid(inid,trim(gppname(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,gppf)
      endif
      !where(gppf .lt. 0.) gppf=0.

      allocate(respf(ntimef))
      respf(:)=badval
      status=nf90_inq_varid(inid,trim(rename(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,respf)
      endif
      !where(respf.lt. 0.) respf=0.

      allocate(neef(ntimef))
      neef(:)=badval
      status=nf90_inq_varid(inid,trim(neename(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,neef)
      endif

      allocate(lhf(ntimef))
      lhf(:)=badval
      status=nf90_inq_varid(inid,trim(lhname(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,lhf)
      endif

      allocate(shf(ntimef))
      shf(:)=badval
      status=nf90_inq_varid(inid,trim(shname(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,shf)
      endif

      allocate(prf(ntimef))
      prf(:)=badval
      status=nf90_inq_varid(inid,trim(prname(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,prf)
      endif
      where(prf .lt. 0.) prf=badval
      
      allocate(swf(ntimef))
      swf(:)=badval
      status=nf90_inq_varid(inid,trim(swname(ns)),varid)
      if (status .eq. nf90_noerr) then
          status=nf90_get_var(inid,varid,swf)
      endif
      where(swf .lt. 0.) swf=badval
   endif !file exists

   print('(2a)'),'   ',sitename(ns)
   call matcht(badval, hrshift(ns), &
        ntimef, timef, &
        gppf, respf, neef, lhf, shf, prf, swf, &
        ntime, time, &
        gpp, resp, nee, lh, sh, pr, sw) 
   
   print('(a,2f10.3)'),'      Min/Max NEE: ', &
        minval(nee),maxval(nee)
   status=nf90_put_var(outid,gppid,gpp,   &
          start=(/ns,1/), count=(/1,ntime/))
   status=nf90_put_var(outid,reid,resp, &
          start=(/ns,1/), count=(/1,ntime/))

   status=nf90_put_var(outid,neeid,nee,  &
          start=(/ns,1/), count=(/1,ntime/))

   status=nf90_put_var(outid,shid,sh, &
          start=(/ns,1/), count=(/1,ntime/))
   status=nf90_put_var(outid,lhid,lh, &
          start=(/ns,1/), count=(/1,ntime/))

   status=nf90_put_var(outid,prid,pr, &
        start=(/ns,1/), count=(/1,ntime/))
   status=nf90_put_var(outid,swid,sw, &
        start=(/ns,1/), count=(/1,ntime/))
   
   status = nf90_close(inid)
   deallocate(timef, gppf, respf)
   deallocate(neef, lhf, shf)
   deallocate(prf, swf)
   
enddo !ns=1,nsites

RETURN
end subroutine write_fluxnet_outfile


!==================================================
subroutine matcht( badval, hrshift,  &
     ntimef, timef, &
     gppf, respf, neef, lhf, shf, prf, swf, &
     ntime, time,   &
     gpp, resp, nee, lh, sh, pr, sw)
!==================================================

implicit none

!input variables
real, intent(in) :: badval
integer, intent(in) :: hrshift
integer, intent(in) :: ntimef
real*8, dimension(ntimef), intent(in) :: timef
real, dimension(ntimef), intent(in) :: &
   gppf, respf, neef, lhf, shf, prf, swf

integer, intent(in) :: ntime
real*8, dimension(ntime), intent(in) :: time
real, dimension(ntime), intent(inout) :: &
   gpp, resp, nee, lh, sh, pr, sw

!local variables
integer :: tstart, tstartf
integer :: tstop, tstopf
integer :: diff, difff
real*8  :: dtimef

!------------------------
gpp(:) = badval
resp(:) = badval
nee(:) = badval
lh(:) = badval
sh(:) = badval
pr(:) = badval
sw(:) = badval

tstart = 0
tstartf= 0
tstop  = 0
tstopf = 0

!See if any data/sib4 times match up
if ((timef(ntimef) .lt. time(1)) .or. &
    (timef(1) .gt. time(ntime))) then
     print('(a)'),'     No Matching Times.'
     RETURN
endif

!Find matching starting times
tstart=1
tstartf=1
if (time(tstart) .gt. timef(tstartf)) then
    dtimef = timef(2)-timef(1)
    do while (time(tstart) .gt. timef(tstartf)+dtimef)
        tstartf=tstartf+1
    enddo
else
    dtimef = timef(2)-timef(1)    
    do while (time(tstart) .lt. timef(tstartf)-dtimef)
        tstart=tstart+1
    enddo
endif
tstartf=tstartf+hrshift


!Find matching end times
tstop=ntime
tstopf=ntimef
diff = tstop-tstart
difff = tstopf-tstartf
if (diff .gt. difff) then
    tstop = tstop - (diff-difff)
elseif (diff .lt. difff) then
    tstopf = tstopf - (difff-diff)
endif
if (tstartf .lt. 1) then
   tstart=tstart+abs(tstartf)+1
   tstartf=1
endif

print('(a,2f14.6,2i10)'), &
     '     SiB4   : ', &
     time(tstart), time(tstop), tstart, tstop
print('(a,2f14.6,2i10)'), &
     '     Fluxnet: ', &
    timef(tstartf),timef(tstopf), tstartf, tstopf

!Check lengths
diff = tstop-tstart
difff = tstopf-tstartf
if (diff .ne. difff) then
    print*,''
    print*,'Mismatching Time Lengths!'
    print*,'   SiB4 (#/start/stop)   : ', &
          diff, tstart, tstop
    print*,'   Fluxnet (#/start/stop): ', &
          difff, tstartf, tstopf
    print*,'Stopping.'
    STOP
endif

!Set output fluxes
gpp(tstart:tstop) = gppf(tstartf:tstopf)
resp(tstart:tstop) = respf(tstartf:tstopf)
nee(tstart:tstop) = neef(tstartf:tstopf)
lh(tstart:tstop) = lhf(tstartf:tstopf)
sh(tstart:tstop) = shf(tstartf:tstopf)
pr(tstart:tstop) = prf(tstartf:tstopf)
sw(tstart:tstop) = swf(tstartf:tstopf)

RETURN
end subroutine matcht
