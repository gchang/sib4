!==================SUBROUTINE PHOVEGNO===================================
!
!     SET PHOTOSYNTHESIS VARIABLES WHEN THERE IS NOT A CANOPY.
!-----------------------------------------------------------------------

subroutine phonveg(  &
    radvbc, radvdc, pco2m, co2t)

    use kinds
    use module_pparams, only: par_conv
    use module_phosib
    use module_sib, only: co2_type

    implicit none

    !...input variables
    real(r8), intent(in) :: radvbc, radvdc, pco2m
    type(co2_type), intent(inout)  :: co2t

    !...set co2 variables
    co2t%apar = dzero
    co2t%aparkk = dzero
    co2t%par = par_conv * (radvbc+radvdc)

    co2t%assim = dzero
    assim_omc = dzero
    assim_ome = dzero
    assim_oms = dzero
    assimfac(:) = dzero

    co2t%assimpot = dzero
    assimpot_omc = dzero
    assimpot_ome = dzero
    assimpot_oms = dzero

    gah2o = dzero
    gbh2o = dzero
    gsh2o = dzero
    
    co2t%cflux = dzero
    co2t%pco2c = pco2m
    co2t%pco2i = pco2m
    co2t%pco2s = pco2m
    co2t%pco2cas = pco2m

    co2t%rst = rst_max
    co2t%vmaxss = dzero

end subroutine phonveg
