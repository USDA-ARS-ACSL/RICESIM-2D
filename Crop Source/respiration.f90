!account for maintenance and growth respiration costs
    ! CHECK UNITS!!!  should be right now in terms of g CH2O loss
SUBROUTINE ORGAN_RESPIRATION
! At this point, we have current organ mass and actual growth rates
! Need to substract maintenance respriation from the organ mass and growth costs from the growth rates
!local variables
    use common_block
    REAL T_eff, RFC
    REAL DVS_Rm
    
    !(1) Maintenance respiration
    !Estimate Q10 factor based on air temperature,  could at some point move to leaf temperature
    T_eff = gas_Q10**((TAIR(iTime)-gas_TREF)/10.) !unitless
    DVS_Rm = grnleaf_dw / (grnleaf_dw+deadleaf_dw) ! per ORYZA, reduce RM as plant ages as an estimate by ratio of green / green_dead leaves 
    Rm_root = (root_dw-WRTI)*gas_MAINRT*T_eff*DVS_Rm ! (kg CH2O / kg CH2Oplant * g plant h-1 =  g CH2O plant-1); don't penalize intiial mass at emergence
    Rm_leaf = (grnleaf_dw-WLVGI)*gas_MAINLV*T_eff*DVS_Rm
    Rm_stem = (stem_dw-WSTI)*gas_MAINST*T_eff*DVS_Rm
    Rm_stemreserve = stemreserve_dw*gas_MAINSO*T_eff*DVS_Rm !assuming stem reserve rm is same as storage
    Rm_storage = (storage_dw-WSOI)*gas_MAINSO*T_eff*DVS_Rm
    Rm_grain = roughrice_dw*gas_MAINSO*T_eff*DVS_Rm !since rough rice is part of storage, still need to subtract out portion of storage which is Rm for accounting purposes, but do not add to total plant
    Rm_total = Rm_root + Rm_leaf + Rm_stem + Rm_stemreserve + Rm_storage !g CH2O plant-1 h-1  

    !!!ORYZA also uses an age affect which may may want to utilize based on age of leave as analogue to N content decrease
    !MNDVS = leaf_dw / NOTNUL(grnleaf_dw+deadleaf_dw)
    !Rm_total = Rm_total * MNDVS

    !(2) Growth respiration
    ! Uses variety parameters for C mass fraction of organ class, 
    !don't understand these conversions fully, commented out ORYZA version, just doing simpler calculation for now
    !  each organ has a differnt Rg coefficient, but I'm leaving the C fractoin alone
    !  (g CH2O loss / g CH2O * 12 g C / 30 g CH2O - 0.4 g C / g CH2O) = g C loss / g CH2O * 44 g CO2/ 12 g C = g CO2 loss per g CH2O per plant,  seems really convoluted
    !Rg_root = (grw_CRGRT *12./30. - cmf_FCRT)*44./12. * act_rootgro !don't understand these con
    !Rg_leaf = (grw_CRGLV *12./30. - cmf_FCLV)*44./12. * act_leafgro
    !Rg_stem = (grw_CRGST *12./30. - cmf_FCST)*44./12. * act_stemgro
    !Rg_storage = (grw_CRGSO *12./30. - cmf_FCSO)*44./12. * act_storagegro
    !Rg_stemreserves = (grw_CRGSTR *12./30. - cmf_FCSTR)*44./12.* &
    !     act_stemreservegro
    Rg_root = act_rootgro * (1. -1./grw_CRGRT) ! g CH2O lost plant-1 h-1 as Rg
    Rg_leaf = act_leafgro * (1. -1./grw_CRGLV)
    Rg_stem = act_stemgro * (1. -1./grw_CRGST)
    Rg_storage = act_storagegro * (1. -1./grw_CRGSO)
    Rg_stemreserve = act_stemreservegro * (1.-1./ grw_CRGSTR)
    Rg_total = Rg_root + Rg_leaf + Rg_stem + Rg_storage + &
        Rg_stemreserve


    R_tot = Rm_total + Rg_total
END SUBROUTINE