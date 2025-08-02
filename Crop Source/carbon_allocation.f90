
subroutine carbon_allocation
!update growth state variables after accounting for actual growth rates and respiratory costs
! includes both carbon and leaf area growth
! adds green stem area to lai
! 50% of the senescence leaf and stem reseve went into assimilate pool, 100% shoudl be subtracted from the dry weight pool however
! accounts for change in N due to senescence

    use common_block
    
    !(0) set stored values for next time-step
    shootPart = max(0.,act_leafgro + act_stemgro + act_stemreservegro + act_storagegro - (Rg_stem + Rm_stem) &
        - (Rg_leaf + Rm_leaf) - (Rg_storage + Rm_storage) - (Rg_stemreserve + Rm_stemreserve)) !avoid negative value shortly after emergence when there is root respiration but no growth yet
    
    rootPart = max(0.,act_rootgro - (Rg_root + Rm_root)  + (PCRS-rootPart_old)) !avoid negative value shortly after emergence when there is root respiration but no growth yet
    
    
    !(1) updated growth state variables
    deadleaf_dw = deadleaf_dw + 0.5*sen_LLV + 0.5*sen_LSTR !50% of senesced weight is assumed to be immediately recovered by assimilate pool in organ growth routine
    GCR = GCR + 0.5*sen_LLV + 0.5 * sen_LSTR !add senesced 50% loss back into CHO pool for next time-step
    !deadleaf_dw = deadleaf_dw + sen_LLV + sen_LSTR
    grnleaf_dw = grnleaf_dw + act_leafgro - (Rg_leaf + Rm_leaf) - sen_LLV
    stem_dw = stem_dw + act_stemgro - (Rg_stem + Rm_stem)
    root_dw = root_dw + act_rootgro - (Rg_root + Rm_root)  + (PCRS-rootPart_old) !the PCRS part either adds the extra CHO allocated by 2DSOIL or it will remove any under-allocated CHO

    storage_dw = storage_dw + act_storagegro - (Rg_storage + &
        Rm_storage)
    stemreserve_dw = stemreserve_dw + act_stemreservegro - &
        (Rg_stemreserve + Rm_stemreserve) - sen_LSTR
    total_grn_dw = grnleaf_dw+stem_dw+root_dw+storage_dw+ &
        stemreserve_dw
        
    LAI = LAI + act_LAIgro - sen_LLV * SLA * (NHH * NPLH) / 10000. !convert g plant- h-1 of leaf mass senesced to area basis using SLA and plant density, need to use a maxSLA instead, assuming older leaves are thinner and flat 
    
    leaf_area = LAI * (1./(NHH*NPLH))*10000. !cm2 plant-1
    
    !(2) updated nitrogen state variables accounting for senescence
    ! remove 75% of N from dead leaf mass from leaf N pool, nit_RFNLV is variety value for min N content in leaves
    if(deadleaf_dw.gt.0.) then !stem reserves don't have their own N content, just soluble CHO so no N is available to lose or gain
        !dead_Npool = dead_Npool + 0.75*sen_LLV*leaf_N + 0.75*sen_LSTR*leaf_N ! sen_LLV is current dead leaf amount in g CHO plant-1 h-1;  sen_LSTR is current dead stem reserve amount in g CHO plant-1 h-1; assuming 25%N is recovered should verify
        dead_Npool = dead_Npool + nit_RFNLV*sen_LLV ! sen_LLV is current dead leaf amount in g CHO plant-1 h-1;; RFNLV is the kg n kg leaf residual that is can't be recovered, variety value
        dead_N = dead_Npool / deadleaf_dw
    else
        dead_Npool = 0.
        dead_N = 0.
    end if     
    !leaf_N = (1. - 0.75*sen_LLV) * leaf_N
    !leaf_N = leaf_N * (grnleaf_dw - 0.75 * sen_LLV)) / (grnleaf_dw - 0.75 *sen_LLV)
    leaf_Npool = leaf_Npool - nit_RFNLV*sen_LLV

       
       
    !(3) derived variables 
    if(isGrainfill.eq. 1) then
        roughrice_dw = roughrice_dw + act_roughricegro - Rg_storage - Rm_grain !this is part of storage organ mass but we need to remove Rm cost here as well
        husk_dw = max(0.,storage_dw - roughrice_dw) ! don't let this value go negative
    end if
    

    
    
end subroutine carbon_allocation