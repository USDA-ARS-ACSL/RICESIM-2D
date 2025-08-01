SUBROUTINE Senescence
!Leaf senesence following ORYZA approach
!Loss rates of green leaves and stem reserves
!Leaf loss is associated with developmental stage, nitrogen stress and shading effect
!There is no loss assocaited with water status...
!Stem reserve loss is associated with developmental stage, and varietal parmaeter
! all senescence rates should be in terms of g leaf (or stem reserve) plant-1 h-1
! at the end there is two types of senescence, leaves and stem reserves
! we assume 50% of leaf senesced mass can be remobilized back into GCR for the hour and 50% is lost, sen_LLV
! we assume 50% of stem reserves mass  ", sen_LLVSH

! we assume .. % of leaf senesced N content can remobilized back into leaf N pool

!TO ADD: Leaf senescence/death due to drought stress, 
!Uses LINT2 function from ORYZA for table look up
!Uses INSW function to id which value to use based on DVS

    use common_block
    REAL LINT2
    REAL INSW
    real basic_rate
    real N_stress
    sen_LLV = 0.
    sen_LSTR = 0.
    
    basic_rate = LINT2('sen_DRLVT',sen_DRLVT,6,DVS) / (10. * NHH * NPLH) !reduce from kg ha-1 to g plant-1 basis
    !N_stress = lf_N_sen / (10. * NHH * NPLH) !reduce from kg ha-1 to g plant-1 basis
    !write(*,*) DAS, basic_rate, N_stress
    !basic_rate = LINT2('sen_DRLVT',sen_DRLVT,6,DVS)  !reduce from kg ha-1 to g plant-1 basis
    N_stress = lf_N_sen !reduce from kg ha-1 to g plant-1 basis
    if (nitroenv.eq.0) N_stress = 1.!no N stress option
    sen_LLV = grnleaf_dw * N_stress * basic_rate !leaf senescence rate influenced by DVS and N status, g leaf plant-1 h-1
    
    if (sen_LLV.gt.grnleaf_dw) sen_LLV = grnleaf_dw
    sen_LSTR = INSW(DVS-1., 0., stemreserve_dw/grw_TCLSTR)  !stem reserve senescence rate, g stem reserves plant -1 h-1
    
    if(stemreserve_dw-sen_LSTR.LT.0.)then
        sen_LSTR = stemreserve_dw
    end if
    if (CROPSTA.GE.3) THEN
        sen_LLVSH = MAX(0.,MIN(0.03,0.03*(LAI - LAI_SHADE) / LAI_SHADE))/24. * grnleaf_dw !leaf senescence due to shade, g leaf plant h-1
       ! sen_LLVSH = 0.
    else
        sen_LLVSH= 0.
    end if

    !fix to make sure as plant ages we don't let LAI go negative
    if ((sen_LLV+sen_LLVSH) * SLA * (NHH * NPLH) / 10000..ge.LAI) then 
        sen_LLV = 0.
        sen_LLVSH = 0.  
    end if
    
    sen_LLV = sen_LLV + sen_LLVSH
    sen_DLDR = sen_LLV !keep prior leaf death rate for lai expansion routine?
    RETURN
END subroutine Senescence