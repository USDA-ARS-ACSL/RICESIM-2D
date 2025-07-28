!******************************
! Run gas_exchange_dll to provide leaf and canopy photosynthesis, transpiration, conductance, and leaf T
! Uses plant and weath common_blocks to pass back and forth variables
    
    
    subroutine Gas_Exchange
!*****
! Call gasexchange.dll to estimate hourly gas exchange processes
! Provide information  as needed to the subroutine
! note that GasExchanger is defined in the gas_exchange.dll project in the crop.h header file
    
!TODO:  variables for leaf dimensions, gas exchange parameters, etc are not being passed back and forth at the moment!
!TODO:  water stress information needs to be evaluated
!TODO:  add source > sink feedback inhibition response - ORYZA does something like this.  Whether I choose to do this based on SLA or some other variable...
    
    use common_block
    REAL LINT2
    real N_vcmax
    !*** Initializing the gas exchange input info
    !weather block
    cDayOfYear = dayofYear          !day of year
    ciTime = iTime                  !hour of day
    ciPerd = 24.                    !24 fixed value
    cWattsm(ciTime) = wattsm(iTime) ![watts/m2] in each hour
    cPar(ciTime) = par(iTime)       ![watts/m2] of PAR in each hour
    cTair(ciTime) = tair(iTime)     ![C] hourly air T
    cCO2 = co2                      !CO2 concentratoin in ppm
    cVPD = vpd(iTime)               !water vapor pressure deficit, in MPa (?)
    cWind = wind                    ![km/h]
    !cPsil_ = lwp_hourly             ![Mpa] hourly leaf water potential 
    cPsil_ = lwp_predawn            ![Mpa] pre-dawn leaf water potential
    cLatitude = latude        
    cLongitude = longitude
    cAltitude = altitude 
    cLareat = leaf_area             !leaf area is in [cm2]
    !cLAI = lai                      !leaf area index only
    cLAI = lai+0.5*sai              !leaf area index + stem area index
    
    !plant block - adjust gas exchange parameters for developmental stage
    cVCMAX = LINT2('vcmax',t_VCMAX, 12, DVS)
    cJMAX = LINT2('jmax',t_JMAX,20,DVS)
    cTPU = LINT2('tpu',t_TPU,12,DVS)
    cg1 = LINT2('g1',t_g1,12,DVS)
    cg0 = LINT2('g0',t_g0,12,DVS)
  
        
    if (lwp_predawn < -0.01) then
        !lwp_predawn = -0.3
        if (ave_SLNcanopy.le.0.5) then !from ORYZA, adjust maximum pmax based on average canopy N concentration (g N m-2 leaf)
    ! I oversimplify this by assuming constant N in sunlit and shaded leaves
    ! I also normalize the relationships ORYZA used to develop the curves, orignally from Keulen & Seligman (1987) and Peng et al. (unpublished from IRR)
    ! See spreadsheet I used to develop normalized version of the above, it saturates at a leaf level of 34 umol m-2 s-1, however, we are relatively scaling the Vcmax value with this
    ! Will need to be tested, for now any SLN less than 2.0 g N m-2 leaf results in decline in Vcmax
            N_vcmax = 1.239*ave_SLNcanopy - 0.232
        else
            N_vcmax = 0.414*ave_SLNcanopy + 0.177
        end if
        if (N_vcmax.lt.0.1) N_vcmax = 0.1
        if (N_vcmax.gt.1.0) N_vcmax = 1.0
        if (nitroenv.eq.0) N_vcmax = 1.0  !no N stress
        cVCMAX = cVCMAX * N_vcmax
        Call GASEXCHANGER(CDayofYear,nratio) !avoid running routine if leaves haven't established yet
    end if
    Pg_gross = Pgtotal * 44. / 1000000. * 1. /(NPLH * NHH) *  3600. !umol co2 m-2 ground s-1 to g CH2O plant-1 hr-1
    GCR = GCR + max(0.,Pg_gross) ! g CHO available for growth before respiration costs are accounted for
    Trans = transpiration       !mmol h2o m-2 ground-1 s-1
    T_canopy(iTime) = T_leaf
    
    return
end subroutine Gas_Exchange