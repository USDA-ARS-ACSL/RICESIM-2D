! *** COMMON BLOCK FOR RICESIM-2D *** DATE Feb 2024
! *
! *** COLLECTION OF ALL FORTRAN TYPE DECLARATIONS FOR RICESIM
! *** Following GOSSYM-2D approach from Sahila Beegum
    
	module common_block
	!DEC$ ATTRIBUTES DLLEXPORT::/ShootR/
    !DEC$ ATTRIBUTES DLLEXPORT::/Time_Public/
	!DEC$ ATTRIBUTES DLlEXPORT::/Module_Public/
	!DEC$ ATTRIBUTES DLLEXPORT::/Weath/
	!DEC$ ATTRIBUTES DLLEXPORT::/bound_public/
	!DEC$ ATTRIBUTES DLLEXPORT::/DataFilenames/ 
    
    
!BELOW ARE VARIABLES AND COMMON BLOCKS FOR CROP
    LOGICAL  INIT
    INTEGER Modnum, Simulationdone
    real Period, NitrogenUptake, POPSLAB
    real AvailableNitrogen, CumulativeNitrogen, HourlyNitrogenDemand
    REAL HourlySoilWaterUptake, TotalSoilWaterUptake, WaterUptake
    REAL TotalPotentialRootWaterUptake
    Integer iTime_old
    
    COMMON / ABC / Period, NitrogenUptake, Modnum, INIT, &
        AvailableNitrogen, CumulativeNitrogen, HourlyNitrogenDemand, &
        HourlySoilWaterUptake, TotalSoilWateruptake, WaterUptake, &
        TotalPotentialRootWaterUptake

    !****Weather variables - additional to those from 2DSOIL
    REAL T_dailyave, T_dailymax, T_dailymin, T_canopydailyave, T_dailymin7
    REAL PAR_dailysum, SRAD_dailysum
    REAL T_canopy !24 element array to store leaf / canopy T per day
    Integer time_sunrise, time_sunset, time_sunriseold, time_sunsetold
    REAL FLHAS, FLTIME, TPANFL, RHFL, VPDFL
    Integer T_counter7
    REAL T_dailymin7ave
    REAL lwp_hourly, lwp_predawn !Mpa
    
   !****Development variables
    Integer CROPSTA !crop stage 0 - before sowing; 1 - day of sowing; 2 - in seedbed
    REAL DVS, DVR, DVSI
    REAL HU !ORYZA hourly (was daily) effective heat units(degrees days(Cd))
    REAL HULV, TSLV ! ORYZA hourly (was daily) temperature for leaf development; temperature sum for leaf development
    REAL TSeed !ORYZA heat units to estimate transplanting shock(degree days(Cd))
    Integer IDAS, ISA !integer days after sowing, days of seed bed
    REAL DAS !decimal day after sowing
    Integer isMatured, isSown
    Character*32 Devstage_name
    REAL DURPI, DURFL, DUREF, DURFM !duration in days : from emergence to PI, PI to flowering; flowering to grainfill; grainfill to maturity
    REAL TMPIF, TMEFL, TMFLM !Average daily Tair during the above periods 
    REAL PLTR !change in plant density at transplanting
    Integer pan_exertion !0 or 1 if panicle exertion has occured
    
     !****Plant growth variables
      !*Note: the primary organs which get C allocation are: grnleaf_dw, stem_dw, root_dw, stemreserve_dw, and storage_dw
      !*Note:  husk_dw and roughrice_dw are derived from storage_dw at some point after C is allocated
      !*Note:  husk_dw is spikelet husk 
      !*Note:  rough rice is the whole rice grain at harvest which includes the hull (also referred to as paddy rie)
      !*Note:     hull in roughrice is about 20% of grain size; removing the hull = brown rice;  removing the hull and bran layer = white rice)
    
    REAL totleaf_dw, grnleaf_dw, deadleaf_dw
    REAL stem_dw, roughrice_dw, husk_dw, root_dw, storage_dw, & 
        stemreserve_dw, total_grn_dw
    REAL leaf_area, SLA, t_SLA, LAI_SHADE, t_SSGA, SAI
    REAL root_depth
    REAL f_totdm_root, f_totdm_shoot
    REAL f_shtdm_store, f_shtdm_stem, f_stemdm_reserve, f_shtdm_leaf
    REAL f_totdm_rootDVS, f_totdm_shootDVS
    REAL f_shtdm_storeDVS, f_shtdm_stemDVS, f_stemdm_reserveDVS, &
        f_shtdm_leafDVS 
    REAL pot_leafgro, pot_stemgro, pot_rootgro, &
        pot_storagegro, pot_stemreservegro, pot_roughricegro
    REAL act_leafgro, act_stemgro,  &
        act_rootgro,act_storagegro, act_stemreservegro, act_LAIgro, &
        act_totalgro, act_roughricegro
    REAL sen_DLDR, sen_LLV, sen_NSLLV   ! ORYZA grn leaf senescence rates, will need to change name at some point
    REAL sen_DRLVT, sen_LLVSH ! 1D 6 element array in growth common block
    REAL act_RGRL
    REAL sen_LSTR !loss of stem reserves
    REAL GCR, Pg_gross, R_tot, Trans
    REAL grain_num, spikelet_num, rate_spikelet_num, rate_grain_num ! yield (rough rice ), max grain size, grain num
    REAL shootPart_old, shootPart, rootPart_old, rootPart, rootPool
    
    !****Gas exchange variables
    REAL t_VCMAX,t_JMAX,t_TPU,t_g0, t_g1
    REAL Rm_total, Rm_leaf, Rm_stem, Rm_stemreserve,Rm_root, Rm_storage, Rm_grain
    REAL Rg_total, Rg_leaf, Rg_stem, Rg_root, Rg_storage, &
        Rg_stemreserve
    
    !****Stress variables
    REAL LESTRS, NCOLD, WGCOR !elongation stress, cumul cold days before plant death, correction for max grain weight due to temps below 20C
    REAL SF3, SFHEAT, SFCOLD, SPFERT ! factors for heat and cold on spikelet fertility
    REAL SF3CNT, SF3SUM ! heat stress counts during flowering period
    REAL COLDTT, HEATTT !thermal time accumulation for cold T stress on grain fill
    real lf_wat_rf !leaf water expansion reductoin factor based on pre-dawn LWP, 0 to 1
    real lf_N_gro !N stress factor (RNSTRS in ORYZA) on relative leaf growth rate, from 0 to 1
    real lf_N_sen !N stress factor on leaf senescence (NSTRES)
    
    !*****Nitrogen Variables
    real t_NFLV, t_NMINSO, t_NMAXL, t_NMINL !look up tables for N fraction per leaf area vs DVS, min N content in storage organs vss total N, max leaf N content vs dvs, and min leaf N content and potential leaf N demand
    real t_NSLLV ! look up talbe of leaf death rate versus N stress level
    real leaf_N, stem_N, root_N, grain_N, storage_N, dead_N !current fractoinal N contents, g N g-1 tissue 
    real NMAXL, NMINL !max or min leaf N content g N g-1 biomass
    real NMINSO !min storage organ N content, g N, g-1 biomass
    real shoot_preN, storage_preN !shoot and storage N content prior to flowering, g g-1
    real NFLVP !potential leaf N demand based on leaf area g N m-2 leaf, akin to leaf specific N 
    real totalplantN ! current total plant N content, g N plant-1
    real leaf_Npool, stem_Npool, root_Npool, grain_Npool, storage_Npool, dead_Npool !g N dw plant-1
    real ave_SLNcanopy ! g N leaf per m-2 canopy, represents average specific leaf nitrogen for Vcmax adjustment
    
    !****Varietal information
    !card - cardinal temp; dev - developmental rate; env - environmental sensivitiy
    !sed - seedling related info; grw - growth parameters
    !gas - gas exchange processes; cmf - carbon mass fractions
    !drt - drought parameters; nit - nitrogen parameters
    character*32 name
    REAL card_TBD,  card_TBLV, card_TMD, card_TOD, card_TODNGHT, &
        card_TSEN
    REAL card_TSENNGHT, card_TSENPSP, card_TSENSPNGHT
    REAL dev_DVRJ, dev_DVRI, dev_DVRP, dev_DVRR
    REAL env_MOPP, env_PPSE, env_COLDMIN, env_COLDEAD
    REAL sed_SHCKD, sed_SHCKL
    REAL grw_RGRLMX, grw_RGRLMN, grw_LRSTR
    REAL grw_FSTR, grw_TCLSTR, grw_SPGF, grw_NSPM2X, grw_NSJA, &
        grw_WRGMX
    REAL grw_GFRCP, grw_WGREC, grw_SLA_nominal
    REAL gas_VCMAX, gas_JMAX, gas_TPU, gas_g0, gas_g1
    REAL gas_MAINLV, gas_MAINST, gas_MAINSO, gas_MAINRT, gas_TREF, &
        gas_Q10
    REAL grw_CRGLV, grw_CRGST, grw_CRGSO, grw_CRGRT, grw_CRGSTR
    REAL cmf_FCLV, cmf_FCST, cmf_FCSO, cmf_FCRT, cmf_FCSTR
    REAL grw_GZRT, grw_ZRTMCW, grw_ZRTMCD
    REAL drt_ULLS, drt_LLS, drt_ULDL, drt_LLDL
    REAL drt_ULLE, drt_LLLE, drt_ULRT, drt_LLRT
    REAL nit_NMAXUP, nit_RFNLV, nit_FNTRT, nit_RFNST, nit_TCNTRF, &
        nit_NFLVI, nit_FNLVI
    REAL nit_NMAXSO
    
    COMMON / weather_ave / T_dailyave, T_dailymax, T_dailymin, T_canopydailyave,  &
        T_dailymin7(7), PAR_dailysum, SRAD_dailysum, &
        T_canopy(24), time_sunrise, time_sunset , time_sunriseold, time_sunsetold, &
        FLHAS, FLTIME, TPANFL, RHFL, VPDFL, T_counter7, T_dailymin7ave, &
        lwp_hourly, lwp_predawn
        
    COMMON / development / CROPSTA, DVS, DVR, DVSI, &
	    HU, HULV, TSLV, TSeed, IDAS, ISA, DAS,  &
        isMatured, isSown, Devstage_name, &
	    DURPI,  DURFL, DUREF, DURFM, TMPIF, TMEFL, TMFLM, &
        PLTR, pan_exertion
    
    COMMON / growth / totleaf_dw, grnleaf_dw, deadleaf_dw, &
	    stem_dw, roughrice_dw, husk_dw, root_dw, storage_dw, &
	    stemreserve_dw, total_grn_dw, leaf_area, SLA, t_SLA(14), LAI_SHADE, &
	    t_SSGA(10), SAI, root_depth, &
        f_totdm_root, f_totdm_shoot, &
        f_shtdm_store, f_shtdm_stem, f_stemdm_reserve, f_shtdm_leaf, &
        f_totdm_rootDVS(12), f_totdm_shootDVS(8), &
        f_shtdm_storeDVS(12), f_shtdm_stemDVS(12), f_stemdm_reserveDVS, &
        f_shtdm_leafDVS(12), pot_leafgro, pot_stemgro, &
        pot_rootgro, pot_storagegro, pot_stemreservegro,pot_roughricegro, act_leafgro,  &
        act_stemgro, act_rootgro, &
        act_storagegro, act_stemreservegro, act_LAIgro,     &
        act_totalgro, act_roughricegro, sen_DLDR, sen_LLV, sen_NSLLV, sen_DRLVT(12), &
        sen_LLVSH, act_RGRL, sen_LSTR,           &
        GCR, Pg_gross, R_Tot, Trans,   &
        grain_num, spikelet_num, rate_spikelet_num, rate_grain_num, &
        shootPart_old, shootPart, rootPart_old, rootPart, rootPool
    
    COMMON / gasexchange / t_VCMAX(12),t_JMAX(20),t_TPU(12),t_g0(12), t_g1(12), &
        Rm_total, Rm_leaf, Rm_stem, Rm_stemreserve, Rm_root, &
        Rm_storage, Rm_grain, Rg_total, Rg_leaf, Rg_stem, Rg_root, &
        Rg_storage, Rg_stemreserve
    
    COMMON / stress / LESTRS, NCOLD, WGCOR, SF3, SFHEAT, SFCOLD, &
        SPFERT, SF3CNT, SF3SUM, COLDTT, HEATTT, lf_wat_rf  
     
    COMMON / nitro / t_NFLV(18), t_NMINSO(12), &
        t_NMAXL(12), t_NMINL(8), t_NSLLV(10), &
        leaf_N, stem_N, root_N, grain_N, storage_N, &
        dead_N, NMAXL, NMINL, NMINSO, &
        shoot_preN, storage_preN, NFLVP, &
        totalplantN, &
        leaf_Npool, stem_Npool, root_Npool, grain_Npool, storage_Npool, dead_Npool, &
        ave_SLNcanopy
    
    COMMON / VARIETY / name,card_TBD, card_TBLV, card_TMD, card_TOD, &
        card_TODNGHT, card_TSEN, card_TSENNGHT, card_TSENPSP,  &
        card_TSENSPNGHT, dev_DVRJ, dev_DVRI, dev_DVRP, dev_DVRR, &
        env_MOPP, env_PPSE, env_COLDMIN, env_COLDEAD, &
        sed_SHCKD, sed_SHCKL, &
        grw_RGRLMX, grw_RGRLMN, grw_LRSTR, &
        grw_FSTR, grw_TCLSTR, grw_SPGF, grw_NSPM2X, grw_NSJA, grw_WRGMX, &
        grw_GFRCP, grw_WGREC, grw_SLA_nominal, &
        gas_VCMAX, gas_JMAX, gas_TPU, gas_g0, gas_g1, &
        gas_MAINLV, gas_MAINST, gas_MAINSO, gas_MAINRT, gas_TREF, &
        gas_Q10, grw_CRGLV, grw_CRGST, grw_CRGSO, grw_CRGRT, grw_CRGSTR, &
        cmf_FCLV, cmf_FCST, cmf_FCSO, cmf_FCRT, cmf_FCSTR, &
        grw_GZRT, grw_ZRTMCW, grw_ZRTMCD, &
        drt_ULLS, drt_LLS, drt_ULDL, drt_LLDL, &
        drt_ULLE, drt_LLLE, drt_ULRT, drt_LLRT, &
        nit_NMAXUP, nit_RFNLV, nit_FNTRT, nit_RFNST, nit_TCNTRF, &
        nit_NFLVI, nit_FNLVI, nit_NMAXSO
    

    
  


 
    
    
    
    !BELOW ARE COMMON BLOCKS FROM 2DSOIL as the DLLEXPORT
    !==================================
	! Includes in common /ShootR/	 
    REAL PCRL, PCRQ, PCRS, PCRTS, ET_demand, LCAI, COVER, CONVR
    Real(8) HourlyCarboUsed, TotalRootWeight
    REAL(4) SHADE, HEIGHT, LAI, AWUPS, NitroDemand, NDemandError
    REAL PSIL_,LAREAT, MaxRootDepth, InitialRootCarbo
    REAL cContentRootY, nContentRootY, cContentRootM, nContentRootM
    integer isGerminated, isEmerged, isTransplanted, isGrainfill

    INTEGER RICETYPE, RUNMODE, PRODENV, WATBAL, NITROENV, ESTAB
    REAL SBDUR, NPLH, NHH, NPLSB, NPLDS
    REAL LAPE, DVIS, WLVGI, WSTI, WRTI, WSOI, ZRTI
    REAL ZRTTR, DVSIMAX, DVSIMIN, SWITIR

	!==================================
	!Variables in Common/Time_public/
	Parameter (NumModD=20)
	Double precision tNext, dtMx,Time,Step, dtOpt,  dtMin, dMul1, &
		dMul2,tTDB, tFin, tatm, timestep
    Real Tinit
    integer sowingDay, emergeDay, transplantDay, endDay
	integer lInput,iter,DailyOutput,HourlyOutput, &
		RunFlag, HourlyWeather, DailyWeather,BeginDay,iTime, &
		IDawn,IDusk, year,OutputSoilNo, OutputSoilYes, dayofyear, &
        tmpday
    
    !==================================
    ! variables in common /Module public/
    Integer NumMod, Movers, NShoot
	
	!===================================
	!variables in Common/bound_public/
    Parameter(NumNPD = 4000, NumElD = 3500, NumBPD = 600, NSeepD = 2, &
    NumSPD = 30, NumSD = 10, NDrainD = 2, NumDR = 30, &
    NumGD = 3, NumPlD = 100, &
    NMatD = 15, MNorth = 4, &
    MBandD = 15, NumSurfDatD = 3 + NumGD + NumSD)
        
    Integer NumBP, NSurf, NVarBW, NVarBS, NVarBT, NVarBG, &
        NumSurfDat, NSeep, NSP, NP, &
        NDrain, NDR, ND, KXB
    Integer CodeW, CodeS, CodeT, CodeG, PCodeW
    REAL Width, VarBW, VarBS, VarBT, VarBG, EO, Tpot

   !=================================
   !Variables in common/weath/
    Integer MSW1, MSW2, MSW3, MSW4, MSW5, MSW6, MSW7
    REAL BSOLAR, ETCORR, BTEMP, ATEMP, ERAIN, BWIND, BIR, WINDA, IRAV
    Integer JDAY, NCD, JDLAST
    REAL CLDFAC, DEL, RINT, RNS, RNC, RAIN, IR
    REAL WIND, CO2, TDUSK, TDUSKY, CPREC, TAIR, VPD
    REAL ROUGH, RADINT, WATTSM, DIFINT
    REAL ROWINC, CLOUD, SHADOW, DIFWAT
    REAL DIRINT, WATACT, WATRAT, WATPOT, RNLU
    Integer NumF, NumFP
    REAL hFur, QF
    Integer IFUR
    REAL GAIR, PG, LATUDE, Longitude, Altitude, RI, par, parint, DAYLNG
    REAL DLNGMAX, AutoIrrigAmt, ESO, TMIN
    Integer AutoIrrigateF

    
    !==================================
   !Variables in common/DataFilenames
    double precision Starter
    character WeatherFile * 256, TimeFile * 256, BiologyFile * 256, &
        ClimateFile * 256, NitrogenFile * 256, SoluteFile * 256, &
        ParamGasFile * 256, SoilFile * 256, &
        ManagementFile * 256, IrrigationFile * 256, DripFile * 256, &
        WaterFile * 256, WaterBoundaryFile * 256, &
        PlantGraphics * 256, InitialsFile * 256, VarietyFile * 256, &
        NodeGraphics * 256, ElemGraphics * 256, &
        NodeGeomFile * 256, &
        GeometryFile * 256, SurfaceGraphics * 256, &
        FluxGraphics * 256, MassBalanceFile * 256, &
        MassBalanceFileOut * 256, LeafGraphics * 256, &
        OrganicMatterGraphics * 256, &
        RunFile * 256, MassBalanceRunoffFileOut * 256, &
        MulchFile * 256, MassBalanceMulchFileOut * 256
    
    
   !=============================================  
   !from Public.ins
    Common /time_public/tNext(NumModD),dtMx(4),Time,Step,dtOpt, &
        dtMin, dMul1, dMul2,  tTDB(4), Tfin,tAtm, Tinit, &
        lInput,Iter,DailyOutput,HourlyOutput,RunFlag, &
        DailyWeather,HourlyWeather,&
        beginDay, sowingDay, emergeDay, transplantDay, endDay,&
        OutputSoilNo, OutPutSoilYes,Year,&
        iTime,iDawn,iDusk,TimeStep, tmpday, dayofyear
      
   !from Public.ins
    Common /module_public/  NumMod,Movers(4), NShoot
      
   !============================================   
   !from puweath.ins
    Common /Weath/ MSW1, MSW2, MSW3, MSW4, MSW5, MSW6, &
        MSW7, BSOLAR, ETCORR, &
        BTEMP, ATEMP, ERAIN, BWIND, BIR, WINDA, IRAV, JDAY, &
        NCD, JDLAST, CLDFAC, DEL(24), RINT(24), RNS, &
        RNC, RAIN, IR, WIND, CO2, TDUSK, TDUSKY, &
        CPREC(NumSD), TAIR(24), VPD(24), ROUGH, &
        RADINT(24), WATTSM(24), DIFINT(24), &
        ROWINC(24), CLOUD, SHADOW(24), DIFWAT(24), &
        DIRINT(24), WATACT, WATRAT, WATPOT, RNLU, &
        NumF(40), NumFP, hFur(40), QF, IFUR, GAIR(NumGD), PG, &
        LATUDE, Longitude, Altitude, RI, PAR(24), &
        PARINT(24), daylng, DLNGMAX, AutoIrrigAmt, &
        AutoIrrigateF, ESO, TMIN
 
   !==============================================   
   !from public.ins
    Common /bound_public/ NumBP, NSurf, NVarBW, NVarBS, NVarBT, &
        NVarBG, NumSurfDat, NSeep, NSP(NSeepD), NP(NSeepD, NumSPD), &
        NDrain, NDR(NDrainD), ND(NDrainD, NumDR), &
        KXB(NumBPD), &
        CodeW(NumNPD), CodeS(NumNPD), CodeT(NumNPD), CodeG(NumNPD), &
        PCodeW(NumNPD), Width(NumBPD), &
        VarBW(NumBPD, 3), &
        VarBS(NumBPD, NumSD), VarBT(NumBPD, 4), &
        VarBG(NumBPD, NumGD, 3), EO, Tpot

     !==============================================   
    !from puplant.ins
    Common /ShootR/ PCRL, PCRQ, PCRS, HourlyCarboUsed, ET_demand, &
        LCAI, COVER, CONVR, &
        MaxRootDepth, SHADE, HEIGHT, LAI, AWUPS, &
        NitroDemand, xBSTEM, yBSTEM, SGT, PSIM, &
        LAREAT, POPROW, ROWSP, ROWANG, PopArea, &
        CEC, EORSCS, AWUPSS, SOLRAD, &
        Total_Eor, Total_Pcrs, sincrsink, PSILD, &
        OSMFAC, EOMULT, PSIL_, NDemandError, &
        CumulativeNDemandError, TotalRootWeight, &
        InitialRootCarbo, PCRTS, &
        ConstI(2), constK(2), Cmin0(2), &
        isGerminated, isEmerged, isTransplanted, isGrainfill, &
        RICETYPE, RUNMODE, PRODENV, WATBAL, NITROENV, &
        ESTAB, SBDUR, &
        NPLH, NHH, NPLSB, NPLDS, &
        LAPE, DVIS, WLVGI, WSTI, WRTI, WSOI, ZRTI, &
        ZRTTR, DVSIMAX, DVSIMIN, SWITIR
    
    !==============================================
    !from public.ins
      Common /DataFilenames/ Starter, WeatherFile, TimeFile, &
        BiologyFile, ClimateFile, NitrogenFile, SoluteFile, &
        ParamGasFile, SoilFile, &
        ManagementFile, IrrigationFile, DripFile, &
        WaterFile, WaterBoundaryFile, &
        PlantGraphics, InitialsFile, VarietyFile, &
        NodeGraphics, ElemGraphics, NodeGeomFile, &
        GeometryFile, SurfaceGraphics, &
        FluxGraphics, MassBalanceFile, &
        MassBalanceFileOut, LeafGraphics, &
        OrganicMatterGraphics, &
        RunFile, MassBalanceRunoffFileOut, &
        MulchFile, MassBalanceMulchFileOut
    !========================================================
    
    
! The following are variables for Gasexchanger method from the gasexchange.dll
!=====================================================
      Integer CDayofYear, CITIME, CIPERD, WATLIMIT
      REAL(4) CWATTSM, CPAR, CTAIR, CCO2, CVPD, CWIND, CPSIL_,       &
           CLATITUDE, CLONGITUDE, CALTITUDE, CLAREAT, CLAI

      COMMON / Weather / CDayOfYear, CITIME, CIPERD, WATLIMIT,       &
            CWATTSM(24),    &
      CPAR(24), CTAIR(24), CCO2, CVPD(24), CWIND, CPSIL_,            &
      CLATITUDE,  CLONGITUDE, CALTITUDE, CLAREAT, CLAI
      
      
      REAL NRATIO, Pgtotal, Pgsun, Pgshade, Pgleaf_sun,              &
            Pgleaf_shade,                                            &
          Pntotal, sunlitPFD, shadedPFD, sunlitLAI, shadedLAI,       &
          Transpiration, Transpiration_sunleaf,                      &
          Transpiration_shadeleaf, StomConduc, T_leaf,  temp1,       &
          Ags, ARH, EET, cVCMAX,  cJMAX, cTPU, cg0, cg1
      !    REAL  cTPU, cg0, cg1 !table values which change with DVS
      
      
      COMMON / Plant / NRATIO, Pgtotal, Pgsun, Pgshade, Pgleaf_sun,  &
            Pgleaf_shade,                                            &
          Pntotal, sunlitPFD, shadedPFD, sunlitLAI, shadedLAI,       &
          Transpiration, Transpiration_sunleaf,                      &
          Transpiration_shadeleaf, StomConduc, T_leaf,  temp1,       &
          Ags, ARH, EET, cVCMAX, cJMAX, cTPU, cg0, cg1

      
      
      
      
    end module common_block
    
    