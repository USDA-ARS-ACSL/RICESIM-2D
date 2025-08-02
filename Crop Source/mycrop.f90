! called from 2DSOIL program
! DEC statement tells compiler to expose the subroutine name and 
! common  block
!  Crop - subroutine 
    
    subroutine myCROP()
	use common_block
    !DEC$ ATTRIBUTES DLLEXPORT :: myCROP
    
    Real CurrentNUptakeError, CumulativeNuptakeError
    
     	!Write(*,*) Time 
    !***initializing crop module----------	
    If (lInput .eq. 1) then
		Write(*,*) 'Initializing crop module'
        Simulationdone = 0
	    Period = (timeStep/60.)/24. !Local var: This is 1h period which only used in crop. TimeStep is 2DSOIL var
		IPERD = 24
        EQSC   = 0.0
       
        !from 2DSOIL
		POPSLAB  = PopRow/100.0*EOMult !note: this is really-> poprow * (100/row-spacing) * (row-spacing / 10000) * EOMult or (0.01 m/slab * rowspacing/ 100 * eomult) * plantdensity)
		Convr = 1.0  !2DSoil VAR: wsun all carbon currency should be on C basis both 2dsoil and crop
		AWUPS = 0.0   !2DSoil VAR:initialize AWUPS, AWUPS_old and psil_ in 2DSOIL 
		!psil_ = -0.5  !2DSoil VAR:note, this is hourly leaf water potential in bars
        PCRS = 0.0 !2DSoil VAR:carbon used by roots at prior timestep, g slab-1 d-1, gets converted to g plant-1 h-1

        !TRWU_SIM = 0.0 !total root water extractable by current roots in system
        !PSILT_SIM = -15.0 ! thershold LWP at which shoot growth can't occur
        !PSISM_SIM = -0.5 !initial average slab soil water potential
		ET_demand = 0.0  !2DSoil VAR: g H2O cm-2 d-1
		NitrogenUptake = 0. !should be zero at emergence since all plant N has come from seedpiece
		NDemandError = 0.0 !2DSoil VAR:
		CumulativeNDemandError = 0.0 !2DSoil VAR:
        CumulativeNUptakeError=0.0  !Local VAR:
	    RunFlag = 1 
        !iTime = 1
        NumMod=NumMod+1
        ModNum=NumMod
        tnext(ModNum) = time+period
		!tNext(ModNum) = emergeDay
        !tNext(ModNum) = sowingDay
        !INIT = .TRUE.
               
        !plant block, these are all local variables
        NRATIO = 1 !Local var:
        lwp_hourly = psil_ / 10.
        lwp_predawn = psil_ / 10.
        AvailableNitrogen = 0.
        CumulativeNitrogen = 0.
        HourlyNitrogenDemand = 0.
        WaterUptake = 0.
        HourlySoilWaterUptake = 0.
        TotalSoilWateruptake = 0.
        Trans = 0.
        iTime_old = 0
        
         !***A new plant model object is created and initialized (calls initialize function) here
		CALL Initialize ! Read variety file and generate OUTPUT with plant related file (PlantGraphics,LeafGraphics) 
		CALL First_Init ! Based on variety file and initial conditions, determing phenological status of plant, add unit conversions too
		CALL Daily_Ave  ! Computes daily 24h values primarily needed for phenology and 24h output info
   
    End if   
    
    !***End Initialization

    if (simulationdone.eq.1) return !when crop is matured, dll will be exited to avoid error message at end of simulation
    If (isMatured .eq. 1) return
    
   
    !Simulate nitrogen and water uptake for each interval if less than 60min
    If (NShoot .GT. 0) then
		WaterUptake = WaterUptake + AWUPS*Step			   !local var: g water per slab taken up in an hour
		NitrogenUptake = NitrogenUptake + SIncrSink/1.0e6  !local var: Cumulative N (mass, g N (plant)-1) in this time step ;  sincrsink is ug N
        !Below, is now computed in 2DSOIL
		!HourlyCarboUsed = HourlyCarboUsed + PCRS*Step      !2DSOIL var: carbon allocated from shoot to root current used for root growth, g CHO slab-1 h-1
    End if 	 

  !Loop through the crop model each hour
    if (abs(time-tNext(Modnum)).le. 0.001*step) then ! code inside this if statement is executed every hour once the crop has emerged   
    !setsurf02 (daily weather) routine tnext(1) does not always advance in sync with crop (tnext(6)).  Crop tnext will advance while weather does not, causing a repeat
    !    and or skip of an hour before things realign. To avoid this I want to link the two next time-steps
    !if (abs(time-tNext(ModNum)).lt.0.001*step.and.not(iTime.eq.iTime_old)) then !the and is needed to fix bug that pops up when setsurface02 (daily weather) iteration slows down such that the crop Modnum next increments but setsurface modnum1 does not
          !get initial phenology set up   
          !*set sowing date, doesn't matter if direct seed (estab = 1) or transplanted (estab = 0)      
          !if (ESTAB.eq.1.and.NShoot.eq.0.and.int(time).ge.sowingDay.and.isSown.eq.0) then 
          if (Nshoot.eq.0. .and. int(time).ge.sowingDay .and. isSown.eq.0) then
             CROPSTA = 1             !local var:
             DAS = 0.                    !local var:
             ISA = 0                 !local var:
             isSown = 1              !local var:
          endif
        !*track days after sowing before emergence occurs
          !if (int(time).ge.sowingDay.and.Nshoot.eq.0) then !adjusted by difference in emergence date and sowing date
          if(isSown.eq.1.and.not(DAS.EQ.0..and.iTime.eq.1)) then !start counting DAS 1 time-step after sowing starts
              DAS = DAS + period
              IDAS = int(DAS)
          endif
      
          !Germination occurs 1 day after sowing?  Emergence day is provided as an input in the initals file...
          if (int(time).eq.(sowingDay+1).and.isGerminated.eq.0) then 
                isGerminated = 1  !local var: Is this correct?  
                !***below - we only use initialrootcarbo if the nodal file was not pre-populated with root mass at germination. as of now, nodal root is set to zero so use it.  
                initialrootcarbo = WRTI*popslab !2DSOIL variable g slab-1 !circle back, initial root mass can be different for direct seed versus transplanting where it is already a few days post germination and emergence! 
          end if
      
          !if it is emergence date, initialize again for direct (ESTAB 1) or transplanted (ESTAB 0) rice
          !for transplanted plants, ESTAB = 0, letting emergence date be equivalent to transplant date, will have to adjust in future
          !for direct seed, ESTAB = 1, but if emergence day is reached, will also use the following routine
          if (NShoot.eq.0.and.int(time).eq.emergeDay.and.isEmerged.eq.0) then
            DVS = 0.        !local var: emergence
            isEmerged = 1
            CALL Emerge_Init
            if(ESTAB.eq.0) then 
                CROPSTA = 3 !set stage number for tranplanted rice assuming emergence = transplant date, will have to adjust in future
                NShoot = 0  !2DSOIL var: won't simulate growth yet for transplanted rice
            end if
            if(CROPSTA.EQ.1) then
                CROPSTA = 4 !jump direct seed to mainstream growth, CHECK THIS!!!
                NShoot = 1
            endif
          endif
      
          !if it is transplanting day, and we are using transplanted rice then move to mainstream growth
          if (NShoot.eq.0.and.ESTAB.eq.0.and.int(time).eq.transplantDay) then 
              ISA = int(time) - sowingDay          
              ISA = ISA + SBDUR !local var: age of seed plus seedbed duration (SBDUR in init file, but should also just calculat from transpalnt date - emerge date)
              call Transplant_Init
              if(CROPSTA.EQ.3) then !cropstage switches to 4 in the phenology routine 
                  NShoot = 1
              end if
          end if

        
          !obtain potentially useful soil-plant-atmos variable data
          lwp_hourly = psil_ / 10. !local var: convert from Bar to Mpa
          if (iTime.eq.5) lwp_predawn = psil_/10. !if time is 5am, stores this as predawn leaf water potential
          !do carbon balancing here to account for unused CHO previously sent to root growth that was not utilized last timestep
          PCRS = HourlyCarboUsed/POPSLAB    !2DSOIL var: pass CHO used for root growth in 2DSOIL back to plant, after dividing by popslab, units g CH2O plant-1 h-1
          HourlyCarboUsed = 0.0             !2DSOIL var: HourlyCarbonUsed is total amount of CHO used for root grwowth by 2dsoil in prior soil iteration loop, dividing by popslab coverts units to g plant-1 h-1

          !		wthr.TotalRootWeight = SHOOTR->TotalRootWeight/PopSlab; !2DSOIL var: update of current rootgrowth - g CHO plant-1 season-1
	      !		wthr.MaxRootDepth=SHOOTR->MaxRootDepth  !2DSOIL var: not sure if this is current root depth or maximum allowed
          if (NitrogenUptake.gt.0.) then
              !AvailableNitrogen = NitrogenUptake / POPSLAB  !Local var: Nitrogenuptake is the current N taken up from the soil for plant growth for current hour only, g N plant-1 h-1
              AvailableNitrogen = AvailableNitrogen + NitrogenUptake / POPSLAB !" Local var: current N available to the plant for allocation
              !CumulativeNitrogen = CumulativeNitrogen + AvailableNitrogen !Local Var: cumulative N take from soil since emergence, g plant-1 season-1
              CumulativeNitrogen = CumulativeNitrogen + NitrogenUptake / POPSLAB !" see note for AvailableNitrogen
              !write(*,*) DAS, leaf_N, leaf_Npool, grnleaf_dw, leaf_N*grnleaf_dw
          end if
      
          !wthr.ET_supply = WaterUptake*((1/18.01)/((24*3600)*(SHOOTR->RowSp*SHOOTR->EOMult/10000)*SHOOTR->LAI));   //into MAIZESIM Yang 8/15/06
          !wthr.ET_supply = WaterUptake/(SHOOTR->EOMult*SHOOTR->PopRow)*100; // units are now area gram per plant per hour
          !wthr.ET_supply = wthr.ET_supply * pSC->getInitInfo().plantDensity / pSC->getPlant()->get_greenLAI()/18 / 3600; // units are now mol m-2 leaf s-1
	      !ET_supply is used in energy balance calculation for leaf temperature and must be in units of mol m-2 leaf s-1
	      !as far as I can tell, ET_supply is not being utilized anywhere else in MAIZSIM

          !run basic routines either at, or after transplanting/emergence date, CROPSTA will be either 3 or 4 at this point
          if (Nshoot.eq.1.OR.CROPSTA.ge.3) then       
		    CurrentNUptakeError = AvailableNitrogen  - HourlyNitrogenDemand     !Local var: difference in N uptake versus demand, positive means surplus, g N plant h-1
            CumulativeNUptakeError = CumulativeNUptakeError + CurrentNUptakeError   !Local var: cumulative difference ", g N plant-1
            
    !Start here will call the main PLANT 
            CALL Daily_ave
            CALL Phenology
            CALL Nitrogen
            CALL Stresses
            !CALL Gas_Exchange
            CALL Grain_formation
            CALL Organ_Growth
            CALL Organ_Respiration
            CALL Senescence
            CALL Carbon_allocation

            !write(*,*) DAS, availableNitrogenstart, totalplantNstart, availableNitrogen, totalplantN, totalplantNstart+availableNitrogenstart
            PCRL = (rootPart )*24.*popslab              !2DSOIL var: minimum amount of CHO to allocate to roots, g ch2o slab-1 d-1
            PCRQ = (rootPart + rootPool)*24.*popslab   !2DSOIl var: maximum amount of CHO to allocate to roots g ch2o slab-1 d-1

            
            !2DSOIL and Whole Plant Accounting at end of hourly time-step
            if(iTime.eq.24.AND.int(time).eq.(emergeDay).and.CROPSTA.eq.3) CROPSTA = 4 !move transplanted plants to stage 4 after 1 day post emergence day    
            HourlySoilWaterUptake = WaterUptake/PopSlab                       !Local var: hourly water uptake from soil in g H2O plant-1 h-1
            TotalSoilWaterUptake = TotalSoilWaterUptake + WaterUptake/PopSlab !Local var: cumulative water uptake from soil in g H2O plant-1
            LAREAT = leaf_area                                  !2DSOIL var: I believe this is supposed to be in cm2?
            LAI = LAREAT*PopArea/(100.*100.)                    !2DSOIL var: not sure what differnece is wrt LCAI...
            LCAI = leaf_area*PopArea/(100.*100.)                !2DSOIL var: PopArea is the plant density read from the initials file, will need to check difference with # hills and plants/hill
            !LCAI = LAREAT*POPROW/100.0/MIN(HEIGHT,ROWSP)       !2DSOIL var: "
            Cover = 1.0 - exp (-0.79*LCAI)                      !2DSOIL var: need to modifiy for rice
            Shade = Cover*RowSp                                 !2DSOIL var: "
		    Height=max(min(Shade,RowSp),2.0)                    !2DSOIL var: "
            !ET_demand = Trans*0.018*3600.*24./PopArea           !2DSOIL var: pass ET demand from shoot to root, g plant-1 d-1 converted from instanteous transpiration, mmol H2O m-2 ground s-1
            ET_demand = trans*0.018*3600.*24./10000.            !2DSOIL var: pass ET demand from shoot to root, g cm-2 ground d-1
		    nitroDemand = HourlyNitrogenDemand*POPSLAB*1e6*24.  !2DSOIL var: pass the N demand into 2dsoil, units are ug N slab-1 d-1
		    NDemandError = CurrentNUptakeError                  !2DSOIL var: but these units are g N plant-1 ?, check units, is it being used in 2DSOIL?
		    CumulativeNDemandError = CumulativeNUptakeError     !2DSOIL var: these units are g N plant-1 - why is nitroDemand in ug N slab-1?
        
            !if(iTime.eq.24) write(*,*) CROPSTA, DVS, DVR, HU
            !write(*,*) rootPart, rootPool, PCRL/popslab/24., PCRQ/popslab/24., PCRS
            !write(*,*) totalrootweight/popslab, root_dw ! check if values are consistent between 2DSOIL and Crop 
            
            CALL Crop_output
            iTime_old = iTime
        end if
        !check to make sure when crop is matured, dll is exited (avoids error message at end of simulation
        if(isMatured.eq.1 ) then
            write(*,*) "Completing crop simulation..."
		    Simulationdone = 1
		    NShoot = 0             !tell 2dsoil that crops harvested
            tnext(ModNum)=1.e+32
        else
            tnext(ModNum) = Time + Period
		    WaterUptake = 0.
		    NitrogenUptake = 0.
		    TotalPotentialRootWaterUptake = 0.
        end if
    end if

!***End of Daily crop routine***
      return
	end