!Includes subroutines for growth rate, leaf area expansion, and senescence
    
    Subroutine Organ_Growth
!Calculate potential, unstressed, growth rate for all organs
!ORYZA originally assumes all assimilate is used at each time-step
!There is no surplus supply except there is technically a stem reserve storage
!I have not figured out how that stem reserve storage goes back into the C pool, ORYZA assumes that 0.947% of the reserve is added into the C_pool
!  I account for this by dividing the stem_reseve by 24 hour time step and multiple by this facctor which accounts for 5.3% yield loss 
!  but I am going to push some of it into roots when water or nitrogen stressed?

!We assume 50 of senesced leaf mass and stem reserve is recaptured by assimilate pool each hour; elsewhere we assume 25% of N is recaptured
    
!Initially we use ORYZA dry matter partitioning coefficients which will vary over time
! organs that are considered are: leaf; stem; stemreserves; roots; storage organs
! Growth is set to fixed fraction of assimilate (GCR) which are adjusted by DVS
! As exapmle, shoot takes 85% of total, root is 15%, and leaf, stem, storage are fractions of shoot
! 50% of senesced material returns to leaf or stem
! stem mass and stem reserve are separate dry mass pools.  Stemreserve size of which is a genetic coefficient (e.g. 20%)
!    which can be mobilized for grain productoin, pools are kept separate within ORYZA

    
    use common_block
    REAL LINT2 !TTUTIL selection functoin from ORYZA
    
    
! local variables
    real pot_leafgro_transplant, pot_stemgro_transplant, &
        pot_stemreservegro_transplant, pot_rootgro_transplant
    real PWRR 
    real deficit, C_surplus
    
    rootPart_old = rootPart
    shootPart_old = shootPart
    pot_leafgro_transplant =0.
    pot_stemgro_transplant =0.
    pot_stemreservegro_transplant =0.
    pot_rootgro_transplant =0.
    
    PWRR = 1.
    
!hourly unstressed growth rates
!(1) ----Relative growth rates of shoot organs ORYZA1.f90 start at line 685
   ! GCR = GCR + 0.5*sen_LLV + 0.5 * sen_LSTR + stemreserve_dw / 24. * 0.947 !g CHO2 h-1 plant-1; 50% contributions from dead leaf / stem taken care of in carbon_allocation at prior time-step
    GCR = GCR + max(stemreserve_dw/24. * 0.947,0.)
    stemreserve_dw = max(0.,stemreserve_dw * (1 - 0.947/24.))
    !GCR = GCR
       
    !1 Determine potential carbon partitioning fractions
    !Potential Fractions of CARB partitioning, variety parameters, need to adjust for DVS
    !Actual Fractions of CARB partitioning - hardcoded array dimension for now
    f_totdm_shoot = LINT2('shootfr',f_totdm_shootDVS,8,DVS)
    ! modify this fraction under water stress, using pre-dawn leaf expansion factor for now
    if (DVS.lt.1.) then
        f_totdm_shoot = (f_totdm_shoot) * lf_wat_rf !reduces shoot allocatoin proportionally based on expansion factor, is htis double-penalizing for drought though since we are reducing it for leaf expansion already?
    end if
    f_totdm_root = 1 - f_totdm_shoot
    f_shtdm_leaf = LINT2('leaffr',f_shtdm_leafDVS,12,DVS) !fraction of shoot dm partitioned to leaf
    !This has to be fixed, leaf, stem, and store are set fractions of shoot, but they can add up to more than 1.0!!! DHF
    f_shtdm_stem = LINT2('stemfr',f_shtdm_stemDVS,12,DVS) !fraction of shoot dm partitioned to stem 
    f_shtdm_store  = LINT2('stirefr',f_shtdm_storeDVS,12,DVS) !fraction of shoot dm partitoined to panicle, usually 0 before flowering
  
      
!(2a) Determine potential growth rates in terms of total hourly available C for each organ
    if (cropsta.eq.3.and.DVR.lt.0.1) then !reduced organ growth at transplanting
        !PLTR was determined in phenology routine
        pot_leafgro_transplant = (grnleaf_dw*(1.-PLTR))/timeStep !reduction in leaf growth rate
        pot_stemgro_transplant = (stem_dw*(1.-PLTR))/timeStep !reduction in stem growth rate
        pot_stemreservegro_transplant  = (stemreserve_dw*(1.-PLTR))&
            /timeStep
        pot_rootgro_transplant = (root_dw *(1.-PLTR))/timeStep
    endif 
    
    !(2b) prior to flowering:
    pot_rootgro = GCR*f_totdm_root - pot_rootgro_transplant
    pot_leafgro = GCR*f_totdm_shoot*f_shtdm_leaf - pot_leafgro_transplant
    pot_stemgro = GCR*f_totdm_shoot*f_shtdm_stem*(1.-f_stemdm_reserve) - &
        pot_stemgro_transplant
    pot_stemreservegro = GCR*f_totdm_shoot*f_shtdm_stem*f_stemdm_reserve - &
        pot_stemreservegro_transplant !this is a fraction of the stem CHO which can be remobilized, it is part of stem dry mass pool
    pot_storagegro = GCR * f_totdm_shoot * f_shtdm_store

    !(2c) check assimilate flow to grain after flowring and re-adjust partitioning coefficients if this assimilate flow exceeds max due to decreased spikelets
    if (DVS.GT.0.95) then! at flowering rough rice growth (or grain_dw in ORYZA) is the same rate as storage organ growth
       pot_roughricegro = pot_storagegro !rough rice growth is set equal to stroage growth moving forward, it is a class within storage growth, so we do not account for it as part of the mass balance
    ELSE 
        pot_roughricegro = 0.

    END IF    
    !Below from ORYZA
    !Idea is that roughrice is likley source limited while PWRR is the yield in case all spikelets had been completely filled (i.e no source limits)
    !  the differene between the two quantities is the maximum amount of CHO that could be shipped to the spikelets
    !  so if the potential rough rice growth is greater than this value, we need to reduce it
        !1(b) if grainfill    
    !Per ORYZA, to avoid overshoot in storage sink limitation calcs, based on prior day (hours) growth rates
    !If grainfill has started:
    ! pvo20131223: WGCOR is based on D:\africarice\ORYZASAHEL\ORYZAS1S\ORYZA1S.FOR
    ! Parameters 10. and 20. oC are from WGCOTB in D:\africarice\ORYZASAHEL\ORYZAS1S\RICE.DAT
    ! it means that when TMIN during DVS 1.0 to 1.4 (start of grain filling phase) is below 20oC then maximum grain weight will be reduced
    if((DVS.ge.1.).and.(DVS.LE.1.4)) then
        WGCOR = max(0.,min(1.,(T_dailymin-10.)/(20.-10.)))
    end if
    PWRR = max(roughrice_dw, grain_num * grw_WRGMX * WGCOR)
    !----------Check sink limitation based on yesterday's growth rates
!          and adapt partitioning stem-storage organ accordingly
    if (isGrainfill.eq.1) then
        !original ORYZA idea is to restrict roughrice growth when spikelet numbers are much less than the actual assimilate being shipped to the grain
        !however, in practice, this results in sudden increase in stem dry weight when we should see no growth or slightly decline due to respiration / stem senescence
        !solution may be to push this into stem reserves and eventually use that to cause feedback assimilation rate to reduce GCR
        !For now, I just restrict storage growth and we have unused assimilate 
        if (pot_roughricegro.GE.(PWRR-roughrice_dw)) then ! only change store fraction if current assimilate status is greater than 0
            if ((GCR*f_totdm_shoot).GT.0.) then
                f_shtdm_store = max(0.,(PWRR-roughrice_dw)/(GCR*f_totdm_shoot)) !original ORYZA approach
                f_shtdm_stem  = 1.-f_shtdm_store-f_shtdm_leaf !original ORYZA approach
                
                !recalculate stem and storage growth
                !pot_stemgro = GCR*f_totdm_shoot*f_shtdm_stem*(1.-f_stemdm_reserve) - &
                !    pot_stemgro_transplant
                pot_stemreservegro = GCR*f_totdm_shoot*f_shtdm_stem*f_stemdm_reserve !this is a fraction of the stem CHO which can be remobilized, it is part of stem dry mass pool
                pot_storagegro = GCR * f_totdm_shoot * f_shtdm_store
                pot_roughricegro = pot_storagegro 
            end if
        end if
    end if 
    

    
    !(2b) Adjust C partitioning based on root C demand from previous time-step
    shootPart = pot_leafgro+pot_stemgro+pot_stemreservegro+pot_storagegro
    rootPart = pot_rootgro
    if (PCRS.GT.rootPart_old) then
        !if true, 2DSOIL allocated more CHO than was originally available to roots at prior time-step
        !therefore, need to subtract this extra amount to prevent mass balance issue
        !note in 2DSOIL, the cause is that roots have 100% priority during water stress so growth to all other organs, except for grain, should be penalized
        !priority - subtract excess assimilate supply, from reserves, then from leaf/stem growth
        deficit = PCRS - rootPart_old
        C_surplus = max(GCR - shootPart - pot_rootgro,0.) !this is amount of current assimilate beyond what is needed for current growth.  Note there will never be a surplus because ORYZA uses up all current assimilate each step.  Can potentially use storage and stemreserves instead
        !1 - there is sufficient assimilate supply to account for this
        if (C_surplus.ge.deficit) then !if positive, then nothing else needs to be adjusted
            GCR = GCR - deficit
            deficit = 0.
        !2 - insufficient assimilate supply so need to check on potential growth rates next
        else if (C_surplus.lt.deficit) then
            deficit = deficit - C_surplus
            GCR = GCR - C_surplus
            C_surplus = 0.
            !2a - not enough potential growth rate in shoot, so need to substract from current reserves
            if (shootPart.lt.deficit) then ! not enough CHO in potential shoot growth to compensate so have to take from existing reserves, first from stem reserve, then from storage
                if ((stemreserve_dw + storage_dw).gt.deficit) then
                    if (stemreserve_dw.ge.deficit) then 
                            stemreserve_dw = stemreserve_dw - deficit 
                            deficit = 0.
                    else if (stemreserve_dw.lt.deficit) then
                            deficit = deficit - stemreserve_dw
                            stemreserve_dw = 0.
                            storage_dw = storage_dw - deficit 
                            deficit = 0.
                    end if
                !2a-1 - still can't meet deficit with pulling from reserves, so need to pull from reserves and reduce potential growth
                else
                    !may cause slight mass balance issue here if insufficient reserve and storage to account for priortime-step root growth, 
                    !rather than penalize future root growth, reduce leaf and stem instead equally
                    deficit = deficit - stemreserve_dw - storage_dw
                    stemreserve_dw = 0.
                    storage_dw = 0.
                    pot_leafgro = pot_leafgro * (1 - deficit/shootPart) !reduce in proportion to original amount
                    pot_stemgro = pot_stemgro * (1 - deficit/shootPart)
                    pot_stemreservegro = pot_stemreservegro * (1 - deficit/shootPart)
                    pot_storagegro = pot_storagegro * (1 - deficit/shootPart)
                    deficit = 0.
                end if
            !2b - there is enough potential growth rate in shoot to leave resersves alone
            else !take from stem reserves first, then equally from shoot and leaf growth
                if (stemreserve_dw.ge.deficit) then
                    stemreserve_dw = stemreserve_dw - deficit
                    deficit = 0.
                else
                    deficit = deficit - stemreserve_dw
                    stemreserve_dw = 0.
                    pot_leafgro = pot_leafgro * ((pot_leafgro+pot_stemgro)-deficit) / (pot_leafgro+pot_stemgro)
                    pot_stemgro = pot_stemgro * ((pot_leafgro+pot_stemgro)-deficit) / (pot_leafgro+pot_stemgro)
                    
                    shootPart = shootPart - deficit
                    deficit = 0.
                end if
            end if
        rootPart = pot_rootgro 
        else !if false, means less or equal CHO was allocated at prior time-step
            
            !place any exces in root carbon reserve whihc can be used to grow root mass at night
            rootPool = rootPool + max(0., rootPart_old-PCRS)
            shootPart = pot_leafgro + pot_stemgro + pot_stemreservegro + pot_storagegro
            rootPart = pot_rootgro - max(0., rootPart_old - PCRS) ! subtract unused CHO out of current root growth since it was sent to pool instead, do I need to add rootPool to total growth?
        end if
    end if
            
    
    !(3) Set all values to actual growth  - eventually we will need to reduce potential by stresses
    act_leafgro = pot_leafgro
    act_rootgro = pot_rootgro
    act_stemgro = pot_stemgro
    act_stemreservegro = pot_stemreservegro 
    act_storagegro = pot_storagegro
    if (isGrainfill.eq.1) act_roughricegro = pot_roughricegro ! is a portion of storage growth, and its mass is therefore already accounted for in the storage gro organ
    act_totalgro = act_leafgro+act_rootgro+act_stemgro+&
        act_stemreservegro+act_storagegro
    
    GCR = max(0.,GCR-act_totalgro)
    
    
    !(4) Determine leaf area expansion
    ! (4a) Get leaf physiological age from hourly thermal units, modified to hourly Tair and SLA
    CALL Temperature_SUBDD 
    HULV = HU !hourly TT
    TSLV = TSLV + HULV !cumulative TT for leave developmet
    !specific leaf area at current time step
    !SLA = grw_SLA_nominal
    SLA = 1./LINT2('SLA',t_SLA,14,DVS)
    SLA = 222.

    !(4b) Deterine leaf area growth based on carbon allocated to it
    CALL LAI_growth !sets the actual increment of LAI growth, act_LAIgro
    
    !(5) Specific stem area
    SSGA = LINT2('SSGA',t_SSGA,10,DVS)
    SAI = SSGA * stem_dw 
    

    end subroutine Organ_Growth

    

subroutine LAI_growth
!ported from ORYZA, 
!----------------------------------------------------------------------*
!  SUBROUTINE SUBLAI3                                                  *
!  Version 2: January 2001                                             *
!          Version august, 2003                                        *
!                                                                      *
!  Purpose: This subroutine calculates the rate of growth of LAI of    *
!    of the crop in the seedbed and after transplanting in the field.  *
!    Reductions by N-stress and water-stress are taken into account.   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CROPSTA I4  Crop stage (-)                                        I  *
! RWLVG   R4  Green leaves growth rate (kg d-1 ha-1)                I  *
! sen_DLDR    R4  Death rate green leaves (g plant-1 h-1)                 I  *
! TSLV    R4  Temperature sum for leaf development (oC)             I  *
! HULV    R4  Daily temperature for leaf development (oC)           I  *
! sed_SHCKL   R4  Delay parameter in development ((oCd)(oCd)-1)         I  *
! LESTRS  R4  Reduction factor for leaf elongation (-)              I  *
! SLA     R4  Specific leaf area (ha kg-1)                          I  *
! NH      R4  Number of hills (hills m-2)                           I  *
! NPLH    I4  Number of plants per hill (pl/hill)                   I  *
! NPLSB   R4  Number of plants in seedbed (pl/m2)                   I  *
! DVS     R4  Development stage of the crop (-)                     I  *
! LAI     R4  Leaf area index (ha ha-1)                             I  *
! grw_RGRLMX  R4  Maximum relative growth rate leaves ((oCh)-1)         I  *
! grw_RGRLMN  R4  Minimum relative growth rate leaves ((oCh)-1)         I  *
! ESTAB   C*  Establishment method (-)                              I  *
! act_LAIgro    R4  Growth rate leaf area index (ha h-1 ha-1)             O  *
! act_RGRL    R4  Actual relative growth rate leaves ((oCh)-1)          O  *

!ToDO;  add stresss component to LAI estamte on line 121, thi sis DLDR (drough stress) and corresponding reduction in leaf elongation rate (LESTRS)
      use common_block
!-----Local parameters
      REAL          TSLVTR, TSHCKL, GLAI, GLAI1,GLAI2, X, TESTSET
      REAL          WLVGEXP, LAIEXP, WLVGEXS, LAIEXS, TEST, DVSE
      LOGICAL       TESTL 
      
      SAVE
      
      if (CROPSTA.LE.1.OR.DAS.LT.4.) then !not sure if DAS modification works, idea is to get this to initiate early in the process...
          X = 1.
          TESTL = .FALSE.
          TESTSET = 0.01
      end if
      
!-----Transplanted rice
!      Calculate RGRL as function of N stress limitation
      act_RGRL = grw_RGRLMX - (1.-lf_N_gro)*(grw_RGRLMX-grw_RGRLMN) 
      if (nitroenv.eq.0) act_RGRL = grw_RGRLMX !no N stress
      if (estab.eq.0) then !0 - transplant
!------- 1. Seed-bed; no drought stress effects in seed-bed!          
          if(CROPSTA.LT.3) then !prior to transplant DAT, not main growth yet
              if (LAI.LT.1.) then
                  act_LAIgro = LAI * act_RGRL 
                  act_LAIgro = LESTRS * LAI*act_RGRL*HULV !LESTRS is elongation stress, set to 1 for now; HULV leaf heat units
                  WLVGEXS = grnleaf_dw !prior to leaf C allocation at this time-step, used only when LAI LT 1
                  LAIEXS = LAI
              else
                  if (.NOT.TESTL) then
                      TEST = ABS((LAI/(grnleaf_dw))*1./(NHH*NPLH) &
                          *10000.-SLA)/SLA
                      if (TEST.LT.TESTSET) TESTL = .TRUE.
                  end if
                  if (TESTL) then
                      act_LAIgro = ((grnleaf_dw+act_leafgro)*SLA)- LAI
                  else
                      GLAI1 = ((grnleaf_dw+act_leafgro-WLVGEXS)* &
                          (NHH*NPLH)/10000. *SLA+LAIEXS)-LAI
                      GLAI2 = ((grnleaf_dw+act_leafgro)* &
                          (NHH*NPLH)/10000.*SLA)-LAI
                      if (GLAI2.GT.0.) then
                          act_LAIgro = (GLAI1+X*GLAI2)/(X+1.)
                      else
                          act_LAIgro = GLAI1
                      end if
                      X = X+1.
                  end if
              end if
 !------- 2. Transplanting effects: dilution and shock-setting
          else if (CROPSTA.EQ.3) then !DAT is today
            TSLVTR = TSLV
            TSHCKL = sed_SHCKL*TSLVTR !transplant shock, uses variety data and current leaf development TT
            !act_LAIgro   = (LAI*NHH*NPLH/NPLSB) - LAI
            act_LAIgro = LAI*1.1 - LAI
            TESTL  = .FALSE.
            X      = 1.
!--------3. After transplanting: main crop growth
         else if (CROPSTA .EQ. 4) then
!--------3.1. During transplanting shock-period
            if (TSLV.LT.(TSLVTR+TSHCKL)) then
               act_LAIgro = 0.0
               DVSE = DVS
!--------3.2. After transplanting shock; drought stress effects
            else
               if ((LAI.LT.1.0).AND.(DVS.LT.1.0)) then
                  act_LAIgro = LESTRS * LAI*act_RGRL*HULV
                  WLVGEXP = grnleaf_dw
                  LAIEXP  = LAI
            else
!                 There is a transition from RGRL to SLA determined growth
!                 when difference between simulated and imposed SLA is less than 1%
                  IF(act_leafgro.LT.0.0) THEN
                     TESTL =.TRUE.   
                  ELSEIF((act_leafgro.GE.0.0).AND.(TESTL)) THEN
                     TESTL =.FALSE.
                  ENDIF                  !Added by TaoLi, 10 Aug, 2010
                  if (.NOT. TESTL) then
                     TEST = ABS((LAI/(grnleaf_dw))*1./(NHH*NPLH)*10000. &
                         -SLA)/SLA !compares relative difference in full canopy SLA minus current SLA for new growth)
                     if (TEST .LT. TESTSET) TESTL = .TRUE. !testset was set to 0.01 as a threshold, so if change in SLA is less than 1% do more gradual LAI growth calc
                  end if
                  if (TESTL) then
                     act_LAIgro = ((grnleaf_dw+act_leafgro-sen_DLDR)*SLA) *(NHH*NPLH) &
                   /10000. -LAI !DLDR dead leaf rate not accounted for yet
                  else
                     GLAI1 = ((grnleaf_dw+act_leafgro-sen_DLDR-WLVGEXP)* &
                          (NHH*NPLH)/10000.*SLA+LAIEXP)-LAI !" 
                     GLAI2 = ((grnleaf_dw+act_leafgro-sen_DLDR)* &
                          (NHH*NPLH)/10000.*SLA)-LAI ! "
                     if (GLAI2 .LT. 0. .AND. GLAI1 .GT. 0.) then
                        act_LAIgro = GLAI1/(X+1.)
                     else
                        act_LAIgro  = max(0.,(GLAI1+X*GLAI2)/(X+1.))
                     end if
                     X     = X+1.
                  end if
               end if
            end if
         end if            
         

!===================================================================*
!------Direct-seeded rice                                           *
!===================================================================*
       else if (ESTAB .EQ. 1) then
         IF ((LAI.LT.1.0).AND.(DVS.LT.1.0)) then
            act_LAIgro    = LAI*act_RGRL*HULV * LESTRS
            WLVGEXP = grnleaf_dw
            LAIEXP  = LAI
         else
!           There is a transition from RGRL to SLA determined growth
!           when difference between simulated and imposed SLA is less than 10%
            IF(act_leafgro.LT.0.0) THEN
               TESTL =.TRUE.   
            ELSEIF((act_leafgro.GE.0.0).AND.(TESTL)) THEN
               TESTL =.FALSE.
            ENDIF                  !Added by TaoLi, 10 Aug, 2010
            if (.NOT. TESTL) then !TESTL will be true if transition is less than 10% of SLA based expansion
               TEST = ABS((LAI/(grnleaf_dw))*1./(NHH*NPLH)*10000. &
                   -SLA)/SLA
               IF (TEST .LT. TESTSET) TESTL = .TRUE.
            END IF
            IF (TESTL) THEN
               act_LAIgro = ((grnleaf_dw+act_leafgro-sen_DLDR)*SLA)*(NHH*NPLH) &
                   /10000. -LAI !DLDR dead leaf rate not accounted for yet
            ELSE
               GLAI1 = ((grnleaf_dw+act_leafgro-sen_DLDR-WLVGEXP)* & !gain in LAI based on leaf dry weight above that obtained at DVS 0.99 stage minus the current LAI
                          (NHH*NPLH)/10000.*SLA+LAIEXP)-LAI !"
               GLAI2 = ((grnleaf_dw+act_leafgro-sen_DLDR)* & !gain in LAI based on current/projected leaf dry weight * SLAI minus the current LAI amount
                          (NHH*NPLH)/10000.*SLA)-LAI !"
               if (GLAI2 .LT. 0. .AND. GLAI1 .GT. 0.) then
                  act_LAIgro = GLAI1/(X+1.)
               else
                  act_LAIgro  = max(0.,(GLAI1+X*GLAI2)/(X+1.))
               end if
               X     = X+1.
               !Below, I'm bypassing the above computations, just seems to be weighted 
               !act_LAIgro = act_leafgro * SLA * (NHH*NPLH)/10000. 
            end if
         end if
       end if
    if (act_leafgro.eq.0.) act_LAIgro = 0.
    
    
    !here, reduce act_LAIgro via predawn leaf water potential
    !this uncouples dry weight <-> LAI for this time step, but seems okay - can get carbon allocated but expansion gets reduced due to water stresss
    sf = 3.21 !parameters come from analyseis of slow droughted rice leaves that were on plants previously droughted so should be conservative response, data from Cutler (1980)
    phy = -0.18
    lf_wat_rf = (1.+exp(sf*phy))/(1.+exp(sf*(phy-lwp_predawn)))
    if (watlimit.eq.1) act_LAIgro = act_LAIgro * lf_wat_rf !stress
    
    return
    END
        


    
    
