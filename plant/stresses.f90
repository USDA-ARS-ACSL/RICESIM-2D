!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Keep subroutines that simulate plant stress all in one file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
!!!!!! SUBROUTINE STRESSES    
    subroutine stresses
    use common_block
    call Lowtemperature_survival
    call lowtemperature_grainwt
    call lowtemperaturestress_grain
    call hightemperaturestress_grain
    call nitrogen_stress
    return
    end subroutine stresses
    
    
    subroutine nitrogen_stress
!**************************************
! Determine effect of nitrogen status on leaf senescence rate and leaf growth rate
! We follow the logic, but not the exact calculations in the ORYZA NCROP2 module
! In ORYZA, N stress is based on ratio between potential N status in above ground organs versus current N status in above ground organs; this is set to either 1 or 2
! This ratio analogue is then used to determine N stress on leaf senescence and on leaf relative growth rate
! There is no direct N stress on leaf photosynthetic rate; instead ORYZA uses a leaf N function versus Pmax value to account for it, we will mimic this factor in the GasExchange method
    
! Here, we ratio of N potential versus N actual content in the plant
!as NSTRES increases, this means there is a higher N deficit (ratio of potential versus actual)
    use common_block
    REAL LINT2 !TTUTIL selection functoin from ORYZA
    real NSTRES
    real N_cropref
    
    !Set N stress factor for leaf sensescence
    if (CROPSTA.ge.4) then
        N_cropref = grnleaf_dw*NMAXL + stem_dw*NMAXL*0.5 + root_dw*NMAXL*0.5 + storage_dw*nit_NMAXSO !this would be the max N level, gN gplant-1 that coudl be incorporated into biomass
        if (totalplantN.eq.0.) then 
            NSTRES = 2.
        else
            NSTRES = N_cropref / (totalplantN-availableNitrogen)
        end if
        if (NSTRES.LT.1.) NSTRES = 1.
        if (NSTRES.GT.2.) NSTRES = 2.
    
        lf_N_sen = LINT2('NSLLVT', t_NSLLV, 10, NSTRES)

        ! Set N stress factor for RGRL
        ! 1 should be no stress, while 0 means full stress
        ! in practice, I'm finding this range too narrow in original ORYZA
        ! I now assume if leaf_N is in excess of 90% of NMAXL then there is no stress
        ! However, stress linearly increases as leaf N approaches NMINL
        !lf_N_gro = (leaf_N-0.9*NMAXL)/(NMAXL-0.9*NMAXL) !original ORYZA, this results in N stress reducing growth rate by half at anythin less than 0.04, seems too resstrictive
        !setting this to 0.3 seems to provide some benefit until a minimum fo 0.025 c/n is reached
        if (leaf_N.ge.0.9*NMAXL) then
            lf_N_gro = 1.0
        else
            lf_N_gro = (leaf_N - 0.3*NMINL) / (NMAXL - 0.3*NMAXL) !this will now scale between 0 to 1 as leaf_N falls behlow 0.9 * NMAXL value
        end if
        IF (lf_N_gro .GT. 1.) lf_N_gro = 1.
        IF (lf_N_gro .LT. 0.) lf_N_gro = 0.
        IF (DVS.LT.0.1) lf_N_gro = 1. !no penalty on initial leaf growth rate when plant is recently emerged
    end if 
    end subroutine nitrogen_stress
    
    
    subroutine lowtemperaturestress_grain
!************************************
! Determine effect of low temperature on grain sterility based on ORYZA subgrn5 with SWICOLD = 1
! Use 24 hour
    use common_block
    
    !local variables
    real TAV, CTT, TINCR
    
    TINCR = 0. ! if we want to implement T increase due to elaf rolling as in ORYZA
    !-----Temperature increase due to leaf rolling (BAS): 1.6 degree
!     per unit leaf rolling (Turner et al., 1986; p 269)
      !TINCR = 5.*(1.-LRSTRS)*1.6
! pvo20130507: not sure if this also applies for spikelets
      !TINCR = 0.
    
    if(iTime.eq.24) then 
        !TAV = T_dailyave  !timestep issue causing T_dailyave to be incorreclty calculated, until I can fix, use min value
        TAV = T_dailymin
        if ((DVS.GE.0.75).AND.(DVS.LE.1.2)) then
            CTT = max(0.,22.0-(TAV-TINCR))
            COLDTT = COLDTT + CTT
            SFCOLD = max(0.,(1.-(4.6 + 0.054*COLDTT**1.56)/100.))
        else
            !SFCOLD = 1.
        end if
    end if
    
    return
    end subroutine lowtemperaturestress_grain
    
    
    subroutine hightemperaturestress_grain
!***********************************************
!Determine flowering time and hot heat stress on sterility, this is Sanai's SWIHEAT = 4 in SUBGRN5
!Use 24 hour
    use common_block
    
    !local variables
    real HTT, EMF, TSSET, TAIRFL
    EMF = 0.
    HTT = 0.
    
    if (iTime.eq.24) then
      if (DVS.GT.0.95 .AND. DVS.LT.1.2) then ! with this line flowering occurs between 0.95<DVS<1.2, from ca 5 days before 50% flowering to 5 days after 50% flowering. 
        !calculate flowering time
          FLHAS = 12.7 - 0.348*T_dailymin7ave + EMF !flowering hours
          FLTIME = time_sunrise + FLHAS !uses today's  sunrise time if running every 24 hour time-step otherwise use yesterday's sunrise
          TSSET = TAIR(time_sunset) !approximately air T at sunset, 
          TAIRFL = TAIR(FLTIME) ! air T at flowering time
          VPDFL = VPD(FLTIME) ! VPD at flowering time
          TPANFL = T_canopy(FLTIME) !T canopy comes from T leaf from average sunlit/shaaded leaf T in gas exchange/energy balance
          
          !the following are Sanai's heat stress linear response, may need to tune to adjust to leaf T instead of air T at some point, DHF 4/3/2024
          HTT = MAX(0.,(TAIRFL-33.7)) ! for 33.7 for CL151, Wells; 34.2 cocodrie, x1753
          HEATTT = HEATTT + HTT
          SF2=MAX(0.,EXP(-0.65-0.167*HEATTT)/(1-EXP(-0.65-0.167*HEATTT)))   !SANAI 
          SF3=MIN(SF2,1.0)
          
          !average heat sterility over the flowering period, not sure I need this information righ tnow
          SF3CNT = SF3CNT + 1.
          SF3SUM = SF3SUM + SF3
          SFHEAT = SF3SUM / SF3CNT
         !TMAXA=TMAXTT/COUNT
         !TMINA=TMINTT/COUNT
         !TFLOWERA=TFLOWERTT/COUNT
      else
          !SFHEAT = 1.
      end if
    end if
          
    return
        
    end subroutine hightemperaturestress_grain
    
    
    subroutine Lowtemperature_survival
!*************************************************
!Determine effect of period of low T stress on crop survival
!from ORYZA subCD2 routine
! Just tabulates the # of days , need to cross check with # of days that kill crop elsewhere
!*************************************************
                                            
!  Purpose: This subroutine calculates number of days below a certain  *
!           average temperature (TAV), which is used to terminate the  *
!           simulation after a maximum number of cold days the crop    *
!           can survive.                                               *
! Adapted from version 1, March 2006, Bouman                           *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! env_COLDMIN I4 lower T threshold for growth                           I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! TAV     R4  Average daily temperature (oC)                        I  *
! TIME    R4  Time of simulation (d)                                T  *
! NCOLD   R4  Number of cold days (-)                               O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
        use common_block

 !SAVE

	  IF (CROPSTA .EQ. 3) NCOLD = 0.
      IF (T_dailyave.LT.env_COLDMIN) THEN
         NCOLD = NCOLD+1.
      ELSE
         NCOLD = 0.
      END IF
      RETURN
      
    END subroutine Lowtemperature_survival
    

    
    
    
    
    SUBROUTINE lowtemperature_grainwt

!From ORYZA, when min daily temp between DVS1 to 1.4 (flowering to  grain fillstart) is below 20C then max grain weight is reduced
!modified to use hourly Tair.  Only when Tair is less than 20C will a reduction occur
    use common_block
    if((DVS.GE.1.).AND.(DVS.LE.1.4)) THEN
        WGCOR = MAX(0.,MIN(1.,(TAIR(iTime)-10.)/(20.-10.)))
    end if
    
    WGCOR = 1.0 
    end subroutine lowtemperature_grainwt
    
