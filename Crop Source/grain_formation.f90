SUBROUTINE GRAIN_FORMATION
! Ported pieces from ORYZA SUBGRN5
! Heat and cold stress run at 24H period in the stress.f90 file
! Formation itself is taking place on hourly timestep
! Can't use fixed spikelet number from observed data yet
    
!----------------------------------------------------------------------*
!  SUBROUTINE SUBGRN5                                                  *
!  Version SUBGRN5: P.A.J. van Oort, Sept 2016                         *
!  3. Add two new SWICOLD options, with SFCOLD as calibrated for       *
!     Senegal (SWICOLD=4) and for Madagascar (SWICOLD=5) as reported   *
!     for IR64 in Figure 3 in Dingkuhn et al 2015, Field phenomics for *
!     response of a rice diversity panel to ten environments in        *
!     Senegal and Madagascar. 2. Chilling-induced spikelet sterility   *
!     This is a temporary fix, in the long run we want to have one     *
!     cold sterility model that works well in all environments.        *
!                                                                      *
!  Purpose: This subroutine calculates spikelet formation rate and     *
!           spikelet fertility as affected by low and high temperature *
!           and the grain growth rate.                                 *
!                                                                      *
! Choose your heat and cold sterility model.                           *
! SWICOLD parameter comes from crop file                               *
! SWICOLD = 1 = Oryza2000 default, from SUBGRN.f90
! SWIHEAT = 3 = function Julia, C. and Dingkuhn, M., 2013. Predicting temperature induced sterility of rice spikelets requires simulation of crop generated microclimate. European Journal of Agronomy 49, pp. 50-60
! We are SWIHEAT 4 which is Sanai's modification of 3  *                                                *
! About the new heat fertility:                                        *
!  (a) flowering time (FLTIME): rice often flowers in the morning,     *
!      when it is cooler than TMAX, the value used in previous version *
!  (b) diurnal temperature model: in ORYZA2000 model normally a        *
!      sinusoid pattern is assumed, but this model overestimates air   *
!      temperature in the morning and does not account for differences *
!      in daylength. Improved diurnal model added.                     *
!  (c) transpirational cooling: more transpirational cooling in a hot  *
!      and dry air. Using an empirical equation to calculate panicle   *
!      temperature (TPANFL) from air temperature at flowering (TAIRFL) *
!      and relative humidity at flowering (RHFL) or VPD at flowering.  * 
!  (d) calculating spikelet fertility based on more recent studies on  * 
!      TPANFL and observed fertility with iodide tainting of pollen    *
!      and stigma.                                                     *
!  (e) previous version calculated average temperature (TFERT) during  *
!      flowering period (DVS 0.96 to 1.2, about 10 days) and fertility *
!      SF2 from TFERT. In this new version we calculate for each day   *
!      the fertility (SFHEAT) and then the average of SFHEAT values.   *
!      (calculate first, then average)                                 *
!  (f) More research is needed on disentangling heat & drought effects *
!      In ORYZA2000 drought has an effect on the number of spikelets   *
!      (GNSP = GCR*SPGF) which reduced sink size. Leaf rolling         *
!      increases leaf temprature (see below, TINCR). Not clear though  *
!      if spikelet temperature increases as much as leaf temperature.  *
!      Possibly increases less, crop protecting its reproductive organ *
!      in case of drought stress. Needs further investigation.         *
!                                                                      *
!  Furthermore I have made growth of NGR and WRR consistent. In        *
!      the original ORYZA2000 version NGR would be calculated once at  *
!      the end of the flowering period based on SPFERT. As a result it *
!      was possible that at DVS = 1.2 the number of grains would be    *
!      very low (due to low SPFERT) such that PWRR = NGR*WGRMX would   *
!      be lower than grain weight (WRR) of which accumulation already  *
!      started DVS 0.95 onwards. Clearly that is illogical.            *
!      My solution: calculate NGR from DVS 0.95 onwards (same as WRR   *
!      which starts DVS 0.95 onwards in ORYZA1.F90).                   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! NSP_FRC I   Integer for forcing (yes 2, no 0) NSP to observed NSP I  *
! NSP_OBS R4  Observed number of spikelets (no ha-1)                I  *
! GCR     R4  Gross growth rate of the crop (kg DM/ha/d)            I  *
! CROPSTA R4  Crop stage (-)                                        I  *
! LRSTRS  R4  Leaf rolling stress factor (-)                        I  *
! DVS     R4  Development stage of the crop (-)                     I  *
! LAI     R4  Leaf Area Index (-)                                   I  !
! SPGF    R4  Spikelet growth factor (no kg-1)                      I  *
! DAYL    R4  Astronomical daylength (base = 0 degrees)      h      I  *
! TAV     R4  Average daily temperature (oC)                        I  *
! TMIN    R4  Daily minimum temperature (oC)                        I  *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! TMIN7   R4  Minimum temperature over previous 7 days (oC)         I  *
! VP      R4  Early morning vapour pressure (kPa)                   I  *
! SUMTMI  R4  Sum of TMIN values during cold sensitive phase (oC)   I  *
! CNTTMI  R4  Count of days in cold sensitive phase (-)             I  *
! NSP     R4  Number of spikelets (no)                              I  *
! NGR     R4  Number of fertilised spikelets (no ha-1 d-1)          I  *
! SF3SUM  R4  Sum of heat fertility values (-)                      I  *
! SF3CNT  R4  Count of heat fertility values (-)                    I  *
! GNSP    R4  Rate of increase in spikelet number (no ha-1 d-1)     O  *
! GNGR    R4  Rate of increase in fertilised spikelet number        O  *
! FLHAS   R4  Flowering time in hours after sunrise (h)             O  *
! SUNRIS  R4  Sunrise time (h)                                      O  *
! FLTIME  R4  Flowering time time (h)                               O  *
! TAIRFL  R4  Air temperature at flowering time (oC)                O  *
! VPSFL   R4  Saturated Vapour Pressure at flowering time (kPa)     O  *
! RHFL    R4  Relative Hunidity at flowering time (%)               O  *
! TPANFL  R4  Panicle temperature at flowering time (oC)            O  *
! SF3     R4  Daily Spikelet fertility factor due to heat (-)       O  *
! SFHEAT  R4  Average Spikelet fertility factor due to heat (-)     O  *
! SFCOLD  R4  NEW Cold Fertility (-)                                O  *
! SPFERT  R4  Spikelet fertility (-)                                O  *
! GRAINS  L*  Fortran logical function whether grains are formed    O  *
!                                                                      *
! FILE usage : none                                                    *
! Subroutines called: SVPS1                                            *
!                                                                      *
!----------------------------------------------------------------------*

    use common_block

!   local variables
      REAL    TINCR, DVSPI, DVSF, TAV, CTT, SF2
      REAL    t_SUNSET, TSSET, NGHTL, NTIME, VPSL, VPS,t_VPD  
      REAL    PI,P,TAU,W0,W1
      REAL    CTMINC, STERC,TCRITC, STERH, I, EMF
      REAL    LTR,TWMIN,SFC1CNT,SFC2CNT,SFC1SUM,SFC2SUM,SFCOLD1,SFCOLD2
      REAL    NSPJUV,GCR2BF,FNAB,NCLV,NCST,NBIOM
      REAL     TMAXA,TMINA,TFLOWERA, TMAXTT,TMINTT,TFLOWERTT ! NEW PARAMTERS Sanai June 18 2020
      INTEGER   COUNT                            ! NEW PARAMTERS Sanai April 8 18 2021 
      INTEGER SWICOLD,SWIHEAT,SWINSP,CNT2BF,TM      
      
!   Initialize / Reset each hour
    !Not sure why this is needed, just don't run subroutine unless CROPSTA > 3?
    !if (CROPSTA.LE.1) then
    
         NSPJUV = 0.
         SPFERT = 1.
         GCR2BF = 0.
         CNT2BF = 0 
         FNAB   = 1.
         TMAXTT=0.    ! new Sanai April 8 2021 
         TMINTT=0.
         TFLOWERTT=0.
         COUNT=0
         EMF = 0. ! value that reflects variety flowering time response to earlier in the morning
   ! END IF
       

    
!-----Spikelet formation between PI and Flowering
      DVSPI = 0.65
      DVSF  = 1.
      if ((DVS.GE.DVSPI).AND.(DVS.LE.DVSF)) then !developmental stage in between 0.65 and 1.0, between PI and 1st flower formation   
         rate_spikelet_num = MIN(GCR*grw_SPGF, grw_NSPM2X*(1./(NHH*NPLH)) - spikelet_num) !makes sure total number won't exceed max # per plant
         rate_spikelet_num = GCR*grw_SPGF
      else
         rate_spikelet_num = 0.
      end if
      
!-----Add cold stress options here - see lowtemperaturestress_grain in Stresses file
!-----Add heat stress options here - see hightemperaturestress_grain in Stresses, file
      
      if (DVS.GE.1.0 .AND. pan_exertion.EQ.0) then !correct for poor panicle exsertion and abortion has not taken place, then force iit now
          FNAB = 1.
          NSPJUV = spikelet_num
          rate_spikelet_num = (FNAB * NSPJUV) - spikelet_num
          pan_exertion = 1
      end if
    

!----- NEW: Calculate number of fertilised grains when DVS > 0.95 and not at DVS = 1.2 as in SUBGRN.f90
! Note this is consistent with GGR = GSO for DVS > 0.95
      spikelet_num = spikelet_num + rate_spikelet_num
      IF (DVS.GE.0.95) THEN
         if (isGrainfill.eq.0) isGrainfill = 1
         SPFERT = min(SFCOLD,SFHEAT)
         rate_grain_num = max((spikelet_num*SPFERT) - grain_num,0.)
      ELSE
         rate_grain_num   = 0.
      END IF
      grain_num = grain_num + rate_grain_num
      
! Correction for spikelet abortion and non-exeration
      
      
      return
      END SUBROUTINE GRAIN_FORMATION