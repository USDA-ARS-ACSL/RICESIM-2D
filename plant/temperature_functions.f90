
    
SUBROUTINE Temperature_SUBDD
! ported from ORYZA for effective T for growth development
! removed guestimation of 24h T values and replaced with hourly values
! this may cause some deviation from thermal time response (heat units)
! This routine gets called each hour from potential_growth, thus I removed the 24 hour loop here
!  This may again cause some deviation since we are accumulating resopnse now each hour instead of 24 hour bases

!----------------------------------------------------------------------*
!  SUBROUTINE SUBDD                                                    *
!  Purpose: This subroutine calculates the daily amount of heat units  *
!           for calculation of the phenological development rate and   *
!           early leaf area growth.                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! TMIN    R4  Daily minimum temperature (oC)                        I  *
! TBD     R4  Base temperature for development (oC)                 I  *
! TOD     R4  Optimum temperature for development (oC)              I  *
! TMD     R4  Maximum temperature for development (oC)              I  *
! HU      R4  Heat units (oCd d-1)                                  O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
    
      use common_block
!-----Local parameters
      REAL    TD, TM, TT, X1, X2, Thour
      INTEGER I,SUNSETH
!      SAVE
      TM = (T_dailymax+T_dailymin)/2.
      TT = 0.
	  X1= (card_TOD-card_TBD)/(card_TMD-card_TOD)		!TAOLI, APRIL 1,2009
	  X2=0.0						!TAOLI, APRIL 1,2009
      
      Thour = TAIR(iTime)
      !Thour = TM !temporarily check if this solves the DVS better
      !here I am mixing and matching hourly air T with daily cardinal Temps, I may have to rethink this pending model testing
      if ((Thour.GT.card_TBD).AND.(Thour.LT.card_TMD)) then
			X2=(card_TMD-Thour)/(card_TMD-card_TOD)*((Thour-card_TBD)/(card_TOD-card_TBD))**X1 !TAOLI, APRIL 1,2009
!           IF (Thour.GT.card_TOD) Thour = card_TOD-(Thour-card_TOD)*(card_TOD-card_TBD)/(card_TMD-card_TOD) !TAOLI, APRIL 1,2009, COMMENT OUT
            !TT = TT+(Thour-card_TBD)
			 TT=TT+(Thour-card_TBD)*X2		!TAOLI, APRIL 1,2009
      end if

      HU = TT/24.
 
      RETURN
    END SUBROUTINE Temperature_SUBDD
    
    
    
SUBROUTINE Temperature_SUBDDN
! ported from ORYZA for effective T for phenological development
! removed guestimation of 24h T values and replaced with hourly values
! this may cause some deviation from thermal time response (heat units)

!----------------------------------------------------------------------*
!  SUBROUTINE SUBDDN                                                  *
!  Purpose: This subroutine calculates the daily amount of heat units  *
!           for calculation of the phenological development rate and   *
!           early leaf area growth.                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! T_dailymax    R4  Daily maximum temperature (oC)                        I  *
! T_dailymin    R4  Daily minimum temperature (oC)                        I  *
! card_TBD     R4  Base temperature for development (oC)                 I  *
! card_TOD     R4  Optimum temperature for development (oC)              I  *
! card_TMD     R4  Maximum temperature for development (oC)              I  *
! HU      R4  Heat units (scaled 0 to 1)                                  O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
    use common_block

    Real TD, TM, TT, X1, X2

    TM = (T_dailymax+T_dailymin)/2.
    TT = 0.
    
    DO I = 1,24
        TD = TAIR(I) 
        !TD = TM !remove to go back to hourly
         IF ((TD.GT.card_TBD).AND.(TD.LT.card_TMD)) THEN
            IF (TD.lt.card_TOD) then
                TT = TT + (TD-card_TBD)/(card_TOD-card_TBD)
            else
                TT = TT + (card_TMD-TD)/(card_TMD-card_TOD)
            endif
         endif
         
 
      END DO
      HU = TT/24.
 
    return
    END SUBROUTINE Temperature_SUBDDN
    
  
    
    
    SUBROUTINE Temperature_SUBDDB
!  SUBROUTINE SUBDD   beta function                                                 *
!  Purpose: This subroutine calculates the daily amount of heat units  *
!           for calculation of the phenological development rate and   *
!           early leaf area growth.                                    *
!                                                                      *
! Usually called at end of day (once per 24 hours)
! In contrast with ORYZA, we use hourly air T direclty instead of estimating from sunrise hours
    
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! card_TBD     R4  Base temperature for development (oC)                 I  *
! card_TOD     R4  Optimum temperature for development (oC)              I  *
! card_TMD     R4  Maximum temperature for development (oC)              I  *
! HU      R4  Heat units (oCd d-1)                                  O  *
!                                                                      *
!  FILE usage : none                                                   *
!-


    use common_block

!Calculate daily developmental rate based on developmental stage and other facts
!   only run at end of each 24-hour period 
!   at this stage daily_calcs subroutine has already calculated end of day summaries for weather variables

!------Local parameters 
	REAL TD, TM, TT, DL , EFP,LAT
	REAL SUNRIS, SUNSET,PI
	INTEGER ID,SUNSETH
    
    TT = 0.
    
    if(iTime.eq.24) then 
        if (DVS.GT.0.65) then
            Do I = 1,24
                TD = TAIR(I) !should have full 24 hours for current day still in the array from 2DSOIL
                !TD = (T_dailymax+T_dailymin)/2. !delete after testing
                if (TD.LT.card_TBD) TD = card_TBD
                if (TD.GT.card_TMD) TD = card_TMD
                
                if ((FLOAT(I).GT.time_sunrise).AND.(FLOAT(I).LT.time_sunset)) then
                    TT = TT + (((TD-card_TBD)/(card_TOD-card_TBD))*((card_TMD-TD)/(card_TMD-card_TOD))**&
                        ((card_TMD-card_TOD)/(card_TOD-card_TBD)))**card_TSEN
                else
                    TT = TT + (((TD-card_TBD)/(card_TODNGHT-card_TBD))*((card_TMD-TD)/(card_TMD-card_TODNGHT))**&
                        ((card_TMD-card_TODNGHT)/(card_TODNGHT-card_TBD)))**card_TSENNGHT
                end if
            end do
        else
            Do I = 1,24
                TD = TAIR(I)
                ! TD = (T_dailymax+T_dailymin)/2. !delete after testing
                if (TD.LT.card_TBD) TD = card_TBD
                if (TD.GT.card_TMD) TD = card_TMD
                if ((FLOAT(I).GT.time_sunrise).AND.(FLOAT(I).LT.time_sunset)) THEN
                    TT=TT+(((TD-card_TBD)/(card_TOD-card_TBD))*((card_TMD-TD)/(card_TMD-card_TOD))** &
                        ((card_TMD-card_TOD)/(card_TOD-card_TBD)))**card_TSENPSP
                else
                    TT=TT+(((TD-card_TBD)/(card_TODNGHT-card_TBD))*((card_TMD-TD)/(card_TMD-card_TODNGHT))** &
                        ((card_TMD-card_TODNGHT)/(card_TODNGHT-card_TBD)))**card_TSENSPNGHT
                end if
            end do
        end if
    end if
    
        
    HU = TT/24.
    
    return
    end subroutine temperature_SUBDDB
    
    