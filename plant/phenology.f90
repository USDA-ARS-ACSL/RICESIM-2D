!*************************************************
!* Phenology Subroutine
!* Calculate developmental progress
!* Routines run on 24h basis, may need to modify later

    
SUBROUTINE Phenology
!*   BELOW IS FROM ORYZA
!*  Combines Subroutine SUBDDN or SUBDDB and Phenol2
!* plus some original tweaks and changes
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! DVS     R4  Development stage of the crop (-)                     I  *
! dev_DVRJ    R4  Development rate juvenile ((oCd)-1)                   I  *
! dev_DVRI    R4  Development rate, photoperiod-sensitive phase         I  *
!             ((oCd)-1)                                                *
! dev_DVRP    R4  Development rate, PI phase ((oCd)-1)                  I  *
! dev_DVRR    R4  Development rate, reproductive phase ((oCd)-1)        I  *
! HU      R4  Heat units (oCd d-1)                                  I  *
! DAYL    R4  Astronomic daylength (base = 0 degrees) (h)           I  *
! env_MOPP    R4  Maximum optimum photoperiod (h)                       I  *
! env_PPSE    R4  Photoperiod sensitivity (h-1)                         I  *
! TSeed      R4  Temperature sum (oCd)                                 I  *
! sed_SHCKD   R4  Delay parameter in phenology ((oCd)(oCd)-1)           I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! DVR     R4  Development rate of the crop (d-1)                    O  *
! sed_TSHCKD  R4  Transpl. shock for phenol. development (oCd)          O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
!*************************
use common_block
REAL DL, TSTR, PPFAC

!Calculate daily developmental rate based on developmental stage and other facts
!   only run at end of each 24-hour period 
if(iTime.eq.24) then 
    !1 Get heat units towards development
    !CALL Temperature_SUBDDN !original ORYZA
    CALL Temperature_SUBDDB !beta function ORYZA
    !2 Determine developmental rate
    if(CROPSTA.eq.1) TSTR = 0.
    if(DVS.ge.0..AND.DVS.lt.0.40) DVR = dev_DVRJ * HU !basic vegetative phase
    if(DVS.ge.0.40.AND.DVS.lt.0.65) then !to PI start, includes photoperiod sensitivity
        DL = daylng + 0.9
        if(DL.lt.env_MOPP) then
            PPFAC = 1.
        else
            PPFAC = 1.-(DL-env_MOPP)*env_PPSE
        endif
        PPFAC = min(1.,max(0.,PPFAC))
        DVR = dev_DVRI*HU*PPFAC
    endif
    IF (DVS.GE.0.65.AND.DVS.LT.1.00) DVR = dev_DVRP*HU  ! PI through 1st flowering
    !IF (DVS.GE.1.00)    DVR = dev_DVRR !1st flowering to physical maturity
    !pvo20110110: in several cvs I found no relation of duration flowering to maturity
!so what we do now here is calculate DVRR as 1/DURFM (in the crop file), with DURFM 
!the average duration from flowering to maturity
    IF (DVS.GE.1.00) DVR = dev_DVRR*HU    
    
    IF (CROPSTA .EQ. 3) TSTR = TSeed ! estimate transplant shock delay on phenology
    sed_TSHCKD = sed_SHCKD*TSTR !delay in degree days based on variety and thermal time TSTR
    
    IF (CROPSTA .GT. 3 .AND.TSeed.LT.(TSTR+sed_TSHCKD)) then
        DVR = 0. ! no rate for transplant shock duration
        PLTR = NPLH * NHH / NPLSB !plant density at transplatning based on # hills (NHH) * # plants/hill (NPLH an divided by the plants m-2 of direct seed)
    else
        PLTR = 1.
    end if
       
    If (DVS.GT.0.AND.DVS.LT.0.65) DURPI = DURPI + 1 !# of days between emergence to panicl initation
    If (DVS.GE.0.65.AND.DVS.LT.1.) DURFL = DURFL + 1 !# of days between PI and first flowering
    If (DVS.GE.1.0.AND.DVS.LT.1.4) DUREF = DUREF + 1 !# of days between first flowering to start of  grainfill
    If (DVS.GE.1.4.AND.DVS.GE.2.0) DURFM = DURFM +1 !# of days between grainfill to maturity
    
    !4 integrate certain state variables depending on cropstage
    if(CROPSTA.GE.1) then
        TSeed = TSeed + HU
        DVS = DVS + DVR
    endif
    
    if(CROPSTA.EQ.3.AND.DVS > 0.1) then
        If (IDAS.GT.transplantDay+SBUR) CROPSTA = 4!force main crop production after SBUR time
    end if
    

    !5 Check for developmental switch to next DVS stage    
    if(DVS.LT.0.4) then 
        Devstage_name = "Basic Vegetative"
    else if(DVS.GE.0.4.and.DVS.LT.0.65) then
        Devstage_name = "Phoperiod-sensitive Vegetative"     
    else if(DVS.GE.0.65.and.DVS.LT.1.) then
        Devstage_name = "Panicle formation"
        if (DVS.GE.0.95) isGrainfill = 1
    else if(DVS.GE.1.) then
        Devstage_name = "Grainfill"
    else if(DVS.GE.2.) then
        isMatured = 1
        Devstage_name = "Maturity"
    endif
    
    
endif
END SUBROUTINE Phenology



    
