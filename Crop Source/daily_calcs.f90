!*************************************************
!Routine calculates daily weather values from hourly data
!Mostly needed for phenology routine
!Provides 24 hour averaged Tair, PAR, Tmax/Tmin data
!*************************************************
 subroutine Daily_Ave
 !include 'common.h'
 !include 'plant.h'
 use common_block


Real oldSRAD_dailysum, sum

sum = 0.

if (iTime.eq.1) then !first hour of day
	T_dailyave = TAIR(iTime)
    T_dailymax = TAIR(iTime)
    T_dailymin = TAIR(iTime)
    T_canopydailyave = 0.
    PAR_dailysum = 0.
    SRAD_dailysum = 0.
    time_sunriseold = time_sunrise
    time_sunsetold = time_sunset
    time_sunrise = 1
    time_sunset = 1
endif

if (iTime.gt.1) then !from 1 am to end of 24 hour period
    if(TAIR(iTime).gt.T_dailymax) T_dailymax = TAIR(iTime)
    if(TAIR(iTime).lt.T_dailymin) T_dailymin = TAIR(iTime)
    T_dailyave = T_dailyave + TAIR(iTime)
    T_canopydailyave = T_canopydailyave + T_canopy(iTime) !circle back when photo routine working
    oldSRAD_dailysum = SRAD_dailysum
    PAR_dailysum = PAR_dailysum + 0.5*WATTSM(iTime) !check conversion
    SRAD_dailysum = SRAD_dailysum + WATTSM(iTime)
    if((time_sunrise.eq.1).and.(int(oldSRAD_dailysum).eq.0).and.(SRAD_dailysum.gt.0.)) then
        time_sunrise = iTime
    endif
    if((WATTSM(iTime-1).gt.0.).and.(int(WATTSM(iTime)).eq.0)) time_sunset = iTime
endif
    
if (iTime.eq.24) then !at end of 24 hour period, compute averages / convert sum units
    T_dailyave = T_dailyave/(24.*60./timeStep)
    T_canopydailyave = T_canopydailyave/(24.*60./timeStep)
    PAR_dailysum = PAR_dailysum * (60.*timeStep)/1000000. !mol m-2 timestep-1
    SRAD_dailysum = SRAD_dailysum * (60.*timeStep) / 4.57 *2. / 1000000. !MJ srad m-2 timeinc-1
    if (T_counter7.GE.7) T_counter7 = 0 !restart counter for 7 day moving average as needed
    T_counter7 = T_counter7 + 1
    T_dailymin7(T_counter7) = T_dailymin
    
    do i = 1,7
        sum = sum + T_dailymin7(T_counter7)
    end do
    T_dailymin7ave = sum/7.

    !write(*,*) T_dailymax, T_dailymin, T_dailyave, SRAD_dailysum, time_sunrise, time_sunset
    
endif
 
   
end subroutine Daily_Ave