========================================================================
    Fortran Dll : "plant" Project Overview
========================================================================

Variables

Simulation inputs
PRODENV = 0 (unlimited W), 1 (limited W)
WATBAL = 1,2,3,4 when PRODENV = 1 (1-paddy, 2-SAHEL, 3-SAWAH, 4-LOWBAL, 5-SOILPF)
NITROENV = 0 (unlimited N), 1 (limited N)
ESTAB = 0 (transplanted rice), 1 (direct seed)
SBUR = # (# of days between emergence and transplanting when ESTAB = 0)

Development
CROPSTA - crop stage, 0-before sowing, 1-day of sowing, 2-in seedbed, 3-day of transplanting, 4-maingrowth period
DAS - days after sowing

DVR - developmental rate (rate Cd-1 where, Cd-1 = degree days)
dev_DVRI - development rate, photoperiod-sensitive phase 
dev_DVRJ - juvenile developmental rate (")
DVS - developmental stage (0 - emergence, 1 - flowering, 2 - maturity) 
HU - heat units for phenological development
IDAS - integer day after sowing
ISA - seedling age in days
TSTR - age of transplanted seedlings in degree days
