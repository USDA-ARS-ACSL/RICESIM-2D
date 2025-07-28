
 subroutine First_Init
 !*************************************************
!Initialize starting values regarding plant status at first run
!*************************************************
 !include 'common.h'
 !include 'plant.h'
 use common_block
     
 !convert units from variety file
 !grw_RGRLMX = grw_RGRLMX / 24. !1/(C-d) to 1/(C-h) - keep these on daily basis as in variety file, since we are using hourly increments it is already adjusted for in the organ_growth routines
 !grw_RGRLMN = grw_RGRLMN / 24. !"
 !grw_TCLSTR = grw_TCLSTR / 24. !time coefficient for loss of stem reserves from d-1 to h-1
 
 gas_MAINLV = gas_MAINLV  /24. !kg ch2o kg-1 ch2o d-1 to kg / kg h-1
 gas_MAINST = gas_MAINST / 24.
 gas_MAINSO = gas_MAINSO / 24.
 gas_MAINRT = gas_MAINRT / 24.
 grw_SPGF = grw_SPGF / 1000.  !# per kg-1 to # per g-1 of current assimilate supply
 !grw_NSPM2X = grw_NSPM2X / 1000. ! total number from # per m-2 to # per plant-1, leave as is for now, converts to # per plant in grain formation routine
 grw_WRGMX = grw_WRGMX * 1000. !kg grain-1 to g grain-1
 grw_WGREC = grw_WGREC / 1000. !kg grain-1 to g grain-1
 
 !Initialize plant variables
 !developmental states
 CROPSTA = 0 !before sowing
 DVR = 0.
 DVS = 0.
 DVSI = 0. 
 HU = 0.
 TSeed = 0.
 Devstage_name = ""
 DURPI = 0.
 DURFL = 0.
 DUREF = 0.
 DURFM = 0.
 TMPIF = 0.
 TMEFL = 0.
 TMFLM = 0.
 PLTR = 1.
 pan_exertion = 0
 
 !growth states
 totleaf_dw = 0.01
 grnleaf_dw = totleaf_dw
 deadleaf_dw = 0.
 stem_dw = 0.01
 stemreserve_dw = 0.
 storage_dw = 0.
 panicle_dw = 0.
 grain_dw = 0.
 root_dw = 0.01
 roughrice_dw = 0.
 total_grn_dw = grnleaf_dw + stem_dw + root_dw + storage_dw + &
     stemreserve_dw
 leaf_area = 0.
 LAI = 0.
 SAI = 0.
 GCR = 0. ! carbon available for growth, g CHO plant-1 h-1
 grain_num = 0.
 spikelet_num = 0.
 f_totDM_shoot = 1 - grw_FSTR
 f_stemdm_reserve = grw_FSTR
 shootPart_old = 0.
 shootPart = 0.
 rootPart_old = 0.
 rootPart = 0.
 rootPool = 0.
 initialrootcarbo = 0.
 
 !stresses and senescence
 sen_LSTR = 0.
 sen_LLV = 0.
 sen_LLVSH = 0.
 sen_DLDR = 0.
 !LSTR = 0.
 lf_wat_rf = 1.
 NCOLD = 0.
 SFCOLD = 1.
 SFHEAT = 1.
 COLDTT = 0.
 HEATTT = 0.
 SF3CNT = 0.
 SF3SUM = 0.
 
 !respiration
 Rg_leaf =0.
 Rg_stem =0.
 Rg_root =0.
 Rg_storage = 0.
 Rg_stemreserve =0.
 Rg_total = 0.
 R_tot = 0.
 
 Rm_leaf =0.
 Rm_stem =0.
 Rm_root =0.
 Rm_storage =0.
 Rm_grain = 0.
 Rm_total = 0.

 !stresses
 sen_NSLLV = 1. !effect of N stress on leaf senescence rate
 sen_LLV = 0.   !leaf senescence rate, g leaf plant-1 h-1
 
 isSown = 0
 isGerminated = 0
 isEmerged = 0
 isTransplanted = 0
 isGrainfill = 0
 isMatured = 0
 
 !stress variables
 WATLIMIT = Prodenv !0 for no water stress, 1 means water limiting
 !WATLIMIT = 0
 LESTRS = 1
 NRATIO = 1
 WGCOR = 1
 lf_N_sen = 1. ! N affect on leaf senescence
 lf_N_gro = 1. ! N affect on leaf relative growth rate
 
 !Initialize daily weather variables
 T_dailyave = 0.
 T_dailymax = 0.
 T_dailymin = 0.
 T_dailymin7 = 0.
 T_canopydailyave = 0.
 PAR_dailysum = 0.
 SRAD_dailysum = 0.
 time_sunset = 1
 time_sunrise = 1
 T_counter7 = 0
 T_dailymin7ave = 0.
 
 !******************************************
 !Parameters that may need to go into variety file
 LAI_SHADE = 4.0 ! critical LAI above which shading senescence occurs, from SUCROS
 
 !Tables of DVS values - rather than read from variety file, I've hard coded these for now from Sanai's wells.crp file
 !Using ORYZA LINTUL2 function which linearly interpolates between xy coordinates for a give response
 !Values for a given response are entered as 1D array with alternative x,y,x2,y2...xn,yn pairs
 
 !**DRLVT - leaf death coefficent (d-1) to (h-1) as a function of development, DVS
 !originally these came from Wells, decline seems too severe, adjusted in
 sen_DRLVT(1)= 0.00
 sen_DRLVT(2)= 0.00/24.
 sen_DRLVT(3)= 0.60
 sen_DRLVT(4)= 0.00/24.
 sen_DRLVT(5)= 1.00
 sen_DRLVT(6) = 0.005/24.
 !sen_DRLVT(6)= 0.015/24.
 !sen_DRLVT(6) = 0.00015/24.
 sen_DRLVT(7)= 1.60
 sen_DRLVT(6) = 0.005/24.
 !sen_DRLVT(8)= 0.025/24. 
 !sen_DRLVT(8) = 0.0015/24.
 sen_DRLVT(9)= 2.10
 sen_DRLVT(10) = 0.005/24.
 !sen_DRLVT(10)= 0.050/24.
 !sen_DRLVT(10) = 0.0025/24.
 sen_DRLVT(11)= 2.50
 sen_DRLVT(12)= 0.050/24.
 
 !**FSHTB - fraction of totaldm to shoot based on DVS
 f_totdm_shootDVS(1) = 0.00
 f_totdm_shootDVS(2) = 0.75
 f_totdm_shootDVS(3) = 0.43
 f_totdm_shootDVS(4) = 0.85
 f_totdm_shootDVS(5) = 1.00
 f_totdm_shootDVS(6) = 1.00
 f_totdm_shootDVS(7) = 2.50
 f_totdm_shootDVS(8) = 1.00
 
 !** FLVTB - fraction of shoot dry matter to leaves based on DVS
 f_shtdm_leafDVS(1) = 0.00
 f_shtdm_leafDVS(2) = 0.60
 f_shtdm_leafDVS(3) = 0.50
 f_shtdm_leafDVS(4) = 0.60
 f_shtdm_leafDVS(5) = 0.75
 f_shtdm_leafDVS(6) = 0.40
 f_shtdm_leafDVS(7) = 1.00
 f_shtdm_leafDVS(8) = 0.00
 f_shtdm_leafDVS(9) = 1.20
 f_shtdm_leafDVS(10) = 0.00
 f_shtdm_leafDVS(11) = 2.50
 f_shtdm_leafDVS(12) = 0.00
            
 !** FSTTB -fraction of shoot dry matter to stems "
 f_shtdm_stemDVS(1) = 0.00
 f_shtdm_stemDVS(2) = 0.40 
 f_shtdm_stemDVS(3) = 0.50
 f_shtdm_stemDVS(4) = 0.40 
 f_shtdm_stemDVS(5) = 0.75
 f_shtdm_stemDVS(6) = 0.60
 f_shtdm_stemDVS(7) = 1.00
 f_shtdm_stemDVS(8) = 0.10
 f_shtdm_stemDVS(9) = 1.20
 f_shtdm_stemDVS(10) = 0.00
 f_shtdm_stemDVS(11) = 2.50
 f_shtdm_stemDVS(12) = 0.00
 
 !** FSOTB - fraction of shoot dry matter to panicles
 f_shtdm_storeDVS(1) = 0.00
 f_shtdm_storeDVS(2)  = 0.00
 f_shtdm_storeDVS(3) = 0.5
 f_shtdm_storeDVS(4)  = 0.00
 f_shtdm_storeDVS(5) = 0.75 
 f_shtdm_storeDVS(6)  =0.00
 f_shtdm_storeDVS(7) = 1.00
 f_shtdm_storeDVS(8)  = 0.90
 f_shtdm_storeDVS(9) = 1.20
 f_shtdm_storeDVS(10)= 1.00
 f_shtdm_storeDVS(11) = 2.50
 f_shtdm_storeDVS(12)= 1.00
 
!** Specific stem area to leaf area (cm2 area g-1 dw stem)
 !revisit, should really be decay with increasing shoot mass
 t_ssga(1) = 0.00
 t_ssga(2) = 0.000006*10000.
 t_ssga(3) = 0.45
 t_ssga(4) = 0.000003*10000.
 t_ssga(5) = 0.9
 t_ssga(6) = 0.000001*10000.
 t_ssga(7) = 2.1
 t_ssga(8) = 0.000 * 10000.
 t_ssga(9) = 2.5
 t_ssga(10) = 0.000 * 10000.
 
 
!** VCMAX as a function of DVS
 !here I scale the VCMAX variety file values using Sanai's numbers for Wells cutlivar
 t_vcmax(1) = 0.00
 t_vcmax(2) = gas_VCMAX
 t_vcmax(3) = 0.63
 t_vcmax(4) = 104./132.*gas_VCMAX
 t_vcmax(5) = 1.09
 t_vcmax(6) = 88./132.*gas_VCMAX
 t_vcmax(7) = 1.74
 t_vcmax(8) = 57./132.*gas_VCMAX
 t_vcmax(9) = 2.00
 t_vcmax(10) = 47./132.*gas_VCMAX
 t_vcmax(11) = 2.50
 t_vcmax(12) = 47./132.*gas_VCMAX
 
!** JMAX as a function of DVS
 !here I scale the JMAX vareity file using Sanai's numbers for Wells cultivar
 t_jmax(1) = 0.00
 t_jmax(2) = 52./146.*gas_JMAX
 t_jmax(3) = 0.63
 t_jmax(4) = 134./146.*gas_JMAX
 t_jmax(5) = 0.75
 t_jmax(6) = 141./146.*gas_JMAX
 t_jmax(7) = 0.96
 t_jmax(8) = 146./146.*gas_JMAX
 t_jmax(9) = 1.09
 t_jmax(10) = 144./146.*gas_JMAX
 t_jmax(11) = 1.22
 t_jmax(12) = 139./146.*gas_JMAX
 t_jmax(13) = 1.5
 t_jmax(14) = 116./146.*gas_JMAX
 t_jmax(15) = 1.74
 t_jmax(16) = 80./146.*gas_JMAX 
 t_jmax(17) = 2.0
 t_jmax(18) = 50./146.*gas_JMAX 
 t_jmax(19) = 2.5
 t_jmax(20) = 50./146.*gas_JMAX 
 
!** TPU as a function of DVS
 t_tpu(1) = 0.0
 !t_tpu(2) = 0.25
 t_tpu(2) = 9.6
 t_tpu(3) = 0.63
 !t_tpu(4) = 8.05
 t_tpu(4) = 9.6
 t_tpu(5) = 1.09
 t_tpu(6) = 9.6
 t_tpu(7) = 1.74
 t_tpu(8) = 5.2
 t_tpu(9) = 2.0
 t_tpu(10) = 3.37
 t_tpu(11) = 2.50
 t_tpu(12) = 3.37
 
!** g0 as a function of DVS
 t_g0(1) = 0.00
 t_g0(2) =  0.1
 t_g0(3) = 0.63
 t_g0(4) = 0.1
 t_g0(5) = 1.09
 t_g0(6) = 0.1
 t_g0(7) = 1.74
 t_g0(8) = 0.01
 t_g0(9) = 2.00
 t_g0(10) = 0.01
 t_g0(11) = 2.50
 t_g0(12) = 0.01
 
!** g1 as a function of DVS
 t_g1(1) = 0.00
 t_g1(2) = 8.2
 t_g1(3) = 0.63
 t_g1(4) = 8.2
 t_g1(5) = 1.09
 t_g1(6) = 4.2
 t_g1(7) = 1.74
 t_g1(8) = 11.6
 t_g1(9) = 2.00
 t_g1(10) = 11.6
 t_g1(11) = 2.50
 t_g1(12) = 11.6

!SLAfunction...
t_SLA(1) = 0.00
t_SLA(2) = 0.0045
t_SLA(3) = 0.16
t_SLA(4) = 0.0045
t_SLA(5) = 0.33
t_SLA(6) = 0.0033
t_SLA(7) = 0.65
t_SLA(8) = 0.0028
t_SLA(9) = 0.79
t_SLA(10) = 0.0024
t_SLA(11) = 2.10
t_SLA(12) = 0.0023
t_SLA(13) = 2.5
t_SLA(14) = 0.0023

!** N parameters
!min N concentration in storage organs (kg N kg-1) as a function of the amount of N in the crop until flowering (kg N ha-1)
t_NMINSO(1) = 0.0
t_NMINSO(2) = 0.006
t_NMINSO(3) = 50.0
t_NMINSO(4) = 0.0008
t_NMINSO(5) = 150.0
t_NMINSO(6) = 0.0125
t_NMINSO(7) = 250.0
t_NMINSO(8) = 0.015
t_NMINSO(9) = 400.0
t_NMINSO(10) = 0.017
t_NMINSO(11) = 1000.0
t_NMINSO(12) = 0.017

!fraction of N in leaves on a leaf area basis (g N m-2 leaf)
t_NFLV(1) = 0.0
t_NFLV(2) = 0.54
t_NFLV(3) = 0.16
t_NFLV(4) = 0.54
t_NFLV(5) = 0.33
t_NFLV(6) = 1.53
t_NFLV(7) = 0.65
t_NFLV(8) = 1.22
t_NFLV(9) = 0.79
t_NFLV(10) = 1.56
t_NFLV(11) = 1.00
t_NFLV(12) = 1.29
t_NFLV(13) = 1.46
t_NFLV(14) = 1.37
t_NFLV(15) = 2.02
t_NFLV(16) = 0.83
t_NFLV(17) = 2.50
t_NFLV(18) = 0.83



!Table of maximum leaf N fraction on weight basis (kg N kg-1 leaves; Y value)
! as a function of development stage (-; X value):
t_NMAXL(1) = 0.0
t_NMAXL(2) = 0.053
t_NMAXL(3) = 0.4
t_NMAXL(4) = 0.053
t_NMAXL(5) = 0.75
t_NMAXL(6) = 0.04
t_NMAXL(7) = 1.0
t_NMAXL(8) = 0.028
t_NMAXL(9) = 2.0
t_NMAXL(10) = 0.022
t_NMAXL(11) = 2.5
t_NMAXL(12) = 0.015

! Table of minimum leaf N fraction on weight basis (kg N kg-1 leaves; Y value)
! as a function of development stage (-; X value):
t_NMINL(1) = 0.0
t_NMINL(2) = 0.025
t_NMINL(3) = 1.0
t_NMINL(4) = 0.012
t_NMINL(5) = 2.1
t_NMINL(6) = 0.007
t_NMINL(7) = 2.5
t_NMINL(8) = 0.007

!--- Table of effect of N stress on leaf death rate (-; Y value)
! as a function of N stress level (NSTRES) (-; X value):
t_NSLLV(1) = 0.0
t_NSLLV(2) = 1.0
t_NSLLV(3) = 1.1
t_NSLLV(4) = 1.0
t_NSLLV(5) = 1.5
t_NSLLV(6) = 1.4
t_NSLLV(7) = 2.0
t_NSLLV(8) = 1.5
t_NSLLV(9) = 2.5
t_NSLLV(10) = 1.5


    end subroutine
    
subroutine Emerge_Init
!*************************************************
!Initialize emergence starting values - fordirect seeded plants
!Occurs when day of year equals emergence date
!Initial values are determined in the *.ini file
!*************************************************
    use common_block
    !emergeDay = iDAS
    grw_SLA_nominal = 250 !eventually need to read in from variety file, fixed for now but could be modified as function of age
    !DLDR = 0 !no death rate of green leaves to start   
    
    if (CROPSTA.EQ.1) Then ! plant was direct sown, now emerged
        isEmerged = 1
        leaf_area = LAPE
        LAI = LAPE * NHH*NPLH ! 1 direct seed, # plants per m2, technically this is the NPLDS value but I'm using # hills and plants per hill to be pseudo link w. PopRow and ROWSP
        DVS = DVSI !far as I can tell, this will always be zero, would make more sense maybe to adjust
        !These values are read from the variety file
        root_depth = ZRTI !root depth at emergence for direct seeded
        grnleaf_dw = WLVGI
        stem_dw = WSTI
        root_dw = WRTI
        storage_dw = WSOI
        deadleaf_dw = 0.
        roughrice_dw = 0. 
        husk_dw = 0.
        stemreserve_dw = 0.
        
        !Above were read from variety file
        totleaf_dw = grnleaf_dw + deadleaf_dw
        total_grn_dw = grnleaf_dw + stem_dw + root_dw + storage_dw + stemreserve_dw 
        leaf_N = nit_FNLVI ! g N / g-1 leaf read from variety file
        stem_N = 0.5 * leaf_N
        root_N = 0.5 * leaf_N
        grain_N = 0.
        storage_N = 0.
        dead_N = 0.
        shoot_preN = leaf_N + stem_N 
        storage_preN = 0.
        leaf_Npool = leaf_N * grnleaf_dw ! g N in leaves / plant-1
        stem_Npool = stem_N * stem_dw
        root_Npool = root_N * root_dw
        grain_Npool = grain_N * grain_DW
        storage_Npool = storage_N * storage_dw
        dead_Npool = dead_N * deadleaf_dw
        
        totalplantN = leaf_Npool + stem_Npool + root_Npool 
    end if
    

   

    

    
end subroutine Emerge_Init
    
    
    

subroutine Transplant_Init
!*************************************************
!Initialize transplant starting values - for transplanted seeded plants
!Occurs when day of year equals transplant date
!Initial values are determined in the *.ini file
!*************************************************
    use common_block
    if (CROPSTA.EQ.3) Then ! plant was transplanted assumed at emergence
        isTransplanted = 1
        LAI = LAPE * NPLSB ! 0 transplant, # plants in seedbed
        root_depth = ZRTTR !root depth at transplanting
        DVS = DVIS !(0 - emergence, 1 - flowering, 2 - maturity) !should this be slighlty adjusted since plants are a bit ahead of emergence?
            isEmerged = 1
        leaf_area = LAPE
        LAI = LAPE * NHH*NPLH ! 1 direct seed, # plants per m2, technically this is the NPLDS value but I'm using # hills and plants per hill to be pseudo link w. PopRow and ROWSP
        DVS = DVSI !far as I can tell, this will always be zero, would make more sense maybe to adjust
        
        !These values are read from the variety file
        root_depth = ZRTI !root depth at emergence for direct seeded
        grnleaf_dw = WLVGI
        stem_dw = WSTI
        root_dw = WRTI
        storage_dw = WSOI
        deadleaf_dw = 0.
        roughrice_dw = 0.
        husk_dw = 0.
        stemreserve_dw = 0.
        !Above were read from variety file
        
        totleaf_dw = grnleaf_dw + deadleaf_dw
        total_grn_dw = grnleaf_dw + stem_dw + root_dw + storage_dw + stemreserve_dw 

    
    end if
    

end subroutine Transplant_Init