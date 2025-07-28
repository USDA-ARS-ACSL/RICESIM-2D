  ! **********************************************
  ! * This subroutine reads in variety coefficients
  ! * and outputs to the plantgraphics file
  ! ************************************************
    
    subroutine Initialize
	!include 'common.h'
    !include 'plant.h'
	use common_block
    
    Character InString1*120,                                   &
	nitrogencrp*256, plantstresscrp*256, gasexchangecrp*256
    Character*256 extract_path, path
    

   Write(*,*)'************************ RICESIM *************************'  
   Write(*,*)'*                    Version 1.0.0                       *'  
   Write(*,*)'*                                                        *'  
   Write(*,*)'*  A DYNAMIC SIMULATOR FOR RICE CROPS                    *'
   Write(*,*)'*  BASED ON ORYZA (Bouman et al., 2001; Li et al., 2017) *'
   Write(*,*)'*  WITH MODIFICATIONS FOR                                *'
   Write(*,*)'*  INCORPORATED INTO 2DSOIL AND LINKED WITH              *'
   Write(*,*)'*  FARQUHAR PHOTOSYNTHESIS MODEL,                        *'
   Write(*,*)'*  IMPROVED HEAT STERILITY, AND PHENOLOGY ROUTINES AS    *'
   Write(*,*)'*      PER Li et al (2021;2022).                         *'
   Write(*,*)'*  BY DAVE FLEISHER; SANAI LI; SERHAN YESILKOY           *'
   Write(*,*)'*  USDA-ARS,ADAPTIVE CROPPING SYSTEMS LABORATORY         *'  
   Write(*,*)'*  BELTSVILLE, MD  20705.   TEL:(301)504-5872            *'   
   Write(*,*)'**********************************************************'
   
  ! Read Variety File
   Open(40,file = VarietyFile, status = 'old', ERR=13)
	im = 1
	il = 0
	Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*,ERR=13) NAME
    im = im + 1
    il = il + 1
	Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1    
    Read(40,*, ERR=13) card_TBD, card_TBLV, card_TMD, card_TOD, &
        card_TODNGHT, card_TSEN, card_TSENNGHT, card_TSENPSP, &
        card_TSENSPNGHT
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1    
    Read(40,*, ERR=13) dev_DVRJ, dev_DVRI, dev_DVRP, dev_DVRR
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) env_MOPP, env_PPSE
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) sed_SHCKD, sed_SHCKL
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) env_COLDMIN, env_COLDEAD
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) grw_RGRLMX, grw_RGRLMN, grw_LRSTR
    im = im + 1
    il = il + 1
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1     
    Read(40,*, ERR=13) gas_VCMAX, gas_JMAX, gas_TPU, gas_g0, gas_g1
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1     
    Read(40,*, ERR=13) gas_MAINLV, gas_MAINST, gas_MAINSO, gas_MAINRT, &
        gas_TREF, gas_Q10
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1     
    Read(40,*, ERR=13) grw_CRGLV, grw_CRGST, grw_CRGSO, grw_CRGRT, &
        grw_CRGSTR
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1   
    Read(40,*, ERR=13) grw_FSTR, grw_TCLSTR, grw_SPGF, grw_NSPM2X, &
        grw_NSJA, grw_WRGMX, grw_GFRCP, grw_WGREC
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1   
    Read(40,*, ERR=13) cmf_FCLV, cmf_FCST, cmf_FCSO, cmf_FCRT, cmf_FCSTR
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1   
    Read(40,*, ERR=13) grw_GZRT, grw_ZRTMCW, grw_ZRTMCD
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1   
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) drt_ULLS, drt_LLS, drt_ULDL, drt_LLDL
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1   
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) drt_ULLE, drt_LLLE, drt_ULRT, drt_LLRT
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1   
    Read(40,*, ERR=13)
    im = im + 1
    il = il + 1  
    Read(40,*, ERR=13) nit_NMAXUP, nit_RFNLV, nit_FNTRT, nit_RFNST, &
        nit_TCNTRF, nit_NFLVI, nit_FNLVI, nit_NMAXSO
    close(40)
    
    
 ! Summarize to user options - portions of this code section from ORYZA
  IF (PRODENV.EQ.0) THEN
       WRITE(*,*) 'Non-limiting water rice production'
  ELSE IF (PRODENV.EQ.1) THEN
!      Select water balance model
       IF (WATBAL.EQ.0) THEN
            WRITE(*,*)'Water balance PADDY used'
       ELSE IF (WATBAL.EQ.1) THEN
            WRITE(*,*) 'Water balance SAWAH used'
       ELSE IF (WATBAL.EQ.2) THEN
            WRITE(*,*) 'Water balance SAHEL used'
       ELSE IF (WATBAL.EQ.3) THEN
            WRITE(*,*) 'Water balance LOWBAL used'
       ELSE IF (WATBAL.EQ.4) THEN
            WRITE(*,*) 'Water balance SOILPF used'
       ELSE
            WRITE(*,*) 'ERROR:unknown name for soil water balance'
       END IF
    ELSE
        WRITE(*,*) 'ERROR: unknown name for production situation'
    END IF
!--------Choose and check info on nitrogen production situation setting
    IF (NITROENV.EQ.0) THEN
        WRITE (*,*) 'Non-limiting N production'
    ELSE IF (NITROENV.EQ.1) THEN
        WRITE (*,*) 'Nitrogen balance used'
    ELSE
        WRITE(*,*) 'ERROR:unknown name for nitrogen balance'
    END IF    
!--------Choose and check establishment setting
    IF (ESTAB.EQ.0) THEN
        WRITE(*,*) 'Rice crop is transplanted'
    ELSE IF (ESTAB.EQ.1) THEN
        WRITE(*,*) 'Rice crop is direct-seeded'
        !BB: set SBDUR to 0. if direct seeded:
        SBDUR = 0.
    ELSE
        WRITE(*,*) 'ERROR:unknown name for establishment'
    END IF
    
    
    
 ! Open and initialize output files
   Open(75,file = PlantGraphics)
   Open(76,file = LeafGraphics)
  
   Path = extract_path(PlantGraphics)
   nitrogencrp = trim(Path)//'nitrogen.crp'
   plantstresscrp = trim(Path)//'plantstress.crp'
   gasexchangecrp = trim(Path)//'gasexchange.crp'
   
   Open(77,file = nitrogencrp)
   Open(78,file = plantstresscrp)
   Open(79,file = gasexchangecrp)
   
! To Do, work on output variables
   Write(75,5) "date", "das", "hour","stage","DVS","Tair","Tcan",&
       "SRAD","LAI","Area","Pgross","Rtotal","potTrans","actTrans",&
       "leafN","stemN","rootN","storeN","Nplant","cumN", &
       "tot_dw","grnlf_dw","dead_dw","stem_dw","stemres_dw",&
       "root_dw","stor_dw","grain_dw","husk_dw"
   
   

   
   
   Write(76,6) "date", "das", "time"
   Write(77,7) "date", "das", "time"
   Write(78,8) "date", "das", "time"
   write(79,9) "date", "das", "hour", "SRAD", "PPFsun", "PPFshade", "LAIsun", "LAIshade","Tcan", "Pgtot", "Pgsun", "Pgshade",&
       "Transp", "Gs", "LWPhour"
   
  
5  FORMAT (A,8(",",A6), (",",A8), 19(",",A6))
6  FORMAT (A10, 26(",",A10))
7  FORMAT (A10, 13(",",A10))
8  FORMAT (A10, 7(",",A10))   
9  format (A,2(",",A6),12(",",A8))
    
 ! Body of plant

   RETURN
13 Write(*,*) 'Error in VARIETY FILE'
   GOTO 11
11 CONTINUE
    end subroutine Initialize



    
     function extract_path(filename)
       character *256 filename, path, extract_path
       integer :: i, len

       len = len_trim(filename)
       path = ""
    ! Find the last occurrence of the directory separator '\'
       
       do i = len, 1, -1
          if ((filename(i:i) == '\').OR.(filename(i:i) == '/')) then
              path = filename(1:i)
              if (filename(i:i) == '/') then   ! if windows
                  path=path // '/'
               else 
                 path=path // '\'               ! if linux
              end if
             exit
          end if
       end do

       extract_path = path
       end function extract_path
       