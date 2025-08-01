SUBROUTINE CROP_OUTPUT
    use common_block
    
!Hourly output for now

! G01 output file   
    REAL temp
    pot_tr = transpiration*0.018*3600./(NPLH*NHH) !transpiration should be g h2o plant-1 h-1 to be consistent
    
      write(75,7) dayofyear, IDAS, iTime, CROPSTA, DVS, TAIR(iTime), &
        T_canopy(iTime), WATTSM(iTIME), LAI, leaf_area, Pg_gross, R_tot, &
        pot_tr, hourlysoilwateruptake, &
        leaf_N, stem_N, root_N, storage_N, totalplantN, CumulativeNitrogen, &      
        total_grn_dw, grnleaf_dw, deadleaf_dw, stem_dw, stemreserve_dw, &
         root_dw, storage_dw, roughrice_dw, husk_dw 
! Pg_gross is g CH2O m-2 hour-1  
! N units are g N g-1 organ and cumultaive nitrogen is g plant-1 season-1
    
      
! Temporary gas exchange file
!      write(79,9) dayofyear, IDAS, iTIME, WATTSM(iTIME), sunlitPFD, shadedPFD, sunlitLAI, shadedLAI, &
!          T_canopy(iTime), Pgtotal, Pgsun, Pgshade, &
!          Transpiration, StomConduc, lwp_hourly

      write(79,9) dayofyear, IDAS, iTIME, sunlitPFD, shadedPFD, CTAIR(24), T_canopy(iTime),lwp_hourly, lwp_predawn, &
          Pgsun, Pgshade, transpiration_sunleaf, transpiration_shadeleaf, StomConduc
      
           


      
7   FORMAT(I4,3(",",I4),5(",",F6.2),1(",",F8.2),19(",",F7.3))
9   FORMAT(I4,2(",",I4),5(",",F7.1),1(",",F6.2),6(",",F7.3))

    return
    end