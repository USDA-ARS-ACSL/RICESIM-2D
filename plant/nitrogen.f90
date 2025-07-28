subroutine nitrogen
!Should be called after carbon_allocation to ensure respiratory costs are accounted for
!Uses similar methodology as in ORYZA NCROP2.f90 routine except N uptake and allocation differs
! N demand for leaf, stem, storage organs is calculated from look up tables and DVS
!   Demand is thus based on DVS, current dry matter size of each organ and current N content
! Allocation will differ since N uptake in 2DSOIL is not necessarily driven by variations in specific crop demand for N but by fixed MM kinetics for active update and passive transpiration flows
!   In this case, for starters, I assume the following
!   When N supply is in excess, excess goes into leaves and/or storage organs
!       However in case of very high surplus, N remains in the availableNitrogen pool without being allocated.  This sort of 'floats' in the plant which is kind of weird...but I do add it to totalplant Ncontent
!   When N supply is deficient, we first take luxury excess from leaves, then stems and leaves as secondary amount, for which N goes to support storage organs
! Not sure yet how N stress is used in ORYZA except to mediate gas exchange process, that may be all we do here as well...
    
! I assume leaf N can ranage between NMINL and NMAXL*1.5 and that leaf N should be sitting at NMAXL or higher to avoid any sort of N stress on relative leaf growth
    
    
   use common_block
   
   !Local variables
   REAL LINT2 !TTUTIL selection functoin from ORYZA
   real pre_N, pdensity
   real leaf_Nmax, leaf_Nmin
   real leaf_Ndemand, stem_Ndemand, root_Ndemand, store_Ndemandmx, store_Ndemandmn !N demands for leaf, stem, root, max-storage, min_storage organs, g N plant-1
   real total_Ndemandmn, total_Ndemandmx !total N demand for current time step, minimum and maximum, g N plant-1
   real leaf_Nuptake, stem_Nuptake, storage_Nuptake, total_Nuptake 
   real lfNpool, stNpool, rtNpool !N potentially available for translocation from vegetative / root organs g N plant-1
   real a_lfNpool, a_stNpool, a_rtNpool ! N actually available for translocatoin per hour, g N plant-1
   real totNpool_tot, totNpool_hour !N which can be translocated in current time step, g N plant-1 h-1
   real leafcapacity, stemcapacity, rootcapacity
   real ratio, Nused
   integer test
   
   test = 0 !0 for oryza N approach, 1 for my ridiculously more complicated one

   pdensity = NHH * NPLH
   pre_N = (leaf_Npool + stem_Npool + storage_Npool)*pdensity*10. !converts g N plnat-1 to kg N ha-1,used as the x-value in look up table for min N content in storage organs
   
   !Set current N fractions
   
   !Get potential new N fractions for time-step based on DVS
   NMINSO = LINT2('minN_so',t_NMINSO,12,pre_N) !min N content in storage units, based on total N in vegetative and storage organs before flowering
   NMAXL = LINT2('maxN_lf',t_NMAXL,12,DVS) !max leaf N content, Kg N / kg leaf, I assume 
   NMINL = LINT2('minN_lf',t_NMINL,8,DVS) !min leaf N content, Kg N / kg leaf
   NFLVP = LINT2('potN_lf',t_NFLV,18,DVS) !g N m-2 leaf, i.e., how much N should the canopy have at this stage, not sure how this is used by ORYZA yet
   
   if (test.eq.0) then
   !ORYZA ROUTINE HERE
       !Get potential N demand of crop organs
       leaf_Ndemand = MAX(0.,NMAXL*grnleaf_dw - leaf_Npool) ! current leaf N demand, g N plant-1, after new leaf gro was accounted for including respiratory costs
       stem_Ndemand = MAX(0.,NMAXL*0.5*stem_dw - stem_Npool)! current stem N demand, g N plant-1
       root_Ndemand = MAX(0.,NMAXL*0.5 *root_dw - root_Npool) ! this is my estimate for root N demand, ORYZA doesn't seem to have separate accouting for it, this makes it equad to minimum leaf N concentration
       store_Ndemandmx = MAX(0., nit_NMAXSO * storage_dw - storage_Npool) ! current maximum storage organ N demand g N plant-1, where NMAXSO is variety parameter for max value (kg N kg-1) 
       store_Ndemandmn = MAX(0., NMINSO * storage_dw - storage_Npool) ! minimum N demand ", where NMINSO was variety parmaeter read from table (kg N kg-1 organ)
   
       !Estimate N translocatoin rates from leaf, stem, and roots
       if (DVS.LT.0.95) then
           lfNpool = 0.
           stNpool = 0.
           rtNpool = 0.
           totNpool_hour = 0.
           totNpool_tot = 0.
       else
           lfNpool = MAX(0., leaf_Npool-grnleaf_dw*nit_RFNLV)!amount of N translocatable from leaf (g N plant-1), nit_RNFLV is variety parameter (kg n kg-1 leaf)
           stNpool = MAX(0., stem_Npool-stem_dw*nit_RFNST)!as above for stem,
           !rtNpool = (lfNpool + stNpool)*nit_FNTRT!fraction of N translocatable based on shoot N translocatable, nit_FNTRT is a variety param (0 to 1)
           rtNpool = MAX(0., root_Npool-root_dw*nit_RFNST)
           totNpool_tot = (lfNpool + stNpool + rtNpool)
           totNpool_hour = (lfNpool + stNpool + rtNpool)/nit_TCNTRF! total N available for translocation, limited by nit_TNCTRF, variety param which is # hours required for translocation
           if (totNpool_hour.gt.store_Ndemandmx) totNpool_hour = store_Ndemandmx
       end if
       
       if (totNpool_tot.gt.0.) then
           a_lfNpool = totNpool_hour * lfNpool / totNpool_total
           a_stNpool = totNpool_hour * stNpool / totNpool_total
           a_rtNpool = totNpool_hour * rtNpool / totNpool_total
       else
           a_lfNpool = 0.
           a_stNpool = 0.
           a_rtNpool = 0.
       end if
       
       !total max N uptake rates from leaves, stems, and storage organs
       total_Ndemandmx = (leaf_Ndemand + a_lfNpool) + (stem_Ndemand +a_stNpool) + (root_Ndemand + a_rtNpool) + (store_Ndemandmx - totNpool_hour) !not sure I understand this last part
       hourlynitrogendemand = total_Ndemandmx
   
       !Actual uptake per plant organ based on minimum of availability and demand)
       if (total_Ndemandmx.gt.0.) then
           leaf_Nuptake = MAX(0., MIN(leaf_Ndemand+a_lfNpool, availableNitrogen*((leaf_Ndemand+a_lfNpool)/total_Ndemandmx)))
           stem_Nuptake = MAX(0., MIN(stem_Ndemand+a_stNpool, availableNitrogen*((stem_Ndemand+a_stNpool)/total_Ndemandmx)))
           root_Nuptake = MAX(0., MIN(root_Ndemand+a_rtNpool, availableNitrogen*((root_Ndemand+a_rtNpool)/total_Ndemandmx)))
           storage_Nuptake = MAX(0., MIN(store_Ndemandmx-totNpool_hour, availableNitrogen*((store_Ndemandmx-totNpool_hour)/total_Ndemandmx)))
       else
           leaf_Nuptake = 0.
           stem_Nuptake = 0.
           root_Nuptake = 0.
           storage_Nuptake = 0.
        end if
       total_Nuptake = leaf_Nuptake + stem_Nuptake + root_Nuptake + storage_Nuptake
       
       !Net N flows
       leaf_Npool = leaf_Npool + leaf_Nuptake
       stem_Npool = stem_Npool + stem_Nuptake
       root_Npool = root_Npool + root_Nuptake
       storage_Npool = storage_Npool + storage_Nuptake
       
       availableNitrogen = availableNitrogen - leaf_Nuptake - stem_Nuptake - root_Nuptake - storage_Nuptake
       
   end if
   
   
    if (test.eq.1) then
   
   if (CROPSTA.ge.4) then    
       !(1) potential N demand calculations, g N plant-1 
       NFLVP = LINT2('potN_lf',t_NFLV,18,DVS) !g N m-2 leaf, i.e., how much N should the canopy have at this stage, not sure how this is used by ORYZA yet
       leaf_Ndemand = MAX(0.,NMAXL*grnleaf_dw - leaf_Npool) ! current leaf N demand, g N plant-1, after new leaf gro was accounted for including respiratory costs
       stem_Ndemand = MAX(0., max(NMAXL*0.5,nit_RFNST)*stem_dw - stem_Npool)! current stem N demand, g N plant-1, but don't let C/N drop below the minimal C/N level as specified in itials file
       root_Ndemand = MAX(0.,NMAXL*0.5 *root_dw - root_Npool) ! this is my estimate for root N demand, ORYZA doesn't seem to have separate accouting for it, this makes it equad to minimum leaf N concentration
       store_Ndemandmx = MAX(0., nit_NMAXSO * storage_dw - storage_Npool) ! current maximum storage organ N demand g N plant-1, where NMAXSO is variety parameter for max value (kg N kg-1) 
       store_Ndemandmn = MAX(0., NMINSO * storage_dw - storage_Npool) ! minimum N demand ", where NMINSO was variety parmaeter read from table (kg N kg-1 organ)
       total_Ndemandmn = leaf_Ndemand + stem_Ndemand + store_Ndemandmn + root_Ndemand
       total_Ndemandmx = leaf_Ndemand + stem_Ndemand + store_Ndemandmx + root_Ndemand
       hourlynitrogendemand = total_Ndemandmx  
       !!!! Prior to Flowering
       if (DVS.LT.0.95) then
            avail_lfNpool = 0.
            avail_stNpool = 0.
            avail_rtNpool = 0.
            avail_totNpool = 0.
            if (availableNitrogen.GE.total_Ndemandmx) then !there is a surplus condition before flowering
               !allocate N demand to all organs, then determine extent of surplus and allocate it to mx level in storage, then to leaf
               leaf_Npool = leaf_Npool + leaf_Ndemand !g N in leaves plant-1
               stem_Npool = stem_Npool + stem_Ndemand
               root_Npool = root_Npool + root_Ndemand
               availableNitrogen = availableNitrogen - leaf_Ndemand - stem_Ndemand - root_Ndemand
               if(availableNitrogen.GE.store_Ndemandmx) then !there is luxury uptake if remaning available N exceeds maximum storage demand
                   if (storage_dw.gt.0.) then
                       storage_Npool = storage_Npool + store_Ndemandmx !g plant-1
                       availableNitrogen = availableNitrogen - store_Ndemandmx !will still have excess after this which we will put in leaves and stems
                   end if
                   !now any excess N can be further stored in excess about leaf, stem, root max amounts but I'm sort of arbitarily restricting this amount to 1.5* the NMAXL value
                   leaf_N = leaf_Npool / grnleaf_dw !g N g-1
                   stem_N = stem_Npool / stem_dw
                   root_N = root_Npool / root_dw               
                   leafcapacity = MAX(0., (1.5*NMAXL - leaf_N) * grnleaf_dw) !amount of capacity remaining for leaf surplus, g N plant-1
                   stemcapacity = max(0.,(1.5*NMAXL*0.5 - stem_N) *stem_dw)
                   rootcapacity = max(0.,(1.5*NMAXL*0.5 - root_N) * root_dw)
                   if (availableNitrogen.ge.leafcapacity) then
                       leaf_Npool = leaf_Npool + leafcapacity !g N plant-1
                       availableNitrogen = availableNitrogen - leafcapacity
                       if (availableNitrogen.ge.stemcapacity) then
                           stem_Npool = stem_Npool + stemcapacity
                           availableNitrogen = availableNitrogen - stemcapacity
                       else
                           stem_Npool = stem_Npool + availableNitrogen !g N plant-1
                           availableNitrogen = 0.
                       end if
                       if (availableNitrogen.ge.rootcapacity) then ! at this point, add capacity to root with anything left over staying in the available N pool
                           root_Npool = root_Npool + rootcapacity !g N plant-1
                           availableNitrogen = availableNitrogen - rootcapacity
                       else
                           root_Npool = root_Npool + availableNitrogen !g N plant-1
                           availableNitrogen = 0.
                       end if                       
                   else !just add any excess N below leafcapacity to the leaves for now then 
                       leaf_Npool = leaf_Npool + availableNitrogen
                       availableNitrogen = 0.
                   end if
               else !there is sufficient to meet the minumum requirement, but not enough to exceed maximum, so we will use up all availableNitrogen
                   storage_Npool = storage_Npool + availableNitrogen
                   availableNitrogen = 0.
               end if
           !(3b) Deficiency exists, don't have translocatable N prior to flowering so need to divy up what is available according to rules
               !can we meet minimum demand?
            else if (availableNitrogen.GE.total_Ndemandmn) then ! In this case, we can meet minimum N demand, so satisfy all organs and put any extra into leaves
               leaf_Npool = leaf_Npool + MAX(0.,(NMINL-leaf_N) * grnleaf_dw) !add minimum N needed to support leaf growth, g N plant-1 leaf
               stem_Npool = stem_Npool + MAX(0.,(NMINL*0.5-stem_N) * stem_dw)  !add minimum N needed for new stem growth based on min L N demand, g N plant-1 stem
               root_Npool = root_Npool + MAX(0.,(NMINL*0.5-root_N) * root_dw) !as above for root, 
               if (storage_dw.gt.0.) then
                   storage_Npool = storage_Npool + store_Ndemandmn
               else
                   storage_Npool = storage_Npool
               end if
               availableNitrogen = availableNitrogen - MAX(0.,(NMINL-leaf_N) * grnleaf_dw) -  MAX(0.,(NMINL*0.5-stem_N) * stem_dw) &
                   -  MAX(0.,(NMINL*0.5-root_N) * root_dw) - store_Ndemandmn

               !now we should have leftover N which we first use to address leaf N deficiency then storage_N
               if (availableNitrogen.gt.0.) then !minmum N demand for all organs is met, so add any extra to the other organs as surplus
                    !now any excess N can be further stored in excess about leaf, stem, root max amounts but I'm sort of arbitarily restricting this amount to 1.5* the NMAXL value
                   leafcapacity = max(0.,(1.5*NMAXL - leaf_N) * grnleaf_dw) !amount of capacity remaining for leaf surplus, g N plant-1
                   stemcapacity = max(0.,(1.5*NMAXL*0.5 - stem_N) *stem_dw)
                   rootcapacity = max(0.,(1.5*NMAXL*0.5 - root_N) * root_dw)
                   if (availableNitrogen.ge.leafcapacity) then
                       leaf_Npool = leaf_Npool + leafcapacity
                       availableNitrogen = availableNitrogen - leafcapacity
                       if (availableNitrogen.ge.stemcapacity) then
                           stem_Npool = stem_Npool + stemcapacity
                           availableNitrogen = availableNitrogen - stemcapacity
                       end if
                       if (availableNitrogen.ge.rootcapacity) then
                           root_Npool = root_Npool + rootcapacity
                           availableNitrogen = availableNitrogen - rootcapacity
                       else
                           root_Npool = root_Npool + availableNitrogen
                           availableNitrogen = 0.
                       end if
                   else ! just add to leaves
                        leaf_Npool = leaf_Npool + availableNitrogen
                       availableNitrogen = 0.
                   end if       
               end if
            else !can't meet minimum demand and have no N translocation, 
               !this seems to occur intermittently, particularly in early growth stages
               !each organ should have enough N to meet it's initial requirements due to organ growth
               !but when this condition is reached, it indicates there will be deficit
               ! for now, assume leaf gets as much as it cn still up to max, luxury value, followed by stem and root
               leaf_Nmin = MAX(0.,(NMINL - leaf_N) * grnleaf_dw) !can we satisfy minimum leaf N, g N leaves-1
               leaf_Nmax = MAX(0.,leaf_Ndemand)
               !leaf_Nmin = (leaf_Nmax+leaf_Nmin)/2.  !Nmin can go to zero want to ensure at least some N has to contribute to new leaf growth
               if (availableNitrogen.ge.leaf_Nmax) then
                   leaf_Npool = leaf_Npool + leaf_Nmax ! new leaf N content up to max room in leaf surplus available
                   availableNitrogen = availableNitrogen - leaf_Nmax
               else if (availableNitrogen.lt.leaf_Nmax) then ! new leaf N content, no N pool left 
                   leaf_Npool = leaf_Npool + availableNitrogen
                   availableNitrogen = 0.
               end if
               if (availableNitrogen.gt.0.) then !any left over surplus after leaf in max luxury state can go to meet minimum stem need, then root, then storage
                   !stemcapacity = max(0.,(1.5*NMAXL*0.5 - stem_N) *stem_dw) !g N plant-1
                   stemcapacity = stem_Ndemand
                   rootcapcity = root_Ndemand
                   !rootcapacity = max(0.,(1.5*NMAXL*0.5 - root_N) * root_dw)
                   if (availableNitrogen.ge.stemcapacity) then
                       stem_Npool = stem_Npool + stemcapacity
                       availableNitrogen = availableNitrogen - stemcapacity                    
                   else if (availableNitrogen.lt.stemcapacity) then
                       stem_Npool = stem_Npool + availableNitrogen
                       availableNitrogen = 0.
                   end if
                   if (availableNitrogen.gt.0..AND.storage_dw.gt.0.) then
                       if(availableNitrogen.gt.store_Ndemandmn) then
                           storage_Npool = storage_Npool + availableNitrogen
                           availableNitrogen = availableNitrogen - store_Ndemandmn
                       else
                           storage_Npool = storage_Npool + availableNitrogen
                           availableNitrogen = 0.
                       end if
                       if (availableNitrogen.gt.0.) then ! put any left over into roots, not sure we will every reach this situation
                            root_Npool = root_Npool + availableNitrogen
                            availableNitrogen = 0.
                       end if                 
                   end if
               else if (availableNitrogen.gt.0.) then ! now allocated to roots if no storage organs and still left over N
                   if (availableNitrogen.gt.rootcapacity) then 
                       root_Npool = root_Npool + rootcapacity
                       availableNitrogen = availableNitrogen - rootcapacity
                   else
                       root_Npool = root_Npool + availableNitrogen
                       availableNitrogen = 0.
                   end if
               end if
           end if
       end if
       
           
    !(6) Determine if surplus exists and allocated as needed during and after flowering
       !(6a) Surplus exists and is greater than max demand
       if (DVS.ge.0.95) then
          !(2) translocation of N from vegetative to storage organs only occurs post flowering, g N plant-1
            !It is assumed translocation only occurs to storage and does not occur prior to anthesis,
           avail_lfNpool = MAX(0.,(leaf_N - nit_RFNLV)*grnleaf_dw)/nit_TCNTRF !amount of N translocatable from leaf (g N plant-1), nit_RNFLV is variety parameter (kg n kg-1 leaf) and TCNTRF is the amount of time for full pool to become available
           avail_stNpool = MAX(0.,(stem_N - nit_RFNST)*stem_dw) /nit_TCNTRF!as above for stem, 
           avail_rtNpool = (avail_lfNpool + avail_stNpool) * nit_FNTRT !fraction of N translocatable based on shoot N translocatable, nit_FNTRT is a variety param (0 to 1)
           avail_totNpool = avail_lfNpool + avail_stNpool + avail_rtNpool ! total N available for translocation
           !reduce each organ contribution by ratio of any changes from in the potential available N to be translocated if it exceeds total demand
           if (total_Ndemandmn.eq.0.) then
               avail_lfNpool = 0.
               avail_stNpool = 0.
               avail_rtNpool = 0.
               avail_totNpool = 0.
           else if (total_Ndemandmn.gt.0..and.total_Ndemandmx.gt.0.) then !means N demand exists, 
               if (avail_totNpool.gt.total_Ndemandmx) then ! reduce amount of each pool available based on total demand and time-fraction if pool is excess of need
                   ratio = total_Ndemandmx / avail_totNpool
                   avail_lfNpool = min(avail_lfNpool * ratio,avail_lfNpool)
                   avail_stNpool = min(avail_stNpool * ratio,avail_stNpool)
                   avail_rtNpool = min(avail_rtNpool * ratio,avail_rtNpool)
               end if
           end if          
           if(availableNitrogen.GE.total_Ndemandmx) then
            !allocate N demand to all organs, then determine extent of surplus and allocate it to mx level in storage
               leaf_Npool = leaf_Npool + leaf_Ndemand ! g N leaf plant-1
               stem_Npool = stem_Npool + stem_Ndemand
               root_Npool = root_Npool + root_Ndemand      
               storage_Npool = storage_Npool + store_Ndemandmx
               availableNitrogen = availableNitrogen - leaf_Ndemand - stem_Ndemand - root_Ndemand - store_Ndemandmx !will have residual N
       !(6b) Surplus exists and is greater than min demand, but less than max
            else if(availableNitrogen.GE.total_Ndemandmn.AND.availableNitrogen.LT.total_Ndemandmx) then
               leaf_Npool = leaf_Npool + leaf_Ndemand ! g N plant-1 leaf
               stem_Npool = stem_Npool + stem_Ndemand
               root_Npool = root_Npool + root_Ndemand      
               storage_Npool = storage_Npool + store_Ndemandmn
               availableNitrogen = availableNitrogen - leaf_Ndemand - stem_Ndemand - root_Ndemand - store_Ndemandmn !should be zero
           !(6c) Insufficient to meet minimum demand, will add translocation to pool
           else if(availableNitrogen.LT.total_Ndemandmn) then
               if (availableNitrogen + avail_lfNpool.ge.total_Ndemandmn) then
                   Nused = total_Ndemandmn - availableNitrogen
                   leaf_Npool = leaf_Npool - Nused
                   availableNitrogen = availableNitrogen + Nused
               else if (availableNitrogen + avail_lfNpool + avail_stNpool.ge.total_Ndemandmn) then !use up leaf first, then stem
                   Nused = total_Ndemandmn - availableNitrogen
                   leaf_Npool = leaf_Npool - avail_lfNpool
                   stem_Npool = stem_Npool - (total_Ndemandmn - avail_lfNpool - availableNitrogen)
                   availableNitrogen = availableNitrogen + Nused
               else if (availableNitrogen + avail_totNpool.ge.total_Ndemandmn) then ! use all as needed
                   Nused = total_Ndemandmn - availableNitrogen
                   leaf_Npool = leaf_Npool - avail_lfNpool
                   stem_Npool = stem_Npool - avail_stNpool
                   root_Npool = root_Npool - (total_Ndemandmn - avail_lfNpool - avail_stNpool - availableNitrogen)
                   availabelNitrogen = availableNitrogen + Nused
               end if
               if (availableNitrogen.ge.total_Ndemandmn) then ! now we should have the adjusted amount from translocated N
                   leaf_Npool = leaf_Npool + leaf_Ndemand !g N g-1 leaf
                   stem_Npool = stem_Npool + stem_Ndemand
                   root_Npool = root_Npool + root_Ndemand      
                   storage_Npool = storage_Npool + store_Ndemandmn ! g N g-1 storage
                   availableNitrogen = availableNitrogen - (leaf_Ndemand + stem_Ndemand + root_Ndemand + store_Ndemandmn)
                else !we still can't meet the minimum after translocated N, so prioritize to storage, 
                        if (availableNitrogen.ge.store_Ndemandmn) then
                            storage_Npool = storage_Npool + store_Ndemandmn ! g N plant-1 storage
                            availableNitrogen = availableNitrogen - store_Ndemandmn 
                            if (availableNitrogen.ge. leaf_Ndemand + stem_Ndemand) then
                                leaf_Npool = leaf_Npool + leaf_Ndemand
                                stem_Npool = stem_Npool + stem_Ndemand
                                availableNitrogen = availableNitrogen - leaf_Ndemand - stem_Ndemand
                            end if
                        else if (availableNitrogen.lt.storeNdemandmn) then
                            storage_Npool = storage_Npool + availableNitrogen
                            availableNitrogen = 0.
                        end if
                end if   
           end if
       end if
       
   end if
   end if
   
  
   leaf_N = leaf_Npool / grnleaf_dw
   stem_N = stem_Npool / stem_dw
   root_N = root_Npool / root_dw
   if (storage_dw.gt.0.) storage_N = storage_Npool / storage_dw
   
   ave_SLNcanopy = leaf_Npool / leaf_area * 10000. !g N m-2 leaf averaged through the canopy.  at some point would like to split for sunlit or shaded leaves
   !totalplantN = leaf_N*grnleaf_dw + stem_N*stem_dw + root_N*root_dw + storage_N*storage_dw  ! g N plant-1  note that availableNitrogen isn't actually allocated to plant N content so may cause some oddities when tracking whole plant N content
   totalplantN = leaf_Npool + stem_Npool + root_Npool + storage_Npool + dead_Npool + availableNitrogen
   return
end