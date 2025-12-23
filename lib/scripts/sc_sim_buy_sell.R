sc_sim_buy_sell <- function(naics_set){
  
  t0 <- Sys.time()
  
  if(USER_COST_CORES > 1){
    require(parallel)
    
    clust <- makeCluster(USER_COST_CORES)
    
    clusterCall(clust, 
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH)
    
    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
    clusterExport(clust, 
                  c("create_buy_sell",
                    "naics_set"), 
                  envir = environment())
    
    naicslist <- parLapply(clust, 
                           1:nrow(naics_set), 
                           function(x){
                             create_buy_sell(market = as.character(naics_set$Market[x]),
                                                      groups = naics_set$groups[x],
                                                      sprod = ifelse(naics_set$Split_Prod[x], 1, 0))
                           })
    stopCluster(clust)
    
  } else {
    
    naicslist <- lapply(1:nrow(naics_set), 
                        function(x){
                          create_buy_sell(market = as.character(naics_set$Market[x]),
                                                   groups = naics_set$groups[x],
                                                   sprod = ifelse(naics_set$Split_Prod[x], 1, 0))
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_buy_sell.csv"))
  naics_missing <- naics_set[!Market %in% naics_completed]$Market
  if(length(naics_missing) > 0) cat("Markets missing from market creation in sc_sim_buy_sell: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  return(naics_set)
  
}

create_buy_sell <- function(market, groups, sprod){
  
  # Load the consumers and producers tables for this market
  consc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_consc.fst")),
                    as.data.table = TRUE)
  prodc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_prodc.fst")),
                    as.data.table = TRUE)
  
  for (g in 1:groups){
    # Create the set of buy inputs file for this groups
    fwrite(consc[group == g,
                 .(InputCommodity, BuyerID, FAFZONE, Zone, NAICS,
                   Size, OutputCommodity, PurchaseAmountTons,
                   PrefWeight1_UnitCost, PrefWeight2_ShipTime, 
                   SingleSourceMaxFraction)], 
           file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".buy.csv")))
    
    # create a simpler version for use in the costs function
    conscg <- consc[group == g,.(InputCommodity, Commodity_SCTG, NAICS, FAFZONE, Zone, BuyerID, 
                                 Size, PurchaseAmountTons, ConVal)]
    
    print(paste(Sys.time(), "Finished writing buy file for ",market,"group",g))
    
    # If not splitting producers, write out the complete set for each group with output capacity reduced
    if(sprod==1){
      fwrite(prodc[group == g,
                   .(OutputCommodity, SellerID, FAFZONE, Zone, NAICS,
                     Size, OutputCapacityTons, NonTransportUnitCost)], 
             file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".sell.csv")))
      
      # create a simpler version for use in the costs function
      prodcg <- prodc[group == g,.(OutputCommodity, NAICS, Commodity_SCTG, SellerID, Size, 
                         FAFZONE, Zone, OutputCapacityTons)]
    
    } else {
      
      #reduce capacity based on demand in each group
      consamount <- sum(conscg$PurchaseAmountTons)/sum(consc$PurchaseAmountTons)
      prodc[group == g, OutputCapacityTons_g:= OutputCapacityTons * consamount]
      
      fwrite(prodc[group == g,
                   .(OutputCommodity, SellerID, FAFZONE, Zone, NAICS,
                     Size, OutputCapacityTons = OutputCapacityTons_g, NonTransportUnitCost)], 
             file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".sell.csv")))
      
      prodcg <- prodc[group == g,.(OutputCommodity, NAICS, Commodity_SCTG, SellerID, Size, 
                                   FAFZONE, Zone, OutputCapacityTons = OutputCapacityTons_g)]
    }
    
    print(paste(Sys.time(), "Finished writing sell file for ",market,"group",g))
    
    # Save consc and prodc
    write_fst(conscg, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_consc.fst")))
    write_fst(prodcg, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_prodc.fst")))
    
  }
  
  return(market)
  
}


