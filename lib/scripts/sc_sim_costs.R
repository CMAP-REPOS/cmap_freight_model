sc_sim_costs <- function(naics_set){
  
  t0 <- Sys.time()
  
  # Expanded set of market-group combinations
  naics_set_expanded <- data.table(NAICS = rep(naics_set$NAICS, naics_set$groups),
                                   SCTG = rep(naics_set$SCTG, naics_set$groups),
                                   Market = rep(naics_set$Market, naics_set$groups),
                                   Group = unlist(lapply(naics_set$groups, seq, from=1)))
  naics_set_expanded[, Market_Group := paste(Market, Group, sep = "_")]
  
  if(USER_COST_CORES > 1){
    require(parallel)
    
    clust <- makeCluster(USER_COST_CORES)
    
    clusterCall(clust, 
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH)
    
    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
    clusterExport(clust, 
                  c("create_costs"), 
                  envir = environment())
    
    naicslist <- parLapplyLB(clust, 
                             1:nrow(naics_set_expanded), 
                             function(x){
                               create_costs(market= as.character(naics_set_expanded$Market[x]),
                                            g = naics_set_expanded$Group[x])
                             },
                             chunk.size = 1)
    stopCluster(clust)
    
  } else {
    
    naicslist <- lapply(1:nrow(naics_set_expanded), 
                        function(x){
                          
                          print(paste(x,
                                      as.character(naics_set_expanded$Market[x]),
                                      naics_set_expanded$Group[x]))
                          
                          create_costs(market= as.character(naics_set_expanded$Market[x]),
                                       g = naics_set_expanded$Group[x])
                          
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_costs.csv"))
  naics_missing <- naics_set_expanded[!Market_Group %in% naics_completed]$Market_Group
  if(length(naics_missing) > 0) cat("Market-Group combinations missing from market simulation in sc_sim_costs: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  return(naics_set)
}

create_costs <- function(market, g){
  
  # load the pc table for this market and group
  pc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")),
                 as.data.table = TRUE)
  
  pc[, Attribute1_UnitCost := minc / PurchaseAmountTons]
  pc[, Attribute2_ShipTime := time / (60 * 24)] #Convert from minutes to days
  
  # Save pc and write the costs.csv 
  write_fst(pc, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")))
  
  fwrite(pc[,.(sellerid = SellerID,	buyerid = BuyerID,	Attribute2_ShipTime,	cost,	Attribute1_UnitCost)],
         file = file.path(SCENARIO_OUTPUT_PATH,paste0(market, "_g", g, ".costs.csv")))
  
  return(paste(market, g, sep = "_"))
}

