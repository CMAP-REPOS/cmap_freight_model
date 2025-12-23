sc_sim_shipments <- function(naics_set){
  
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
                  c("runPMG"), 
                  envir = environment())
    
    pairslist <- parLapplyLB(clust, 
                             1:nrow(naics_set_expanded), 
                             function(x){
                               
                               # Combine the pmg outputs
                               process_pmg_out(naics = as.character(naics_set_expanded$NAICS[x]),
                                 market = as.character(naics_set_expanded$Market[x]), 
                                      g = naics_set_expanded$Group[x])
                                      
                             },
                             chunk.size = 1)
    stopCluster(clust)
    
  } else {
    
    pairslist <- lapply(1:nrow(naics_set_expanded), 
                        function(x){
                          
                          print(paste(x,
                                      as.character(naics_set_expanded$Market[x]),
                                      naics_set_expanded$Group[x]))
                          
                          # Combine the pmg outputs
                          process_pmg_out(naics = as.character(naics_set_expanded$NAICS[x]),
                                          market = as.character(naics_set_expanded$Market[x]), 
                                          g = naics_set_expanded$Group[x])
                          
                        })
    
  }
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  # Combine together and return the complete pairs table
  
  pc_pairs <- rbindlist(pairslist)
  rm(pairslist)
  gc()
  
  # Summarize and check by market for completeness
  naics_completed <- unique(pc_pairs$Market_Group)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_shipments.csv"))
  naics_missing <- naics_set_expanded[!Market_Group %in% naics_completed]$Market_Group
  if(length(naics_missing) > 0) cat("Market-Group combinations missing from market simulation in sc_sim_shipments: ", naics_missing)

  # Data types and calculate annual value
  pc_pairs[, Quantity.Traded := as.integer64(Quantity.Traded)]
  pc_pairs[, Last.Iteration.Quantity := as.integer64(Last.Iteration.Quantity)]
  pc_pairs[, AnnualValue := (Last.Iteration.Quantity / PurchaseAmountTons) * ConVal]

  setkey(pc_pairs, Production_zone, Consumption_zone, path)
  
  return(pc_pairs)
}  
  
process_pmg_out <- function(naics, market, g){  
  
  # read the PMG output file
  pmgout <- fread(file = file.path(SCENARIO_OUTPUT_PATH, 
                                   paste0(market, "_g", g, ".out.csv")))

  setnames(pmgout, c("BuyerId", "SellerId"), c("BuyerID", "SellerID"))

  pmgout[, Quantity.Traded := as.integer64(Quantity.Traded)]
  pmgout[, Last.Iteration.Quantity := as.integer64(Last.Iteration.Quantity)]
  
  # Get just the results from the final iteration
  pmgout <- pmgout[Last.Iteration.Quantity > 0]
  
  # Load the group pc file
  pc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")),
                 as.data.table = TRUE)
  
  # Merge the trades from PMG with the saved pc table
  pc_pairs <- merge(pc, pmgout, by = c("BuyerID", "SellerID"))
  
  # Add market identifyers and the Buyer.NAICS to the pc_pairs table
  pc_pairs[, c("NAICS", "Market", "Group") := .(naics, market, g)]
  pc_pairs[conscg, Buyer.NAICS := i.Buyer.NAICS, on = "BuyerID"]
  
  # Save pc_pairs
  write_fst(pc_pairs, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc_pairs.fst")))
  
  # Return the pairs table so that it can be combined with the others from this group for this market
  return(pc_pairs)
}


