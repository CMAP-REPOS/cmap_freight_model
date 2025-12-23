sc_sim_sampling <- function(naics_set){
  
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
                  c("create_prod_cons_combinations",
                    "sample_data_test",
                    "SolveFlowGLPK",
                    "SolveFlowCLP",
                    "naics_set_expanded",
                    "FAF_TON_DIST",
                    "FAF_DISTANCE",
                    "FAF_TON_TRADETYPE"), 
                  envir = environment())
    
    naicslist <- parLapplyLB(clust, 
                             1:nrow(naics_set_expanded), 
                             function(x){
                               create_prod_cons_combinations(market = as.character(naics_set_expanded$Market[x]),
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
                          
                          create_prod_cons_combinations(market = as.character(naics_set_expanded$Market[x]),
                                                        g = naics_set_expanded$Group[x])
                          
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_sampling.csv"))
  naics_missing <- naics_set_expanded[!Market_Group %in% naics_completed]$Market_Group
  if(length(naics_missing) > 0) cat("Market-Group combinations missing from market creation in sc_sim_sampling: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  return(naics_set)
  
}

create_prod_cons_combinations <- function(market, g, check_feasibility = FALSE){
  
  # load the market
  conscg <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_consc.fst")),
                     as.data.table = TRUE)
  prodcg <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_prodc.fst")),
                    as.data.table = TRUE)

  # Key conscg
  setkey(conscg, FAFZONE)
  
  # seed for sampling
  set.seed(BASE_SEED_VALUE)
  # Set the initial ratioweights to 1. 
  # ratioweights attribute is used to track a metric of production to consumption ratio
  prodcg[, ratioweights := 1]
  
  # what proportion of total output capacity does each producer represent?
  prodcg[, outputprop := OutputCapacityTons/sum(OutputCapacityTons)]
  
  # what proportion of total purchases does each consumer represent?
  conscg[, purchprop := PurchaseAmountTons/sum(PurchaseAmountTons)]
  
  # production and consumption quantiles
  prodcg[, tonquant := findInterval(OutputCapacityTons,
                                    quantile(OutputCapacityTons, probs = seq(0, 1, 0.25), type = 5),
                                    rightmost.closed = TRUE)]
  
  conscg[, tonquant := findInterval(PurchaseAmountTons,
                                    quantile(PurchaseAmountTons, type = 5),
                                    rightmost.closed = TRUE)]
  
  # Assign a sample size (of sellers) to buyers
  # estimate a reasonable number of extra samples to allocate amongst larger consumers
  total_samples <- max(BASE_SUPPLIERS_PER_BUYER * 5 * prodcg[,.N], BASE_SUPPLIERS_PER_BUYER * conscg[,.N])
  conscg[, samplesize := round(total_samples * purchprop)]
  # update the sample size to be minimum BASE_SUPPLIERS_PER_BUYER/2, maximum is the number of producers
  min_samples <- ceiling(BASE_SUPPLIERS_PER_BUYER/2)
  conscg[, samplesize := ifelse(samplesize < min_samples, min_samples, samplesize)]
  conscg[, samplesize := ifelse(samplesize > prodcg[,.N], prodcg[,.N], samplesize)]
  
  # get the sampling proportions for this markets commodity
  market_sctg <- unique(prodcg$Commodity_SCTG)
  
  # distance based sampling for domestic shipments
  FAF_TON_DIST_SCTG <- FAF_TON_DIST[SCTG == market_sctg, .(Distance_Bin = DistanceGroup, Proportion)]
  
  FAF_DISTANCE_SCTG <- FAF_DISTANCE[FAF_TON_DIST_SCTG,
                                    Proportion := i.Proportion,
                                    on = "Distance_Bin"]

  FAF_DISTANCE_SCTG[is.na(Proportion), Proportion := 1e-6]

  # get the trade type proportion for split between domestic and imports for domestic consumers
  FAF_TON_TRADETYPE_SCTG <- FAF_TON_TRADETYPE[SCTG == market_sctg & Trade_Type %in% c("Domestic", "Import")]
  FAF_TON_TRADETYPE_SCTG[, Proportion := Proportion/sum(Proportion, na.rm = TRUE)]
  
  # iterative takes samples until there are enough for a feasible solution in PMG
  isInfeasible <- TRUE
  buyeriter <- 0
  
  while(isInfeasible){
    
    # Take the sample
    pc_add <- sample_data_test(prodcg[,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights, outputprop)], 
                               conscg[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
                               FAF_DISTANCE_SCTG,
                               FAF_TON_DIST_SCTG,
                               FAF_TON_TRADETYPE_SCTG)
    
    # ### Does the sample have a reasonable distance distribution and trade type split
    # pc_add[prodcg, oFAFZONE := i.FAF, on = "SellerID"]
    # pc_add[conscg, dFAFZONE := i.FAF, on = "BuyerID"]
    # pc_add[FAF_DISTANCE_SCTG, Distance_Bin := i.Distance_Bin, on = c("oFAFZONE", "dFAFZONE")]
    # pc_add[, Trade_Type := "Domestic"]
    # pc_add[dFAFZONE > 800, Trade_Type := "Export"]
    # pc_add[oFAFZONE > 800, Trade_Type := "Import"]
    # 
    # # poportion by trade type
    # pc_add[, .(OutputCapacityTons = sum(OutputCapacityTons), PurchaseAmountTons = sum(PurchaseAmountTons))
    #        , by = Trade_Type][, c("Trade_Type_Out_Prop", "Trade_Type_Purch_Prop") := .(OutputCapacityTons/sum(OutputCapacityTons), PurchaseAmountTons/sum(PurchaseAmountTons))][]
    # FAF_TON_TRADETYPE[SCTG == market_sctg]
    # 
    # # poportion by distance for domestic
    # pc_add[!is.na(Distance_Bin) & Trade_Type == "Domestic",
    #        .(OutputCapacityTons = sum(OutputCapacityTons), PurchaseAmountTons = sum(PurchaseAmountTons)),
    #        keyby = Distance_Bin]
    # FAF_TON_DIST[SCTG == market_sctg][1:10]
    
    
    if(buyeriter>0){
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("SellerID", "BuyerID"))
    } else {
      pc <- pc_add
    }
    # Checks on sample
    # this is margin of safety to help ensure enough excess capacity relative to purchase requirements
    buffer <- 0.1
    
    # For buyers, which have sufficient capacity in the sellers that they are paired with to meet their requirements
    Buy.PurRatio <- pc[, .(NumSamples = .N, PurRatio = sum(OutputCapacityTons) * buffer/mean(PurchaseAmountTons)), by = BuyerID]
    # Buy.PurRatio[order(PurRatio)][PurRatio < 1]
    
    # If any buyers have a purchase ratio < 1, i.e. the total output capacity * buffer <  purchase amount 
    # add some records to the sample for those buyers
    if(Buy.PurRatio[PurRatio < 1,.N] > 0){
      conscg.pur <- conscg[BuyerID %in% Buy.PurRatio[PurRatio < 1]$BuyerID]
      pc_add <- sample_data_test(prodcg[,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights, outputprop)], 
                                 conscg.pur[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
                                 FAF_DISTANCE_SCTG,
                                 FAF_TON_DIST_SCTG,
                                 FAF_TON_TRADETYPE_SCTG,
                                 check_distance = FALSE) 
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("SellerID", "BuyerID"))
    }
    
    # Which sellers are allocated to buyer samples and how extended are they
    # this is not very meaningful, but in general Sellers (particularly larges ones) should be in many samples 
    Sell.CapRatio <- pc[, .(NumSamples = .N, CapRatio = mean(OutputCapacityTons)/(buffer * sum(PurchaseAmountTons))), by = SellerID]
    # Add some extra cases if there are missing sellers
    # this should generally work to allocate all sellers to at least some buyer sample but don't need to force it
    # not guarenteed in the PMG that all sellers will find a buyer anyway
    Sell.Missing <- prodcg[!SellerID %in% Sell.CapRatio$SellerID]
    
    if(Sell.Missing[,.N] > 0){
      
      # select a reasonable number of buyers to add these sellers too
      avbuyers <- ceiling(pc[,.N]/(prodcg[,.N] - Sell.Missing[,.N]))
      buyerstoadd <- min(avbuyers * Sell.Missing[,.N], conscg[,.N])
      conscg.samp <- conscg[sample.int(conscg[,.N], size = buyerstoadd, replace = FALSE, prob = conscg$purchprop)]
      conscg.samp[, samplesize := 1]
      
      pc_add <- sample_data_test(prodcg[SellerID %in% Sell.Missing$SellerID,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights, outputprop)], 
                                 conscg.samp[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
                                 FAF_DISTANCE_SCTG,
                                 FAF_TON_DIST_SCTG,
                                 FAF_TON_TRADETYPE_SCTG,
                                 check_distance = FALSE)
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("SellerID", "BuyerID"))
      
    }
    
    if(check_feasibility){
      # recalculate Sell cap ratio after dealing with missing
      Sell.CapRatio <- pc[, .(NumSamples = .N, CapRatio = mean(OutputCapacityTons)/(buffer * sum(PurchaseAmountTons))), by = SellerID]
    
      # Check the solution
      system.time(solution <- SolveFlowCLP(pc[,.(SellerID, BuyerID, OutputCapacityTons, PurchaseAmountTons)], 
                                         prodcg, conscg, lp_problems = NULL))
    
      SellersToDo <- solution[[1]]
      BuyersToDo <- solution[[2]]
      lp_problems <- solution[[3]]
      solution <- solution[[4]]
      
      isInfeasible <- (solution > 0)
    } else {
      
      # force to stop after single iteration
      isInfeasible <- FALSE
    }  
    
    totalPairs <- pc[,.N]
    buyeriter <- buyeriter + 1
    
    print(paste0("Is the solution still infeasible:", isInfeasible))
    print(paste0("Iteration:", buyeriter))
    print(paste0("Total number of buyer supplier pairs: ", pc[,.N]))
    
    if(isInfeasible){
      # update the sample size for the next iteration
      # as iterations grow, total sample size will grow quickly to avoid too many iterations
      conscg[, samplesize := samplesize + samplesize * buyeriter]
      # replace the ratio weights with the capaacity ratio to prioritize producers that have not been allocated very often
      # and have in theory a lot of extra capacity
      # give any remaining missing sellers the max ratio weight too
      MaxCapRatio <- max(Sell.CapRatio$CapRatio)
      MinCapRatio <- min(Sell.CapRatio$CapRatio)
      RangeCapRatio <- MaxCapRatio - MinCapRatio
      prodcg[Sell.CapRatio, CapRatio := i.CapRatio, on = "SellerID"]
      prodcg[, ratioweights := ifelse(!is.na(CapRatio), 
                                      CapRatio/RangeCapRatio * 100 + 1, 
                                      MaxCapRatio/RangeCapRatio * 100 + 1)]
    }
  }
  
  if(check_feasibility){
    delProbCLP(lp_problems)
    rm(lp_problems)
  }
  
  # Save pc, conscg and prodcg
  write_fst(conscg, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_consc.fst")))
  write_fst(prodcg, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_prodc.fst")))
  write_fst(pc, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")))
  
  return(paste(market, g, sep = "_"))
  
}

# Sampling function 
sample_data_test <- function(prodcg, 
                             conscg, 
                             FAF_DISTANCE_SCTG,
                             FAF_TON_DIST_SCTG,
                             FAF_TON_TRADETYPE_SCTG,
                             check_distance = TRUE){
  
  # Set up inputs for looping through by FAF zone
  prodcg.faf.all <- copy(prodcg) # domestic buyer any suppliers
  prodcg.faf.dom <- prodcg[FAFZONE < 800] # foreign buyer only domestic suppliers
  consumers.faf <-  conscg[,.N, by = FAFZONE]
  samplesizemult <- 2L
  
  pc_list <- list()
  
  for (FAFZONE.buyer in consumers.faf$FAFZONE){
    
    # select consumers
    conscg.faf <- conscg[FAFZONE == FAFZONE.buyer]
    NumConsumers <- conscg.faf[,.N]
    
    # select producers
    if (FAFZONE.buyer < 800) {
      prodcg.faf <- prodcg.faf.all # domestic buyer any suppliers
    } else {
      prodcg.faf <- prodcg.faf.dom # foreign buyer only domestic suppliers 
    }
    NumProducers <- prodcg.faf[,.N]
    
    if(NumProducers > 0){ # is possible depending on how this called that there are no applicable producers
      
      # add the proportions for this zone and calc probabilities
      
      ### Distance factors for domestic producers
      prodcg.faf[FAF_DISTANCE_SCTG[dFAFZONE == FAFZONE.buyer, .(FAFZONE = oFAFZONE, Distance_Bin)], 
                 Distance_Bin := i.Distance_Bin, 
                 on = "FAFZONE"]
      
      if("Output_Prop" %in% names(FAF_TON_DIST_SCTG)) FAF_TON_DIST_SCTG[, Output_Prop := NULL]
      
      FAF_TON_DIST_SCTG[prodcg.faf[!is.na(Distance_Bin) & FAFZONE < 800,
                 .(Output_Prop = sum(outputprop)), 
                 keyby = Distance_Bin],
                 Output_Prop := i.Output_Prop,
                 on = "Distance_Bin"]
      
      FAF_TON_DIST_SCTG[, Distance_Factor := Proportion/Output_Prop]           
      
      prodcg.faf[FAF_TON_DIST_SCTG,
                 Distance_Factor := i.Distance_Factor, 
                 on = "Distance_Bin"]
      
      prodcg.faf[is.na(Distance_Factor), Distance_Factor := 1e-6]
      prodcg.faf[FAFZONE > 800, Distance_Factor := 1]
      
      ### Factors for trade type shares
      if(FAFZONE.buyer < 800){ 
        
        prodcg.faf[, Trade_Type := "Domestic"]
        prodcg.faf[FAFZONE > 800, Trade_Type := "Import"]
        
        if("Output_Prop" %in% names(FAF_TON_TRADETYPE_SCTG)) FAF_TON_TRADETYPE_SCTG[, Output_Prop := NULL]
        
        FAF_TON_TRADETYPE_SCTG[prodcg.faf[,.(Output_Prop = sum(outputprop * Distance_Factor)), 
                                          keyby = Trade_Type],
                               Output_Prop := i.Output_Prop,
                               on = "Trade_Type"]
        
        FAF_TON_TRADETYPE_SCTG[, Trade_Type_Factor := Proportion/Output_Prop]           
        
        prodcg.faf[FAF_TON_TRADETYPE_SCTG,
                   Trade_Type_Factor := i.Trade_Type_Factor, 
                   on = "Trade_Type"]
      } else {
        # All export
        prodcg.faf[, Trade_Type_Factor := 1]
      }
      
      prodcg.faf[, Prob := ratioweights * outputprop * Distance_Factor * Trade_Type_Factor]
      prodcg.faf[, Prob := Prob/sum(Prob)]
      
      #prodcg.faf[,round(sum(Prob),3), keyby = Distance_Bin]
      
      # create a sample for all of the consumers in one go
      # use the sample size field * 2
      # use the Prob field as prob
      # use replacement and then dedup on the complete table
      samp.faf <- sample.int(NumProducers, size = sum(conscg.faf$samplesize) * samplesizemult, replace = TRUE, prob = prodcg.faf$Prob)
      
      pc.FAFZONE.buyer <- cbind(prodcg.faf[samp.faf, .(SellerID, OutputCapacityTons, Prob, Distance_Bin)], 
                                conscg.faf[rep.int(1:NumConsumers, conscg.faf$samplesize * samplesizemult),.(BuyerID, PurchaseAmountTons, samplesize)])
      
      # check whether the deduped list for this sample has a reasonable number of local producers
      if(FAFZONE.buyer < 800 & check_distance){
        pc.dist <- unique(pc.FAFZONE.buyer[,.(SellerID, BuyerID)])
      
        pc.dist[pc.FAFZONE.buyer, 
                c("PurchaseAmountTons", "Distance_Bin") := .(i.PurchaseAmountTons, i.Distance_Bin), 
                on = c("SellerID", "BuyerID")]
        
        pc.dist.sum <- pc.dist[,.(PurchaseAmountTons = sum(PurchaseAmountTons)), keyby = Distance_Bin]
        
        pc.dist.sum[, Prop := PurchaseAmountTons/sum(PurchaseAmountTons)]
        
        pc.dist.sum[FAF_TON_DIST_SCTG, Target := i.Proportion, on = "Distance_Bin"]
        
        pc.dist.sum[, Diff := Prop - Target]
        
        if(nrow(pc.dist.sum[Distance_Bin == 1 & Diff < 0])>0){
          
          # add additional short distance producers to the sample
          pc.FAFZONE.buyer.short <- merge(prodcg.faf[Distance_Bin == 1, .(SellerID, OutputCapacityTons, Prob, Distance_Bin, k=1)], 
                                          conscg.faf[,.(BuyerID, PurchaseAmountTons, samplesize, k = 1)],
                                          by = "k",
                                          allow.cartesian = TRUE)[, k := NULL]
          
          pc.FAFZONE.buyer <- rbind(pc.FAFZONE.buyer,
                                    pc.FAFZONE.buyer.short)
          
          # trim down the sample to meet the sample threshold size by removing the most distant suppliers
          setkey(pc.FAFZONE.buyer, BuyerID, Distance_Bin)
          pc.FAFZONE.buyer[, seller_rank := 1:.N, by = BuyerID]
          pc.FAFZONE.buyer <- pc.FAFZONE.buyer[seller_rank <= samplesizemult * samplesize]
          pc.FAFZONE.buyer[, seller_rank := NULL]
          
        }
        
      }
      
      pc_list[[FAFZONE.buyer]] <- pc.FAFZONE.buyer[, Distance_Bin := NULL]
      
    }
  }
  
  # combine list, deduplicate, return
  pc <- rbindlist(pc_list)
  pc <- unique(pc, by = c("SellerID", "BuyerID"))
  
  return(pc[,.(SellerID, OutputCapacityTons, BuyerID, PurchaseAmountTons)])   
  
}

# Checking if flows can happen function
SolveFlowGLPK <- function(pc_table, prodcg, conscg, firstrun=TRUE){
  # Get the number of variables
  SellerID <- prodcg[, SellerID]
  BuyerID <- conscg[, BuyerID]
  nSellers <- length(SellerID)
  nBuyers <- length(BuyerID)
  flowlength <- pc_table[,.N]
  sellerweights <- c(1,0.75,0.5,0.25)[prodcg[pc_table, tonquant, on = "SellerID"]]
  buyerweigths <- c(1,0.75,0.5,0.25)[conscg[pc_table, tonquant, on = "BuyerID"]]
  objweights <- sellerweights * buyerweigths
  
  # The objective function to minimize is just the sum of flows, penalty on slack variables for sellers and slack variables on buyers
  ## Slack Variables, Flow Variables
  objective <- c(rep(10, nBuyers), objweights) 
  
  # The first constraint is to not oversell
  # Sellers are i and Buyers are j. Columns are flows and slack and first chunk of rows are sellers.
  # Column 1 will be i=1,j=1, Column 2 will be i=2, j=1, until Column=length(SellerID) where i=length(SellerID), j = 1
  
  rowindex1 <- match(pc_table$SellerID, SellerID)
  colindex1 <- 1:flowlength + nBuyers
  rhsoutput1 <- pc_table[,.N,.(SellerID)][prodcg, OutputCapacityTons,on = "SellerID"]
  
  slackrowindex1 <- 1:nSellers
  slackcolindex1 <- 1:nSellers + nBuyers
  
  rowindex2 <- match(pc_table$BuyerID,BuyerID)
  colindex2 <- 1:flowlength + nBuyers
  rhsoutput2 <- pc_table[,.N,.(BuyerID)][conscg, PurchaseAmountTons, on = "BuyerID"]
  rowindex2 <- rowindex2 + nSellers
  
  slackrowindex2 <- 1:nBuyers
  slackcolindex2 <- 1:nBuyers
  
  constraintmatrix <- slam::as.simple_triplet_matrix(Matrix::sparseMatrix(i=c(rowindex1, rowindex2, slackrowindex2, slackrowindex1), j=c(colindex1, colindex2, slackcolindex2, slackcolindex1), x = 1, dimnames = list(c(SellerID,BuyerID), seq_len(flowlength + nBuyers + nSellers))))
  
  rm(pc_table)
  gc()
  
  if(firstrun){

    ## Create a new problem
    tradeproblem <- initProbGLPK()
    
    ## Set the direction of the objective function
    setObjDirGLPK(tradeproblem, GLP_MIN)
    
    ## Add the number of rows and columns
    addRowsGLPK(tradeproblem, nBuyers+nSellers)
    addColsGLPK(tradeproblem, nBuyers)
    addColsGLPK(tradeproblem, nSellers)
    addColsGLPK(tradeproblem, flowlength)
    
    ## Set the objective coefficients
    setObjCoefsGLPK(tradeproblem, seq_len(flowlength + nBuyers + nSellers), as.double(objective))
    
    ## Set the columns and rows bounds
    setColsBndsGLPK(tradeproblem,
                    seq_len(flowlength + nBuyers + nSellers), 
                    lb = as.double(rep(0, flowlength + nBuyers + nSellers)), 
                    ub = as.double(rep(max(rhsoutput1, rhsoutput2), flowlength + nBuyers + nSellers)))
    
    setRowsBndsGLPK(tradeproblem,
                    seq_len(nSellers + nBuyers),
                    lb = as.double(c(rep(0, nSellers), rhsoutput2)), 
                    ub = as.double(c(rhsoutput1, rep(Inf, nBuyers))))
    
    ## Load constraint matrix
    loadMatrixGLPK(tradeproblem, 
                   as.integer(length(constraintmatrix$i)), 
                   as.integer(constraintmatrix$i), 
                   as.integer(constraintmatrix$j), 
                   as.double(constraintmatrix$v))
    
    ## Set the row names
    setRowsNamesGLPK(tradeproblem,
                     as.integer(seq_len(nSellers + nBuyers + nSellers)),
                     c(SellerID, BuyerID))
    
    
    ## Set Simplex Control Parameters
    
    ## Solve the problem
    solveSimplexGLPK(tradeproblem)
    
    sellers <- SellerID[getRowsDualGLPK(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColsPrimGLPK(tradeproblem)[1:nBuyers]
    buyers <- BuyerID[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(SellerID = sellers),
                data.table(BuyerID = buyers),
                status))
    
  } else {
    
    # Retrieve the original problem
    tradeproblem <- lp_problems
    
    # Retrieve the original number of columns
    num_columns <- getNumColsGLPK(tradeproblem)
    
    ## Add the new columns
    addColsGLPK(tradeproblem, flowlength - (num_columns - nBuyers))
    
    ## Set the objective coefficients
    setObjCoefsGLPK(tradeproblem, colindex1, as.double(objective))
    
    ## Set the columns bounds
    setColsBndsGLPK(tradeproblem, 
                    colindex1, 
                    lb = as.double(rep(0, flowlength)), 
                    ub = as.double(rep(Inf, flowlength)))
    
    ## Load constraint matrix
    loadMatrixGLPK(tradeproblem, 
                   as.integer(length(constraintmatrix$i)), 
                   as.integer(constraintmatrix$i), 
                   as.integer(constraintmatrix$j), 
                   as.double(constraintmatrix$v))
    
    ## Set Simplex Control Parameters
    
    ## Solve the problem
    solveSimplexGLPK(tradeproblem)
    
    sellers <- SellerID[getRowsDualGLPK(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColsPrimGLPK(tradeproblem)[1:nBuyers]
    buyers <- BuyerID[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(SellerID = sellers),
                data.table(BuyerID = buyers),
                status))
  }
}

SolveFlowCLP <- function(pc_table, prodcg, conscg, lp_problems, firstrun = TRUE){
  # Get the number of variables
  SellerID <- prodcg[, SellerID]
  BuyerID <- conscg[, BuyerID]
  nSellers <- length(SellerID)
  nBuyers <- length(BuyerID)
  flowlength <- pc_table[,.N]
  sellerweights <- c(1,0.75,0.5,0.25)[prodcg[pc_table, tonquant, on = "SellerID"]]
  buyerweigths <- c(1,0.75,0.5,0.25)[conscg[pc_table, tonquant, on = "BuyerID"]]
  objweights <- sellerweights * buyerweigths
  
  # The objective function to minimize is just the sum of flows, 
  # penalty on slack variables for sellers and slack variables on buyers
  ## Slack Variables, Flow Variables
  objective <- c(rep(10, nBuyers), objweights)
  
  # Problem formulation:
  # S: Set of sellers
  # B: Set of buyers
  # f_sb: flow from seller s to buyer b
  # c_sb: cost of flow from seller s to buyer b
  # s_b: unfulfilled demand of buyer b
  # D_b: Demand of buyer b
  # C_s: Production capacity of seller s
  ## min sum_sb c_sb * f_sb + sum_b lambda * s_b
  ## s.t.
  ## 		0 <= sum_b f_sb <= C_s for all s
  ##		D_b <= s_b + sum_s f_sb for all b
  
  # The first constraint is to not oversell
  # Sellers are i and Buyers are j. Columns are flows and slack and first chunk of rows are sellers.
  # Column 1 will be i=1,j=1, Column 2 will be i=2, j=1, until Column=length(SellerID) where i=length(SellerID), j = 1
  
  rowindex1 <- match(pc_table$SellerID, SellerID)
  rhsoutput1 <- pc_table[,.N, .(SellerID)][prodcg, OutputCapacityTons, on = "SellerID"]
  
  rowindex2 <- match(pc_table$BuyerID, BuyerID)
  rhsoutput2 <- pc_table[,.N, .(BuyerID)][conscg, PurchaseAmountTons, on = "BuyerID"]
  rowindex2 <- rowindex2 + nSellers
  
  slackrowindex2 <- 1:nBuyers
  
  rowindex <- integer(length = length(rowindex1) + length(rowindex2) + length(slackrowindex2))
  rowindex[1:nBuyers] <- slackrowindex2 - 1
  rowindex[nBuyers + seq(1,2*flowlength,2)] <- rowindex1 - 1
  rowindex[nBuyers + seq(2,2*flowlength,2)] <- rowindex2 - 1
  
  colindex <- c(1:nBuyers, nBuyers + seq(1,2 * flowlength,2)) - 1
  colindex <- c(colindex, length(rowindex))
  
  clb <- as.double(rep(0, flowlength + nBuyers))
  cub <- as.double(rep(max(rhsoutput1, rhsoutput2), flowlength + nBuyers))
  
  rlb <- as.double(c(rep(0, nSellers), rhsoutput2))
  rub <- as.double(c(rhsoutput1, rep(Inf, nBuyers)))
  
  ar <- double(length = length(rowindex)) + 1
  
  rm(pc_table)
  gc()
  
  if(firstrun){
    
    ## Create a new problem
    tradeproblem <- initProbCLP()
    
    ## Set the direction of the objective function
    setObjDirCLP(tradeproblem, 1)
    
    ## Load model problem
    loadProblemCLP(tradeproblem, 
                   nBuyers + flowlength, 
                   nBuyers + nSellers, 
                   ia = rowindex, 
                   ja = colindex, 
                   ra = ar, 
                   rlb = rlb, 
                   rub = rub, 
                   obj_coef = objective, 
                   lb = clb, 
                   ub = cub)
    
    ## Set Simplex Control Parameters
    setLogLevelCLP(tradeproblem, 1)
    
    ## Solve the problem
    solveInitialPrimalCLP(tradeproblem)
    
    sellers <- SellerID[getRowDualCLP(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColPrimCLP(tradeproblem)[1:nBuyers]
    buyers <- BuyerID[buyersslack > 0]
    ifelse(sum(buyersslack) > 0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(SellerID = sellers), 
                data.table(BuyerID = buyers),
                lp_problems,
                status))
    
  } else {
    
    # Retrieve the original problem
    tradeproblem <- lp_problems
    
    # Retrieve the original number of columns
    num_columns <- getNumColsCLP(tradeproblem)
    
    # Number of new columns
    num_new_columns <- flowlength - (num_columns - nBuyers)
    clb_new <- clb[(num_columns + 1):(length(clb))]
    cub_new <- cub[(num_columns + 1):(length(cub))]
    obj_new <- objective[(num_columns + 1):(length(objective))]
    colst_new <- colindex[(num_columns + 1):(length(colindex))]
    colst_new <- colst_new - min(colst_new)
    row_start_index <- num_columns * 2 - nBuyers + 1
    rows_new <- rowindex[(row_start_index):(length(rowindex))]
    ar_new <- ar[(row_start_index):(length(ar))]
    
    ## Add the new columns
    addColsCLP(tradeproblem, num_new_columns, clb_new, cub_new, obj_new, colst_new, rows_new, ar_new)
    
    ## Solve the problem
    solveInitialPrimalCLP(tradeproblem)
    
    sellers <- SellerID[getRowDualCLP(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColPrimCLP(tradeproblem)[1:nBuyers]
    buyers <- BuyerID[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(SellerID = sellers),
                data.table(BuyerID = buyers),
                lp_problems,
                status))
    
  }
}

