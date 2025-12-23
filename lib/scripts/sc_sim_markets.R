sc_sim_markets <- function(naics_set){
  
  t0 <- Sys.time()
  
  if(USER_COST_CORES > 1){
    require(parallel)
    
    clust <- makeCluster(USER_COST_CORES)
    
    clusterCall(clust, 
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH)
    
    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
    clusterExport(clust, 
                  c("create_pmg_sample_groups",
                    "naics_set"), 
                  envir = environment())
    
    naicslist <- parLapply(clust, 
                           1:nrow(naics_set), 
                           function(x){
                             create_pmg_sample_groups(market = as.character(naics_set$Market[x]),
                                                      groups = naics_set$groups[x],
                                                      sprod = ifelse(naics_set$Split_Prod[x], 1, 0))
                           })
    stopCluster(clust)
    
  } else {
    
    naicslist <- lapply(1:nrow(naics_set), 
                        function(x){
                          create_pmg_sample_groups(market = as.character(naics_set$Market[x]),
                                                   groups = naics_set$groups[x],
                                                   sprod = ifelse(naics_set$Split_Prod[x], 1, 0))
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_markets.csv"))
  naics_missing <- naics_set[!Market %in% naics_completed]$Market
  if(length(naics_missing) > 0) cat("Markets missing from market creation in sc_sim_markets: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  return(naics_set)
  
}

# Add group designations to the lists of consumers and producers from each market
create_pmg_sample_groups <- function(market,groups,sprod){
  
  # Load the consumers and producers tables for this market
  consc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_consc.fst")),
                     as.data.table = TRUE)
  prodc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_prodc.fst")),
                     as.data.table = TRUE)
  
  setkey(consc, Size)
  setkey(prodc, Size)
  
  # Bucket round the output capacity and purchase amounts
  # Round by FAF zone so that spatial distribution by FAF zone is maintained
  # Remove zero value producers and consumers after rounding
  # Recalculate the appropriate number of groups to divide the market into
  prodc_orig <- copy(prodc)
  consc_orig <- copy(consc)
  
  prodc[, OutputCapacityTons := as.numeric(bucketRound(OutputCapacityTons)), by = FAFZONE]
  prodc <- prodc[OutputCapacityTons > 0.5]
  
  consc[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = FAFZONE]
  consc <- consc[PurchaseAmountTons > 0.5]
  
  # Number of groups = ( number of consumers * BASE_SUPPLIERS_PER_BUYER ) / BASE_COMBINATION_THRESHOLD
  groups <- ceiling( consc[,.N] * BASE_SUPPLIERS_PER_BUYER / BASE_COMBINATION_THRESHOLD)
  
  # add group id and number of groups to consc and prodc; if not splitting producers assign 0
  consc[, c("numgroups", "group") := .(groups, rep(1:groups, length.out = .N))]
  
  if(sprod==1){
    prodc[, c("numgroups", "group") := .(groups, rep(1:groups, length.out = .N))]
  } else {
    prodc[, c("numgroups", "group") := .(groups, 0)]
  }
  
  # Check that for all groups the capacity > demand
  # Need to consider not just total demand vs. capacity but including consideration of
  # domestic production and consumption vs. immport and exports too:
  # 1. Domestic production + Foreign production - Foreign consumption > Domestic consumption
  # 2. Domestic production + Foreign production - Domestic consumption > Foreign consumption
  # 3. Domestic production > Foreign consumption (needed for cases of high foreign production)
  
  # Tag the producers and consumes as domestic or foreign
  consc[, DomFor := ifelse(FAFZONE < 800, "Domestic", "Foreign")]
  prodc[, DomFor := ifelse(FAFZONE < 800, "Domestic", "Foreign")]
  
  check_market <- TRUE
  
  while(check_market){
    
    # Check the three cases 
    pdom <- sum(prodc[DomFor == "Domestic"]$OutputCapacityTons)
    pfor <- sum(prodc[DomFor == "Foreign"]$OutputCapacityTons)
    cdom <- sum(consc[DomFor == "Domestic"]$PurchaseAmountTons)
    cfor <- sum(consc[DomFor == "Foreign"]$PurchaseAmountTons)
    
    cdom.ratio <- ifelse(cdom > 0, (pdom + pfor - cfor)/cdom, BASE_OUTPUT_PURCHASE_RATIO)
    cfor.ratio <- ifelse(cfor > 0, (pdom + pfor - cdom)/cfor, BASE_OUTPUT_PURCHASE_RATIO)
    pdom.cfor.ratio <- ifelse(cfor > 0, pdom/cfor, BASE_OUTPUT_PURCHASE_RATIO)
    
    # any too small?
    if(any(c(cdom.ratio, cfor.ratio, pdom.cfor.ratio) < BASE_OUTPUT_PURCHASE_RATIO)){ 
      
      # estimate the maximum values of cfor and cdom that allow all three tests to be met
      beta.pd.cf.r <- ifelse(cfor > 0, pdom/(BASE_OUTPUT_PURCHASE_RATIO * cfor), 1)
      beta.cf.r <- ifelse(cfor > 0, (pdom + pfor - cdom)/(BASE_OUTPUT_PURCHASE_RATIO * cfor), 1)
      beta.cd.r <- ifelse(cfor > 0, (pdom + pfor - BASE_OUTPUT_PURCHASE_RATIO * cdom)/cfor, 1)
      alpha.cf.r <- ifelse(cdom > 0, (pdom + pfor - BASE_OUTPUT_PURCHASE_RATIO * cfor)/cdom, 1) 
      alpha.cd.r <- ifelse(cdom > 0, (pdom + pfor - cfor)/(BASE_OUTPUT_PURCHASE_RATIO * cdom), 1)
      
      # calculate minimums, with protection against negative values
      beta.min <- min(beta.pd.cf.r, beta.cf.r, beta.cd.r)
      if(beta.min < 0) beta.min <- 0.05
      alpha.min <- min(alpha.cf.r, alpha.cd.r)
      if(alpha.min < 0) alpha.min <- 0.05
      
      if(beta.min < 1 & alpha.min >= 1) consc[DomFor == "Foreign", 
                                              PurchaseAmountTons := beta.min * PurchaseAmountTons]
      
      if(beta.min >= 1 & alpha.min < 1) consc[DomFor == "Domestic", 
                                              PurchaseAmountTons := alpha.min * PurchaseAmountTons]
      
      if(beta.min < 1 & alpha.min < 1) {
        # scale both down
        consc[DomFor == "Foreign", 
              PurchaseAmountTons := beta.min * PurchaseAmountTons]
        
        consc[DomFor == "Domestic", 
              PurchaseAmountTons := alpha.min * PurchaseAmountTons]
      }
    } else {
      check_market <- FALSE
    }
  }
  
  # too much in just some groups - shuffle consumers between groups to even out
  if(sprod==1 & groups > 1){
    
    prodconsgroup <- merge(prodc[, .(OutputCapacityTons = sum(OutputCapacityTons), Producers = .N), by = group],
                           consc[, .(PurchaseAmountTons = sum(PurchaseAmountTons), Consumers = .N), by = group],
                           by = "group")
    
    prodconsgroup[, prodconsratio := OutputCapacityTons/PurchaseAmountTons]
    prodconsgroup[, consexcess := PurchaseAmountTons - OutputCapacityTons]
    
    iter <- 1 #counter to break in case something goes wrong and we get into an endless loop
    
    while (nrow(prodconsgroup[prodconsratio < 1]) > 0){
      
      mingroup <- prodconsgroup[which.min(prodconsratio)]$group
      maxgroup <- prodconsgroup[which.max(prodconsratio)]$group
      maxgroupprod <- prodconsgroup[Producers>1][which.max(prodconsratio)]$group
      reqtomove <- prodconsgroup[mingroup]$consexcess
      #leave at least one consumer in the group
      maxsample <- nrow(consc[group == mingroup]) - 1 
      
      if (maxsample > 0){ 
        
        # move consumers to other groups
      
        print(paste("Moving Consumers:", mingroup, "to", maxgroup, reqtomove, maxsample))
        
        # create a sample frame of the first maxsample records and identify a set that is just over reqtomove
        sampsellers <- sample.int(maxsample)
        constomove <- consc[group == mingroup][sampsellers, .(BuyerID,PurchaseAmountTons)]
        constomove[, PATCum := cumsum(PurchaseAmountTons)]
        threshold <- sum(constomove$PurchaseAmountTons) - reqtomove
        consc[BuyerID %in% constomove[PATCum > threshold]$BuyerID, group := maxgroup]
        
      } else { 
        
        # no consumers left to move from this group so move some producers to it -- opposite direction
        
        maxsampleprod <- nrow(prodc[group == maxgroupprod]) - 1
        print(paste("Moving Producers:", maxgroupprod, "to", mingroup, reqtomove, maxsampleprod))
        
        # create a sample frame of the first maxsample records and identify a set that is just over reqtomove
        sampbuyers <- sample.int(maxsampleprod)
        prodstomove <- prodc[group == maxgroupprod][sampbuyers, .(SellerID,OutputCapacityTons)]
        prodstomove[, OCTCum := cumsum(OutputCapacityTons)]
        threshold <- sum(prodstomove$OutputCapacityTons) - reqtomove
        prodc[SellerID %in% prodstomove[OCTCum > threshold]$SellerID, group := mingroup]
      }
      
      prodconsgroup <- merge(prodc[, .(OutputCapacityTons = sum(OutputCapacityTons), Producers = .N), by = group],
                             consc[, .(PurchaseAmountTons = sum(PurchaseAmountTons), Consumers = .N), by = group], 
                             by="group")
      
      prodconsgroup[, prodconsratio := OutputCapacityTons/PurchaseAmountTons]
      prodconsgroup[, consexcess := PurchaseAmountTons - OutputCapacityTons]
      
      iter <- iter + 1
      if(iter==100){
        
        # break out of the loop. This should never be necessary but here to stop endless loops. 
        # Groups with excess consumption requirements will potentially run slowly
        break
      }
    }
    
    # check in the individual groups whether there is also balance for 
    # domestic and import/export production and scale down consumption if needed
    for(g in 1:groups){
      # Check the three cases 
      pdom <- sum(prodc[group == g & DomFor == "Domestic"]$OutputCapacityTons)
      pfor <- sum(prodc[group == g & DomFor == "Foreign"]$OutputCapacityTons)
      cdom <- sum(consc[group == g & DomFor == "Domestic"]$PurchaseAmountTons)
      cfor <- sum(consc[group == g & DomFor == "Foreign"]$PurchaseAmountTons)
      
      cdom.ratio <- ifelse(cdom > 0, (pdom + pfor - cfor)/cdom, BASE_OUTPUT_PURCHASE_RATIO)
      cfor.ratio <- ifelse(cfor > 0, (pdom + pfor - cdom)/cfor, BASE_OUTPUT_PURCHASE_RATIO)
      pdom.cfor.ratio <- ifelse(cfor > 0, pdom/cfor, BASE_OUTPUT_PURCHASE_RATIO)
      
      # any too small?
      if(any(c(cdom.ratio, cfor.ratio, pdom.cfor.ratio) < BASE_OUTPUT_PURCHASE_RATIO)){ 
        
        # estimate the maximum values of cfor and cdom that allow all three tests to be met
        beta.pd.cf.r <- ifelse(cfor > 0, pdom/(BASE_OUTPUT_PURCHASE_RATIO * cfor), 1)
        beta.cf.r <- ifelse(cfor > 0, (pdom + pfor - cdom)/(BASE_OUTPUT_PURCHASE_RATIO * cfor), 1)
        beta.cd.r <- ifelse(cfor > 0, (pdom + pfor - BASE_OUTPUT_PURCHASE_RATIO * cdom)/cfor, 1)
        alpha.cf.r <- ifelse(cdom > 0, (pdom + pfor - BASE_OUTPUT_PURCHASE_RATIO * cfor)/cdom, 1) 
        alpha.cd.r <- ifelse(cdom > 0, (pdom + pfor - cfor)/(BASE_OUTPUT_PURCHASE_RATIO * cdom), 1)
        
        beta.min <- min(beta.pd.cf.r, beta.cf.r, beta.cd.r, na.rm = TRUE)
        if(beta.min < 0) beta.min <- 0.05
        alpha.min <- min(alpha.cf.r, alpha.cd.r)
        if(alpha.min < 0) alpha.min <- 0.05
        
        if(beta.min < 1 & alpha.min >= 1) consc[group == g & DomFor == "Foreign", 
                                                PurchaseAmountTons := beta.min * PurchaseAmountTons]
        
        if(beta.min >= 1 & alpha.min < 1) consc[group == g & DomFor == "Domestic", 
                                                PurchaseAmountTons := alpha.min * PurchaseAmountTons]
        
        if(beta.min < 1 & alpha.min < 1) {
          # scale both down but this is likely excessive. 
          # TODO: Solve problem for lower scaling.
          consc[group == g & DomFor == "Foreign", 
                PurchaseAmountTons := beta.min * PurchaseAmountTons]
          
          consc[group == g & DomFor == "Domestic", 
                PurchaseAmountTons := alpha.min * PurchaseAmountTons]
        }
      }
    }
  }
  
  # for casese where the producers are not being split allow consumers to buy all from one producer
  if(sprod==0) consc[, SingleSourceMaxFraction := 1.0]
  
  # after scaling of output capacity and purchase amounts, integerize the values
  # use bucket rounding by FAF zone and grouo and then remove any zero producers or consumers
  prodc[, OutputCapacityTons := bucketRound(OutputCapacityTons), by = .(FAFZONE, group)]
  prodc <- prodc[OutputCapacityTons >= 1L]
  
  consc[, PurchaseAmountTons := bucketRound(PurchaseAmountTons), by = .(FAFZONE, group)]
  consc <- consc[PurchaseAmountTons >= 1L]
  
  # Save consc and prodc
  write_fst(consc, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_consc.fst")))
  write_fst(prodc, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_prodc.fst")))
  
  return(market)
}


