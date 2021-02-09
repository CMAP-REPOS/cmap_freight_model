sc_sim_markets <- function(naics_set){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_COST_CORES) 
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_market.txt")
  file.create(log_file_path)
  
  # Read the outputs from firm synthesis for this market, combine, and allocate to groups
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    sprod <- ifelse(naics_set$Split_Prod[market_number], 1, 0)
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups. sprod: ", sprod)), file = log_file_path, append = TRUE)
    taskName <- paste0( "Create_Markets_market-", market)
    write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
      
    startAsyncTask(
      taskName, # Name of the task running
      future({ # Start the future processor
          
        # Write message to the console
        msg <- write(print(paste0(Sys.time(), " Allocating groups for: ", market)), file = log_file_path, append = TRUE)
          
        # Run create_pmg_sample_groups function
        output <- capture.output(create_pmg_sample_groups(market, groups, sprod))
        return(output) #no need to return anything to future task handler
      }),
        callback = function(asyncResults) {
          # asyncResults is: list(asyncTaskName,
          #                        taskResult,
          #                        startTime,
          #                        endTime,
          #                        elapsedTime,
          #                        caughtError,
          #                        caughtWarning)
          
          #check that files were created
          taskName <- asyncResults[["asyncTaskName"]]
          taskInfo <- data.table::data.table(namedCapture::str_match_named(taskName, "^.*market[-](?P<taskMarket>.*)$"))[1,]
          taskResult <- asyncResults[["taskResult"]]
          write(print(taskResult), file = log_file_path, append = TRUE)
          marketKey <- paste0("market-", taskInfo$taskMarket)
          marketoutputs <- marketInProcess[[marketKey]]
          if (is.null(marketoutputs)) {
            stop(
              paste0(
                "for taskInfo$taskMarket ",
                taskInfo$taskMarket,
                " marketInProcess[[taskInfo$taskMarket]] (marketoutputs) is NULL! "
              )
            )
          }
          
          marketKey <- paste0("market-", taskInfo$taskMarket)
          marketoutputs[[marketKey]] <- paste0(Sys.time(), ": Finished!")
          
          #don't understand why this is necessary but apparently have to re-store list
          marketInProcess[[marketKey]] <<- marketoutputs
          
          market_file_path <- file.path(SCENARIO_OUTPUT_PATH,
                                      paste0(taskInfo$taskMarket,".Rdata"))
          market_file_exists <- file.exists(market_file_path)
          
          write(print(paste0(Sys.time(),": Finished ",taskName,
                             ", Elapsed time since submitted: ",
                             asyncResults[["elapsedTime"]],
                             ", market_file_exists: ",market_file_exists)),
                file = log_file_path,append = TRUE)
          
          if (!market_file_exists) {
            msg <- paste("***ERROR*** Did not find expected market file '",
                         market_file_path,"'.")
            
            write(print(msg), file = log_file_path, append = TRUE)
            stop(msg)
          }
          if (length(marketoutputs) == 1) {
            #delete Market from tracked outputs
            marketInProcess[[marketKey]] <<- NULL
            write(print(paste0(Sys.time(),": Completed Processing Outputs of Market ",
                               taskInfo$taskMarket,". Remaining marketInProcess=",
                               paste0(collapse = ", ", names(marketInProcess)))), 
                  file = log_file_path, append = TRUE)
            } 
        },
        debug = FALSE
      ) #end call to startAsyncTask
      processRunningTasks(wait = FALSE, debug = TRUE, maximumTasksToResolve = 1)
    }# Finished creating sample groups
  
  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_markets there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
  
}

# Add group designations to the lists of consumers and producers from each market
create_pmg_sample_groups <- function(market,groups,sprod){
  
  # Load the consumers and producers tables for this market
  load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
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
        # scale both down but this is likely excessive. 
        # TODO: Solve problem for lower scaling.
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
  
  # Save consc and prodc into a workspace
  save(consc, prodc, consc_orig, prodc_orig, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
  
  return(paste("Completed create_pmg_sample_groups for market:", market))
}


