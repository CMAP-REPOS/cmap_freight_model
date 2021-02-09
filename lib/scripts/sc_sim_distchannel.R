sc_sim_distchannel <- function(naics_set, TAZGCD){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_COST_CORES)
  
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_distchannel.txt")
  file.create(log_file_path)
  
  # Apply the distribution channel model to each market
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups")), file = log_file_path, append = TRUE)
    
    # Start creating input files for pmg for each group in the naics
    for(g in 1:groups){
      
      taskName <- paste0( "Apply_Distchannel_market-", market, "-group-", g, "-of-", groups)
      write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
      
      startAsyncTask(
        taskName, # Name of the task running
        future({ # Start the future processor
          # Write message to the console
          msg <- write(print(paste0(Sys.time(), " Applying Distribution Channel Model for: ", market, ",  Group: ", g)), file = log_file_path, append = TRUE)
          
          # Run apply_distchannel function
          output <- capture.output(apply_distchannel(market, g, TAZGCD))
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
          
          #check that cost files was create
          taskName <- asyncResults[["asyncTaskName"]]
          taskInfo <- data.table::data.table(namedCapture::str_match_named(taskName, "^.*market[-](?P<taskMarket>[^-]+)-group-(?P<taskGroup>[^-]+)-of-(?P<taskGroups>.*)$"))[1,]
          taskResult <- asyncResults[["taskResult"]]
          write(print(taskResult), file = log_file_path, append = TRUE)
          marketKey <- paste0("market-", taskInfo$taskMarket)
          groupoutputs <- marketInProcess[[marketKey]]
          if (is.null(groupoutputs)) {
            stop(
              paste0(
                "for taskInfo$taskMarket ",
                taskInfo$taskMarket,
                " marketInProcess[[taskInfo$taskMarket]] (groupoutputs) is NULL! "
              )
            )
          }
          
          groupKey <- paste0("group-", taskInfo$taskGroup)
          groupoutputs[[groupKey]] <- paste0(Sys.time(), ": Finished!")
          
          #don't understand why this is necessary but apparently have to re-store list
          marketInProcess[[marketKey]] <<- groupoutputs
          
          write(print(paste0(Sys.time(),": Finished ",taskName,
                             ", Elapsed time since submitted: ",
                             asyncResults[["elapsedTime"]],
                             " # of group results so far for this market=",
                             length(groupoutputs))),
                file = log_file_path,append = TRUE)
          
          if (length(groupoutputs) == taskInfo$taskGroups) {
            #delete market from tracked outputs
            marketInProcess[[marketKey]] <<- NULL
            write(print(paste0(Sys.time(),": Completed Processing Outputs of all ",
                               taskInfo$taskGroups," groups for market ",
                               taskInfo$taskMarket,". Remaining marketInProcess=",
                               paste0(collapse = ", ", names(marketInProcess)))), 
                  file = log_file_path, append = TRUE)
          } #end if all groups in naic are finished
        },
        debug = FALSE
      ) #end call to startAsyncTask
      processRunningTasks(wait = FALSE, debug = TRUE, maximumTasksToResolve = 1)
    }
  } # Finished making inputs to the PMG
  
  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_distchannel there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketsInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
}

apply_distchannel <- function(market, g, TAZGCD){
  
  # Load the workspace for this market and group
  load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  # Apply the distribution channel model
  print(paste(Sys.time(), "Applying distribution channel model to", market, "group", g))
  
  # call the distchannel function
  pc[pc_sim_distchannel(pc, prodcg, conscg, TAZGCD, 
                        distchannel_food = distchannel_food, 
                        distchannel_mfg = distchannel_mfg),
     distchannel := i.distchannel,
     on = c("SellerID", "BuyerID")]
  
  # Save the results
  save(pc, prodcg, conscg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  return(paste("Completed apply_distchannel for market:", market, ", group:", g))
}


# Distribution channel model
pc_sim_distchannel <- function(pc, prodcg, conscg, TAZGCD, distchannel_food, distchannel_mfg, calibration = NULL){
  # Define distchannel_food
  # Define distchannel_mfg
  
  ### Create variables used in the distribution channel model
  
  # Add required fields to pc
  pc[prodcg, 
     c("Seller.NAICS", "Seller.Size", "SCTG", "Production_zone") := 
       .(i.Seller.NAICS, i.Seller.Size, i.Commodity_SCTG, i.Production_zone),
     on = "SellerID"]
  
  pc[conscg,
     c("Buyer.NAICS", "Buyer.Size", "Consumption_zone") := 
       .(i.Buyer.NAICS, i.Buyer.Size , i.Consumption_zone),
     on = "BuyerID"]
  
  # Create employment and industry dummy variables
  pc[, c("emple49", "emp50t199", "empge200", "mfgind", "trwind", "whind") := 0L]
  pc[Buyer.Size <= 49, emple49 := 1]
  pc[Buyer.Size >= 50 & Buyer.Size <= 199, emp50t199 := 1]
  pc[Buyer.Size >= 200, empge200 := 1]
  
  pc[,Seller.NAICS2:=substr(Seller.NAICS,1,2)]
  pc[Seller.NAICS2 %in% 31:33, mfgind := 1]
  pc[Seller.NAICS2 %in% 48:49, trwind := 1]
  pc[Seller.NAICS2 %in% c(42, 48, 49), whind := 1]
  
  pc[,Buyer.NAICS2:=substr(Buyer.NAICS,1,2)]
  pc[Buyer.NAICS2 %in% 31:33, mfgind := 1]
  pc[Buyer.NAICS2 %in% 48:49, trwind := 1]
  pc[Buyer.NAICS2 %in% c(42, 48, 49), whind := 1]
  
  # Add the FAME SCTG category for comparison with calibration targets
  pc[, CATEGORY := famesctg[SCTG]]
  
  setkey(pc, Production_zone, Consumption_zone)
  
  # Add zone to zone distances
  pc[TAZGCD[, .(Production_zone, Consumption_zone, GCD)], 
          Distance := i.GCD,
          on = c("Production_zone", "Consumption_zone")]
  
  print(paste(Sys.time(), "Applying distribution channel model"))
  
  # Apply choice model of distribution channel and iteratively adjust the ascs
  # The model estimated for mfg products was applied to all other SCTG commodities
  inNumber <- nrow(pc[SCTG %in% c(1:9)])
  
  if (inNumber > 0) {
    
    # Sort on vars so simulated choice is ordered correctly
    model_vars_food <- c("CATEGORY", distchannel_food[TYPE == "Variable", unique(VAR)])
    model_ascs_food <- distchannel_food[TYPE == "Constant", unique(VAR)]
    setkeyv(pc, model_vars_food) #sorted on vars, calibration coefficients, so simulated choice is ordered correctly
    
    pc_food <- pc[SCTG %in% c(1:9),model_vars_food,with=FALSE]
    pc_food_weight <- pc[SCTG %in% 1:9,PurchaseAmountTons]
    
    df <- pc_food[, list(Start = min(.I), Fin = max(.I)), by = model_vars_food] #unique combinations of model coefficients
    
    df[, (model_ascs_food) := 1] #add 1s for constants to each group in df
    
    print(paste(Sys.time(), nrow(df), "unique combinations"))
    
    if(!is.null(calibration)){
      
      pc[SCTG %in% c(1:9), distchannel := predict_logit(df, distchannel_food, cal = distchan_cal, calcats = distchan_calcats, weight = pc_food_weight, path = file.path(model$inputdir,paste0(naics,"_g",g,"_model_distchannel_food_cal.csv")), iter = 4)]
      
    } else {
      
      pc[SCTG %in% 1:9, distchannel := predict_logit(df, distchannel_food)]
      
    }
    
  }
  
  print(paste(Sys.time(), "Finished ", inNumber, " for SCTG %in% c(1:9)"))
  
  ### Apply choice model of distribution channel for other industries
  
  # The model estimated for mfg products is applied to all other SCTG commoditie
  outNumber <- nrow(pc[!SCTG %in% c(1:9)])
  
  if (outNumber > 0) {
    
    # Sort on vars so simulated choice is ordered correctly
    model_vars_mfg <- c("CATEGORY", distchannel_mfg[TYPE == "Variable", unique(VAR)])
    model_ascs_mfg <- distchannel_mfg[TYPE == "Constant", unique(VAR)]
    
    setkeyv(pc, model_vars_mfg) #sorted on vars so simulated choice is ordered correctly
    
    pc_mfg <- pc[!SCTG %in% c(1:9),model_vars_mfg,with=FALSE]
    pc_mfg_weight <- pc[!SCTG %in% c(1:9),PurchaseAmountTons]
    
    df <- pc_mfg[, list(Start = min(.I), Fin = max(.I)), by = model_vars_mfg] #unique combinations of model coefficients
    
    df[, (model_ascs_mfg) := 1] #add 1s for constants to each group in df
    
    print(paste(Sys.time(), nrow(df), "unique combinations"))
    
    # Simulate choice -- with calibration if calibration targets provided
    if(!is.null(calibration)){
      
      pc[!SCTG %in% c(1:9), distchannel := predict_logit(df, distchannel_mfg, cal = distchannel_calibration, calcats = distchan_calcats, weight = pc_mfg_weight, path = file.path(model$inputdir,paste0(naics,"_g",g,"_model_distchannel_mfg_cal.csv")), iter=4)]
      
    } else {
      
      pc[!SCTG %in% 1:9, distchannel := predict_logit(df, distchannel_mfg)]
      
    }
    
  }
  
  print(paste(Sys.time(), "Finished ", outNumber, " for !SCTG %in% c(1:9)"))
  
  return(pc[,.(BuyerID, SellerID, distchannel)])
  
}


# Override the predict_logit function from rFreight package as there is problem of loading the reshape package in the future processors
predict_logit <- function (df, mod, cal = NULL, calcats = NULL, iter = 1) {
  alts <- max(mod$CHID)
  ut <- diag(alts)
  ut[upper.tri(ut)] <- 1
  if (is.numeric(df$CATEGORY)) 
    df[, `:=`(CATEGORY, paste0("x", CATEGORY))]
  cats <- unique(df$CATEGORY)
  mod <- data.table(reshape::expand.grid.df(mod, data.frame(CATEGORY = cats)))
  if (is.numeric(cal$CATEGORY)) 
    cal[, `:=`(CATEGORY, paste0("x", CATEGORY))]
  for (iters in 1:iter) {
    if (iters > 1 & !is.null(cal) & !is.null(calcats)) {
      sim <- sapply(cats, function(x) tabulate(simchoice[min(df$Start[df$CATEGORY == 
                                                                        x]):max(df$Fin[df$CATEGORY == x])], nbins = alts))
      sim <- sim/colSums(sim)
      if (length(unique(calcats$CHOICE)) < length(unique(calcats$CHID))) {
        sim <- cbind(calcats, sim)
        sim <- melt(sim, id.vars = c("CHOICE", "CHID"), 
                    variable.name = "CATEGORY")
        sim <- sim[, list(MODEL = sum(value)), by = list(CHOICE, 
                                                         CATEGORY)]
        sim <- merge(sim, cal, c("CATEGORY", "CHOICE"))
        sim[, `:=`(ascadj, log(TARGET/MODEL))]
        adj <- merge(sim, calcats, "CHOICE", allow.cartesian = TRUE)[, 
                                                                     list(CATEGORY, CHID, ascadj)]
      }
      if (length(unique(calcats$CHOICE)) > length(unique(calcats$CHID))) {
        caldat <- merge(cal[CATEGORY %in% cats], calcats, 
                        "CHOICE")
        caldat <- caldat[, list(TARGET = sum(TARGET)), 
                         by = list(CATEGORY, CHID)]
        sim <- data.table(CHID = 1:nrow(sim), sim)
        sim <- melt(sim, id.vars = c("CHID"), variable.name = "CATEGORY")
        sim <- merge(sim, caldat, c("CATEGORY", "CHID"))
        sim[, `:=`(ascadj, log(TARGET/value))]
        adj <- sim[, list(CATEGORY, CHID, ascadj)]
      }
      if (length(unique(calcats$CHOICE)) == length(unique(calcats$CHID))) {
        stop("Need to implment calibration for same calcats as choice alts")
      }
      mod <- merge(mod, adj, c("CATEGORY", "CHID"))
      mod[TYPE == "Constant", `:=`(COEFF, COEFF + ascadj)]
      mod[, `:=`(ascadj, NULL)]
    }
    utils <- lapply(cats, function(y) sapply(1:alts, function(x) exp(rowSums(sweep(df[CATEGORY == 
                                                                                        y, mod[CHID == x & CATEGORY == y, VAR], with = F], 
                                                                                   2, mod[CHID == x & CATEGORY == y, COEFF], "*")))))
    if(nrow(df)>1){
      utils <- lapply(1:length(cats), function(x) (utils[[x]]/rowSums(utils[[x]])) %*% ut)
    } else {
      utils <- lapply(1:length(cats), function(x) (utils[[x]]/sum(utils[[x]])) %*% ut)
    }
    utils <- do.call("rbind", utils)
    temprand <- runif(max(df$Fin))
    simchoice <- unlist(lapply(1:nrow(df), function(x) 1L + 
                                 findInterval(temprand[df$Start[x]:df$Fin[x]], utils[x,])))
  }
  return(simchoice)
}
