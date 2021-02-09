sc_sim_buy_sell <- function(naics_set){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_COST_CORES) 
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_buy_sell.txt")
  file.create(log_file_path)
  
  # Read the outputs from firm synthesis for this market, combine, and allocate to groups
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    sprod <- ifelse(naics_set$Split_Prod[market_number], 1, 0)
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups. sprod: ", sprod)), file = log_file_path, append = TRUE)
    taskName <- paste0( "Create_Buy_Sell_market-", market, "-groups-", groups)
    write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
    
    startAsyncTask(
      taskName, # Name of the task running
      future({ # Start the future processor
        
        # Write message to the console
        msg <- write(print(paste0(Sys.time(), " Creating buy and sell files for: ", market)), file = log_file_path, append = TRUE)
        
        # Run create_buy_sell function
        output <- capture.output(create_buy_sell(market, groups, sprod))
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
        taskInfo <- data.table::data.table(namedCapture::str_match_named(taskName, "^.*market[-](?P<taskMarket>[^-]+)-groups-(?P<taskGroups>.*)$"))[1,]
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
        
        # check that all of the outputs were produced
        market_file_path <- file.path(SCENARIO_OUTPUT_PATH,
                                      paste0(taskInfo$taskMarket, "_g", taskInfo$taskGroups,".Rdata"))
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
  }# Finished creating buy and sell files
  
  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_buy_sell there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
  
}

create_buy_sell <- function(market, groups, sprod){
  
  # load the market
  load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
  
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
    
    # Save conscg and prodcg into a workspace
    save(conscg, prodcg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
    
  }
  
  return(paste("Completed create_buy_sell for market:", market))
  
}


