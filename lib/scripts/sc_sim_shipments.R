sc_sim_shipments <- function(naics_set){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_COST_CORES) 
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_shipments.txt")
  file.create(log_file_path)
  
  # Combining the outputs from each group's PMG run
  for(market_number in 1:nrow(naics_set)){
    naics <- as.character(naics_set$NAICS[market_number])
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups")), file = log_file_path, append = TRUE)
    
    # Start processing output files from pmg for each group in the market
    for(g in 1:groups){
      
      taskName <- paste0( "Process_PMG_Outputs_market-", market, "-group-", g, "-of-", groups)
      write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
      
      startAsyncTask(
        taskName, # Name of the task running
        future({ # Start the future processor
          
          # Write message to the console
          msg <- write(print(paste0(Sys.time(), " Processing Outputs from PMG for: ", market, ",  Group: ", g)), file = log_file_path, append = TRUE)
          
          # Run process_pmg_out function
          output <- process_pmg_out(naics, market, g)
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
          
          # get the output
          taskName <- asyncResults[["asyncTaskName"]]
          taskInfo <- data.table::data.table(namedCapture::str_match_named(taskName, "^.*market[-](?P<taskMarket>[^-]+)-group-(?P<taskGroup>[^-]+)-of-(?P<taskGroups>.*)$"))[1,]
          
          # Task results is the pc_pairs table passed back by the process_pmg_out function
          taskResult <- asyncResults[["taskResult"]]
          
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
          groupoutputs[[groupKey]] <- taskResult
          
          #don't understand why this is necessary but apparently have to re-store list
          marketInProcess[[marketKey]] <<- groupoutputs
          
          write(print(paste0(Sys.time(),": Finished ",taskName,
                             ", Elapsed time since submitted: ",
                             asyncResults[["elapsedTime"]],
                             " # of group results so far for this market=",
                             length(groupoutputs))),
                file = log_file_path,append = TRUE)
          
          ### TODO uncomment this to delete the three PMG input files...
          # write(print(paste0(Sys.time(), ": Deleting Inputs: ", taskInfo$taskMarket,
          #                    " Group: ", taskInfo$taskGroup)),
          #       file=log_file_path, append=TRUE)
          # 
          # file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(taskInfo$taskMarket, "_g", taskInfo$taskGroup, ".costs.csv")))
          # file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(taskInfo$taskMarket, "_g", taskInfo$taskGroup, ".buy.csv")))
          # file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(taskInfo$taskMarket, "_g", taskInfo$taskGroup, ".sell.csv")))
          
          if (length(groupoutputs) == taskInfo$taskGroups) {
            
            # convert output list to one table, add to workspace, and save
            pc_pairs <- rbindlist(groupoutputs)
            
            marketRDataFile <- file.path(SCENARIO_OUTPUT_PATH, paste0(taskInfo$taskMarket, ".Rdata"))
            load(marketRDataFile)
            
            write(print(paste0(Sys.time(), ": loaded and then re-saved with'", marketRDataFile,
                               "'", " nrow(consc)=", nrow(consc), " nrow(prodc)=", nrow(prodc),
                               " nrow(pc_pairs)=", nrow(pc_pairs))))
            
            save(consc, prodc, pc_pairs, file = marketRDataFile)
            rm(consc, prodc, pc_pairs)
            
            # delete market from tracked outputs
            marketInProcess[[marketKey]] <<- NULL
            write(print(paste0(Sys.time(),": Completed Processing Outputs of all ",
                               taskInfo$taskGroups," groups for market ",
                               taskInfo$taskMarket,". Remaining marketInProcess=",
                               paste0(collapse = ", ", names(marketInProcess)))), 
                  file = log_file_path, append = TRUE)
          } #end if all groups in market are finished
        },
        debug = FALSE
      ) #end call to startAsyncTask
      processRunningTasks(wait = FALSE, debug = TRUE, maximumTasksToResolve = 1)
    }
  } # Finished processing PMG outputs
  
  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_shipments there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketsInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  # Now that the individual groups are combined, combine together and return the complete pairs table
  marketpairs <- list()
  
  for (market_number in 1:nrow(naics_set)){

    market <- as.character(naics_set$Market[market_number])
    print(paste0("Reading Outputs from Industry ", market_number, ": ", market))
    
    load(file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
    marketpairs[[market_number]] <- pc_pairs
    
  } 

  pc_pairs <- rbindlist(marketpairs)
  rm(marketpairs)
  gc()
  
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
  
  # Load the group workspace
  load(file.path(SCENARIO_OUTPUT_PATH, 
                 paste0(market, "_g", g, ".Rdata")))

  # Merge the trades from PMG with the saved pc table
  pc_pairs <- merge(pc, pmgout, by = c("BuyerID", "SellerID"))
  
  # Add market identifyers and the Buyer.NAICS to the pc_pairs table
  pc_pairs[, c("NAICS", "Market", "Group") := .(naics, market, g)]
  pc_pairs[conscg, Buyer.NAICS := i.Buyer.NAICS, on = "BuyerID"]
  
  # Save the workspace
  save(conscg, prodcg, pc, pc_pairs, 
       file = file.path(SCENARIO_OUTPUT_PATH, 
                        paste0(market, "_g", g, ".Rdata")))
  
  # Return the pairs table so that it can be combined with the others from this group for this market
  return(pc_pairs)
}


