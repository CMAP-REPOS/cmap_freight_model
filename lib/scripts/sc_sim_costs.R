sc_sim_costs <- function(naics_set){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_COST_CORES)
  
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_costs.txt")
  file.create(log_file_path)
  
  # Format and write out the costs files for each markets
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups")), file = log_file_path, append = TRUE)
    
    # Start creating costs files for pmg for each group in the market
    for(g in 1:groups){
      
      taskName <- paste0( "Create_Costs_market-", market, "-group-", g, "-of-", groups)
      write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
      
      startAsyncTask(
        taskName, # Name of the task running
        future({ # Start the future processor
          # Write message to the console
          msg <- write(print(paste0(Sys.time(), " Creating Costs Inputs File for: ", market, ",  Group: ", g)), file = log_file_path, append = TRUE)
          
          # Run create_costs function
          output <- capture.output(create_costs(market, g))
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
          
          costs_file_path <- file.path(SCENARIO_OUTPUT_PATH,
                                       paste0(taskInfo$taskMarket,"_g",
                                              taskInfo$taskGroup,".costs.csv"))
          cost_file_exists <- file.exists(costs_file_path)
          
          write(print(paste0(Sys.time(),": Finished ",taskName,
                             ", Elapsed time since submitted: ",
                             asyncResults[["elapsedTime"]],
                             ", cost_file_exists: ",cost_file_exists,
                             " # of group results so far for this naics=",
                             length(groupoutputs))),
                file = log_file_path,append = TRUE)
          
          if (!cost_file_exists) {
            msg <- paste("***ERROR*** Did not find expected costs file '",
                         costs_file_path,"'.")
            
            write(print(msg), file = log_file_path, append = TRUE)
            stop(msg)
          }
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
      "At end of sc_sim_costs there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketsInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
}

create_costs <- function(market, g){
  
  # load the workspace for this market and group
  load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  pc[, Attribute1_UnitCost := minc / PurchaseAmountTons]
  pc[, Attribute2_ShipTime := time / (60 * 24)] #Convert from minutes to days
  
  save(pc, prodcg, conscg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  fwrite(pc[,.(sellerid = SellerID,	buyerid = BuyerID,	Attribute2_ShipTime,	cost,	Attribute1_UnitCost)],
         file = file.path(SCENARIO_OUTPUT_PATH,paste0(market, "_g", g, ".costs.csv")))
  
  return(paste("Completed create_costs for market:", market, ", group:", g))
}

