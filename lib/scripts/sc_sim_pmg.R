sc_sim_pmg <- function(naics_set){

  # Run the PMGs
  # Write out the PMG ini file
  fwrite(PMGParameters[variable != "pmglogging"], file = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
         sep = "=", row.names = FALSE, col.names = FALSE)
  
  pmgparameters.writelog <- ifelse(PMGParameters[PMGParameters$variable == "pmglogging"]$value == 1, TRUE, FALSE)
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_PMG_CORES)
  
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_pmg.txt")
  file.create(log_file_path)
  
  # Format and write out the costs files for each markets
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups")), file = log_file_path, append = TRUE)
    
    #loop over the groups and run the games, and process outputs
    for (g in 1:groups) {
      
      # Remove old outputs and log files if they exist
      if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))){
        file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))
      }

      # Delete Market_gX.txt file if exists from prior run as well
      if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market,"_g", g, ".txt")))){
        file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".txt")))
      }

      taskName <- paste0( "RunPMG_market-", market, "-group-", g, "-of-", groups)
      
      write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", 
                         getNumberOfRunningTasks(), " currently running tasks")), 
            file = log_file_path, 
            append = TRUE)
      
      startAsyncTask(taskName,# Name of the task running
                     future({ # Start the future processor
                       # Write message to the console
                       msg <- write(print(paste0(Sys.time(), " Running PMG game for: ", market, ",  Group: ", g)), 
                                    file = log_file_path, 
                                    append = TRUE)
                       
                       #call to runPMG to run the game
                       runPMG(market, 
                              g, 
                              writelog = pmgparameters.writelog, 
                              wait = TRUE, 
                              pmgexe = file.path(SYSTEM_PMG_PATH,"pmg.exe"),
                              inipath = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
                              inpath = SCENARIO_OUTPUT_PATH, 
                              outpath = SCENARIO_OUTPUT_PATH,
                              logpath = SCENARIO_OUTPUT_PATH)
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
          marketKey <- paste0("market-", taskInfo$taskMarket)
          groupoutputs <- marketInProcess[[marketKey]]
          groupKey <- paste0("group-", taskInfo$taskGroup)
          groupoutputs[[groupKey]] <- paste0(Sys.time(), ": Finished!")
          
          #don't understand why this is necessary but apparently have to re-store list
          marketInProcess[[marketKey]] <<- groupoutputs
          
          expectedOutputFile <- file.path(SCENARIO_OUTPUT_PATH, 
                                          paste0(taskInfo$taskMarket, 
                                                 "_g", taskInfo$taskGroup, ".out.csv"))
          
          expectedOutputFile_exists <- file.exists(expectedOutputFile)
          
          write(print(paste0(Sys.time(),": Finished ",taskName,
                             ", Elapsed time since submitted: ",
                             asyncResults[["elapsedTime"]],
                             ", expectedOutputFile_exists: ",expectedOutputFile_exists,
                             " # of group results so far for this market=",
                             length(groupoutputs))),
                file = log_file_path,append = TRUE)
          
          if (!expectedOutputFile_exists) {
            msg <- paste("***ERROR*** Did not find expected PMG output file '",
                         expectedOutputFile,"'.")
            write(print(msg), file = log_file_path, append = TRUE)
            stop(msg)
          }
          
      if (length(groupoutputs) == taskInfo$taskGroups) {
        #delete market from tracked outputs
        marketInProcess[[marketKey]] <<- NULL
        write(print(paste0(Sys.time(),": Completed Running PMG for all ",
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
  } # Finished running PMG
  
  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_pmg there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketsInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
}

#' Builds the system call to the PMG application and runs the application
#'
#' Builds the systems call including the command line options to run the PMG
#' application for a particular NAICs market and group sample from within the
#' full set of buyers and sellers in that NAICS market.
#' 
#' @param naics_io_code BEA io code for the commodity to be run, i.e., that matches with the filenaming used for the buy/sell/costs files (character string).
#' @param groupnum is the sample group numbers for the group to be run, i.e., that matches with the numbering used for the buy/sell/costs files (integer).
#' @param writelog TRUE/FALSE to indicate whether to capture standard output from the PMG application is a text file.
#' @param invisible TRUE/FALSE to indicate whether to show the command window or not.
#' @param wait TRUE/FALSE to indicate whether to R should wait for the PMG application to finish, or (if false) should run the PMG application asynchronously.
#' @param pmgexe Path to the pmg executable, defaults to "./PMG/pmg.exe"
#' @param inipath Path to the ini file, defaults to "./PMG/PMG.ini"
#' @param inpath Path to the PMG inputs folder, defaults to "./outputs"
#' @param outpath Path to the PMG outputs folder, defaults to "./outputs"
#' @param logpath Path to the log file folder, defaults to "./outputs"
#' @keywords PMG
#' @export
#' @examples
#' \dontrun{
#' runPMG(naics,g,writelog=FALSE,wait=TRUE)
#' }

runPMG <- function(naics_io_code,groupnum=NA,writelog=FALSE,invisible=TRUE,wait=FALSE,
                   pmgexe="./PMG/pmg.exe",inipath="./PMG/pmg.ini",inpath="./outputs",outpath="./outputs",
                   logpath="./outputs"){
  
  #location of PMG executable: in the PMG folder, called PMG.exe
  pmgexe <- gsub("/","\\",pmgexe,fixed=TRUE)
  
  #command line options
  # 1.  Specify ini file path:
  #     -i C:\path\to\file\pmg.ini 
  inipath <- gsub("/","\\",inipath,fixed=TRUE)
  
  # 2. specify data input and output file name prefixes 
  # -p naics_io_code
  ioprefix <- naics_io_code
  if(!is.na(groupnum)) ioprefix <- paste0(naics_io_code,"_g",groupnum)
  
  # 3. specify data directory path for input files files 
  # location of naics_io_code.buy.csv, naics_io_code.sell.csv and  naics_io_code.costs.csv
  # -d C:\path\to\inputs
  inpath <- gsub("/","\\",inpath,fixed=TRUE)
  
  # 4. specify directory path for output file
  # locations of naics_io_code.out.csv
  # -o C:\path\to\outputs
  outpath <- gsub("/","\\",outpath,fixed=TRUE)
  #
  # divert stout to log for this run
  logcall <- ""
  logpath <- gsub("/","\\",logpath,fixed=TRUE)
  if(writelog) logcall <- paste0(logpath,"\\",naics_io_code,"_g",groupnum,".txt")
  
  # 
  #build system call:
  system2(pmgexe,
          args = paste("-i",inipath,"-p",ioprefix,"-d",inpath,"-o",outpath), 
          stdout = logcall,  
          invisible = invisible, 
          wait = wait)
  
}

