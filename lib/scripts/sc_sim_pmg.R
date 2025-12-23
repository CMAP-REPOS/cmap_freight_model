sc_sim_pmg <- function(naics_set){

  t0 <- Sys.time()
  
  # Run the PMGs:
  # Two methods available.
  # (1) C++ code if USER_PMG_R is FALSE
  # (2) R code if USER_PMG_R is TRUE
  
  # Write out the PMG ini file
  fwrite(PMGParameters[variable != "pmglogging"], file = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
         sep = "=", row.names = FALSE, col.names = FALSE)
  pmgparameters.writelog <- ifelse(PMGParameters[PMGParameters$variable == "pmglogging"]$value == 1, TRUE, FALSE)

  # Expanded set of market-group combinations
  naics_set_expanded <- data.table(NAICS = rep(naics_set$NAICS, naics_set$groups),
                                   SCTG = rep(naics_set$SCTG, naics_set$groups),
                                   Market = rep(naics_set$Market, naics_set$groups),
                                   Group = unlist(lapply(naics_set$groups, seq, from=1)))
  naics_set_expanded[, Market_Group := paste(Market, Group, sep = "_")]
  
  if(USER_PMG_CORES > 1){
    require(parallel)
    
    clust <- makeCluster(USER_PMG_CORES)
    
    clusterCall(clust, 
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH)
    
    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
    clusterExport(clust, 
                  c("runPMG"), 
                  envir = environment())
    
    naicslist <- parLapplyLB(clust, 
                             1:nrow(naics_set_expanded), 
                             function(x){
                               
                               # Remove old outputs and log files if they exist
                               if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))){
                                 file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))
                               }
                               if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market,"_g", g, ".txt")))){
                                 file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".txt")))
                               }
                               
                               # Apply the pmg function
                               runPMG(market = as.character(naics_set_expanded$Market[x]), 
                                      g = naics_set_expanded$Group[x], 
                                      writelog = pmgparameters.writelog, 
                                      wait = TRUE, 
                                      pmgexe = file.path(SYSTEM_PMG_PATH,"pmg.exe"),
                                      inipath = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
                                      inpath = SCENARIO_OUTPUT_PATH, 
                                      outpath = SCENARIO_OUTPUT_PATH,
                                      logpath = SCENARIO_OUTPUT_PATH)
                               
                             },
                             chunk.size = 1)
    stopCluster(clust)
    
  } else {
    
    naicslist <- lapply(1:nrow(naics_set_expanded), 
                        function(x){
                          
                          print(paste(x,
                                      as.character(naics_set_expanded$Market[x]),
                                      naics_set_expanded$Group[x]))
                          
                          # Remove old outputs and log files if they exist
                          if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))){
                            file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))
                          }
                          if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market,"_g", g, ".txt")))){
                            file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".txt")))
                          }
                          
                          # Apply the pmg function
                          runPMG(market = as.character(naics_set_expanded$Market[x]), 
                                 g = naics_set_expanded$Group[x], 
                                 writelog = pmgparameters.writelog, 
                                 wait = TRUE, 
                                 pmgexe = file.path(SYSTEM_PMG_PATH,"pmg.exe"),
                                 inipath = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
                                 inpath = SCENARIO_OUTPUT_PATH, 
                                 outpath = SCENARIO_OUTPUT_PATH,
                                 logpath = SCENARIO_OUTPUT_PATH)
                          
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_pmg.csv"))
  naics_missing <- naics_set_expanded[!Market_Group %in% naics_completed]$Market_Group
  if(length(naics_missing) > 0) cat("Market-Group combinations missing from market simulation in sc_sim_pmg: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
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
  
  # build system call:
  system2(pmgexe,
          args = paste("-i",inipath,"-p",ioprefix,"-d",inpath,"-o",outpath), 
          stdout = logcall,  
          invisible = invisible, 
          wait = wait)
  
  return(paste(market, g, sep = "_"))
  
}

