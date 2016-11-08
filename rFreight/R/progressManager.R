#' Starts and stops the overall progress management system for a model
#'
#' This function is the main manager for the model progress management framework that
#' controls the progress bars, run time recording, logging, and profiling
#' @param StartStop flag to either start or stop progress managements, "Start" or "Stop" character string
#' @param Step_RunTimes file path and file name for run times .csv file
#' @param outputlog TRUE/FALSE for logging in a text file
#' @param Main_Log file path and file name for log text file
#' @param outputprofile TRUE/FALSE for run profiling via Rprof() and summaryRprof() 
#' @param Profile_Log file path and file name for profile output file
#' @param Profile_Summary file path and file name for profile summary file
#' @keywords Management
#' @export
#' @examples
#' \dontrun{
#' progressManager("Start",Step_RunTimes, outputlog, Main_Log, 
#'                 outputprofile, Profile_Log, Profile_Summary)
#' }

progressManager <- function(StartStop, Step_RunTimes, outputlog=FALSE, Main_Log="Main_Log.txt", outputprofile=FALSE, Profile_Log="Profile.out", Profile_Summary="Profile_Summary.txt"){
  
  if(StartStop=="Start"){
    
    #start logging
    if(outputlog){
      log <- file(Main_Log,open="wt") 
      sink(log, split=T)
      sink(log,type="message")
    }
    
    #start profiling
    if(outputprofile) Rprof(Profile_Log)
    
    #create new run times csv file
    runtimes <<- data.frame(model=character(),steps=integer(),stepnames=character(),stepruntimes=numeric(),cumruntimes=numeric())
    write.csv(runtimes,Step_RunTimes,row.names=FALSE)
    rm(runtimes,envir=.GlobalEnv)
    
  } 
  
  if(StartStop=="Stop"){
    
    #add overall cumulative runtime column to run times csv file
    runtimes <<- read.csv(Step_RunTimes)
    runtimes$modelruntimes <<- cumsum(runtimes$stepruntimes)
    write.csv(runtimes,paste(Step_RunTimes),row.names=FALSE)
    rm(runtimes,envir=.GlobalEnv)
    
    #end profiling
    if(outputprofile){
      Rprof(NULL)
      capture.output(summaryRprof(Profile_Log, lines = "show"),file=Profile_Summary)
    }
    
    #end sinking
    if(outputlog){
      sink(type="message")
      sink()
    }
    
  }
}
