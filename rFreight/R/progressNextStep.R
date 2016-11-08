#' Increments the progress bar to the next progress step
#'
#' This function is called at the beginning of a step within a model component. It
#' increments the progress bar and change the text describing the step now taking place.
#' It also calculate the run time for the preceding step.
#' @param stepname Name of the step within a model component about to be run, character string
#' @keywords Management
#' @export
#' @examples
#' \dontrun{
#' progressNextStep("Reading Inputs")
#' }

progressNextStep <- function(stepname){
  
  #Record run time if we are finishing a step
  if(currentstep > 0){
    runtimes$cumruntimes[runtimes$steps == currentstep] <<- (proc.time()-modstarttime)[3]
  }
  
  #Increment step
  currentstep <<- currentstep + 1
  setWinProgressBar(pb_globalsummary, currentstep, title=paste(pbtitle,stepname,sep=": "))
  runtimes$stepnames[runtimes$steps == currentstep] <<- stepname
  print(paste(pbtitle,stepname,sep=": "))
}

