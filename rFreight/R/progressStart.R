#' Starts a model step: progress bar, timing, loading inputs
#'
#' This function is called at the beginning of a model component to initiate the
#' progress bar, to start timing the model steps, and to load the input used during the model
#' @param steplist List object for the current model component
#' @param steps Number of progress bar steps, integer (>=1)
#' @param modellist List object for the model, defaults to model
#' @keywords Management
#' @export
#' @examples
#' \dontrun{
#' progressStart(firm_Synthesis,9)
#' }

progressStart <- function(steplist, steps, modellist=model){
  
  #Record start time for the step and start data frame for recording times
  pbtitle <<- steplist$steptitle
  modstarttime <<- proc.time()
  runtimes <<- data.frame(model=pbtitle,steps=1:steps,stepnames=NA,stepruntimes=as.numeric(0),cumruntimes=as.numeric(0))
  
  #Initiate progress
  pb_globalsummary <<- winProgressBar(title = pbtitle, min = 0, max = steps, width = 600)
  currentstep <<- 0
  print(paste("Starting",pbtitle))
  
  #Load inputs for the step
  progressNextStep("Reading Inputs")
  loadInputs(steplist$inputs,modellist$inputdir)
  if(modellist$scenvars$outputtable){
    loadInputs(steplist$inputtables,modellist$inputdir)
  }
}

