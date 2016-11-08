#' Ends a model step
#'
#' This function is called at the end of a model component. It produces outputs,
#' save workspaces, closes the progress bar, and saves the runtimes for the model component.
#' @param steplist List object for the current model component
#' @param modellist List object for the model, defaults to model
#' @keywords Management
#' @export
#' @examples
#' \dontrun{
#' progressEnd(firm_synthesis)
#' }

progressEnd <- function(steplist,modellist=model){
  
  progressNextStep("Saving Workspace, Summaries, and Tabular Outputs")
  #Save the summaries for this step to csv and to model workspace
  if(modellist$scenvars$outputsummary){
    if(length(steplist$summary) > 0){
      steplist$summarytables <- steplist$summaryfunc(steplist)
      saveSummary(steplist$summarytables,steplist$summary)
      modellist[[steplist$step]] <- steplist
      save(modellist,file=modellist$workspace)
      model <<- modellist
    }  
  }
  
  #Saving an R workspace image for this step
  if(modellist$scenvars$outputRworkspace) save.image(file=file.path(model$outputdir,steplist$workspace))
  
  #Save large tables output tables as csv's:
  if(modellist$scenvars$outputtable) {
    if(length(steplist$outputs) > 0) saveOutputs(steplist$outputs)
  }
  
  close(pb_globalsummary)
  print(paste("Completed",pbtitle))
  
  #Record run time for last step, complete run times and print/write
  if(currentstep > 0){
    runtimes$cumruntimes[runtimes$steps == currentstep] <<- (proc.time()-modstarttime)[3]
    runtimes$stepruntimes <<- c(runtimes$cumruntimes[1],diff(runtimes$cumruntimes)) 
    print(runtimes)
    write.table(runtimes,modellist$logs$Step_RunTimes,row.names=FALSE,col.names=FALSE,sep=",",append=TRUE)  
  }
  
  rm(pb_globalsummary,pbtitle,currentstep,modstarttime,runtimes,envir=.GlobalEnv)
  
  #gc() as this is done at the end of a step after remove uneeded objects
  gc()
  
  return(steplist)
}
