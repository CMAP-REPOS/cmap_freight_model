#' Save a set of objects to .csv files
#'
#' Saves a set of outputs to .csv files and then removes the objects from memory
#' @param filelist list of strings containing file paths; uses the names of the list as the object names
#' @keywords Management
#' @export

saveOutputs <- function (filelist) {
  for (i in 1:length(filelist)){
    write.csv(get(names(filelist)[i]),file=file.path(model$outputdir,filelist[[i]]),row.names=F)
  }
  rm(list=names(filelist),envir=.GlobalEnv)
}