#' Load a set of files to objects
#'
#' Loads a set of inputs files to objects in the global environment
#' @param filelist list of strings containing file names; uses the names of the list as the object names
#' @param inputdir file path to the inputs directory (e.g. model$inputdir)
#' @keywords Management
#' @export

loadInputs <- function (filelist,inputdir) {
  if(length(filelist)>0){
    for (i in 1:length(filelist)){
      assign(names(filelist)[i],fread(file.path(inputdir,filelist[[i]])),envir = .GlobalEnv)
    }
  }
}
