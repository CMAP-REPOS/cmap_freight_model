#' Save a set of summary tables
#'
#' Saves a set of summary tables to locations listed in summary
#' @param objects list of tables
#' @param locations list of file locations (list naming need to be consistent with objects list)
#' @keywords Management
#' @export

saveSummary <- function (objects,locations) {
  for (i in 1:length(objects)){
    write.csv(objects[[i]],file=file.path(model$outputdir,locations[[names(objects)[i]]]),row.names=F)
  }
}