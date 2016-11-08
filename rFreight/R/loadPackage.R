#' Install and load required R packages
#'
#' Load required R packaging, first testing whether a package is 
#' available and installs without popping up the Cran mirror list.
#' @param package Name of R package to load/download and install (character string)
#' @keywords Management
#' @export

loadPackage <- function (package) {
  if(!package %in% .packages(all = TRUE)) {
    install.packages(package, repos = "http://cran.r-project.org")
  }
  eval(parse(text=paste("library(", package, ")", sep="")))
}