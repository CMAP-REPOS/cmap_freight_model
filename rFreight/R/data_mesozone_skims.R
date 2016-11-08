#' Zone to Zone Travel Times Between Mesozones
#'
#' This file shows the zone to zone travel times between mesozones within the CMAP region.
#'  
#' This file is used in the stop sequencing component of the truck-touring model.
#' @format A dataframe with 3 variables
#' \describe{
#'   \item{Origin}{Origin mesozone ID number}
#'   \item{Destination}{Destination mesozone ID number}
#'   \item{Time}{Zone to zone travel time (Hours)}
#' }
#' @source The skims values in this file are derived from CMAPs meso freight network. 
"data_mesozone_skims"
