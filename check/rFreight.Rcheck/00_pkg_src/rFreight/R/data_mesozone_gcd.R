#' Great Circle Distances between Mesozones
#'
#' This file shows the great circle distance (GCD) between the mesozones in the model.
#'  
#' This table is used to calculate distances used as a variable in the distribution channel model.
#' @format A dataframe with 7 variables
#' \describe{
#'   \item{Production_zone}{Production zone ID number}
#'   \item{Production_lon}{Longitude of the Pzone}
#'   \item{Production_lat}{Latitude of the Pzone}
#'   \item{Consumption_zone}{Consumption zone ID number}
#'   \item{Consumption_lon}{Longitude of the Czone}
#'   \item{Consumption_lat}{Latitude of the Czone}
#'   \item{GCD}{Great Circle Distance between the Pzone and Czone (in Miles)}
#' }
#' @source Distances between zones were estimated using the Haversine formula (\url{http://en.wikipedia.org/wiki/Haversine_formula})
"data_mesozone_gcd"
