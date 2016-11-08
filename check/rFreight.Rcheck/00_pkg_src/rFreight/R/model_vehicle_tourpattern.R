#' Vehicle and Tour Pattern Model Variables and Coefficients
#'
#' This file shows the vehicle and tour pattern model and coefficients. A multinomial logit (MNL) model was estimated for the joint choice of vehicle type and tour pattern.
#'  
#' This table is used in the vehicle and tour pattern component of the truck-touring model.
#' @format A dataframe with 5 variables
#' \describe{
#'   \item{CHID}{Vehicle and tour pattern model choice ID}
#'   \item{CHDESC}{Vehicle and tour pattern model choice description}
#'   \item{VAR}{Explanatory variable}
#'   \item{TYPE}{Type of the explanatory variable}
#'   \item{COEFF}{Coefficient of the variable}
#' }
#' @source The vehicle and tour pattern model was estimated using the Texas Commercial Vehicle Survey (RSG (2012) Tour-based and Supply Chain Freight Forecasting Framework Final Report Framework, developed for the Federal Highway Administration with University of Illinois at Chicago and John Bowman BAA DTFH61-10-R-00013.) 
"model_vehicle_tourpattern"
