#' Time of Day Model Variables and Coefficients
#'
#' This file shows the time of day model and coefficients. A multinomial logit (MNL) model was estimated for choice of tour start time of day.
#'  
#' This table is used in the tour time of day component of the truck-touring model.
#' @format A dataframe with 5 variables
#' \describe{
#'   \item{CHID}{Time of day model choice ID}
#'   \item{CHDESC}{Time of day model choice description}
#'   \item{VAR}{Explanatory variable}
#'   \item{TYPE}{Type of the explanatory variable}
#'   \item{COEFF}{Coefficient of the variable}
#' }
#' @source The time of day model was estimated using the Texas Commercial Vehicle Survey (RSG (2012) Tour-based and Supply Chain Freight Forecasting Framework Final Report Framework, developed for the Federal Highway Administration with University of Illinois at Chicago and John Bowman BAA DTFH61-10-R-00013.) 
"model_timeofday"
