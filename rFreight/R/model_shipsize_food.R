#' Shipment Size Model Variables and Coefficients: Food Products
#'
#' This file shows the shipment size model variables and coefficients by shipment size groups for food products. A multinomial logit (MNL) model was estimated for choice of shipment size.
#'  
#' This table is used in the shipment size step of the model for food commodities.
#' @format A dataframe with 5 variables
#' \describe{
#'   \item{CHID}{Shipment size choice ID}
#'   \item{CHDESC}{Shipment size choice description}
#'   \item{VAR}{Explanatory variable}
#'   \item{TYPE}{Type of the explanatory variable}
#'   \item{COEFF}{Coefficient of the variable}
#' }
#' @source The Texas commercial vehicle survey dataset was used for estimating the discrete choice model. 
"model_shipsize_food"
