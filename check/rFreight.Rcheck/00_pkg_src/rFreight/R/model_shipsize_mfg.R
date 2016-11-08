#' Shipment Size Model Variables and Coefficients: Manufactured Goods
#'
#' This file shows the shipment size model variables and coefficients by shipment size groups for manufactured. A multinomial logit (MNL) model was estimated for choice of shipment size.
#'  
#' This table is used in the shipment size step of the model for manufactured goods.
#' @format A dataframe with 5 variables
#' \describe{
#'   \item{CHID}{Shipment size choice ID}
#'   \item{CHDESC}{Shipment size choice description}
#'   \item{VAR}{Explanatory variable}
#'   \item{TYPE}{Type of the explanatory variable}
#'   \item{COEFF}{Coefficient of the variable}
#' }
#' @source The Texas commercial vehicle survey dataset was used for estimating the discrete choice model. 
"model_shipsize_mfg"
