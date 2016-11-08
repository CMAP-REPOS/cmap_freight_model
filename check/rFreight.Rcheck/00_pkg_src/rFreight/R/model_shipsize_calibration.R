#' Shipment Size Calibration Shares
#'
#' This file shows the shipment size shares by shipment weight groups and commodity groups for value and tons.
#'  
#' This table is used for shipment size model calibration.
#' @format A dataframe with 6 variables
#' \describe{
#'   \item{Commodity_SCTG}{Two digits SCTG commodity code}
#'   \item{Commodity_SCTG_desc}{Description of SCTG commodity}
#'   \item{ShipmentWeight}{Shipment weight category (lbs)}
#'   \item{WeightCategory}{Shipment weight category ID}
#'   \item{ValuePct}{Percentage of value in each shipment weight category by SCTG commodity}
#'   \item{TonsPct}{Percentage of tons in each shipment weight category by SCTG commodity}
#' }
#' @source The sources of this table include the Census Bureau and the Bureau of Transportation Statistics (BTS) Commodity Flow Survey (CFS 2007) data. The CFS is the primary source of national and state-level data on domestic freight shipments by American establishments in mining, manufacturing, wholesale, auxiliaries, and selected retail and services trade industries. Data are provided on the types, origins and destinations, values, weights, modes of transport, distance shipped, and ton-miles of commodities shipped.
"model_shipsize_calibration"
