#' Unit Costs for SCTG Commodities
#'
#' This file shows the unit cost (dollar per ton) by two digits SCTG commodity codes. Commodity unit cost is determined using FAF dollar per ton values for all modes by commodity. The total tons carried by all modes all movements are divided to the total value to calculate the unit cost of the commodity.
#'  
#' This table is used to convert production and consumption values from the BEA input output tables to tonnages produced and consumed. 
#' @format A dataframe with 2 variables
#' \describe{
#'   \item{Commodity_SCTG}{Two digits SCTG commodity code}
#'   \item{UnitCost}{Commodity unit cost (Dollars/Ton)}
#' }
#' @source The sources of this table include the FHWAs 2007 Freight Analysis Framework data (\url{http://www.ops.fhwa.dot.gov/freight/freight_analysis/faf/}). 
"data_unitcost"
