#' Data: cost and time preference weights by SCTG commodity categories
#'
#' This file shows the cost and time preference weights and the maximum single source fraction by SCTG commodity categories. Weights are calculated using the categorization of commodities (functional, semi-functional, semi-innovative, and innovative) which are determined using assumptions on commodity characteristics (e.g. demand pattern, time-sensitivity, and cost level). Commodity cost-weight is determined using FAF dollar/ton values. Commodity time-weight is determined based on commodity supply chain-related characteristics (e.g. demand pattern, lead-time focus, transportation strategy).
#'
#' This table is used to develop inputs for the Procurement Market Games (PMGs). In the PMG, agents representing producers of an output commodity (buyers) are instantiated with a quantity of an input commodity to purchase and a set of preference weights that allow them to tradeoff unit costs, service time, and potentially other attributes when considering the utility of a potential trading partner.
#'
#' @format A dataframe with 43 rows and 6 variables
#' \describe{
#'   \item{Commodity_SCTG}{SCTG commodity code}
#'   \item{Commodity_SCTG_desc}{SCTG commodity description}
#'   \item{Commodity_Type}{Commodity category, e.g., Innovative}
#'   \item{CostWeight}{Cost weight as a share (calculated based on commodity type)}
#'   \item{TimeWeight}{Time weight as a share (calculated based on commodity type)}
#'   \item{SingleSourceMaxFraction}{Single source max fraction as a share (calculated based on commodity type)}
#' }
#' @source The sources of this table include the FHWAs 2007 Freight Analysis Framework 
#' data (\url{http://www.ops.fhwa.dot.gov/freight/freight_analysis/faf/}). The article by 
#' Marshall L. Fisher (1997), What is the right supply chain for your product?, is also a 
#' source for this input table that helped with the idea of categorizing product types 
#' (commodities) based on different product characteristics.
"data_firm_pref_weights"
