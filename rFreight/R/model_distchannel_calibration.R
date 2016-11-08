#' Distribution Channel Calibration Shares
#'
#' This file shows the distribution channel type (number of stops, 0, 1 or 2+ stops) shares by commodity groups. Shipping chain or distribution channel indicates whether the goods went through a consolidation center, a distribution center, and/or a warehouse. The commodity group codes are aggregations of the 43 SCTG commodity groups as follows: A: Agricultural Products, B: Chemical/Pharmaceutical products, C: Coal/Mineral/Ores, D: Electronics, E: Prepared Foodstuffs, F: Gravel/Natural Sands/Cements, G: Machinery/Metal Products, H: Mixed Freight/Miscellaneous, I: Motorized and Other Vehicles (incl. parts), J: Wood/Paper/textile/Leather products, K: Other
#'  
#' This table is used for distribution channel  model calibration.
#' @format A dataframe with 3 variables
#' \describe{
#'   \item{Commodity_Category}{Commodity groups based on the reference paper used}
#'   \item{NumberofStops_Choice}{Shipping chain type}
#'   \item{Target_Share}{proportion of shipments by distribution channel for each commodity group}
#' }
#' @source The source of this table is the study done by University of Illinois at Chicago: Pourabdollahi, Z., Karimi, B., Mohammadian, A. K., & Kawamura, K. (2014). Shipping Chain Choices in Long Distance Supply Chains: Descriptive Analysis and a Decision Tree Model 2. In Transportation Research Board 93rd Annual Meeting (No. 14-1706). 
"model_distchannel_calibration"
