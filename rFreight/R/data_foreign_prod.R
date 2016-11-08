#' Data: total import value by Country and commodity
#'
#' This file shows the cost, insurance, and freight (CIF) value of US imports by country and 6-digit NAICS code. The CIF value represents the landed value of the merchandise at the first port of arrival in the US. It is computed by adding the Customs Value to the aggregate cost of all freight, insurance, and other charges (excluding US import duties) incurred in moving merchandise from alongside the carrier at the port of exportation in the country of export and placing it alongside the carrier at the first port of entry in the US.
#' 
#' Since the CBP data does not contain foreign employment data, this table is used to include foreign firms in the model to ensure that international flows to the U.S. from foreign countries can be allocated to producing firms at the foreign country end. This table is used in firm synthesis to develop and characterize a set of agents located in foreign countries that produce goods.
#' @format A dataframe with 28470 rows and 6 variables
#' \describe{
#'   \item{Country}{Foreign country of origin names (trading with USA)}
#'   \item{Commodity_NAICS6}{Six-digit NAICS commodity code}
#'   \item{USImpVal}{Total imports value}
#'   \item{FAFZONE}{FAF zone of the country of origin}
#'   \item{ctrycod}{Country code of the country of origin}
#'   \item{CBPZONE}{CBP Zone code the country of origin}
#' }
#' @source The sources of this table include the USA Census Trade Online (NAICS-based data) 
#' (\url{https://usatrade.census.gov/}). Provided by the U.S. Census Bureau's Foreign Trade 
#' Division, USA Trade Online provide current and cumulative U.S. export and import data 
#' on more than 9,000 export commodities and 17,000 import commodities worldwide. 
#' NAICS-based data as part of Foreign Trade Statistics data is the most detailed available 
#' data on US imports and exports by NAICS codes (6 digits) and by country. 
"data_foreign_prod"
