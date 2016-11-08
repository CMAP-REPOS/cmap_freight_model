#' Data: total export value by Country and commodity
#'
#' This file shows the total exports value (domestic and foreign) valued on a free alongside ship (FAS) basis. The FAS reflects transaction price including inland freight, insurance and other charges incurred in placing the merchandise alongside the ship at the port of export. The export value in this input table is reported by six digits NAICS code and the country where the goods are to be consumed, further processed, or manufactured as known to the shipper at the time of exportation. 
#' 
#' Since the CBP data does not contain foreign employment data, this table is used to include foreign firms in the model to ensure that international flows between the U.S. and foreign countries can be allocated to consuming firms at the foreign country end. This table is used in firm synthesis to develop and characterize a set of agents located in foreign countries that consume goods.
#' 
#' @format A dataframe with 53486 rows and 6 variables
#' \describe{
#'   \item{Country}{Foreign country of destination names (trading with USA)}
#'   \item{Commodity_NAICS6}{Six-digit NAICS commodity code}
#'   \item{USExpVal}{Total exports value in dollars}
#'   \item{FAFZONE}{FAF zone of the country of destination}
#'   \item{ctrycod}{Country code of the country of destination}
#'   \item{CBPZONE}{CBP Zone code of the country of destination}
#' }
#' @source The sources of this table include the USA Census Trade Online (NAICS-based data) 
#' (\url{https://usatrade.census.gov/}). Provided by the U.S. Census Bureau's Foreign Trade 
#' Division, USA Trade Online provide current and cumulative U.S. export and import data 
#' on more than 9,000 export commodities and 17,000 import commodities worldwide. 
#' NAICS-based data as part of Foreign Trade Statistics data is the most detailed available 
#' data on US imports and exports by NAICS codes (6 digits) and by country. 
"data_foreign_cons"
