#' Employment Ranking by Industry by County
#'
#' This file shows the employment ranking by industry by county. A dataset is prepared from employment data that contains the percentile ranking of each of 21 NAICS categories by TAZ based on employment numbers in each of those industries. Higher employment numbers implies a higher percentile rank. Within each county, each mesozone is assigned to one of ten percentile-ranking categories. Thresholds of 10 percent are used to determine the categories. Mesozones with the highest employment are classified as Rank 10, zones with the lowest employment Rank 1, and so on. Mesozones are FAF3 zones outside of the region, county-sized zones on the fringes of the region, and township-sized zones in the inner counties.
#'  
#' For the purpose of mode choice and simulation of freight traffic, the firms in the CMAP region modeling area are assigned to mesozones within each county. A few county zones correspond to only one TAZ. The other counties correspond to more than one TAZ. TAZs are assigned to firms in these counties based on employment ranking by industry. The model uses percentile rankings to assign larger firms to mesozones with more employment and smaller firms to zones with any employment in their industry classes.  The model prevents firms from being assigned to mesozones with no employment in the firms industry class.
#' @format A dataframe with 23 variables
#' \describe{
#'   \item{COUNTY}{County FIPS code}
#'   \item{MESOZONE}{Mesozone ID number}
#'   \item{rank11 to rank3133}{Rank of each 2 digit NAICS code industry}
#' }
#' @source The sources of this table include the 2010 U.S. Census County Business Pattern data (\url{http://www.census.gov/econ/cbp/}).
"data_mesozone_emprankings"
