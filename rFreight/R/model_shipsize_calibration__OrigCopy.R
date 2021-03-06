#' Correspondence between NAICS 6, NAICS IO, and SCTG Codes
#'
#' This file is a correspondence between three classifications and shows the commodities produced by each industry. Industries and commodities are defined at the 6-digit NAICS level using both the systems used by the U.S. Census Bureau and the slightly more aggregated system used by the U.S. Bureau of Economic Analysis. In addition, the table indicates the correspondence between these detailed NAICS 6-digit industry codings and the much more aggregate 2-digit SCTG commodity classification. The table shows the commodities that are produced by each industry. In some cases, industries produce more than one commodity; the final column is a proportion that is used to account for this. In many cases, a single industry is credited with making the entire domestic supply of a commodity (proportion = 1.0), which is expected given the 6-digit level of detail. Where the proportion is less than 1.0, there should be multiple industry entries for the same commodity, such that their proportions sum to 1.0.
#'  
#' This correspondence file is an input to the firm synthesis step to identify what a firm in that industry produces and to convert between the two different systems of NAICS coding (used in employment data and input-output data respectively).
#' @format A dataframe with 7 variables
#' \describe{
#'   \item{Industry_NAICS6_Make}{Six-digit NAICS code of the industry}
#'   \item{Industry_NAICS6_Make_desc}{SCTG two-digit code of the commodity}
#'   \item{Industry_NAICS6_CBP}{Six-digit NAICS (Census) industry code}
#'   \item{Industry_NAICS6_CBP_desc}{Description of the NAICS (Census) industry code}
#'   \item{Commodity_SCTG}{SCTG two-digit code of the commodity}
#'   \item{Commodity_SCTG_desc}{SCTG code description}
#'   \item{Proportion}{Proportion of the SCTG commodity made by the NAICS6_Make industry}
#' }
#' @source The correspondence between commodity NAICS and commodity SCTG codes is based on commodity descriptions. Information about what is produced by particular industries in based on U.S. Bureau of Economic Analysis (2007), BEA Input-Output Make and Use tables (\url{http://www.bea.gov/industry/io_annual.htm}) and is derived from the detailed version of the Make Tables/After Redefinitions in the Industry Input-Output Accounts Data.  
"corresp_naics6_n6io_sctg"
