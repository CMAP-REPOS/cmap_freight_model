#' Data table of employment by county
#'
#' This file shows total employment for each of the zones in the CBP zone system.
#' 
#' This employment data is used in the vehicle and tour pattern model to develop the employment by zone variable for the choice model.
#' @format A dataframe with 151 rows and 2 variables
#' \describe{
#'   \item{COUNTY}{Zone numbering for CBP zone system (combination of FAF zones and counties)}
#'   \item{CBP_EMP}{Total employment in the zone}
#' }
#' @source The sources of this table include the 2010 U.S. Census County Business 
#' Pattern data (\url{http://www.census.gov/econ/cbp/}). The dataset is an annual 
#' series that provides subnational economic data by industry. 
"data_emp_cbpzone"
