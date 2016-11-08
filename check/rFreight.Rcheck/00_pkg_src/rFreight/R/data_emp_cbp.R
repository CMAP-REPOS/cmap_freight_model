#' Correspondence between NAICS 6, NAICS IO, and SCTG Codes
#'
#' This file shows the number of employees and establishments by six digits NAICS industry, 
#' FAF zone, and county, , although the employment data in this file are derived from the County Business Patterns data are subject to censoring. This file also has the number of establishments by eight 
#' different employment size groups.
#' 
#' This employment data is used in the firm synthesis step and in conjunction with information from the Make and Use tables to develop a set of synthetic firms characterized with commodities produced and consumption requirements. County-level employment data for the United States outside of CMAP area, in the form of County Business Patterns (CBP) data, are aggregated to a FAF zone resolution, while the county level data are used with the CMAP area and during the firm synthesis step allocated to the smaller mesozones used in the model.
#' @format A dataframe with 113409 rows and 13 variables
#' \describe{
#'   \item{Industry_NAICS6_CBP}{Six digits NAICS (Census) code}
#'   \item{FAFZONE}{FAF zone}
#'   \item{CBPZONE}{County Business Pattern (CBP) zone}
#'   \item{employment}{Total number of employees (subject to censoring)}
#'   \item{establishment}{Total number of establishments}
#'   \item{e1}{Total number of establishments with 1-19 employees}
#'   \item{e2}{Total number of establishments with 20-99 employees}
#'   \item{e3}{Total number of establishments with 100-249 employees}
#'   \item{e4}{Total number of establishments with 250-499 employees}
#'   \item{e5}{Total number of establishments with 500-999 employees}
#'   \item{e6}{Total number of establishments with 1000-2499 employees}
#'   \item{e7}{Total number of establishments with 2500-4999 employees}
#'   \item{e8}{Total number of establishments with more than 5000 employees}
#' }
#' @source The sources of this table include the 2010 U.S. Census County Business 
#' Pattern data (\url{http://www.census.gov/econ/cbp/}). The dataset is an annual 
#' series that provides subnational economic data by industry.  
"data_emp_cbp"
