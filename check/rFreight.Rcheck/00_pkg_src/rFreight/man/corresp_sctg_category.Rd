% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/corresp_sctg_category.R
\docType{data}
\name{corresp_sctg_category}
\alias{corresp_sctg_category}
\title{Correspondence between SCTG Codes and other Commodity Aggregations}
\format{A dataframe with 43 rows and 5 variables
\describe{
  \item{Commodity_SCTG}{Two digits SCTG commodity code}
  \item{Commodity_SCTG_desc_short}{Short name of the SCTG commodity}
  \item{Commodity_SCTG_desc_long}{SCTG commodity description}
  \item{Category}{Commodity category group 1, used in mode and path choice model}
  \item{Category2}{Commodity category group 2, used in the assignment of cost and time weights for use in the PMGs}
}}
\source{
The sources of this table include the article by Marshall L. Fisher  (1997),
What is the right supply chain for your product? (Harvard business review 75 (1997): 105-117)
which helped with the idea of categorizing product types based on different product
characteristics.
}
\usage{
corresp_sctg_category
}
\description{
This file shows the commodity group categories used for parameter assumptions. Commodities in Category are categorized (animals, bulk natural resources, intermediate processed goods, finished goods and others) based on the commoditys physical characteristics. Commodities in Category2 are categorized (functional, semi-functional, semi-innovative, and innovative) based on the commodity and supply chain characteristics (e.g. demand pattern, time-sensitivity, cost level, etc.).
}
\details{
This table is used to provide a correspondence between the SCTG codes and more aggregate commodity groups used in various model steps. The categories are defined based on physical characteristics or commodity and supply chain characteristics, and specific model parameters are associated with each of these aggregate commodity groups in, for example, the total cost equation used in the mode choice model.
}
\keyword{datasets}

