% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/data_2010io.R
\docType{data}
\name{data_2010io}
\alias{data_2010io}
\title{Data: detailed 2010 use table after redefinitions}
\format{A dataframe with 302 rows and 387 variables
\describe{
  \item{Industry_NAICS6_MakeUse}{NAICS (BEA) commodity code of the making industry}
  \item{X1111A0}{(and all subsequent fields) Annual values of commodities exchanged between industries, where columns are the using industry (units of millions of dollars per year)}
}}
\source{
The sources of this table include the U.S. Bureau of Economic Analysis (2007),
BEA Input-Output Make and Use tables (\url{http://www.bea.gov/industry/io_annual.htm}).
It is derived from Make Tables/After Redefinitions in the detailed version from the
Industry Input-Output Accounts Data. The 2010 detailed use table (6 digits NAICS codes)
was developed by factoring up the 2007 detailed table by growth factors calculated from
the 207 and 2010 summary level (3 digits NAICS codes) tables and it was assumed all
6 digits NAICS industries under a 3 digits NAICS industry grow with the same growth
factor from 2007 to 2010.
}
\usage{
data_2010io
}
\description{
This file contains detailed 2010 Use table after redefinitions data developed using the 2007 benchmark Input-Output (I-O) accounts. The Input-Output accounts show how industries interact. This table shows the inputs to industry production and the commodities that are consumed by final users. For each production industry, the table reports the value of goods consumed by each buyer industry. The table includes 386 different producing industries showing about 5.7 trillion dollar value of inputs.
}
\details{
The model uses this information to identify for each buyer industry the most important commodities that are consumed and their associated supplier industries.
}
\keyword{datasets}

