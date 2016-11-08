#' Convert NAICS 6 to top level 2 digit codes
#'
#' Given naics6 codes (Census, not BEA IO), returns naics2 codes 
#' with some aggregation (e.g. 31,32,33 reclassed as 31 Manufacturing)
#' @param naics6 vector of naics6 codes (Census, not BEA IO)
#' @keywords Freight-Data
#' @export

naics6naics2 <- function(naics6){
  
  #make 2 digit NAICS, recode some values which belong to same broad category
  naics2 <- as.integer(substr(naics6,1,2))
  naics2[naics2 %in% c(32,33)] <- 31L
  naics2[naics2 == 45] <- 44L
  naics2[naics2 == 49] <- 48L
  return(naics2)
}
