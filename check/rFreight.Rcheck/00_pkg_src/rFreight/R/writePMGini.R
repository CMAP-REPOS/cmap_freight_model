#' Writes the ini file for the PMG application
#'
#' Writes the ini file for the PMG application based on variables that have been set
#' and combined into a list, specifically: 
#' RandomSeed, IMax, Verbose, DynamicAlternatePayoffs, ClairvoyantInitialExpectedPayoffs,
#' SellersRankOffersByOrderSize, InitExpPayoff, Temptation, BothCoop, BothDefect,
#' Sucker, RefusalPayoff, WallflowerPayoff, BuyersIgnoreSoldOutSellers, 
#' IgnoreSoldOutSellersMinBuyerSellerRatio, and RawFastParser
#' @param pmg_vars List of named ini file variables
#' @param pmg_ini_path Path to ini file including ini file name, can be a relative path (character string)
#' @keywords PMG
#' @export
#' @examples
#' pmg_vars <- list()
#' # random starting seed for the PMGs
#' pmg_vars$RandomSeed <- 41
#' # number of iterations
#' pmg_vars$IMax <- 6
#' # want lots of detail about tradebots?
#' pmg_vars$Verbose <- 0
#' # recalculate alternate payoffs every iteration based on updated expected payoffs 
#' pmg_vars$DynamicAlternatePayoffs <- 1
#' # should initial expected tradeoffs know size of other traders?
#' pmg_vars$ClairvoyantInitialExpectedPayoffs <- 1
#' # should sellers accept offers based on order size instead of expected payoff?
#' pmg_vars$SellersRankOffersByOrderSize <- 1
#' # multiplier to goose initial expected tradeoff to encourage experimentation with other traders 
#' pmg_vars$InitExpPayoff <- 0.9
#' pmg_vars$Temptation    <- 0.6
#' pmg_vars$BothCoop      <- 1.0
#' pmg_vars$BothDefect    <- 0.6
#' pmg_vars$Sucker        <- 1.0
#' # amount to downgrade expected payoff of seller who outright refuses a trade offer by buyer
#' pmg_vars$RefusalPayoff <- 0.5
#' # negative payoff to sellers for not participating
#' pmg_vars$WallflowerPayoff <- 0.0
#' #buyers don't try to trade with sold out sellers
#' pmg_vars$BuyersIgnoreSoldOutSellers <- 1
#' #ratio at which buyers don't try to trade with sold out sellers
#' pmg_vars$IgnoreSoldOutSellersMinBuyerSellerRatio = 100
#' #faster reading of input files but does less checks (ok for use with R)
#' pmg_vars$RawFastParser = 1
#' #Call the writePMGini function to write out the variables above to the PMG ini file at run time
#' writePMGini(pmg_vars,"./PMG.ini")

# write out the .ini file
writePMGini <- function(pmg_vars, pmg_ini_path){
  pmgini <- paste("//",
                  "// pmg.ini",
                  "//",
                  "[pmg]",
                  "",
                  paste("RandomSeed =", pmg_vars$RandomSeed),
                  "",
                  "// number of iterations",
                  paste("IMax =", pmg_vars$IMax),
                  "",
                  "// want lots of detail about tradebots?",
                  paste("Verbose =", pmg_vars$Verbose),
                  "",
                  "// recalculate alternate payoffs every iteration based on updated expected payoffs",
                  paste("DynamicAlternatePayoffs =", pmg_vars$DynamicAlternatePayoffs),
                  "",
                  "// should initial expected tradeoffs know size of other traders?",
                  paste("ClairvoyantInitialExpectedPayoffs =", pmg_vars$ClairvoyantInitialExpectedPayoffs),
                  "",
                  "// should sellers accept offers based on order size instead of expected payoff?",
                  paste("SellersRankOffersByOrderSize =", pmg_vars$SellersRankOffersByOrderSize),
                  "",
                  "// multiplier to goose initial expected tradeoff to encourage experimentation with other traders",
                  paste0("InitExpPayoff = +", pmg_vars$InitExpPayoff),
                  "",
                  paste("Temptation =", pmg_vars$Temptation),
                  paste("BothCoop =", pmg_vars$BothCoop),
                  paste("BothDefect =", pmg_vars$BothDefect),
                  paste("Sucker =", pmg_vars$Sucker),
                  "",
                  "// amount to downgrade expected payoff of seller who outright refuses a trade offer by buyer",
                  paste("RefusalPayoff =", pmg_vars$RefusalPayoff),
                  "",
                  "// negative payoff to sellers for not participating",
                  paste("WallflowerPayoff =", pmg_vars$WallflowerPayoff),
                  "",
                  "// buyers dont try to trade with sold out sellers",
                  paste("BuyersIgnoreSoldOutSellers =", pmg_vars$BuyersIgnoreSoldOutSellers),
                  "",
                  "// ratio at which buyers don't try to trade with sold out sellers",
                  paste("IgnoreSoldOutSellersMinBuyerSellerRatio =", pmg_vars$IgnoreSoldOutSellersMinBuyerSellerRatio),
                  "",
                  "// faster reading of input files but does less checks (ok for use with R)",
                  paste("RawFastParser =", pmg_vars$RawFastParser),
                  sep = "\n")
  
  pmgConn <- file(pmg_ini_path)
  writeLines(pmgini, pmgConn)
  close(pmgConn)
}
