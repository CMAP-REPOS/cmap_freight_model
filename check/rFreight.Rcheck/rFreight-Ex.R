pkgname <- "rFreight"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "rFreight-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rFreight')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("predict_logit")
### * predict_logit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_logit
### Title: Simulates the application of a logit model, with calibration
### Aliases: predict_logit
### Keywords: Application

### ** Examples

## Not run: 
##D predict_logit(df,mod) #applies the model without calibration
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_logit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("progressEnd")
### * progressEnd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: progressEnd
### Title: Ends a model step
### Aliases: progressEnd
### Keywords: Management

### ** Examples

## Not run: 
##D progressEnd(firm_synthesis)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("progressEnd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("progressManager")
### * progressManager

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: progressManager
### Title: Starts and stops the overall progress management system for a
###   model
### Aliases: progressManager
### Keywords: Management

### ** Examples

## Not run: 
##D progressManager("Start",Step_RunTimes, outputlog, Main_Log,
##D                 outputprofile, Profile_Log, Profile_Summary)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("progressManager", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("progressNextStep")
### * progressNextStep

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: progressNextStep
### Title: Increments the progress bar to the next progress step
### Aliases: progressNextStep
### Keywords: Management

### ** Examples

## Not run: 
##D progressNextStep("Reading Inputs")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("progressNextStep", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("progressStart")
### * progressStart

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: progressStart
### Title: Starts a model step: progress bar, timing, loading inputs
### Aliases: progressStart
### Keywords: Management

### ** Examples

## Not run: 
##D progressStart(firm_Synthesis,9)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("progressStart", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("runPMG")
### * runPMG

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: runPMG
### Title: Builds the system call to the PMG application and runs the
###   application
### Aliases: runPMG
### Keywords: PMG

### ** Examples

## Not run: 
##D runPMG(naics,g,writelog=FALSE,wait=TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("runPMG", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("writePMGini")
### * writePMGini

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: writePMGini
### Title: Writes the ini file for the PMG application
### Aliases: writePMGini
### Keywords: PMG

### ** Examples

pmg_vars <- list()
# random starting seed for the PMGs
pmg_vars$RandomSeed <- 41
# number of iterations
pmg_vars$IMax <- 6
# want lots of detail about tradebots?
pmg_vars$Verbose <- 0
# recalculate alternate payoffs every iteration based on updated expected payoffs
pmg_vars$DynamicAlternatePayoffs <- 1
# should initial expected tradeoffs know size of other traders?
pmg_vars$ClairvoyantInitialExpectedPayoffs <- 1
# should sellers accept offers based on order size instead of expected payoff?
pmg_vars$SellersRankOffersByOrderSize <- 1
# multiplier to goose initial expected tradeoff to encourage experimentation with other traders
pmg_vars$InitExpPayoff <- 0.9
pmg_vars$Temptation    <- 0.6
pmg_vars$BothCoop      <- 1.0
pmg_vars$BothDefect    <- 0.6
pmg_vars$Sucker        <- 1.0
# amount to downgrade expected payoff of seller who outright refuses a trade offer by buyer
pmg_vars$RefusalPayoff <- 0.5
# negative payoff to sellers for not participating
pmg_vars$WallflowerPayoff <- 0.0
#buyers don't try to trade with sold out sellers
pmg_vars$BuyersIgnoreSoldOutSellers <- 1
#ratio at which buyers don't try to trade with sold out sellers
pmg_vars$IgnoreSoldOutSellersMinBuyerSellerRatio = 100
#faster reading of input files but does less checks (ok for use with R)
pmg_vars$RawFastParser = 1
#Call the writePMGini function to write out the variables above to the PMG ini file at run time
writePMGini(pmg_vars,"./PMG.ini")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("writePMGini", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
