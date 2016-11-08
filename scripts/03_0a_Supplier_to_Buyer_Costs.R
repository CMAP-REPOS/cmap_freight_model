## 03_0a_Supplier_to_Buyer_Costs.Rdata
## Heither, CMAP - 07-27-2015

#-----------------------------------------------------------------------------------
#Step 3_0a Create Supplier to Buyer Costs
#-----------------------------------------------------------------------------------
#which NAICS should be run, in how many groups, and do we split producers?
#what are the paths to the base directory of the model and to the current scenario?
args <- commandArgs(TRUE)
naics <- args[1]
groups <- as.integer(args[2])
sprod <- as.integer(args[3])
basedir <- args[4]
outputdir <- args[5]

setwd(basedir)

#Start logging
PMGset_Log <- file.path(outputdir,paste0(naics,"_PMGSetup_Log.txt"))
log <- file(PMGset_Log,open="wt") 
sink(log, split=T)
sink(log,type="message")

#load the pmg workspace
load(file.path(outputdir,"PMG_Workspace.Rdata"))
     
#load the workspace for this naics code
load(file.path(outputdir,paste0(naics,".Rdata")))

#load required packages
library(rFreight)
loadPackage("data.table")
loadPackage("reshape")
loadPackage("reshape2")
options(datatable.auto.index=FALSE)

#check whether sampling within the group has been done and if not run that function
if(!"group" %in% names(prodc)) create_pmg_sample_groups(naics,groups,sprod)

#file to hold timings
setuptimes <- file(file.path(outputdir,paste0(naics,"_PMGSetup_Log.txt")),open="wt")
writeLines(paste("Starting:",naics, "Current time",Sys.time()), con=setuptimes)

#loop over the groups and prepare the files for running the games
for (g in 1:groups){

  model$Current_Commodity <- naics		## Heither, 12-01-2015: store current NAICS value
  model$recycle_check <- file.path(model$outputdir,"recycle_check_initial.txt")

  ## Heither, revised 10-05-2015: File Cleanup if Outputs folder being re-used from previous run
  ## -- Delete NAICS_gX.sell file if exists from prior run (existence will prevent create_pmg_inputs from running)
  if(file.exists(file.path(outputdir,paste0(naics,"_g", g, ".sell.csv")))){
	file.remove(file.path(outputdir,paste0(naics,"_g", g, ".sell.csv")))
  }
  
  if(!file.exists(file.path(outputdir,paste0(naics,"_g", g, ".sell.csv")))){
    print(paste("Starting:",naics,"Group",g, "Current time",Sys.time()))
    writeLines(paste("Starting:",naics,"Group",g, "Current time",Sys.time()), con=setuptimes) 
  
    print(paste("Making Inputs:",naics,"Group",g, "Current time",Sys.time()))
    create_pmg_inputs(naics,g,sprod) 
        
    if(!model$scenvars$pmglogging) {
      pmggrouptimes <- file(file.path(outputdir,paste0(naics,"_g", g, ".txt")),open="wt")
      writeLines(paste("Starting:",naics,"Group",g, "Current time",Sys.time()), con=pmggrouptimes)
    }  
  }     
}

#close off logging
writeLines(paste("Completed Processing Outputs:",naics, "Current time",Sys.time()), con=setuptimes)
close(setuptimes)

#end sinking
sink(type="message")
sink()

