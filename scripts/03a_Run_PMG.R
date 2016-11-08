##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       03a_RunPMG.R produces the inputs and runs the PMG games for a NAICS code                   
#Date:              June 26 28, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 3a Run PMG
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
PMG_Log <- file.path(outputdir,paste0(naics,"_PMG_Log.txt"))
log <- file(PMG_Log,open="wt") 
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
loadPackage("bit64")  
options(datatable.auto.index=FALSE)

#list to hold the summarized outputs, file to hold timings
pmgoutputs <- list() 
pmgtimes <- file(file.path(outputdir,paste0(naics,".txt")),open="wt")
writeLines(paste("Starting:",naics, "Current time",Sys.time()), con=pmgtimes)

#loop over the groups and run the games, and process outputs
for (g in 1:groups){
  
  if(!file.exists(file.path(outputdir,paste0(naics,"_g", g, ".out.csv")))){
    #call to runPMG to run the game
    runPMG(naics,g, writelog=model$scenvars$pmglogging, wait=TRUE, inpath=outputdir, outpath=outputdir, logpath=outputdir)

    if(!model$scenvars$pmglogging) {
      writeLines(paste("Completed:",naics,"Group",g, "Current time",Sys.time()), con=pmggrouptimes)
      close(pmggrouptimes)
    }
        
    writeLines(paste("Completed:",naics,"Group",g, "Current time",Sys.time()), con=pmgtimes)
      
    print(paste("Deleting Inputs:",naics,"Group",g, "Current time",Sys.time()))
    
    file.remove(file.path(outputdir,paste0(naics,"_g", g, ".costs.csv")))
    file.remove(file.path(outputdir,paste0(naics,"_g", g, ".buy.csv")))
    file.remove(file.path(outputdir,paste0(naics,"_g", g, ".sell.csv")))
    
  } 
  
  if (file.exists(file.path(outputdir,paste0(naics,"_g", g, ".out.csv")))){
    print(paste("Processing Outputs:",naics,"Group",g, "Current time",Sys.time()))
    pmgout <- fread(file.path(outputdir,paste0(naics,"_g", g,".out.csv")))
    setnames(pmgout,c("BuyerId","SellerId"),c("BuyerID","SellerID"))
    #get just the results from the final iteration
    pmgout <- pmgout[Last.Iteration.Quantity>0]
    #apply fix for bit64/data.table handling of large integers in rbindlist
    pmgout[,Quantity.Traded:=as.character(Quantity.Traded)]
    pmgout[,Last.Iteration.Quantity:=as.character(Last.Iteration.Quantity)]
    load(file.path(outputdir,paste0(naics,"_g", g,".Rdata")))
    pmgoutputs[[g]] <- merge(pc,pmgout,by=c("BuyerID","SellerID"))
    rm(pmgout,pc)
    
  } 
    
}

#convert output list to one table, add to workspace, and save
#apply fix for bit64/data.table handling of large integers in rbindlist
pairs <- rbindlist(pmgoutputs)
rm(pmgoutputs)
pairs[,Quantity.Traded:=as.integer64.character(Quantity.Traded)]
pairs[,Last.Iteration.Quantity:=as.integer64.character(Last.Iteration.Quantity)]
save(consc,prodc,pairs,file=file.path(outputdir,paste0(naics,".Rdata")))

#close off logging
writeLines(paste("Completed Processing Outputs:",naics, "Current time",Sys.time()), con=pmgtimes)
close(pmgtimes)

#end sinking
sink(type="message")
sink()

