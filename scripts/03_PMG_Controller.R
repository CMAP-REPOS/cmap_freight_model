##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       3_PMG_Controller.R produces the inputs to the PMGs and controls running
#                   the PMGs and cleaning up after them
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 3 PMG Controller
#-----------------------------------------------------------------------------------
progressStart(pmgcon,3)

#------------------------------------------------------------------------------------------------------
#Produce PMG inputs and run PMGs
#------------------------------------------------------------------------------------------------------
progressNextStep("Producing Supplier to Buyer Costs")

## ---------------------------------------------------------------
## Heither, revised 12-01-2015: Add file to hold initial (PMG setup) SCTG recycling check data
model$recycle_check <- file.path(model$outputdir,"recycle_check_initial.txt")
if(file.exists(model$recycle_check)){
	file.remove(model$recycle_check)
  } 
## ---------------------------------------------------------------

naicscostrun <- 1L #counter
load(file.path(model$outputdir,"naics_set.Rdata"))
naicstorun <- nrow(naics_set)

while (naicscostrun <= naicstorun){
  
  #get current running tasks
  tasklist <- system2( 'tasklist' , stdout = TRUE )
  
  #look for number of Groups running
  tasklist.tasks <- substr( tasklist[ -(1:3) ] , 1 , 25 )
  tasklist.tasks <- gsub( " " , "" , tasklist.tasks )
  numrscript <- length(tasklist.tasks[tasklist.tasks=="Rscript.exe"])
  print(paste("Supplier-Buyer Costs Rscript processes running:",numrscript, "Current time",Sys.time()))
  
  #is this less than max to run at once? 
  #If yes, run one more, if the next is available, else wait and then check
  if (numrscript < model$scenvars$maxcostrscripts){
    naics <- naics_set$NAICS[naicscostrun]
    groups <- naics_set$groups[naicscostrun]
    sprod <- ifelse(naics_set$Split_Prod[naicscostrun],1,0)
    system(paste("Rscript .\\scripts\\03_0a_Supplier_to_Buyer_Costs.R",naics,groups,sprod,model$basedir,model$outputdir),wait=FALSE)
	print(paste("Starting:",naics))
    naicscostrun <- naicscostrun + 1L
  } else {
    Sys.sleep( 30 )
  }
}


### --------------------------------
progressNextStep("Running PMGs")

naicsrun <- 1L #counter
load(file.path(model$outputdir,"naics_set.Rdata"))
naicstorun <- nrow(naics_set)

#Call the writePMGini function to write out the variables above to the PMG ini file at run time
writePMGini(model$scenvars,"./PMG/PMG.ini")

#start monitoring
if (model$scenvars$pmgmonitoring) system(paste("Rscript .\\scripts\\03b_Monitor_PMG.R",model$basedir,model$outputdir),wait=FALSE) 

while (naicsrun <= naicstorun){
  
  #get current running tasks
  tasklist <- system2( 'tasklist' , stdout = TRUE )
  
  #look for number of PMGs running
  tasklist.tasks <- substr( tasklist[ -(1:3) ] , 1 , 25 )
  tasklist.tasks <- gsub( " " , "" , tasklist.tasks )
  numrscript <- length(tasklist.tasks[tasklist.tasks=="Rscript.exe"])
  print(paste("Rscript processes running:",numrscript, "Current time",Sys.time()))
  
  #is this less than max to run at once? 
  #If yes, run one more, if the next is available, else wait and then check
  if (numrscript < model$scenvars$maxrscriptinstances){
    naics <- naics_set$NAICS[naicsrun]
    groups <- naics_set$groups[naicsrun]
    sprod <- ifelse(naics_set$Split_Prod[naicsrun],1,0)
    system(paste("Rscript .\\scripts\\03a_Run_PMG.R",naics,groups,sprod,model$basedir,model$outputdir),wait=FALSE)
    print(paste("Starting:",naics))
    naicsrun <- naicsrun + 1L
  } else {
    Sys.sleep( 30 )
  }
}



pmgcon <- progressEnd(pmgcon)

