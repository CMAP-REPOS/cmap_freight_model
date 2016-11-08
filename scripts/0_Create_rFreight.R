##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       0_Create_rFreight.R creates the rFreight package
#Date:              May 7, 2015
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2015 RSG, Inc. - All rights reserved.
##############################################################################################

#Create and Build the rFreight package
#Rerun this after updating any functions in the rFreight package

#install.packages("devtools")
library("devtools")
#find_rtools()
#devtools::install_github("klutometis/roxygen")
library(roxygen2)
#create the package structure
#create("rFreight")

#Add functions
# Add them as individual R script files in the R folder in the new package structure

#Add documentation
# use Roxygen style

#set the default author and licenses
# options(devtools.desc.author = '"Colin Smith <colin.smith@rsginc.com> [aut,cre]"',
#         devtools.desc.license = "GPL-3")
# create_description("C:/Projects/FHWA Chicago Freight/chicago-freight/rFreight", extra = getOption("devtools.desc"), quiet = FALSE)

#load the latest data into the data folder so those can be documented and included in the package
pathtobase <- "./scenarios/base/inputs"
inputfiles <- list.files(file.path(pathtobase))
inputrclist <- list() #for holding row and col numbers

for (i in 1:length(inputfiles)){
  objname <- sub(".csv","",inputfiles[i])
  assign(objname, read.csv(file.path(pathtobase,inputfiles[i])))  
  save(list=objname, file=paste0("./rFreight/data/",sub(".csv","",inputfiles[i]),".rda"))
  inputrclist[[i]] <- list(objname,
                           nrow(get(objname)),
                           ncol(get(objname)),
                           names(get(objname)),
                           head(get(objname)))
  rm(list=objname)
}
rm(inputfiles,pathtobase)

#Keep the list and also create R files for the documentation if they don't exist
inputfilenames <- sapply(inputrclist,"[[",1)
for (i in 1:length(inputfilenames)){
  if(!file.exists(paste0("./rFreight/R/",inputfilenames[i],".R"))){
    file.copy(from="./rFreight/R/corresp_naics6_n6io_sctg.R",
              to=paste0("./rFreight/R/",inputfilenames[i],".R"))
  }
}

#Process documentation
document(pkg="rFreight")

#Check and create documentation
check(pkg="rFreight",cleanup=FALSE,check_dir="./check")
file.copy(from="./check/rFreight.Rcheck/rFreight-manual.pdf",
          to="./rFreight/rFreight-manual.pdf",
          overwrite=TRUE)

#Build zip
build(pkg="rFreight",binary=TRUE)
