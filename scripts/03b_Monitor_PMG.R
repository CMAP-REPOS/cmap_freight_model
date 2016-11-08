##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       03b_MonitorPMG.R monitors the progress of the run and send summaries by email                   
#Date:              June 26 28, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 3b Monitor PMG
#-----------------------------------------------------------------------------------
#Set the base directory (the directory in which the model resides)
args <- commandArgs(TRUE)
basedir <- args[1]
outputdir <- args[2]
setwd(basedir)

#load the workspace for creating the PMG inputs
#includes naics_set table and monitoring settings in model$scenvars
load(file.path(outputdir,"PMG_Workspace.Rdata"))
library(rFreight)
loadPackage("data.table")
loadPackage("sendmailR")

#send email update function
Send_Email_Update <- function(from,to,smtp){
  subject <- "CMAP Freight Update - MOD01"
  body <- paste("Update on CMAP Freight Model run currently in progress at", Sys.time())                     
  mailControl=list(smtpServer=smtp)
  
  #needs full path if not in working directory
  attachmentPath <- file.path(outputdir,"sum_pmg_log.txt")
  attachmentName <- "sum_pmg_log.txt"
  
  #key part for attachments, put the body and the mime_part in a list for msg
  attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
  bodyWithAttachment <- list(body,attachmentObject)
  
  sapply(to, function(x) sendmail(from=from,to=x,subject=subject,msg=bodyWithAttachment,control=mailControl))
}

pmgstorun <- sum(naics_set$groups)
pmgsrun <- 0L

#wait for a while before starting sending emails to let the first few groups get going
Sys.sleep( 600 ) #10 minutes

while (pmgsrun < pmgstorun){

  #look at list of outputs
  outputs <- file.info(file.path(outputdir,list.files(outputdir)))
  outputs <- data.table(filename = basename(row.names(outputs)),outputs)
  
  #pmg log files
  pmglogs <- outputs[grep("g[0-9]+.txt",filename),]
  if(nrow(pmglogs) > 0){
    pmglogs[,rtime:=as.numeric(mtime - ctime, units= "secs")]
    pmglogs[,stime:=as.numeric(Sys.time() - ctime, units= "secs")]
    pmglogs[,NAICS:= sapply(strsplit(filename,"[/_g.]"),"[",1)]
    pmglogs[,group:= sapply(strsplit(filename,"[/_g.]"),"[",3)]
  }
  
  #pmg outputs
  pmgout <- outputs[grep("out.csv",filename),]
  pmgsrun <- nrow(pmgout)
  if (pmgsrun > 0){
    pmgout[,NAICS:= sapply(strsplit(filename,"[/_g.]"),"[",1)]
    pmgout[,group:= sapply(strsplit(filename,"[/_g.]"),"[",3)]
    setnames(pmgout,"filename","outfilename")
  
    #compare logs and outputs and list of naics groups to be run
    pmglogs <- merge(pmglogs,pmgout[,list(outfilename,NAICS,group)],by=c("NAICS","group"),all=TRUE)
    pmgsummary <- pmglogs[!is.na(outfilename),list(groupsrun=.N,mintime=min(rtime),maxtime=max(rtime),meantime=mean(rtime)),by=NAICS]
    pmgsummary <- merge(pmgsummary, naics_set,by="NAICS",all=TRUE)
  }
  
  #Summarize
  sum_pmg_log <- list()
  sum_pmg_log[["Total_Groups"]] <- pmgstorun
  sum_pmg_log[["Groups_Run"]] <- pmgsrun
  if (nrow(pmglogs) > 0) sum_pmg_log[["Running_Now"]] <- pmglogs[is.na(outfilename),] #still running (no out file)
  if (pmgsrun > 0) sum_pmg_log[["Output_Summary"]] <- data.frame(pmgsummary)
  
  #Writeoutput and email it
  capture.output(print(sum_pmg_log),file=file.path(outputdir,"sum_pmg_log.txt") )
  Send_Email_Update(from=model$scenvars$pmgmonfrom,
                    to=model$scenvars$pmgmonto,
                    smtp=model$scenvars$pmgmonsmtp)
  
  #wait for a while before repeating
  Sys.sleep( model$scenvars$pmgmoninterval )
  
}  