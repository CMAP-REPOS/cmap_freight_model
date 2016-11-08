##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       9_Stop_Duration.R estimates the stop durations for each stop on each tour
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 9 Stop Duration
#-----------------------------------------------------------------------------------
progressStart(stopdur,3)

#-----------------------------------------------------------------------------------
#Apply Stop Duration Model
#-----------------------------------------------------------------------------------
progressNextStep("Apply Stop Duration Model")

#Creating additional variables required
shipments[,Veh_class_1:=ifelse(Veh_Type == 1,1L,0L)]
shipments[,Veh_class_2:=ifelse(Veh_Type == 2,1L,0L)]
shipments[,Nstops:=NTrips-1L]
shipments[,Direct:=ifelse(TP_Veh %in% 1:3,1L,0L)]

#Predict stop duration category
shipments[,CATEGORY:="A"] #Add category placeholder
setkeyv(shipments, c("CATEGORY", unique(stopduration$VAR[stopduration$TYPE == "Variable"]))) #sorted on vars so simulated choice is ordered correctly
df <- shipments[Trip_Seq<=Nstops, list(Start = min(.I), Fin = max(.I)), by = eval(c("CATEGORY", unique(stopduration$VAR[stopduration$TYPE == "Variable"])))] #unique combinations of model coefficients
df[, eval(unique(stopduration$VAR[stopduration$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
print(paste(nrow(df), "unique combinations"))
shipments[Trip_Seq<=Nstops,StopDur:= predict_logit(df,stopduration)]
shipments[,CATEGORY:=NULL]
rm(df)

#Assign an exact stop duration based on the midpoint of predicted duration category
shipments[,StopDur1:=c(0.25, 0.375, 0.625, 0.875, 1.125,2)[StopDur]]

stopdur$summaryfunc <- function(steplist){
  
  labels_sd <- c("<15 mins","15-30 mins","30-45 mins","45-60 mins","60-75 mins",">75 mins")
  
  durationbytourtype <- shipments[!is.na(StopDur),.N,by=list(Direct,StopDur)]
  durationbytourtype[,Stop_Duration:=labels_sd[StopDur]]
  durationbytourtype[,TourType:=ifelse(Direct==1,"DirectTour_Stops","PeddlingTours_Stops")]
  
  stopdurlist <- list()
  stopdurlist$stopdurdurationbytourtype <- dcast.data.table(durationbytourtype,Stop_Duration~TourType,fun=sum,value.var="N")            
  return(stopdurlist)
  
}

stopdur <- progressEnd(stopdur)
