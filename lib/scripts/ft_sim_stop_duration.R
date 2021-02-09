#Estimates the stop durations for each stop on each tour
ft_sim_stop_duration <- function(shipments){

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
  
  # Return the processed shipments table
  return(shipments)

}
