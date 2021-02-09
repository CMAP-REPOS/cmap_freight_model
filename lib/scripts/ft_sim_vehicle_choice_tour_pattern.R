# Applies the vehicle and tour pattern choice model to assign a tour pattern and vehicle to each shipment
ft_sim_vehicle_choice <- function(shipments){

  #Create variables required for simulating the choice of tour and vehicle type
  shipments[,Food:=ifelse(Commodity_SCTG %in% c(1:3,5:7),1L,0L)]
  shipments[,Mac:=ifelse(Commodity_SCTG %in% c(34,38:40),1L,0L)]
  shipments[,DO:=ifelse(triptype %in% c(1,3,5,7,9),1L,0L)] #includes a drop off in the region
  shipments[,DO_Weight:=ifelse(DO==1,log(weight+1),0)]
  shipments[,PU_Weight:=ifelse(DO==0,log(weight+1),0)]
  shipments[,Dest_Mac:=ifelse((DO==1 & substr(Buyer.NAICS,1,2) %in% 31:33)|(DO==0 & substr(NAICS,1,2) %in% 31:33),1,0)]
  shipments[,Dest_Office:=ifelse((DO==1 & substr(Buyer.NAICS,1,2) %in% 51:56)|(DO==0 & substr(NAICS,1,2) %in% 51:56),1,0)]
  shipments[,Dest_Retail:=ifelse((DO==1 & substr(Buyer.NAICS,1,2) %in% 44:45)|(DO==0 & substr(NAICS,1,2) %in% 44:45),1,0)]
  
  #Add total employment by county and merge in based on buyer
  emp_cbpzone <- merge(emp_cbpzone,mzemp[,list(COUNTY,MESOZONE)],by="COUNTY",all.x=TRUE,allow.cartesian=TRUE)
  emp_cbpzone[COUNTY<=123,MESOZONE:=COUNTY+150L]
  ### -- Heither, 05-09-2016: modified to reflect that Zonal values are in orig_zone/dest_zone
  ##shipments[,CBP_EMP:=ifelse(DO==1,emp_cbpzone$CBP_EMP[match(dest_zone,emp_cbpzone$MESOZONE)],emp_cbpzone$CBP_EMP[match(orig_zone,emp_cbpzone$MESOZONE)])]
  shipments[,CBP_EMP:=ifelse(DO==1,emp_cbpzone$CBP_EMP[match(MzCz,emp_cbpzone$MESOZONE)],emp_cbpzone$CBP_EMP[match(MzPz,emp_cbpzone$MESOZONE)])]
  shipments[,CBP_EMP:=log(CBP_EMP+1)]
  
  #Tour pattern and vehicle choices (TP_Veh): 
  #1 - direct, 2 axle; 2 - direct, 3-4 axle; 3 - direct, semi/trailer; 
  #4 - peddling, 2 axle; 5 - peddling, 3-4 axle; 6 - peddling, semi/trailer
  shipments[,CATEGORY:="A"] #Add category placeholder
  setkeyv(shipments, c("CATEGORY", unique(vehtourpat$VAR[vehtourpat$TYPE == "Variable"]))) #sorted on vars so simulated choice is ordered correctly
  df <- shipments[, list(Start = min(.I), Fin = max(.I)), by = eval(c("CATEGORY", unique(vehtourpat$VAR[vehtourpat$TYPE == "Variable"])))] #unique combinations of model coefficients
  df[, eval(unique(vehtourpat$VAR[vehtourpat$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
  print(paste(nrow(df), "unique combinations"))
  shipments[,TP_Veh:= predict_logit(df, vehtourpat)]
  shipments[,CATEGORY:=NULL]
  rm(df)
  
  # For distribution channel = 1, update tour pattern to be direct but maintain chosen vehicle type
  shipments[distchannel == 1 & TP_Veh > 3,TP_Veh:=TP_Veh - 3L]
  
  ### Adjust Vehicle Choice Based on Shipment Size 
  
  ## -- Heither, revised 04-19-2016: Modify high weights to allow shipments to fit on trucks.
  shipments[weight>=100000,daily_load:=daily_load*2]		## double number of loads
  shipments[weight>=100000,weight:=as.integer(weight/2)]	## and divide weight in half
  ## ---------------------------------------------------------------
  
  shipments[,load_wgt:=daily_load*weight]    ### total weight of daily shipment
  #Adjustments:
  #1. If weight of single shipment exceeds maximum load value for 
  #vehicle types 1/4, 2/5: move up to next larger truck.
  shipments[,upgrade:=0L] #flag that truck was upgraded, value shows how many upgrade steps
  shipments[TP_Veh %in% c(1,4) & weight > BASE_WGTMAX_2XL,
            c("upgrade","TP_Veh"):=list(1L,TP_Veh+1L)]
  shipments[TP_Veh %in% c(2,5) & weight > BASE_WGTMAX_3XL,
            c("upgrade","TP_Veh"):=list(upgrade+1L,TP_Veh+1L)]
  
  #2. If total shipment load (weight*daily_load) exceeds maximum load value for trucks: 
  #split into useable shiments for truck size.
  cat(" -> Number of Shipments Prior to Shipment Load Adjustment: ", nrow(shipments), fill=T)
  shipments[,MaxWgt:=c(BASE_WGTMAX_2XL, BASE_WGTMAX_3XL, BASE_WGTMAX_SEMI, BASE_WGTMAX_2XL, BASE_WGTMAX_3XL, BASE_WGTMAX_SEMI)[TP_Veh]]
  shipments[,ShipSplit:=ifelse(load_wgt>MaxWgt, 1L, 0L)]
  split <- shipments[ShipSplit==1]
  cat(" -> Number of Shipments That Require Splitting: ", nrow(split), fill=T)
  shipments <- shipments[ShipSplit==0]
  split[,Split_Id:=.I] # Assign unique ID to each initial shipment for tracking later
  
  
  #if weight of 1 shipment more than half of truck capacity - only use one shipment per truck
  #otherwise, estimate the number of trucks needed if each filled to 80% capacity
  split[,new_load:=ifelse(weight/MaxWgt>0.5, daily_load, as.integer(MaxWgt*0.8)/weight)]
  split[,flag:=ifelse(weight/MaxWgt>0.5, 1L, 0L)]	# Flag 1-shipment-per-truck flows
  split[,trucks:=ifelse(daily_load==new_load, new_load, ceiling(daily_load/new_load))]
  split<-split[rep(seq_len(.N),trucks),]	# Enumerate trucks
  #Adjust Loads to ensure a match with original value
  split[,Obs:=.I]
  split[,LastObs:=max(Obs),by=Split_Id]
  split[,new_load:=ifelse(flag==1, 1,
                           ifelse(Obs==LastObs, daily_load-((trucks-1)*new_load), new_load))]
  #Adjust Tonnage, Value and Load Weight 
  split[,load_wgt:=weight*new_load]
  split[,daily_load:=new_load]
  split[,c("new_load","trucks","Obs","LastObs","flag"):=NULL]
  
  ### --- Heither, 04-18-2016: check load weights
  ###chk <- split[load_wgt>75000]
  ###write.csv(chk, file="C:/cmh/heavy_daily_loads.csv", row.names=F) 
  
  #3. Combine Data
  shipments <- rbind(shipments,split,fill=TRUE)
  shipments[is.na(Split_Id),Split_Id:=0L]
  cat(" -> Number of Shipments After Shipment Load Adjustments: ", nrow(shipments), fill=T)
  rm(split) 
  
  ## -- Heither, revised 04-21-2016: calculate daily shipment value
  shipments[,DailyValue:=(load_wgt/PurchaseAmountTons)*ConVal]  ### daily shipment value in dollars
  
  # Return the processed shipments table
  return(shipments)

}
