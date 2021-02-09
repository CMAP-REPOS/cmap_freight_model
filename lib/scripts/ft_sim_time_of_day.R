# Estimates a start time for each tour
ft_sim_time_of_day <- function(shipments){

   # Total stop duration is the sum of all stops that trucks have to make in one day
   shipments[distchannel ==1,Day_ID:=Tour_ID2]
   directdctours <- max(shipments$Day_ID,na.rm=TRUE)
   shipments[distchannel > 1,Day_ID:= directdctours + .GRP,by=list(warehouse, NTours, Veh_Type)]
   shipments[,TotStopDur:=sum(StopDur1,na.rm=TRUE),by=Day_ID]
   shipments[,TotTourTime:=sum(Time),by=Day_ID]
   shipments[,TotPUDOWeight:=sum(PU_DO_Weight,na.rm=TRUE),by=Day_ID]
   
   #create Day_ID dataset
   shipments_day <- shipments[,list(TotStopDur=max(TotStopDur),TotTourTime=max(TotTourTime),TotPUDOWeight=max(TotPUDOWeight)),by=Day_ID]
   #Predict TOD: 1 = <6AM; 2 = 6-8AM; 3 = 8-9AM; 4 = 9-10AM; 5 = >10AM
   shipments_day[,CATEGORY:="A"] #Add category placeholder
   setkeyv(shipments_day, c("CATEGORY", unique(tod$VAR[tod$TYPE == "Variable"]))) #sorted on vars so simulated choice is ordered correctly
   
   df <- shipments_day[, list(Start = min(.I), Fin = max(.I)), by = eval(c("CATEGORY", unique(tod$VAR[tod$TYPE == "Variable"])))] #unique combinations of model coefficients
   df[, eval(unique(tod$VAR[tod$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
   print(paste(nrow(df), "unique combinations"))
   shipments_day[,TOD:= predict_logit(df,tod)]
   shipments_day[,CATEGORY:=NULL]
   rm(df)
   
   #Assign an exact start time for tours based on the midpoint of predicted TOD category
   shipments_day[,StartTime:= c(5,7,8.5,9.5,10.5)[TOD]]
   
   #Merge TOD and StartTime back onto full trip list
   shipments <- merge(shipments,shipments_day[,list(Day_ID, TOD, StartTime)],by=c("Day_ID"),all.x=TRUE)
   
   #For direct trips, allow some in time period 5 to start later
   #revised CMAP TOD Distribution for trips in Period 5 based on ISTHA/IDOT ATR traffic analysis)
   #TODO should this only apply to direct trips?
   shipments[distchannel ==1 & TOD == 5,temprand:=runif(.N)]
   shipments[distchannel ==1 & TOD == 5,StartTime:= c(12,15,17,19)[1+findInterval(temprand,c(0.431,0.665,0.858))]]
   shipments[,temprand:= NULL]
   
   #calculate start time, end for each trip
   #TODO what about sequencing the multiple tours in multi-tour patterns?
   for(i in 2:max(shipments$Trip_Seq)){
     
     tourruntime <- shipments[Trip_Seq==i-1,list(Tour_ID2,StartTime,Time,StopDur1)]
     tourruntime[is.na(Time),Time:=0]
     tourruntime[is.na(StopDur1),StopDur1:=0]
     tourruntime[,StartTime:=StartTime+Time+StopDur1]
     tourruntime[,c("Time","StopDur1"):=NULL]
     shipments[Trip_Seq==i,StartTime:=tourruntime$StartTime[match(Tour_ID2,tourruntime$Tour_ID2)]]
        
   }
   rm(tourruntime)
   shipments[,EndTime:=StartTime+Time]
   
   #Run a loop to add additional vehicle\drivers if:
   # any trips start later than 10 PM
   # if truck is overloaded
   #Just do this for the indirect trips
   
   shipments[,TourID_temp:=Tour_ID2]
   shipments[,WgtRatio:= max(Leg_Start_Wgt)/min(MaxWgt,na.rm=TRUE),by="TourID_temp"]
   shipments[,split:=1]
   
   iter <- 0
   while (max(shipments[distchannel>1]$StartTime)>22 | max(shipments[distchannel>1]$WgtRatio)>1){  
      
      shipments[distchannel>1,max_st:=max(StartTime),by=TourID_temp]
      shipments[distchannel>1,split:=ifelse(max_st/8 <2.5, 1, ifelse(max_st/8 < 3,2,floor(max_st/8)))]
      shipments[distchannel>1,split:=ifelse(split>=WgtRatio,split,ceiling(WgtRatio))]
      cat(" \nSplitting Tours ...", fill=T)
      cat("  Initial Number of Tours: ", max(shipments$TourID_temp), fill=T)
      shipments <- shipments[split==1|Trip_Seq < NTrips]   # drop return-to-warehouse trip
   
      #Run Cluster Analysis on Stops to Create New Tours based on Split
      shipments[split>1,Tour_ID:=cluster_shipments(data.frame(x_coord,y_coord),unique(split)),by=TourID_temp]
      
      #Update the Tour_ID_temp and Trip_ID to have seperate values for the split tours
      directdctours <- max(shipments[distchannel==1]$TourID_temp,na.rm=TRUE)
      shipments[distchannel > 1,TourID_temp:=directdctours + .GRP,by=list(TourID_temp,Tour_ID)]
      shipments[,Trip_ID:=1:.N,by=list(TourID_temp)]
      
      #sequence split trips
      trips_sequenced_split <- seq_trip(shipments[split > 1,list(Tour_ID2=TourID_temp,Trip_ID,o=Warehouse_zone,d=stop_zone)],mz_skims)
      setnames(trips_sequenced_split,"Tour_ID2","TourID_temp")
      #update the shipments table with these trips
      shipments_split <- shipments[split>1]
      shipments_split[,c("Trip_Seq","Time","o","d"):=NULL]
      shipments_split <- merge(trips_sequenced_split,shipments_split,by=c("TourID_temp","Trip_ID"),all=TRUE)
      shipments_split[,NTrips:=.N,by=TourID_temp] #add trips by tour include final return trip to warehouse
      shipments_split[,NTours:=max(NTours,na.rm=TRUE),by=TourID_temp] #copy NTours to all records in tour to overwite NA
      shipments_split[,Tour_ID:=max(Tour_ID,na.rm=TRUE),by=TourID_temp] #copy Tour_ID to all records in tour to overwite NA
      shipments_split[,Veh_Type:=max(Veh_Type,na.rm=TRUE),by=TourID_temp] #copy Veh_Type to all records in tour to overwite NA
      shipments_split[,warehouse:=max(warehouse,na.rm=TRUE),by=TourID_temp] #copy warehouse to all records in tour to overwite NA
      shipments_split[,distchannel:=max(distchannel,na.rm=TRUE),by=TourID_temp] #copy distchannel to all records in tour to overwite NA
      shipments_split[,split:=max(split,na.rm=TRUE),by=TourID_temp] #copy split to all records in tour to overwite NA
      shipments_split[,TOD:=max(TOD,na.rm=TRUE),by=TourID_temp] #copy TOD to all records in tour to overwite NA
         
      #Determine weight on truck throughout tour
      tourweights <- shipments_split[DO==1,list(Leg_Start_Wgt=sum(weight)),by=TourID_temp]
      shipments_split[Trip_Seq==1,Leg_Start_Wgt:=tourweights$Leg_Start_Wgt[match(TourID_temp,tourweights$TourID_temp)]]
      shipments_split[is.na(Leg_Start_Wgt),Leg_Start_Wgt:=0L]
      
      for(i in 2:max(shipments_split$Trip_Seq)){
        
        tourweights <- shipments_split[Trip_Seq==i-1,list(TourID_temp,Leg_Start_Wgt,DO,weight)]
        tourweights[,Leg_Start_Wgt:=Leg_Start_Wgt+ifelse(DO==1,-weight,weight)]
        shipments_split[Trip_Seq==i,Leg_Start_Wgt:=tourweights$Leg_Start_Wgt[match(TourID_temp,tourweights$TourID_temp)]]
        
      }
      rm(tourweights)
      
      #replace the splits in the shipments table
      shipments <- shipments[split==1]
      shipments <- rbind(shipments, shipments_split)
      
      #update the start times for the tours and trips
      #recalc these vars for the split tours
      shipments[,TotStopDur:=sum(StopDur1,na.rm=TRUE),by=Day_ID]
      shipments[,TotTourTime:=sum(Time),by=Day_ID]
      shipments[,TotPUDOWeight:=sum(PU_DO_Weight,na.rm=TRUE),by=Day_ID]
      
      #Assign an exact start time for tours based on the midpoint of predicted TOD category
      shipments[split>1,StartTime:= c(5,7,8.5,9.5,10.5)[TOD]]
      
      #calculate start time, end for each trip
      #TODO what about sequencing the multiple tours in multi-tour patterns?
      for(i in 2:max(shipments[split>1]$Trip_Seq)){
        
        tourruntime <- shipments[split>1 & Trip_Seq==i-1,list(TourID_temp,StartTime,Time,StopDur1)]
        tourruntime[is.na(Time),Time:=0]
        tourruntime[is.na(StopDur1),StopDur1:=0]
        tourruntime[,StartTime:=StartTime+Time+StopDur1]
        tourruntime[,c("Time","StopDur1"):=NULL]
        shipments[split> 1 & Trip_Seq==i,StartTime:=tourruntime$StartTime[match(TourID_temp,tourruntime$TourID_temp)]]
        
      }
      rm(tourruntime)
      shipments[split>1,EndTime:=StartTime+Time]
      
      #recalculate weight ratio
      shipments[,WgtRatio:= max(Leg_Start_Wgt)/min(MaxWgt,na.rm=TRUE),by="TourID_temp"]
      #print out status
      iter = iter + 1
      cat("Iteration:", iter," Max Start Time: ", max(shipments[distchannel>1]$StartTime), " and Max Weight Ratio: ", max(shipments[distchannel>1]$WgtRatio))
      
   }   
   
   cat("  Final Number of Tours: ", max(shipments$TourID_temp), fill=T)
   #clean up the shipments table
   shipments[,Tour_ID2:=TourID_temp]
   shipments[,c("split","max_st","TourID_temp"):=NULL]
   shipments[,Day_ID:=max(Day_ID,na.rm=TRUE),by=Tour_ID2] #copy Day_ID to all records in tour to overwite NA
   shipments[,Nstops:=NTrips-1L]
   
   # Return the processed shipments table
   return(shipments)

}
