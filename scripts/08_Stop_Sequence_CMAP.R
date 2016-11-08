##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       8_Stop_Sequence.R group the stops into tours and sequences the stops
#                   in each tour
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 8 Stop Sequence
#-----------------------------------------------------------------------------------
progressStart(stopseq,6)

#-----------------------------------------------------------------------------------
#Apply the Number of Tours Model
#-----------------------------------------------------------------------------------
progressNextStep("Apply the Number of Tours Model")

#8.1 First determine the the category of number of truck tours each shipment falls in:
#all in one tour, two tours, three tours, or four tours
#all in one tour means the shipment belongs to a tour category in which 
#a truck covers all the stop assigned to it in a single tour
#direct distribution channel shipments are in single direct tour
#shipments with complex dist channels but are in direct tours can be in multiple tour patterns

shipments[,Dest_Dist:=0L] #TODO is this correct? Should there sometimes be a variable here? Check specification
shipments[,Dest_Ware:=ifelse((DO==1 & substr(Buyer.NAICS,1,2) %in% 48:49)|(DO==0 & substr(NAICS,1,2) %in% 48:49),1,0)]
shipments[,Dest_Cont:=ifelse((DO==1 & substr(Buyer.NAICS,1,2) == 23)|(DO==0 & substr(NAICS,1,2) == 23),1,0)]
shipments[,PU_DO_Weight:=log(weight+1)]

#Simulate the number of truck tours category for each shipment.
#Each shipment also represents a stop to be made on a truck tour.
#NTours: 1 - all stops in one tour, 2 - all in two tours, 3 - all in three tours, 4 - all in four tours
shipments[,CATEGORY:="A"] #Add category placeholder
setkeyv(shipments, c("CATEGORY", unique(numberoftours$VAR[numberoftours$TYPE == "Variable"]))) #sorted on vars so simulated choice is ordered correctly
shipments_nd <- shipments[distchannel > 1]
df <- shipments_nd[, list(Start = min(.I), Fin = max(.I)), by = eval(c("CATEGORY", unique(numberoftours$VAR[numberoftours$TYPE == "Variable"])))] #unique combinations of model coefficients
df[, eval(unique(numberoftours$VAR[numberoftours$TYPE == "Constant"])) := 1] #add 1s for constants to each group in df
print(paste(nrow(df), "unique combinations"))
shipments[distchannel > 1,NTours:= predict_logit(df,numberoftours)]
shipments[distchannel == 1,NTours:= 1L]
shipments[,CATEGORY:=NULL]
rm(df,shipments_nd)

#-----------------------------------------------------------------------------------
#Cluster the Stops into Tours
#-----------------------------------------------------------------------------------
progressNextStep("Clustering Stops into Tours")

#8.2 Cluster analysis to determine the stops in a particular tour for indirect shipments
#Assign vehicle type: 1=2 axle, 2=3-4 axle, 3=semi\trailer
shipments[,Veh_Type:= c(1L,2L,3L,1L,2L,3L)[TP_Veh]]

#Aggregate the number of trips\stops by warehouse, number of tours category, and vehicle type
#Vehicle type is also used so that stops grouped\clustered together have the same vehicle type
shipments[distchannel > 1,NTours_Trips:=.N,by=list(warehouse,NTours,Veh_Type)]
shipments[distchannel == 1,NTours_Trips:=1L]

#Force the number of tours to be less than or equal to the number of stops and recalc SStrips
shipments[,NTours:=ifelse(NTours>NTours_Trips, NTours_Trips, NTours)]
shipments[distchannel > 1,NTours_Trips:=.N,by=list(warehouse,NTours,Veh_Type)]

#Merge in the coordinates for each stop
shipments[,stop_zone:=ifelse(DO==1,dest_zone,orig_zone)] #i.e, the non-warehouse end of the trip
### Heither, 05-09-2016: read in zonal coordinates
mz_centroids <- as.data.table(read.csv(file=paste0(model$inputdir,"/cmap_data_zone_centroids.csv"),sep=",",header=T))
setkey(mz_centroids,stop_zone)
shipments <- merge(shipments,mz_centroids,by="stop_zone")

#Assign cluster for each tour category (2 tours, 3 tours, 4 tours)
cluster_shipments <- function(df,k){ 
  if(k>1){
    d <- dist(df, method = "euclidean")
    #fastcluster hclust function
    fit <- hclust(d, method="ward.D")
    #Assign tour id based on cluster group number
    Tour_ID <- cutree(fit, k=k)
  } else {
    Tour_ID <- 1L
  }
}
  
shipments[,Tour_ID:=cluster_shipments(data.frame(x_coord,y_coord),unique(NTours)),by=list(warehouse,NTours,Veh_Type)]

stopseqstopclustersample <- data.frame(shipments[WarehouseID %in% sample(unique(shipments[distchannel>1]$WarehouseID),5),])
qplot(x_coord, y_coord, data = stopseqstopclustersample, colour = Tour_ID) + facet_grid(warehouse ~ NTours)  
ggsave(file.path(model$outputdir,"stopseq_stopcluster_sample.png"))

#-----------------------------------------------------------------------------------
#Sequence the Stops in Each Tour
#-----------------------------------------------------------------------------------
progressNextStep("Sequencing the Stops in Each Tour")

#8.3 Assign stop sequence to the clustered stops within each tour
#Assign a unique tour ID across all tours, and a trip ID within each tour
shipments[distchannel == 1,Tour_ID2:=1:.N]
directdctours <- max(shipments$Tour_ID2,na.rm=TRUE)
shipments[distchannel > 1,Tour_ID2:=directdctours + .GRP,by=list(warehouse,NTours,Veh_Type,Tour_ID)]
shipments[,Trip_ID:=1:.N,by=list(Tour_ID2)]

#Tour starts at the warehouse, stops are sequenced, final trip is last to warehouse
seq_trip <- function(df,skims){
  
  df[,c("Trip_Seq","to","td"):=0L]
  
  for (i in 1:max(df$Trip_ID)){
    df <- merge(df,skims,by=c("o","d"),all.x=TRUE) #merge skim
    df[is.na(Time),Time:=0] #missing skims, e.g. externals
    df[Trip_Seq==0,Trip_Seq:=ifelse(.I==.I[which.min(Time)],i,0L),by=Tour_ID2] #find shortest trip from o
    df[,c("to","td"):=list(ifelse(Trip_Seq==i,o,to),ifelse(Trip_Seq==i,d,td))] #update trip o and d
    df[,o:=d[Trip_Seq==i],by=Tour_ID2] #update origin
    df[,Time:=NULL] #remove time field
  }
  
  df[,c("o","d"):=list(to,td)]
  df[,c("to","td"):=NULL]
  
  #construct return trip
  dfr <- data.table(d=df[,list(o=o[Trip_Seq==1]),by=Tour_ID2]$o,
  o=df$d[df[,.I[which.max(Trip_Seq)],by=Tour_ID2]$V1],
  Tour_ID2=unique(df$Tour_ID2),
  Trip_ID=df[,list(Trip_ID=max(Trip_ID)+1L),by=Tour_ID2]$Trip_ID,
  Trip_Seq=df[,list(Trip_Seq=max(Trip_Seq)+1L),by=Tour_ID2]$Trip_Seq)
  
  df <- rbind(df,dfr,use.names=TRUE) #add return trips to complete list
  df <- merge(df,skims,by=c("o","d"),all.x=TRUE) #merge skim
  df[is.na(Time),Time:=0] #missing skims, e.g. externals
  
}

### Heither, 05-09-2016: read in zonal skim times (use Peak times)
mz_skims <- as.data.table(read.csv(file=paste0(model$inputdir,"/cmap_data_zone_skims.csv"),sep=",",header=T))
mz_skims[,c("OffPeak","Miles"):=NULL]
setnames(mz_skims,c("Origin","Destination","Peak"),c("o","d","Time"))
setkey(mz_skims,o,d)

#sequence warehouse trips first, and then the direct distchannel DO and PU trips
#Note skims are just internal -- any IE/EI trips will get a time = 0
trips_sequenced_wh <- seq_trip(shipments[distchannel > 1,list(Tour_ID2,Trip_ID,o=Warehouse_zone,d=stop_zone)],mz_skims)
trips_sequenced_do <- seq_trip(shipments[distchannel == 1 & DO == 1,list(Tour_ID2,Trip_ID,o=orig_zone,d=stop_zone)],mz_skims)
trips_sequenced_pu <- seq_trip(shipments[distchannel == 1 & DO == 0,list(Tour_ID2,Trip_ID,o=stop_zone,d=dest_zone)],mz_skims)
trips_sequenced <- rbind(trips_sequenced_wh,trips_sequenced_do,trips_sequenced_pu)

#merge the sequenced trips with the shipments dataset
shipments <- merge(trips_sequenced,shipments,by=c("Tour_ID2","Trip_ID"),all=TRUE)
shipments[,NTrips:=.N,by=Tour_ID2] #add trips by tour include final return trip to warehouse
shipments[,NTours:=max(NTours,na.rm=TRUE),by=Tour_ID2] #copy NTours to all records in tour to overwite NA
shipments[,Tour_ID:=max(Tour_ID,na.rm=TRUE),by=Tour_ID2] #copy Tour_ID to all records in tour to overwite NA
shipments[,Veh_Type:=max(Veh_Type,na.rm=TRUE),by=Tour_ID2] #copy Veh_Type to all records in tour to overwite NA
shipments[,warehouse:=max(warehouse,na.rm=TRUE),by=Tour_ID2] #copy warehouse to all records in tour to overwite NA
shipments[,distchannel:=max(distchannel,na.rm=TRUE),by=Tour_ID2] #copy distchannel to all records in tour to overwite NA
#TODO should we merge back on to shipments or maintain as diff table? what about all NAs after merge?

### -- Heither, 12-28-2015: Set values for return-to-warehouse trips
shipments[Trip_Seq==NTrips,Quantity.Traded:=0]
shipments[Trip_Seq==NTrips,Last.Iteration.Quantity:=0]
shipments[Trip_Seq==NTrips,BuyerID:=-999]
shipments[Trip_Seq==NTrips,SellerID:=-999]

#-----------------------------------------------------------------------------------
#Calculate the Truck Load During the Tour
#-----------------------------------------------------------------------------------
progressNextStep("Calculate the Truck Load During the Tour")

#Determine weight on truck throughout tour
tourweights <- shipments[DO==1,list(Leg_Start_Wgt=sum(weight)),by=Tour_ID2]
shipments[Trip_Seq==1,Leg_Start_Wgt:=tourweights$Leg_Start_Wgt[match(Tour_ID2,tourweights$Tour_ID2)]]
shipments[is.na(Leg_Start_Wgt),Leg_Start_Wgt:=0L]

for(i in 2:max(shipments$Trip_Seq)){
  
  tourweights <- shipments[Trip_Seq==i-1,list(Tour_ID2,Leg_Start_Wgt,DO,weight)]
  tourweights[,Leg_Start_Wgt:=Leg_Start_Wgt+ifelse(DO==1,-weight,weight)]
  shipments[Trip_Seq==i,Leg_Start_Wgt:=tourweights$Leg_Start_Wgt[match(Tour_ID2,tourweights$Tour_ID2)]]
  
}
rm(tourweights)

stopseq$summaryfunc <- function(steplist){
  
  labels_ss <- c("<999 lbs","1k-10k lbs","> 10k lbs")
  labels_nt <- paste("All stops in",c("1 tour","2 tours","3 tours","4 tours"))
  
  shipsizebynumtours <- shipments[!is.na(ship_size),.N,by=list(ship_size,NTours)]
  shipsizebynumtours[,ss_lab:=labels_ss[ship_size]]
  shipsizebynumtours[,Tour_Category:=labels_nt[NTours]]
  
  stopseqlist <- list()
  stopseqlist$stopseqtourcatbyshipsize <- dcast.data.table(shipsizebynumtours,Tour_Category~ss_lab,fun=sum,value.var="N")            
  stopseqlist$stopseqnumstopspertour <- shipments[,list(stop_count=max(NTrips,na.rm=TRUE)),by=list(Tour_ID2)][,list(Frequency=.N),by=stop_count][order(stop_count)]
  
  return(stopseqlist)
  
}

stopseq <- progressEnd(stopseq)