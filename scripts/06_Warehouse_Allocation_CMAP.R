##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       6_WarehouseAllocation.R assigns each shipment with an intermediate
#                   stop to a warehouse locations within the model region
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 6 Warehouse Allocation
#-----------------------------------------------------------------------------------
progressStart(whouse,5)

#-----------------------------------------------------------------------------------
#Identify Shipment Type
#-----------------------------------------------------------------------------------
progressNextStep("Identify Shipment Type")

#Add a trip type to identify what the shipment represents
#Combination of shipment end locations and types
#Sample only includes shipments with at least one end in model region
#Types do not include warehouse to warehouse, warehouse to external, or external to warehouse trips 

#Type Seller Buyer   Distchannel Origin            Destination       Description             
#1    In     In      Direct      Seller            Buyer             Direct within region              
#2    In     Out     Direct      Seller            Buyer (External)  Direct from region      
#3    Out    In      Direct      Seller (External) Buyer             Direct to region        
#4    In     In      Indirect-1  Seller            Warehouse 1       Indirect within region, one warehouse, seller to warehouse
#5    In     In      Indirect-1  Warehouse 1       Buyer             Indirect within region, one warehouse, warehouse to buyer
#6    In     In      Indirect-2  Seller            Warehouse 1       Indirect within region, two warehouses, seller to warehouse 1
#7    In     In      Indirect-2  Warehouse 2       Buyer             Indirect within region, two warehouses, warehouse 2 to buyer
#8    In     Out     Indirect    Seller            Warehouse         Indirect from region, seller to warehouse
#9    Out    In      Indirect    Warehouse         Buyer             Indirect to region, warehouse to buyer

#Add triptype field and fill it with values
shipments[,triptype:=0L] 
shipments[distchannel==1,triptype:=ifelse(Production_zone<150 & Consumption_zone<150,1L,ifelse(Consumption_zone>150,2L,3L))]
shipments[distchannel>1,triptype:=ifelse(Production_zone<150 & Consumption_zone<150,4L,ifelse(Consumption_zone>150,8L,9L))]

#split the triptype 4s into 4-7, including adding the additional records for the delivery portion of the trip
shipments[distchannel>2 & triptype==4,triptype:=6L]
shipments_add <- shipments[triptype %in% c(4,6)]
shipments_add[,triptype:= triptype + 1L]
shipments <- rbind(shipments,shipments_add,use.names=TRUE)
rm(shipments_add)

#-----------------------------------------------------------------------------------
#Allocate Warehouses
#-----------------------------------------------------------------------------------
progressNextStep("Allocate Warehouses")

#For firm pairs using indirect distribution channels 2, 3, 4
#Get the mesozone of the wareshouse that would represent:
#the last stop for incoming shipment (requires delivery)
#the first stop for an outgoing shipment (requires pickup).
#Pick one warehouse randomly selected as last\first stop.

## -- Heither, read in CMAP warehouse data
###load(file.path(model$outputdir,"warehouses.Rdata")) #saved in firmsyn step
warehouses <- as.data.table(read.csv(file=paste0(model$inputdir,"/cmap_warehouses.csv"),sep=",",header=T))
warehouses[,warehouse:=.I]

######warehouses <- warehouses[,list(MESOZONE,warehouse,BusID)]
setnames(warehouses,c("zone09","MESOZONE","BusID"),c("Warehouse_zone","Mz","WarehouseID"))
#pick a warehouse for the indirect trips, but just the first portion of internal to internal
shipments[,warehouse:=ifelse(triptype %in% c(4,6,8,9),sample.int(max(warehouses$warehouse),.N,replace=TRUE),0L)]
shipments[triptype==5,warehouse:=shipments[triptype==4]$warehouse[match(ShipmentID,shipments[triptype==4]$ShipmentID)]]
shipments[triptype==7,warehouse:=sapply(1:.N,function(x) sample(warehouses$warehouse[-shipments[triptype==6][x]$warehouse],1))]
shipments <- merge(shipments, warehouses, by = "warehouse", all.x=T)

#-----------------------------------------------------------------------------------
#Transform firm locations from Mesozones to Zones
#-----------------------------------------------------------------------------------
## -- Heither, 05-06-2016
     #### -- CREATE LIST OF UNIQUE CMAP FIRMS -- #### 
firm1 <- shipments[Production_zone<132,list(SellerID,Production_zone)]
firm2 <- shipments[Consumption_zone<132,list(BuyerID,Consumption_zone)]
setnames(firm1,c("SellerID","Production_zone"),c("firmID","mesozone"))
setnames(firm2,c("BuyerID","Consumption_zone"),c("firmID","mesozone"))
allb <- as.data.table(rbind.fill(firm1,firm2))
allb_Uniq <- allb[!duplicated(allb$firmID),]                                    ### list of unique business locations
setkey(allb_Uniq,firmID)
set.seed(86753)
allb_Uniq[,zn_prob:=runif(nrow(allb_Uniq))]          
rm(allb) 

     #### -- SUMMARIZE ZONAL EMPLOYMENT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -- ####
  ## -- C16Q1 TOTAL EMPLOYMENT: ATTACH ZONES/MESOZONES, SUMMARIZE AT ZONAL LEVEL, CALCULATE CUMULATIVE SHARE OF MESOZONE EMPLOYMENT IN EACH ZONE -- ## 
zn_totemp <- as.data.table(read.csv(file=paste0(model$inputdir,"/cmap_data_zone_employment.csv"),header=T,sep=","))
mz_totemp <- zn_totemp[,list(Zone,totalemp,Cumsum=cumsum(totalemp)),list(mesozone)]       ### cumulative employment by mesozone
mz_tot <- zn_totemp[,list(MZemp=sum(totalemp)),by=list(mesozone)]
setkey(mz_totemp,mesozone)
setkey(mz_tot,mesozone)
mz_totemp <- merge(mz_totemp,mz_tot,by="mesozone",all.x=T)
mz_totemp[,emp_share:=Cumsum/MZemp]
  ## -- DETERMINE LOCATION OF FIRM -- ##
setkey(allb_Uniq,mesozone)
allb_Uniq <- merge(allb_Uniq,mz_totemp,by="mesozone",all.x=T,allow.cartesian=T)		
allb_Uniq <- allb_Uniq[zn_prob <= emp_share]                   	 ### keep zones with ge probabilities
setkey(allb_Uniq,firmID,Zone)                                  
allb_Uniq <- allb_Uniq[,.SD[which.min(Zone)],by=firmID]          ### locate firm in zone
allb_Uniq[,c("mesozone","zn_prob","totalemp","Cumsum","MZemp","emp_share"):=NULL]

  ## -- ATTACH ZONAL EMPLOYMENT SHARES TO INTERNAL FIRM DATA -- ##
setnames(allb_Uniq,c("firmID"),c("SellerID"))
setkey(allb_Uniq,SellerID)
setkey(shipments,SellerID)
shipments[,MzPz:=Production_zone]
shipments[,MzCz:=Consumption_zone]
shipments <- merge(shipments,allb_Uniq,by="SellerID",all.x=T)
shipments[,Production_zone:=ifelse(is.na(Zone),Production_zone,Zone)]	### convert Production_zone to Zone09
shipments[,c("Zone"):=NULL]
setkey(shipments,BuyerID)
setnames(allb_Uniq,c("SellerID"),c("BuyerID"))
setkey(allb_Uniq,BuyerID)
shipments <- merge(shipments,allb_Uniq,by="BuyerID",all.x=T)
shipments[,Consumption_zone:=ifelse(is.na(Zone),Consumption_zone,Zone)]	### convert Consumption_zone to Zone09
shipments[,c("Zone"):=NULL]

### need to address POEs yet
### Heither, 05-09-2016: for now, just randomly assign a POE to these (improved procedures are comming soon)
### -- Direct distchannel, External Buyer --
shipments[triptype==2, Consumption_zone:=as.integer(runif(.N, min=1945, max=1962))]		### range will allow values of 1945, 1961
### -- Direct distchannel, External Seller --
shipments[triptype==3, Production_zone:=as.integer(runif(.N, min=1945, max=1962))]		### range will allow values of 1945, 1961


#-----------------------------------------------------------------------------------
#Add shipment trip origin and destination zones and business IDs
#-----------------------------------------------------------------------------------
progressNextStep("Add shipment trip origins and destinations")

shipments[,orig_zone:=ifelse(triptype %in% c(5,7,9),Warehouse_zone,Production_zone)]
shipments[,dest_zone:=ifelse(triptype %in% c(4,6,8),Warehouse_zone,Consumption_zone)]
shipments[,origID:=ifelse(triptype %in% c(5,7,9),WarehouseID,SellerID)]
shipments[,destID:=ifelse(triptype %in% c(4,6,8),WarehouseID,BuyerID)]
cat(" -> Production_zone & Consumption_zone converted from Mesozones to Zones", fill=T)

whouse <- progressEnd(whouse)