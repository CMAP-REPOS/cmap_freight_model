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

load(file.path(model$outputdir,"warehouses.Rdata")) #saved in firmsyn step
warehouses[,warehouse:=.I]
warehouses <- warehouses[,list(MESOZONE,warehouse,BusID)]
setnames(warehouses,c("MESOZONE","BusID"),c("Warehouse_zone","WarehouseID"))
#pick a warehouse for the indirect trips, but just the first portion of internal to internal
shipments[,warehouse:=ifelse(triptype %in% c(4,6,8,9),sample.int(max(warehouses$warehouse),.N,replace=TRUE),0L)]
shipments[triptype==5,warehouse:=shipments[triptype==4]$warehouse[match(ShipmentID,shipments[triptype==4]$ShipmentID)]]
shipments[triptype==7,warehouse:=sapply(1:.N,function(x) sample(warehouses$warehouse[-shipments[triptype==6][x]$warehouse],1))]
shipments <- merge(shipments, warehouses, by = "warehouse", all.x=T)

#-----------------------------------------------------------------------------------
#Add shipment trip origin and destination zones and business IDs
#-----------------------------------------------------------------------------------
progressNextStep("Add shipment trip origins and destinations")

shipments[,orig_zone:=ifelse(triptype %in% c(5,7,9),Warehouse_zone,Production_zone)]
shipments[,dest_zone:=ifelse(triptype %in% c(4,6,8),Warehouse_zone,Consumption_zone)]
shipments[,origID:=ifelse(triptype %in% c(5,7,9),WarehouseID,SellerID)]
shipments[,destID:=ifelse(triptype %in% c(4,6,8),WarehouseID,BuyerID)]

whouse <- progressEnd(whouse)