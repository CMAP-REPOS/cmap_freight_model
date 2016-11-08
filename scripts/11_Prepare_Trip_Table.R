##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       11_Prepare_Trip_Table.R converts the trip list to a trip table
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 11 Prepare Trip Table
#-----------------------------------------------------------------------------------
progressStart(preptt,3)

#-----------------------------------------------------------------------------------
#Aggregate Trip List to Trip Table
#-----------------------------------------------------------------------------------
progressNextStep("Aggregate Trip List to Trip Table")

#Assigning a text Time of Day category 
shipments[,TimeOfDay:=c("EM","AM","MD","PM","NT")[1+findInterval(StartTime,c(6,10,15,19))]]
#Assigning a text Vehicle type
shipments[,Vehicle:=c("2-axle","3,4 axle","semi/trailer")[Veh_Type]]
#Aggregate by zone
trip_table <- shipments[,list(Trips=.N),by=list(ORI_mz=o, DES_mz=d,Vehicle,TimeOfDay)]

#save the trip table and the shipments table
save(shipments,file=file.path(model$outputdir,"shipments.Rdata"))
write.csv(trip_table,file=file.path(model$outputdir,"trip_table.csv"),row.names=FALSE)

preptt <- progressEnd(preptt)
