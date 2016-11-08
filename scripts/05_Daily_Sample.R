##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       5_Daily_Sample.R take a one day sample from the set of annual shipments
#Date:              June 30, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#-----------------------------------------------------------------------------------
#Step 5 Daily Sample
#-----------------------------------------------------------------------------------
progressStart(daysamp,3)

#-----------------------------------------------------------------------------------
#Take Daily Sample
#-----------------------------------------------------------------------------------
progressNextStep("Take Daily Sample")

# Take sample for just Chicago originating or destined shipments
shipments <- pairs[Production_zone<150|Consumption_zone<150]

# Annual frequency = annual purchase quantity / shipment weight, convert to days
shipments[,daily_freq:= Last.Iteration.Quantity/(weight * model$scenvars$annualfactor)]

#Simulate the chance of making a delivery on a given day. 
#If daily_freq is larger than 1, then delivery\pickup must occur.
shipments[,temprand:=runif(.N)]
shipments[,make_trip:= ifelse(temprand < daily_freq, 1, 0)]
      
#Simulate only those shipments that have a delivery\pickup scheduled on the simulation day
shipments <- shipments[make_trip == 1,]

#Calculate the number of daily loads by rounding the daily frequency 
shipments[,daily_load:= ifelse(daily_freq<1|(daily_freq>=1 & daily_freq!=trunc(daily_freq) & (daily_freq-trunc(daily_freq))>temprand),
                               ceiling(daily_freq), trunc(daily_freq))]

###TODO does the number of daily loads need to be enumerated to give one row per shipment?

#Add shipment ID
shipments[,ShipmentID:=.I]

#clean up
shipments[,c("temprand","make_trip"):=NULL]
rm(pairs)

daysamp <- progressEnd(daysamp)
