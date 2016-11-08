##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       scenario_variables.R creates variables for use in the model, grouped by
#                   model step. The variables are specific to a scenario
#Date:              January 28, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
##############################################################################################

#---------------------------------------------------------------------
#Declare variables used in the model
#---------------------------------------------------------------------
print("Declaring variables used in the model")

#---------------------------------------------------------------------
#Step 0 Overall Model Flow and Control Variables
#---------------------------------------------------------------------

#These flags determine whether various outputs are saved
#Set to TRUE to save output
outputtable       <- FALSE #large output tabulations
outputsummary     <- TRUE #summary output files
outputRworkspace  <- FALSE #if TRUE save workspace with all files at the end of each model step
outputlog         <- FALSE #sink output and messages to log.txt
outputprofile     <- FALSE #profiling (set to FALSE for production)

#---------------------------------------------------------------------
#Step 1 Firm Synthesis
#---------------------------------------------------------------------

provalthreshold       <- 0.8     # threshold for percentage of purchase value for each commodity group met by producers
combinationthreshold  <- 7000000 # max number of combinations of producers and consumers to enter into a procurement market game
consprodratiolimit    <- 1000000     # limit on ratio of consumers to producers to enter into the procurement market game
foreignprodcostfactor <- 0.9     # producer cost factor for foreign produers (applied to unit costs) 
wholesalecostfactor    <- 1.2     # markup factor for wholesalers (applied to unit costs)

#---------------------------------------------------------------------
#Step 2 Mode-Path Paramaters
#---------------------------------------------------------------------

#Path parameters: assume the following parameters for all alternatives
B1                 <- 100            #Constant unit per order 
B4                 <- 2000           #Storage costs per unit per year
j                  <- 0.01           #Fraction of shipment that is lost/damaged
LT_OrderTime       <- 10             #Expected lead time for order fulfillment (to be added to in-transit time)
sdLT               <- 1              #Standard deviation in lead time

#Path parameters: specific costs for mode specific rates and handlings fees
BulkHandFee        <- 1              #Handling charge for bulk goods ($ per ton)
WDCHandFee         <- 15             #Warehouse/DC handling charge ($ per ton)
IMXHandFee         <- 15             #Intermodal lift charge ($ per ton)
TloadHandFee       <- 10             #Transload charge ($ per ton; at international ports only)
AirHandFee         <- 20             #Air cargo handling charge ($ per ton)
WaterRate          <- 0.005          #Line-haul charge, water ($ per ton-mile)
CarloadRate        <- 0.03           #Line-haul charge, carload ($ per ton-mile)
IMXRate            <- 0.04           #Line-haul charge, intermodal ($ per ton-mile)
AirRate            <- 3.75           #Line-haul charge, air ($ per ton-mile)
LTL53rate          <- 0.08           #Line-haul charge, 53 feet LTL ($ per ton-mile)
FTL53rate          <- 0.08           #Line-haul charge, 53 feet FTL ($ per ton-mile)
LTL40rate          <- 0.1            #Line-haul charge, 40 feet LTL ($ per ton-mile)
FTL40rate          <- 0.1            #Line-haul charge, 40 feet FTL ($ per ton-mile)
ExpressSurcharge   <- 1.5            #Surcharge for direct/express transport (factor)
BulkTime           <- 72             #Handling time at bulk handling facilities (hours)
WDCTime            <- 12             #Handling time at warehouse/DCs (h⌠ours)
IMXTime            <- 24             #Handling time at intermodal yards (hours)
TloadTime          <- 12             #Handling time at transload facilities (hours)
AirTime            <- 12              #Handling time at air terminals (hours)

#Allow discount rate to vary by type of good.
#Assume discount rate (B2, B3, B5)) is same for goods in transit (B3) + goods in storage (B5)
LowDiscRate       <- 0.01
MedDiscRate       <- 0.05
HighDiscRate      <- 0.25

#Mode availability parameters based on shipment size vs. vehicle capacity
CAP1FTL           <- 30*2000        #Capacity of 1 Full Truckload = 30 tons
CAP1Carload       <- 100000	        #Capacity of 1 Full Railcar = 85 tons (85*2000); Heither, 10-05-2016: test 100000 lbs so mode is actually used
CAP1Airplane      <- 25*2000        #Capacity of 1 Airplane Cargo Hold = 25 ton


#Allow "a" to vary by type of commodity (Functional vs. Innovative)
#Assume "a" is the same for goods inside three different groups
LowMultiplier      <- 0.5
MediumMultiplier   <- 1.0
HighMultiplier     <- 2.33

#Allow "sdQ" to vary by type of commodity (Functional vs. Innovative)
#Assume "sdQ" is the same for goods inside three different groups
LowVariability      <- 0.03
MediumVariability   <- 0.06
HighVariability     <- 0.09

#---------------------------------------------------------------------
#Step 3 Run PMG
#---------------------------------------------------------------------

# max number of R script instances to run at once for determining Supplier to Buyer costs (1 for monitoring if that is run)
maxcostrscripts <- 8

# max number of R script instances to run at once (1 for monitoring if that is run, the rest for running PMGs)
maxrscriptinstances <- 32

# should monitoring be run?
pmgmonitoring <- TRUE

#monitoring settings
pmgmonfrom <- "cheither@cmap.illinois.gov"
pmgmonto <- c("cheither@cmap.illinois.gov","lcruise@cmap.illinois.gov")
pmgmonsmtp <- "outlook.office365.com"
pmgmoninterval <- 43200

# should pmg logging be on? (see also verbose below for level of detail)
pmglogging <- TRUE

# random starting seed for the PMGs
RandomSeed <- 41

# number of iterations
IMax <- 6

# want lots of detail about tradebots?
Verbose <- 0

# recalculate alternate payoffs every iteration based on updated expected payoffs 
DynamicAlternatePayoffs <- 1

# should initial expected tradeoffs know size of other traders?
ClairvoyantInitialExpectedPayoffs <- 0

# should sellers accept offers based on order size instead of expected payoff?
SellersRankOffersByOrderSize <- 0

# multiplier to goose initial expected tradeoff to encourage experimentation with other traders 
InitExpPayoff <- 1.1
Temptation    <- 0.6
BothCoop      <- 1.0
BothDefect    <- 0.6
Sucker        <- 1.0

# amount to downgrade expected payoff of seller who outright refuses a trade offer by buyer
RefusalPayoff <- 0.5

# negative payoff to sellers for not participating
WallflowerPayoff <- 0.0

#buyers don't try to trade with sold out sellers
BuyersIgnoreSoldOutSellers <- 1

#ratio at which buyers don't try to trade with sold out sellers
IgnoreSoldOutSellersMinBuyerSellerRatio = 100

#faster reading of input files but does less checks (ok for use with R)
RawFastParser = 1

#---------------------------------------------------------------------
#Step 7 Vehicle Tour Choice Pattern
#---------------------------------------------------------------------
#Based on Illinois weight plates (March 2013) the GVW for truck types are:
#light duty (plates D-J): 28000 lbs , medium duty (plates K-T): 64000 lbs. , heavy duty (plates V-Z): 80000 lbs.
#For simplicity and to allow some wiggle room, assume maximum load weights 0f 35000, 65000, 100000 lbs. (ignore weight of truck)
wgtmax_2axl <- 35000
wgtmax_3axl <- 65000
wgtmax_semi <- 100000




#---------------------------------------------------------------------
#Step 11 Prepare trip table
#---------------------------------------------------------------------
annualfactor        <- 310  #sampling factor to convert annual truck flows to daily
