# Define TAZ and Mesozone ranges for different elements of the model region
BASE_TAZ_INTERNAL <- 1L:3632L #range of TAZs that covers the CMAP model region 
BASE_MZ_DOMESTIC <- 1L:273L #range of Mesozones that covers the USA
BASE_MZ_INTERNATIONAL <- 274L:1000L #range of Mesozones covering foreign countries

# Define application time periods, run years, and other temporal inputs
BASE_SCENARIO_BASE_NAME <- "base" #base year scenario name
BASE_SCENARIO_BASE_YEAR <- 2015 #base year scenario year
BASE_SCENARIO_YEARS <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045) 

# Define other application parameters
BASE_NEW_FIRMS_PROP <- 0.3 #proportion of growth in employment in already developed TAZs that comes from new firm formation as opposed to existing firm growth
BASE_PROVALTHRESHOLD <- 0.8 #production value threshold for supplier selection
BASE_FOREIGN_PROD_COST_FACTOR <- 0.9 # producer cost factor for foreign produers (applied to unit costs)
BASE_FOREIGN_FIRM_SIZE_LIMIT <- 500000000 # production capacity limit or consumption requirement limit in pounds for foreign firms
BASE_WHOLESALE_COST_FACTOR <- 1.2 # markup factor for wholesalers (applied to unit costs)
BASE_SUPPLIERS_PER_BUYER <- 20L #number of suppliers to sample in PMG
BASE_COMBINATION_THRESHOLD <- 3500000 # max number of combinations of producers and consumers to enter into a procurement market game
BASE_CONS_PROD_RATIO_LIMIT <- 1000000 # limit on ratio of consumers to producers to enter into the procurement market game
BASE_OUTPUT_PURCHASE_RATIO <- 1.1 # minimum ratio between output capacity and purchase amounts to ensure markets clear
BASE_SEED_VALUE  <- 5 #seed for sampling to ensure repeatable results
BASE_TIME_PERIOD_TRIP_POINT <- "START" #point in trip for time period allocation, from ("START", "MIDDLE", "END")
BASE_ANNUALFACTOR <- 310  #sampling factor to convert annual truck flows to daily

#Based on Illinois weight plates (March 2013) the GVW for truck types are:
#light duty (plates D-J): 28000 lbs , medium duty (plates K-T): 64000 lbs. , heavy duty (plates V-Z): 80000 lbs.
#For simplicity and to allow some wiggle room, assume maximum load weights 0f 35000, 65000, 100000 lbs. (ignore weight of truck)
BASE_WGTMAX_2XL <- 35000
BASE_WGTMAX_3XL <- 65000
BASE_WGTMAX_SEMI <- 100000