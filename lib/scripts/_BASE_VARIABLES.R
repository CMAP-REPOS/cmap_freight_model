# Define TAZ and Mesozone ranges for different elements of the model region

# CMAP region
TAZ_System <- read.csv(file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))
BASE_TAZ_INTERNAL <- TAZ_System$TAZ #range of TAZs that covers the complete CMAP model region 
BASE_MZ_INTERNAL <- sort(unique(TAZ_System$Mesozone)) #range of mesozones that covers the complete CMAP model region 
BASE_FIPS_INTERNAL <- sort(unique(TAZ_System$CountyFIPS)) #range of county fips code that covers the complete CMAP model region 
BASE_TAZ_CHICAGO <- TAZ_System$TAZ[TAZ_System$chicago == 1] #range of TAZ is City of Chicago
BASE_TAZ_COOK <- TAZ_System$TAZ[TAZ_System$CountyFIPS == 17031] #range of TAZ is Cook County
BASE_TAZ_CMAP <- TAZ_System$TAZ[TAZ_System$cmap == 1] #range of TAZ is CMAP MPO region
rm(TAZ_System)

# National and international zone ranges
BASE_MZ_DOMESTIC <- 1L:273L #range of Mesozones that covers the USA
BASE_MZ_INTERNATIONAL <- 274L:1000L #range of Mesozones covering foreign countries

# Define application time periods, run years, and other temporal inputs
BASE_SCENARIO_BASE_NAME <- "base" #base year scenario name
BASE_SCENARIO_BASE_YEAR <- 2015 #base year scenario year

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

# 3. Define time-of-day groupings for skims and trip tables
# # Units are minutes after midnight 
# p1 - 8 pm to 6 am
# p2 - 6 am to 7 am
# p3 - 7 am to 9 am
# p4 - 9 am to 10 am
# p5 - 10 am to 2 pm
# p6 - 2 pm to 4 pm
# p7 - 4 pm to 6 pm
# p8 - 6 pm to 8 pm
BASE_TOD_RANGES <- list(P1 = list(c(0,360), c(1200,1440)),
                        P2 = list(c(360,420)),
                        P3 = list(c(420,540)),
                        P4 = list(c(540, 600)),
                        P5 = list(c(600, 840)),
                        P6 = list(c(840, 960)),
                        P7 = list(c(960, 1080)),
                        P8 = list(c(1080, 1200))
)

# 4. Define settings used in Dashboard/spreadsheet report
# Column name from TAZ_System.csv that labels each TAZ with the desired group
# names for use in the dashboard. This also determines how TAZs will be grouped
# into larger regions for display in the dashboard.
BASE_DASHBOARD_GEOGRAPHY <- "DistrictName"

# Unit used for display in dashboard
BASE_DASHBOARD_LENGTH_UNIT <- "miles"
