# CMAP Freight Model
# dev script: data_taz_system.R
#
# Purpose:
# Create TAZ correspodence file
#
# Outputs:
# Model inputs in lib\data:
# TAZ_System.csv
#
# use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### READ INPUT FILES ==================================================

emp_control <- fread("./scenarios/base/inputs/data_emp_control_taz.csv")
zone17 <- read_sf(file.path(SYSTEM_DEV_DATA_PATH,"TAZ", "zones17.shp"))

# US Counties
county <- st_read(file.path(SYSTEM_DEV_DATA_PATH, 
                            "USCountyState", 
                            "tl_2015_us_county.shp"))
st_crs(county)

# QCEW county list
qcew_county <- fread(file.path(SYSTEM_DEV_DATA_PATH, "QCEW", "qcew_county.csv"))

# FAF3 to county correspondence and other existing correspondence files
c_county_faf3 <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_countyfips_faf.csv"))
c_faf_cbp <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_fafzone_cbpzone.csv"))
c_mz_faf <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_meso_faf3_region.csv"))
c_mz_cbp <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_mesozone_cbpzone.csv"))

# foreign prod/cons data
for_prod <- fread("./scenarios/base/inputs/data_foreign_prod.csv")
for_cons <- fread("./scenarios/base/inputs/data_foreign_cons.csv")

### CREATE COMPLETE CORRESPONDENCE ==============

# CMAP Model Region

c_taz_mz <- unique(emp_control[,.(TAZ = Zone17, Mesozone)])

TAZ_System <- as.data.table(zone17)
TAZ_System <- TAZ_System[,.(TAZ = zone17, cbd, chicago, cmap, CountyFIPS = county_fip, county = county_nam, state, township = township_r, sqmi)]

TAZ_System[c_taz_mz, Mesozone := i.Mesozone, on = "TAZ"]
TAZ_System[, county_state := paste(county, state, sep = ", ")]

# Create a grouping for use in the dashboard and elsewhere in summary tabulations 
# (add a numerical integer version for ordering and a text field)
# 1 "Chicago" City of Chicago (chicago  == 1)
# 2 "COOk, IL (Outside Chicago)" (county_state == "COOK, IL" & chicago  != 1)
# 3 "DUPAGE, IL" (county_state == "DUPAGE, IL" & cmap == 1)
# 4 "KANE, IL" (county_state == "KANE, IL" & cmap == 1)
# 5 "KENDALL, IL (county_state == "KENDALL, IL" & cmap == 1)
# 6 "LAKE, IL" (county_state == "LAKE, IL" & cmap == 1)
# 7 "MCHENRY, IL" (county_state == "MCHENRY, IL" & cmap == 1)
# 8 "WILL, IL" (county_state == "WILL, IL" & cmap == 1)
# 9 "GRUNDY, IL (CMAP Part)" (county_state == "GRUNDY, IL" & cmap == 1)
# 10 "DEKALB, IL (CMAP Part)" (county_state == "DEKALB, IL" & cmap == 1)
# 11 "Non-CMAP Part of Model Region" (cmap == 0)

TAZ_System[, DistrictNum := ifelse(chicago  == 1,1,0)]
TAZ_System[, DistrictNum := ifelse(county_state == "COOK, IL" & chicago  != 1,2,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "DUPAGE, IL" & cmap == 1,3,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "KANE, IL" & cmap == 1,4,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "KENDALL, IL" & cmap == 1,5,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "LAKE, IL" & cmap == 1,6,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "MCHENRY, IL" & cmap == 1,7,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "WILL, IL" & cmap == 1,8,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "GRUNDY, IL" & cmap == 1,9,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(county_state == "DEKALB, IL" & cmap == 1,10,DistrictNum)]
TAZ_System[, DistrictNum := ifelse(cmap == 0,11,DistrictNum)]
TAZ_System[DistrictNum == 0]

# Rest of US and Global

# Create the national and global zones outside the CMAP zones
# smallest unit is county for national zones
# add aggregate zones (TAZ, Mesozone, CBPZONE for matching with establishment data, FAF zone)

county_dt <- as.data.table(county)
county_dt <- unique(county_dt[,.(StateFIPS = as.integer(STATEFP), 
                          CountyFIPS = as.integer(STATEFP) * 1000 + as.integer(COUNTYFP),
                          county = toupper(NAME))])

# Just keep the FIPS codes that are in the QCEW control data (but not US territories)
county_dt <- county_dt[CountyFIPS %in% qcew_county[CountyFIPS < 56999]$CountyFIPS][order(CountyFIPS)]

# Label the states
county_dt[FIPS.current, state := StateAbbr, on = "StateFIPS"]
county_dt[, StateFIPS := NULL]

# Create additional fields to match the TAZ_System files
county_dt[, county_state := paste(county, state, sep = ", ")]
county_dt[, c("cbd", "chicago", "cmap") := 0L]
county_dt[, DistrictNum := 12]

# Combine (remove the CMAP region records from county_dt)
TAZ_System <- rbind(TAZ_System,
                    county_dt[!CountyFIPS %in% TAZ_System$CountyFIPS],
                    fill = TRUE)
TAZ_System[, CountyFIPS := as.integer(CountyFIPS)]

# Update the spatial fields
TAZ_System[c_county_faf3[,.(FAFZONE, FAFNAME, CountyFIPS = as.integer(FIPS))], 
           c("FAFZONE","FAFNAME") := .(i.FAFZONE, i.FAFNAME),
           on = "CountyFIPS"]

# CBPZONE is an ID by FAFZONE and then the CountyFIPS in the CMAP region
TAZ_System[c_faf_cbp[CBPZONE <= 123],
           CBPZONE := i.CBPZONE, on = "FAFZONE"]
TAZ_System[!is.na(TAZ), CBPZONE := CountyFIPS]

# Mesozone for rest of USA is CBPZONE + 150
TAZ_System[is.na(Mesozone), Mesozone := CBPZONE + 150L]

# TAZ for the rest of USA is max taz + CBPZONE
max_taz <- max(TAZ_System[!is.na(TAZ)]$TAZ)
TAZ_System[is.na(TAZ), TAZ := max_taz + CBPZONE]

# Add rows for the international FAF zones/countries
country_zones <- unique(rbind(for_prod[,.(Country,FAFZONE, ctrycod, CBPZONE)],
                              for_cons[,.(Country,FAFZONE, ctrycod, CBPZONE)]))
# remove unidentified countries zone
country_zones <- country_zones[!is.na(FAFZONE)][order(CBPZONE)]

# Create additional fields to match the TAZ_System files
country_zones[, c("cbd", "chicago", "cmap") := 0L]
country_zones[, DistrictNum := 13]

# Combine 
TAZ_System[, Country := "United States of America"]
TAZ_System[, ctrycod := 0L]
TAZ_System <- rbind(TAZ_System,
                    country_zones,
                    fill = TRUE)

# Mesozone for rest of world is CBPZONE + 150
TAZ_System[is.na(Mesozone), Mesozone := CBPZONE + 150L]

# TAZ for the rest of world is max internal taz + CBPZONE
TAZ_System[is.na(TAZ), TAZ := max_taz + CBPZONE]

# Add labels
district_labels = c("Chicago","COOK, IL (Outside Chicago)","DUPAGE, IL","KANE, IL",
                    "KENDALL, IL","LAKE, IL","MCHENRY, IL","WILL, IL",
                    "GRUNDY, IL (CMAP Part)","DEKALB, IL (CMAP Part)",
                    "Non-CMAP Part of Model Region", "Rest of USA", "International")
TAZ_System[, DistrictName := district_labels[DistrictNum]]
TAZ_System[, DistrictName := factor(DistrictName, levels = district_labels)]

TAZ_System[c_mz_faf[,.(Mesozone = MESOZONE, REGION, SUBREGION)],
           c("REGION", "SUBREGION") := .(i.REGION, i.SUBREGION),
           on = "Mesozone"]

faf_global_labels = data.table(FAFZONE = 801:808,
                               FAFNAME = c("Canada", "Mexico", "Americas",
                                           "Europe", "Africa", "SWC Asia", 
                                          "E Asia", "SE As Oc"))

TAZ_System[faf_global_labels, FAFNAME := i.FAFNAME, on = "FAFZONE"]

# Add a couple more grouping variables
TAZ_System[, modelregion := ifelse(Mesozone < 150, 1L, 2L)] #for sorting with model region first
TAZ_System[, TAZ_TYPE := ifelse(Mesozone < 150, "MODELREGION", 
                                ifelse(DistrictNum < 13, "NATIONAL", "INTERNATIONAL"))]

### SAVE FINAL CORRESPONDENCE ===================================

# write the file to lib/data in the application
fwrite(TAZ_System, file.path("lib", 
                                "data", 
                                "TAZ_System.csv"))
