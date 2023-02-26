# CMAP Freight Model
# dev script: data_taz_system.R
#
# Purpose:
# Create TAZ correspodence file
#
# Outputs:
# Model inputs in lib\data:
# TAZ_System.csv
# TAZ_System_Shape.shp
# TAZ_System_Shape_Small.shp
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

# FAF3 to county correspondence and other existing correspondence files
c_county_faf3 <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_countyfips_faf.csv"))
c_faf_cbp <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_fafzone_cbpzone.csv"))
c_mz_faf <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_meso_faf3_region.csv"))
c_mz_cbp <- fread(file.path(SYSTEM_DEV_DATA_PATH, "ZoneCorresp", "corresp_mesozone_cbpzone.csv"))

# CBP data for 2017
cbp <- fread(file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"))
unique(cbp$FAFZONE)
### CREATE COMPLETE CORRESPONDENCE ==============

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

# Create the national and global zones outside the CMAP zones
# smallest unit is county for national zones
# add aggregate zones (TAZ, Mesozone, CBPZONE for matching with establishment data, FAF zone)

county_dt <- as.data.table(county)
county_dt <- unique(county_dt[,.(STATEFP, 
                          CountyFIPS = as.integer(STATEFP) * 1000 + as.integer(COUNTYFP),
                          county = toupper(NAME))])



# add labels
district_labels = c("Chicago","COOK, IL (Outside Chicago)","DUPAGE, IL","KANE, IL",
                    "KENDALL, IL","LAKE, IL","MCHENRY, IL","WILL, IL",
                    "GRUNDY, IL (CMAP Part)","DEKALB, IL (CMAP Part)",
                    "Non-CMAP Part of Model Region")
TAZ_System[, DistrictName := district_labels[DistrictNum]]
TAZ_System[, DistrictName := factor(DistrictName, levels = district_labels)]

### CREATE SHAPE FILE VERSION ===================================

TAZ_System_Shape <- st_transform(zone17, crs = st_crs(county))
TAZ_System_Shape$TAZ <- TAZ_System$TAZ

### SAVE FINAL CORRESPONDENCE ===================================

# write the file to lib/data in the application
fwrite(TAZ_System, file.path("lib", 
                                "data", 
                                "TAZ_System.csv"))

# write the shapefile version 
st_write(TAZ_System_Shape, 
         dsn = file.path("lib", 
                         "data", 
                         "TAZ_System_Shape.shp"), 
         delete_layer = TRUE)

# Create a simplified shape file for quicker mapping
TAZ_System_Shape <- st_read(file.path("lib", "data", "TAZ_System_Shape.shp"))
plot(TAZ_System_Shape[,c("county_nam")])
plot(st_simplify(TAZ_System_Shape[,c("county_nam")]
                 , dTolerance = 100))

TAZ_System_Shape_Small <- st_simplify(TAZ_System_Shape,
                                      preserveTopology = TRUE,
                                      dTolerance = 30)
st_write(TAZ_System_Shape_Small, 
         dsn = file.path(SYSTEM_DEV_DATA_PATH,
                         "TAZ", 
                         "TAZ_System_Shape_Small.shp"), 
         delete_layer = TRUE)

# overwrite the model version with the small one
st_write(TAZ_System_Shape_Small, 
         dsn = file.path("lib", 
                         "data", 
                         "TAZ_System_Shape.shp"), 
         delete_layer = TRUE)

plot(TAZ_System_Shape_Small[,c("DistrictNa")])