# CMAP Freight Model
# dev script: data_qcewR
#
# Purpose:
# Generate a set of county level targets by NAICS industry from QCEW data
#
# Outputs:
# Model inputs in scenarios\base\inputs:
# data_emp_control_county.csv
#
# use init_dev.R to run here instead of sourcing from _Master_Dev.R
source("./dev/init_dev.R")

### READ INPUT FILES ==================================================

qcew.files <- list.files(file.path(SYSTEM_DEV_DATA_PATH, "QCEW", "2019.annual.by_area"),full.names = TRUE)

readQCEW <- function(qcew.file){
  qcew <- fread(qcew.file)
  levels_to_keep <- ifelse(grepl("Statewide",qcew.file),54,74)
  qcew <- qcew[agglvl_code == levels_to_keep,.(estabs = sum(annual_avg_estabs_count),
                                   emp = sum(annual_avg_emplvl)),
               keyby = .(year, agglvl_code, area_fips, area_title, industry_code, industry_title)]
}

qcew <- lapply(qcew.files,readQCEW)

### PROCESS ==================================================

qcew <- rbindlist(qcew)
qcew[,.(estabs = sum(estabs), emp = sum(emp)),by = agglvl_code]

# correct the county employment by industry to match the total statewide employment by industry
qcew.state <- qcew[agglvl_code == 54]
qcew <- qcew[agglvl_code == 74][,agglvl_code := NULL]
qcew[, state_fips := as.character(floor(as.numeric(area_fips)/1000)*1000)]

# remove unclassified employment and unknown locations
qcew <- qcew[industry_code != 99]
qcew <- qcew[!grep("999", area_fips)]

# scale the state employment to account for unclassified employment
unclass_scale <- sum(qcew.state$emp)/sum(qcew.state[industry_code != "99"]$emp)
qcew.state <- qcew.state[industry_code != "99"]
qcew.state[, emp := emp * unclass_scale]
qcew.state[, emp := bucketRound(emp)]

setnames(qcew.state, "area_fips", "state_fips")
qcew.state[qcew[,.(county_emp = sum(emp)), keyby = .(state_fips, industry_code)],
           county_emp := i.county_emp,
           on = c("state_fips", "industry_code")]

qcew.state[, emp_scale := ifelse(county_emp > emp,1,emp/county_emp)]
qcew.state[is.na(emp_scale), emp_scale := 1]

# scale the county employment to match statewide
qcew[qcew.state, emp_scale := i.emp_scale, on = c("state_fips", "industry_code")]
qcew[is.na(emp_scale) | is.infinite(emp_scale), emp_scale := 1]
qcew[, emp := emp * emp_scale]
qcew[, emp := bucketRound(emp)]

# clean up the table and format it for use in the model
qcew[, c("state_fips", "emp_scale") := NULL ]
qcew[, NAICS := as.integer(substr(industry_code,1,2))]
data_emp_control_county <- qcew[,.(CountyFIPS = area_fips, NAICS,	Employment = emp)]

# create a list of uniques counties from the data for use in the development of a complete  TAZ_System for the model
qcew_county <- unique(qcew[,.(year, CountyFIPS = area_fips, CountyName = area_title)])

### WRITE ==================================================

fwrite(data_emp_control_county, file.path(SYSTEM_DEV_DATA_PATH, "QCEW", "data_emp_control_county.csv"))
fwrite(qcew_county, file.path(SYSTEM_DEV_DATA_PATH, "QCEW", "qcew_county.csv"))
