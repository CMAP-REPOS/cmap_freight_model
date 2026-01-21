# CMAP Freight Model
# dev script: cmap_scenarios_build_from_forecasts.R
#
# Purpose:
# Document and run in sequence scripts to create
# baseline scenario structure and inputs
#
# Outputs:
# dev/FutureScenarios/scenarios/base2022/inputs (all files)
# dev/FutureScenarios/scenarios/future2025/inputs (all files)
# dev/FutureScenarios/scenarios/future2030/inputs (all files)
# dev/FutureScenarios/scenarios/future2035/inputs (all files)
# dev/FutureScenarios/scenarios/future2040/inputs (all files)
# dev/FutureScenarios/scenarios/future2045/inputs (all files)
# dev/FutureScenarios/scenarios/future2050/inputs (all files)
# dev/FutureScenarios/scenarios/future2055/inputs (all files)
# dev/FutureScenarios/scenarios/future2060/inputs (all files)
#
# use init_dev.R to start application
source("./dev/init_dev.R")

# source functions used in the processing
source("./dev/data_unitcost.R")
source("./dev/data_trade.R")

### PROCESS ====================================================================

### Create Folder structure ----------------------------------------------------

# Create a series of folders to go under the scenarios directory
# Create these in the "dev/FutureScenarios" folder for transfer to application

dev_forecasts_dir <- file.path(SYSTEM_DEV_PATH, "FutureScenarios")

# New model base year is 2022
# Future years are 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060

future_years <- seq(2025, 2060, by = 5)
scenario_names <- c("base_2022", paste("future", future_years, sep = "_"))

# Individual scenario folders should have a folder name [scenario]
# "inputs" folder inside that to contains the inputs
# "outputs" folder is created during a model run so do not create

scenario_paths <- file.path(dev_forecasts_dir, "scenarios", scenario_names)
scenario_input_paths <- file.path(scenario_paths, "inputs")
lapply(scenario_paths, dir.create)
lapply(scenario_input_paths, dir.create)

# Copy the base year inputs into the inputs folders
scenario_input_paths_base_old <- file.path(dev_forecasts_dir, "scenarios", "base", "inputs")
for(thescenario in 1:length(scenario_input_paths)){
  file.copy(from = list.files(scenario_input_paths_base_old, full.names = TRUE), 
            to = scenario_input_paths[thescenario])  
}

### Update Input Files ------------------------------------------------

# Overwrite files that have updated inputs in the new base and future years
# Macroeconomic forecast inputs produced by EBP for the baseline scenario

macro_inputs_path <- file.path(dev_forecasts_dir, "Macroeconomic Inputs")

# Employment
data_emp_control_county_wide <- fread(file.path(macro_inputs_path, "data_emp_control_county_BaseCase_v3.csv"))

# Firm size targets
data_firmsize_wide <- fread(file.path(macro_inputs_path, "data_firmsize_BaseCase.csv"))

# Productivity
data_productivity_wide <- fread(file.path(macro_inputs_path, "data_productivity_factors.csv"))

# Unit Costs
# Process the FAF data to create units costs by year
data_unitcost(macro_inputs_path)
data_unitcost_wide <- fread(file.path(macro_inputs_path, "data_unitcost_timeseries_2022_2060.csv"))

# Trade data: Foreign production (imports) and foreign consumption (exports)
# Process the Trade data to create processed inputs by year
data_trade(macro_inputs_path)
for_prod_wide <- fread(file.path(macro_inputs_path, "data_foreign_prod_BaseCase_cleaned.csv"))
for_cons_wide <- fread(file.path(macro_inputs_path, "data_foreign_cons_BaseCase_cleaned.csv"))

# Clean up the files: replace NA values with 0
data_emp_control_county_wide[is.na(data_emp_control_county_wide)] <- 0
data_firmsize_wide[is.na(data_firmsize_wide)] <- 0
data_productivity_wide[is.na(data_productivity_wide)] <- 0
data_unitcost_wide[is.na(data_unitcost_wide)] <- 0
for_prod_wide[is.na(for_prod_wide)] <- 0
for_cons_wide[is.na(for_cons_wide)] <- 0

# Local and skim inputs produced by CMAP: 
# data_modepath_skims.csv
# data_modepath_miles.csv
# data_mesozone_skims.csv
# cmap_warehouses.csv
# cmap_data_zone_skims.csv
# cmap_data_zone_employment.csv
current_cmap_forecast <- "2025-02-13 CMAP Inputs/FinalFilesOut"
cmap_forecast_dir <- file.path(dev_forecasts_dir, current_cmap_forecast)

# Structure inside current_cmap_forecast

# "year_specific" folder contains:
# cmap_data_zone_employment_2022 etc, 
# cmap_data_zone_skims_2022 etc, 
# data_mesozone_skims_2022 etc

# "universal" folder contains 
# cmap_warehouses (which are year specific scenario inputs)
# cmap_data_zone_skims (which are year specific scenario inputs)

# "scenario_noLogistics140" contains 
# data_modepath_miles_2022, 
# data_modepath_skims_2022 etc
# (cmap_data_truck_IE_poe_2022, and data_modepath_ports_2022 are common data files, not scenario files)

# cmap produced scenarios for 2022, 2030, 2040, 2050, 2060 (no forecasts for the 20x5s)
cmap_forecast_years <- c(2022, 2030, 2040, 2050, 2060)

# Read in inputs to lists 
# year_specific
cmap_data_zone_employment_list <- lapply(1:length(cmap_forecast_years), 
                                         function(x){ fread(file.path(cmap_forecast_dir, 
                                                                       "year_specific", 
                                                                       paste0("cmap_data_zone_employment_",
                                                                              cmap_forecast_years[x],
                                                                              ".csv"))) 
                                           })
names(cmap_data_zone_employment_list) <- cmap_forecast_years

cmap_data_zone_skims_list <- lapply(1:length(cmap_forecast_years), 
                                    function(x){ fread(file.path(cmap_forecast_dir, 
                                                                 "year_specific", 
                                                                 paste0("cmap_data_zone_skims_",
                                                                        cmap_forecast_years[x],
                                                                        ".csv"))) 
                                    })
names(cmap_data_zone_skims_list) <- cmap_forecast_years
  
data_mesozone_skims_list <- lapply(1:length(cmap_forecast_years), 
                                   function(x){ fread(file.path(cmap_forecast_dir, 
                                                                "year_specific", 
                                                                paste0("data_mesozone_skims_",
                                                                       cmap_forecast_years[x],
                                                                       ".csv"))) 
                                   })
names(data_mesozone_skims_list) <- cmap_forecast_years

# universal
cmap_warehouses <- fread(file.path(cmap_forecast_dir, 
                                   "universal", 
                                   "cmap_warehouses.csv"))

# scenario_noLogistics140 
data_modepath_miles_list <- lapply(1:length(cmap_forecast_years), 
                                    function(x){ fread(file.path(cmap_forecast_dir, 
                                                                 "scenario_noLogistics140", 
                                                                 paste0("data_modepath_miles_",
                                                                        cmap_forecast_years[x],
                                                                        ".csv"))) 
                                    })
names(data_modepath_miles_list) <- cmap_forecast_years

data_modepath_skims_list <- lapply(1:length(cmap_forecast_years), 
                                   function(x){ fread(file.path(cmap_forecast_dir, 
                                                                "scenario_noLogistics140", 
                                                                paste0("data_modepath_skims_",
                                                                       cmap_forecast_years[x],
                                                                       ".csv"))) 
                                   })
names(data_modepath_skims_list) <- cmap_forecast_years

# The year specific inputs need to be interpolated to produce the 20x5 inputs
cmap_data_zone_employment_dt <- rbindlist(cmap_data_zone_employment_list, idcol = "year")
cmap_data_zone_employment_dt <- dcast.data.table(cmap_data_zone_employment_dt,
                                                 Zone + mesozone ~ year,
                                                 fun.aggregate = sum, value.var = "totalemp")

cmap_data_zone_employment_dt[, `2025` := `2022` + round((`2030`-`2022`)/(2030-2022)*(2025-2022))]
cmap_data_zone_employment_dt[, `2035` := `2030` + round((`2040`-`2030`)/(2040-2030)*(2035-2030))]
cmap_data_zone_employment_dt[, `2045` := `2040` + round((`2050`-`2040`)/(2050-2040)*(2045-2040))]
cmap_data_zone_employment_dt[, `2055` := `2050` + round((`2060`-`2050`)/(2060-2050)*(2055-2050))]

cmap_data_zone_skims_dt <- rbindlist(cmap_data_zone_skims_list, idcol = "year")
cmap_data_zone_skims_dt_peak <- dcast.data.table(cmap_data_zone_skims_dt,
                                                 Origin + Destination ~ year,
                                                 fun.aggregate = sum, 
                                                 value.var = "Peak")
cmap_data_zone_skims_dt_offpeak <- dcast.data.table(cmap_data_zone_skims_dt,
                                                 Origin + Destination ~ year,
                                                 fun.aggregate = sum, 
                                                 value.var = "OffPeak")
cmap_data_zone_skims_dt_miles <- dcast.data.table(cmap_data_zone_skims_dt,
                                                 Origin + Destination ~ year,
                                                 fun.aggregate = sum, 
                                                 value.var = "Miles")

cmap_data_zone_skims_dt_peak[, `2025` := `2022` + round((`2030`-`2022`)/(2030-2022)*(2025-2022))]
cmap_data_zone_skims_dt_peak[, `2035` := `2030` + round((`2040`-`2030`)/(2040-2030)*(2035-2030))]
cmap_data_zone_skims_dt_peak[, `2045` := `2040` + round((`2050`-`2040`)/(2050-2040)*(2045-2040))]
cmap_data_zone_skims_dt_peak[, `2055` := `2050` + round((`2060`-`2050`)/(2060-2050)*(2055-2050))]

cmap_data_zone_skims_dt_offpeak[, `2025` := `2022` + round((`2030`-`2022`)/(2030-2022)*(2025-2022))]
cmap_data_zone_skims_dt_offpeak[, `2035` := `2030` + round((`2040`-`2030`)/(2040-2030)*(2035-2030))]
cmap_data_zone_skims_dt_offpeak[, `2045` := `2040` + round((`2050`-`2040`)/(2050-2040)*(2045-2040))]
cmap_data_zone_skims_dt_offpeak[, `2055` := `2050` + round((`2060`-`2050`)/(2060-2050)*(2055-2050))]

cmap_data_zone_skims_dt_miles[, `2025` := `2022` + round((`2030`-`2022`)/(2030-2022)*(2025-2022))]
cmap_data_zone_skims_dt_miles[, `2035` := `2030` + round((`2040`-`2030`)/(2040-2030)*(2035-2030))]
cmap_data_zone_skims_dt_miles[, `2045` := `2040` + round((`2050`-`2040`)/(2050-2040)*(2045-2040))]
cmap_data_zone_skims_dt_miles[, `2055` := `2050` + round((`2060`-`2050`)/(2060-2050)*(2055-2050))]

data_mesozone_skims_dt <- rbindlist(data_mesozone_skims_list, idcol = "year")
data_mesozone_skims_dt <- dcast.data.table(data_mesozone_skims_dt,
                                                Origin + Destination ~ year,
                                                 fun.aggregate = sum, value.var = "Time")

data_mesozone_skims_dt[, `2025` := `2022` + round((`2030`-`2022`)/(2030-2022)*(2025-2022))]
data_mesozone_skims_dt[, `2035` := `2030` + round((`2040`-`2030`)/(2040-2030)*(2035-2030))]
data_mesozone_skims_dt[, `2045` := `2040` + round((`2050`-`2040`)/(2050-2040)*(2045-2040))]
data_mesozone_skims_dt[, `2055` := `2050` + round((`2060`-`2050`)/(2060-2050)*(2055-2050))]

# For the national skims, insert the 5 year increments as a copy of the skim from the previous year
# they don't vary over time at least in the baseline
data_modepath_miles_list[["2025"]] <- data_modepath_miles_list[["2022"]]
data_modepath_miles_list[["2035"]] <- data_modepath_miles_list[["2030"]]
data_modepath_miles_list[["2045"]] <- data_modepath_miles_list[["2040"]]
data_modepath_miles_list[["2055"]] <- data_modepath_miles_list[["2050"]]

data_modepath_skims_list[["2025"]] <- data_modepath_skims_list[["2022"]]
data_modepath_skims_list[["2035"]] <- data_modepath_skims_list[["2030"]]
data_modepath_skims_list[["2045"]] <- data_modepath_skims_list[["2040"]]
data_modepath_skims_list[["2055"]] <- data_modepath_skims_list[["2050"]]

### Write Out Files ----------------------------------------------------------------

# Write out the base case files into the correct folders
# Foreign Production
names(for_prod_wide) 
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
 # field name
  fieldname <- paste("USImpVal", scenario_years[thescenario], sep = "_")
  cols <- c(names(for_prod_wide)[1:5], fieldname)
  # create a file
  for_prod <- for_prod_wide[,..cols]
  setnames(for_prod, c("Country", "Commodity_NAICS6",	"FAFZONE", "ctrycod", "CBPZONE", "USImpVal"))
  # write the file
  fwrite(for_prod, file.path(scenario_input_paths[thescenario], "data_foreign_prod.csv"))
}

# Foreign Consumption
names(for_cons_wide) 
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- paste("USExpVal", scenario_years[thescenario], sep = "_")
  cols <- c(names(for_cons_wide)[1:5], fieldname)
  # create a file
  for_cons <- for_cons_wide[,..cols]
  setnames(for_cons, c("Country", "Commodity_NAICS6",	"FAFZONE", "ctrycod", "CBPZONE", "USExpVal"))
  # write the file
  fwrite(for_cons, file.path(scenario_input_paths[thescenario], "data_foreign_cons.csv"))
}

# Employment controls
names(data_emp_control_county_wide) # v3 includes 2055 and 2060 files
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- as.character(scenario_years[thescenario])
  cols <- c(names(data_emp_control_county_wide)[1:2], fieldname)
  # create a file
  data_emp_control_county <- data_emp_control_county_wide[,..cols]
  setnames(data_emp_control_county, c("CountyFIPS",	"NAICS",	"Employment"))
  # write the file
  fwrite(data_emp_control_county, file.path(scenario_input_paths[thescenario], "data_emp_control_county.csv"))
}

# Firm size targets
names(data_firmsize_wide)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- paste0("GrowthRate",as.character(scenario_years[thescenario]))
  cols <- c(names(data_firmsize_wide)[1], fieldname)
  # create a file
  data_firmsize <- data_firmsize_wide[,..cols]
  setnames(data_firmsize, c("NAICS",	"FirmSizeFactor"))
  # write the file
  fwrite(data_firmsize, file.path(scenario_input_paths[thescenario], "data_firmsize_factors.csv"))
}

# Productivity factors
names(data_productivity_wide)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- paste0("prod_factor_",as.character(scenario_years[thescenario]))
  cols <- c(names(data_productivity_wide)[2], fieldname)
  # create a file
  data_productivity <- data_productivity_wide[,..cols]
  setnames(data_productivity, c("NAICS3",	"ProdFactor"))
  # write the file
  fwrite(data_productivity, file.path(scenario_input_paths[thescenario], "data_productivity_factors.csv"))
}

# Unit Costs
names(data_unitcost_wide)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- paste0("UnitCost",as.character(scenario_years[thescenario]))
  cols <- c(names(data_unitcost_wide)[1], fieldname)
  # create a file
  data_unitcost <- data_unitcost_wide[,..cols]
  setnames(data_unitcost, c("Commodity_SCTG",	"UnitCost"))
  # write the file
  fwrite(data_unitcost, file.path(scenario_input_paths[thescenario], "data_unitcost.csv"))
}


# cmap_data_zone_employment.csv
names(cmap_data_zone_employment_dt)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- as.character(scenario_years[thescenario])
  cols <- c(names(cmap_data_zone_employment_dt)[1:2], fieldname)
  # create a file
  cmap_data_zone_employment <- cmap_data_zone_employment_dt[,..cols]
  setnames(cmap_data_zone_employment, c("Zone", "mesozone", "totalemp"))
  # write the file
  fwrite(cmap_data_zone_employment, file.path(scenario_input_paths[thescenario], "cmap_data_zone_employment.csv"))
}

# cmap_data_zone_skims.csv
names(cmap_data_zone_skims_dt_peak)
scenario_years <- c(2022, future_years)
setnames(cmap_data_zone_skims_dt_peak, 
         c(as.character(scenario_years)),
         paste0("Peak", c(as.character(scenario_years))))
setnames(cmap_data_zone_skims_dt_offpeak, 
         c(as.character(scenario_years)),
         paste0("OffPeak", c(as.character(scenario_years))))
setnames(cmap_data_zone_skims_dt_miles, 
         c(as.character(scenario_years)),
         paste0("Miles", c(as.character(scenario_years))))
cmap_data_zone_skims_dt <- merge(cmap_data_zone_skims_dt_peak,
                                 cmap_data_zone_skims_dt_offpeak,
                                 by = c("Origin", "Destination"),
                                 all = TRUE)
cmap_data_zone_skims_dt <- merge(cmap_data_zone_skims_dt,
                                 cmap_data_zone_skims_dt_miles,
                                 by = c("Origin", "Destination"),
                                 all = TRUE)

for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname1 <- paste0("Peak",as.character(scenario_years[thescenario]))
  fieldname2 <- paste0("OffPeak",as.character(scenario_years[thescenario]))
  fieldname3 <- paste0("Miles",as.character(scenario_years[thescenario]))
  cols <- c(names(cmap_data_zone_skims_dt)[1:2], fieldname1, fieldname2, fieldname3)
  # create a file
  cmap_data_zone_skims <- cmap_data_zone_skims_dt[,..cols]
  setnames(cmap_data_zone_skims, c("Origin", "Destination", "Peak", "OffPeak", "Miles"))
  # write the file
  fwrite(cmap_data_zone_skims, file.path(scenario_input_paths[thescenario], "cmap_data_zone_skims.csv"))
}

# data_mesozone_skims.csv
names(data_mesozone_skims_dt)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # field name
  fieldname <- as.character(scenario_years[thescenario])
  cols <- c(names(data_mesozone_skims_dt)[1:2], fieldname)
  # create a file
  data_mesozone_skims <- data_mesozone_skims_dt[,..cols]
  setnames(data_mesozone_skims, c("Origin", "Destination", "Time"))
  # write the file
  fwrite(data_mesozone_skims, file.path(scenario_input_paths[thescenario], "data_mesozone_skims.csv"))
}

# cmap_warehouses.csv
for (thescenario in 1:length(scenario_input_paths)){
  # write the file
  fwrite(cmap_warehouses, file.path(scenario_input_paths[thescenario], "cmap_warehouses.csv"))
}

# data_modepath_miles.csv
# this is a list of tables not a wide table
names(data_modepath_miles_list)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # write the file
  fwrite(data_modepath_miles_list[[as.character(scenario_years[thescenario])]], 
         file.path(scenario_input_paths[thescenario], "data_modepath_miles.csv"))
}

# data_modepath_skims.csv
# this is a list of tables not a wide table
names(data_modepath_skims_list)
scenario_years <- c(2022, future_years)
for (thescenario in 1:length(scenario_input_paths)){
  # write the file
  fwrite(data_modepath_skims_list[[as.character(scenario_years[thescenario])]], 
         file.path(scenario_input_paths[thescenario], "data_modepath_skims.csv"))
}

# Three other inputs files that are not forecasted (but could be)

# data_hh.csv --- Needed for code to work but included just for consistency with the firm synthesis component of the CSVM, 
#                 Approach taken to just copy the base year file is fine
# data_emp_control_taz.csv --- Need for code to work but currently scaling base year to match the county targets
#                              Approach taken to just copy the base year file is fine
# data_emp_cbpzone.csv -- Needed in freight truck touring model

