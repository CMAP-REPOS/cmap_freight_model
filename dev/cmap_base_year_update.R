# CMAP Freight Model
# dev script: cmap_base_year_update.R
#
# Purpose:
# Document and run in sequence scripts to update:
# baseline base year inputs 
#
# Outputs: 
# Files in lib/data:
# <add here>
#
# use init_dev.R to start application
source("./dev/init_dev.R")

# source dev scripts containing data processing functions 
source(file.path("dev", "data_naics2017_corresp.R"))
source(file.path("dev", "data_taz_system_update_country.R"))
source(file.path("dev", "data_cbp_2022.R"))
source(file.path("dev", "data_ag_2022.R"))
source(file.path("dev", "data_trade.R"))
source(file.path("dev", "data_io.R"))

### RUN SCRIPTS ==================================================

# Develop updated common data inputs for the base year of 2022 for the baseline scenario

# Location for copy of old SYSTEM_DATA files (starting point for this update)
SYSTEM_DATA_OLD_PATH <- file.path(SYSTEM_DEV_DATA_PATH, "_Inputs_Data_Old")
# location for new SYSTEM_DATA files (for use in the updated model)
SYSTEM_DATA_NEW_PATH <- file.path(SYSTEM_DEV_DATA_PATH, "_Inputs_Data_Revised")

# Forecast inputs location
dev_forecasts_dir <- file.path(SYSTEM_DEV_PATH, "FutureScenarios")
macro_inputs_path <- file.path(dev_forecasts_dir, "Macroeconomic Inputs")

# Firm synthesis inputs ------------------------------------------------

# Environment for the old inputs to firm synthesis
firm_inputs <- new.env()

# Data files read by firm_sim_process_inputs
project.files <- c( c_n2_empcats         = file.path(SYSTEM_DATA_OLD_PATH, "corresp_naics2_empcats.csv"),    # Correspondence between NAICS2 groups and aggregated employment groups
                    c_n6_n6io_sctg       = file.path(SYSTEM_DATA_OLD_PATH, "corresp_naics6_n6io_sctg.csv"),  # Correspondence between NAICS 6-digit, I/O NAICS, and SCTG
                    cbp                  = file.path(SYSTEM_DATA_OLD_PATH, "data_emp_cbp.csv"),              # CBP data file
                    cbp_ag               = file.path(SYSTEM_DATA_OLD_PATH, "data_emp_cbp_ag.csv"),           # CBP data file -- Agriculture records generated separately
                    EstSizeCategories    = file.path(SYSTEM_DATA_OLD_PATH, "data_est_size_categories.csv"),  # Establishment size categories and labels
                    io                   = file.path(SYSTEM_DATA_OLD_PATH, "data_2010io.csv"),               # Inputs output table
                    prefweights          = file.path(SYSTEM_DATA_OLD_PATH, "data_firm_pref_weights.csv"),    # Preference weights (time vs cost)
                    mzemp                = file.path(SYSTEM_DATA_OLD_PATH, "data_mesozone_emprankings.csv"), # Industry rankings data by mesozone based on employment
                    TAZ_System           = file.path(SYSTEM_DATA_OLD_PATH, "TAZ_System.csv"))                # TAZ system 

loadInputs(files = project.files, envir = firm_inputs)
names(firm_inputs)

# Environment for the old inputs to supply chain
sc_inputs <- new.env()

# Data files read by sc_sim_process_inputs
project.files <- c(ModeChoiceParameters       = file.path(SYSTEM_DATA_OLD_PATH, "ModeChoiceParameters.rds"),
                   PMGParameters              = file.path(SYSTEM_DATA_OLD_PATH, "PMGParameters.rds"), # Input parameters to PMG
                   sctg                       = file.path(SYSTEM_DATA_OLD_PATH, "corresp_sctg_category.csv"), #correspondence between SCTG and descriptions
                   mode_availability          = file.path(SYSTEM_DATA_OLD_PATH, "data_modepath_availability.csv"), # Modes available by SCTG and ODSegment
                   mode_description           = file.path(SYSTEM_DATA_OLD_PATH, "data_mode_description.csv"), # Mode description
                   FAF_DISTANCE               = file.path(SYSTEM_DATA_OLD_PATH, "data_faf_distance.csv"),
                   FAF_TON_DIST               = file.path(SYSTEM_DATA_OLD_PATH, "data_faf_ton_distribution.csv"),
                   FAF_TON_TRADETYPE          = file.path(SYSTEM_DATA_OLD_PATH, "data_faf_ton_tradetype.csv"),
                   mesozone_gcd               = file.path(SYSTEM_DATA_OLD_PATH, "data_mesozone_gcd.csv"),  #Mesozone to mesozone gcds
                   shipsize                   = file.path(SYSTEM_DATA_OLD_PATH, "data_commodity_shipmentsizes.csv"),
                   skims_airports             = file.path(SYSTEM_DATA_OLD_PATH, "data_modepath_airports.csv"),
                   skims_ports                = file.path(SYSTEM_DATA_OLD_PATH, "data_modepath_ports.csv"),
                   distchannel_calibration    = file.path(SYSTEM_DATA_OLD_PATH, "model_distchannel_calibration.csv"),
                   distchannel_food           = file.path(SYSTEM_DATA_OLD_PATH, "model_distchannel_food.csv"),
                   distchannel_food_cal       = file.path(SYSTEM_DATA_OLD_PATH, "model_distchannel_food_cal.csv"),
                   distchannel_mfg            = file.path(SYSTEM_DATA_OLD_PATH, "model_distchannel_mfg.csv"),
                   distchannel_mfg_cal        = file.path(SYSTEM_DATA_OLD_PATH, "model_distchannel_mfg_cal.csv"))

loadInputs(files = project.files, envir = sc_inputs,
           fread.args = list(c_sctg_cat = list(stringsAsFactors = TRUE)))
names(sc_inputs)

# Environment for the old inputs to freight truck touring model
ft_inputs <- new.env()

### Load project input files
project.files <- c(vehtourpat                = file.path(SYSTEM_DATA_PATH, "model_vehicle_tourpattern.csv"), #Vehicle tour MNL model
                   emp_cbpzone               = file.path(SYSTEM_DATA_PATH, "data_emp_cbpzone.csv"), #Employment by CBP Zone
                   #mzemp                     = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), #Commment out here as already used in firm synthesis
                   numberoftours             = file.path(SYSTEM_DATA_PATH, "model_numberoftours.csv"), #Number of tours MNL model
                   mz_centroids              = file.path(SYSTEM_DATA_PATH, "cmap_data_zone_centroids.csv"), #Centroid coordinates of zones
                   mz_skims                  = file.path(SYSTEM_DATA_PATH, "cmap_data_zone_skims.csv"), #zonal skim times
                   stopduration              = file.path(SYSTEM_DATA_PATH, "model_stopduration.csv"), #Stop duration MNL model
                   tod                       = file.path(SYSTEM_DATA_PATH, "model_timeofday.csv"), #TOD MNL model
                   warehouses                = file.path(SYSTEM_DATA_PATH, "cmap_warehouses.csv"), #CMAP Warehouse list
                   zn_totemp                 = file.path(SYSTEM_DATA_PATH, "cmap_data_zone_employment.csv")) #CMAP Zone employment
loadInputs(files = project.files, envir = ft_inputs)
names(ft_inputs)

# Review and update the files, by type:
# Correspondences, labels, and parameters:

# 1. corresp_naics2_empcats.csv: Correspondence between NAICS2 groups and aggregated employment groups
firm_inputs$c_n2_empcats 
# No changes needed
file.copy(from = file.path(SYSTEM_DATA_OLD_PATH, "corresp_naics2_empcats.csv"),
          to = file.path(SYSTEM_DATA_NEW_PATH, "corresp_naics2_empcats.csv"), 
          overwrite = TRUE)

# 2. corresp_naics6_n6io_sctg.csv: Correspondence between NAICS 6-digit, I/O NAICS, and SCTG
firm_inputs$c_n6_n6io_sctg
# Update to 2017 NAICS IO codes and NAICS6 codes (old version was in 2007 codes)
data_naics2017_corresp()

# 3, data_est_size_categories.csv: Establishment size categories and labels
firm_inputs$EstSizeCategories
# No changes needed
file.copy(from = file.path(SYSTEM_DATA_OLD_PATH, "data_est_size_categories.csv"),
          to = file.path(SYSTEM_DATA_NEW_PATH, "data_est_size_categories.csv"), 
          overwrite = TRUE)

# 4. data_firm_pref_weights.csv: Preference weights (time vs cost)
firm_inputs$prefweights
# Any changes to be made in calibration
file.copy(from = file.path(SYSTEM_DATA_OLD_PATH, "data_firm_pref_weights.csv"),
          to = file.path(SYSTEM_DATA_NEW_PATH, "data_firm_pref_weights.csv"), 
          overwrite = TRUE)

# 5. TAZ_System.csv: TAZ system
firm_inputs$TAZ_System
# Requires updates to countries to match the new trade data, leave the rest of the TAZ system file unchanged
data_taz_system_update_country(macro_inputs_path)

# Data files:

# 6. data_emp_cbp.csv: CBP data file
firm_inputs$cbp
# Update to 2022 CBP (which still use the 2017 NAICS codes)
# The script scales that CBP data to match state totals 
# for employment and establishments by NAICS 2 digit code
data_cbp_2022()

# 7. data_emp_cbp_ag.csv: CBP data file -- Agriculture records generated separately
firm_inputs$cbp_ag
# Update to 2022 using the 2022 USDA census of agriculture
data_ag_2022()

# 8. data_2010io.csv: Input output table
# Requires the processed trade data for building the imports and exports
# data_foreign_prod_BaseCase_cleaned.csv/data_foreign_cons_BaseCase_cleaned.csv
#source(file.path("dev", "data_trade.R"))

# Process the 2017 io data and scale to 2022 using productivity factors
#source(file.path("dev", "data_io.R"))


# 10. data_mesozone_emprankings.csv: Industry rankings data by mesozone based on employment
firm_inputs$mzemp
### prepared by CMAP -- use their files  and copy here to the correct locations
### TODO better organization needed for where to put there files
cmap_prepared_files_path <- "../cmap_scenarios/Baseline Scenario/2025-02-13 CMAP Inputs/freight_model_inputs"

### TODO there are year specific files from CMAP for this input
### but are they just used in base year firm synthesis for allocation of CBP establishments to TAZs?
### Copy the 2022 file to the new data inputs folder

### fields should be CountyFIPS and Mesozone

file.copy(from = file.path(cmap_prepared_files_path, "data_mesozone_emprankings_2022.csv"),
          to = file.path(SYSTEM_DATA_NEW_PATH, "data_mesozone_emprankings.csv"), 
          overwrite = TRUE)



# Update the data in the main model data folder
files_to_copy <- list.files(SYSTEM_DATA_NEW_PATH, full.names = TRUE)
file.copy(from = files_to_copy,
          to = SYSTEM_DATA_PATH, 
          overwrite = TRUE)

