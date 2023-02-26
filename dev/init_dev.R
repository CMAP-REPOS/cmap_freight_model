# Initialize model (default scenario)
# Note this is not a function, script to be sourced, load variables into global environments
# Includes additional packages and standard objects used throughout development work
# Any SYSTEM variables used in this script but not defined here are defined in _SYSTEM_VARIABLES.R

### Initialize Application -------------------------------------------------------------------

# Start the rFreight application 
source(file.path("lib", "scripts", "init_start_rFreight_model.R"))

# Load additional dev packages
loadPackages(SYSTEM_DEV_PKGS, SYSTEM_PKGS_PATH)

### Define development paths -----------------------------------------------------------------

# Define standard file paths for dev folder struture
SYSTEM_DEV_DATA_PATH                   <- file.path(SYSTEM_DEV_PATH, "Data_Processed")
SYSTEM_DEV_MODEL_PATH                  <- file.path(SYSTEM_DEV_DATA_PATH, "Models")
SYSTEM_DEV_ESTIMATION_PATH             <- file.path(SYSTEM_DEV_PATH, "Estimation")
SYSTEM_DEV_TESTING_PATH                <- file.path(SYSTEM_DEV_PATH, "Testing")
SYSTEM_DEV_CALIBRATION_PATH            <- file.path(SYSTEM_DEV_PATH, "Calibration")
SYSTEM_DEV_CALIBRATION_DOC_PATH        <- file.path(SYSTEM_DEV_CALIBRATION_PATH, "_Documentation")
SYSTEM_USER_GUIDE_PATH                 <- file.path(SYSTEM_DEV_PATH, "User_Guide")


