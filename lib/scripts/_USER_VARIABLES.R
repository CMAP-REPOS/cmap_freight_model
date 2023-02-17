# User options to control outputs, hardware use, and run mode

# Model outputs settings
USER_GENERATE_DASHBOARD <- TRUE # Should the model generate the output dashboard? (TRUE/FALSE)
USER_GENERATE_DASHBOARD_MAPS <- TRUE # Should the output dashboard inclde maps? (TRUE/FALSE)
USER_SAVE_INTERMEDIATEFILES <- TRUE # Should intermediate files be saved? (TRUE/FALSE)

# Parallel processing settings
USER_COST_CORES <- 16L # How many processors should be used to create inputs for PMG (except for the mode choice component)? (a number, followed by L to indicate integer)
USER_MODE_CHOICE_CORES <- 4L # How many processors should be used to run the mode choice model? (a number, followed by L to indicate integer)
USER_PMG_CORES <- 8L # How many processors should be used to run PMG models? (a number, followed by L to indicate integer)
USER_MAX_RAM <- 64 # Amount of RAM not to exceed. Units: Gb
USER_FUTURE_MAX_SIZE <- 20 # Maximum amount of RAM that can be used by a future processor. Units: Gb

# Settings for markets to run to support partial runs
USER_MARKETS_ALL <- TRUE # Markets to run, TRUE: ALL, FALSE: Partial
USER_COMMODITIES_RUN <- c(1:3) # Commodities to run if running partial market run. An array of SCTG.

# Testing/calibration settings
USER_RUN_MODE <- "Application" # What type of run is being done? Options are "Application" or "Calibration" where calibration triggers certain model components to run iterative adjustments
USER_RUN_TEST <- "None" # What type of test is being done? Options are "None", "Sensitivity", "Parameters"