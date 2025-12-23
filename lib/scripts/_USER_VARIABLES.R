# User options to control hardware use and run mode

# Parallel processing settings
USER_COST_CORES <- 16L # How many processors should be used to create inputs for PMG (except for the mode choice component)? (a number, followed by L to indicate integer)
USER_MODE_CHOICE_CORES <- 4L # How many processors should be used to run the mode choice model? (a number, followed by L to indicate integer)
USER_PMG_CORES <- 8L # How many processors should be used to run PMG models? (a number, followed by L to indicate integer)

# Settings for markets to run to support partial runs
USER_MARKETS_ALL <- TRUE # Markets to run, TRUE: ALL, FALSE: Partial
USER_COMMODITIES_RUN <- c(1:3) # Commodities to run if running partial market run. An array of SCTG.

# Testing/calibration settings
USER_RUN_MODE <- "Application" # What type of run is being done? Options are "Application" or "Calibration" where calibration triggers certain model components to run iterative adjustments
USER_RUN_TEST <- "None" # What type of test is being done? Options are "None", "Sensitivity", "Parameters"