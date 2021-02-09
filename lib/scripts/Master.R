
### Initialize Application -------------------------------------------------------------------

# Load global variables
source(file.path("lib", "scripts", "_SYSTEM_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_BASE_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_SCENARIO_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_USER_VARIABLES.R"))

# Install any packages not available on CRAN
source(file.path(SYSTEM_SCRIPTS_PATH, "install_special_packages.R"))

# Load current rFreight installation
suppressWarnings(suppressMessages(library(rFreight, lib.loc = SYSTEM_PKGS_PATH)))

# Check for new rFreight version, load rFreight and other packages, create output folder
initializeApp(rFreight.path = SYSTEM_RFREIGHT_PATH,
              output.path = SCENARIO_OUTPUT_PATH,
              lib = SYSTEM_PKGS_PATH,
              packages = SYSTEM_PKGS,
              reload.rFreight = FALSE)

# Load any other scripts of general functions for use in the model
source(file.path(SYSTEM_SCRIPTS_PATH, "omx.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "FutureTaskProcessor.R"))

cat("Running the", SCENARIO_NAME, "scenario for", SCENARIO_YEAR, "\n")
SCENARIO_RUN_START   <- Sys.time()

### 1. Firm Synthesis ------------------------------------------------------------------------
 
if (SCENARIO_RUN_FIRMSYN) {

  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim.R"))

  # Process inputs
  firm_inputs <- new.env()
  cbp <- firm_sim_process_inputs(envir = firm_inputs)

  # Run simulation
  firm_sim_results <- suppressMessages(
    run_sim(
      FUN = firm_sim,
      data = cbp,
      packages = SYSTEM_PKGS,
      lib = SYSTEM_PKGS_PATH,
      inputEnv = firm_inputs
    )
  )
  
  # Save inputs and results
  save(firm_sim_results, 
       firm_inputs, 
       file = file.path(SCENARIO_OUTPUT_PATH,
                        SYSTEM_FIRMSYN_OUTPUTNAME))
  
  rm(firm_sim_results, 
     firm_inputs,
     cbp)
  
  gc(verbose = FALSE)
  
}

### 2. Simulate Supply Chain ------------------------------------------------------------------------

if (SCENARIO_RUN_SCM) {

  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim.R"))

  # Process inputs
  sc_inputs <- new.env()
  naics_set <- sc_sim_process_inputs(envir = sc_inputs)

  # Run simulation
  sc_sim_results <- suppressMessages(
    run_sim(
      FUN = sc_sim,
      data = naics_set,
      packages = SYSTEM_PKGS,
      lib = SYSTEM_PKGS_PATH,
      inputEnv = sc_inputs
    )
  )

  # Save inputs and results
  save(sc_sim_results,
       sc_inputs,
       file = file.path(SCENARIO_OUTPUT_PATH,
                        SYSTEM_SCM_OUTPUTNAME))

  rm(sc_sim_results,
     sc_inputs,
     naics_set)

  gc(verbose = FALSE)

}

### 3. Simulate Freight Truck Movements  ------------------------------------------------------------------------

if (SCENARIO_RUN_FTTM) {

  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_process_inputs.R"))

  # Process inputs
  ft_inputs <- new.env()
  AnnualShipments <- ft_sim_process_inputs(envir = ft_inputs)

  # Run simulation
  ft_trips <- suppressMessages(
    run_sim(
      FUN = ft_sim,
      data = AnnualShipments,
      packages = SYSTEM_PKGS,
      lib = SYSTEM_PKGS_PATH,
      inputEnv = ft_inputs
    )
  )

  # Save inputs and results
  save(ft_trips,
       ft_inputs,
       file = file.path(SCENARIO_OUTPUT_PATH,
                        SYSTEM_FTTM_OUTPUTNAME))

  rm(AnnualShipments,
     ft_trips,
     ft_inputs)

  gc(verbose = FALSE)

}

### 4. Produce Trip Tables -------------------------------------------------------------------------

if (SCENARIO_RUN_TT) {

  # Load executive functions
  source(file.path(SYSTEM_SCRIPTS_PATH, "tt_build.R"))
  source(file.path(SYSTEM_SCRIPTS_PATH, "tt_process_inputs.R"))

  # Process inputs
  tt_inputs <- new.env()
  ft_trips <- tt_process_inputs(envir = tt_inputs)

  # Create trip tables
  tt_list <- suppressMessages(
    run_sim(
      FUN = tt_build,
      data = ft_trips,
      packages = SYSTEM_PKGS,
      lib = SYSTEM_PKGS_PATH,
      inputEnv = tt_inputs
    )
  )

  # Save inputs and results
  save(tt_list,
       file = file.path(SCENARIO_OUTPUT_PATH,
                        SYSTEM_TT_OUTPUTNAME))

  rm(tt_list,
     tt_inputs)

  gc(verbose = FALSE)

}

### Produce Dashboard -------------------------------------------------------------------------

SCENARIO_RUN_DURATION <- Sys.time() - SCENARIO_RUN_START

if (SCENARIO_RUN_DB) {

  # Load executive functions
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_build.R"))
  
  # source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_process_inputs.R"))
  # 
  # # Process inputs
  # db_inputs <- new.env()
  # db_process_inputs(envir = db_inputs)
  # 
  
  # Generate dashboard
  DashboardRender(data.display = "both", scenarios = SCENARIO_NAME)
  
  # dashboardFileLoc <- suppressWarnings(suppressMessages(
  #   run_sim(
  #     FUN = db_build,
  #     data = NULL,
  #     packages = SYSTEM_REPORT_PKGS,
  #     lib = SYSTEM_PKGS_PATH,
  #     inputEnv = db_inputs
  #   )
  # ))
  
}
