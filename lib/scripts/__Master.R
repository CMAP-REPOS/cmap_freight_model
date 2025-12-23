
### Initialize Application -------------------------------------------------------------------

# Start the rFreight application
source(file.path("lib", "scripts", "init_start_rFreight_model.R"))

cat("Running the", SCENARIO_NAME, "scenario for", SCENARIO_YEAR, "\n")
SCENARIO_RUN_START   <- Sys.time()

### 1. Firm Synthesis ------------------------------------------------------------------------
 
if (SCENARIO_RUN_FIRMSYN) {

  cat("Starting Firm Synthesis Step", "\n")
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim.R"))

  # Process inputs
  cat("Processing Firm Synthesis Inputs", "\n")
  firm_inputs <- new.env()
  Establishments <- firm_sim_process_inputs(envir = firm_inputs)

  # Run simulation
  cat("Running Firm Synthesis Simulation", "\n")
  firm_sim_results <- suppressMessages(
    run_sim(
      FUN = firm_sim,
      data = Establishments,
      packages = SYSTEM_PKGS,
      lib = SYSTEM_PKGS_PATH,
      inputEnv = firm_inputs
    )
  )
  
  # Save inputs and results
  cat("Saving Firm Synthesis Database", "\n")
  save(firm_sim_results, 
       firm_inputs, 
       file = file.path(SCENARIO_OUTPUT_PATH,
                        SYSTEM_FIRMSYN_OUTPUTNAME))
  
  rm(firm_sim_results, 
     firm_inputs,
     Establishments)
  
  gc(verbose = FALSE)
  
}

### 2. Simulate Supply Chain ------------------------------------------------------------------------

if (SCENARIO_RUN_SCM) {

  cat("Starting Supply Chain Model Step", "\n")
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim.R"))

  # Process inputs
  cat("Processing Supply Chain Model Inputs", "\n")
  sc_inputs <- new.env()
  naics_set <- sc_sim_process_inputs(envir = sc_inputs)

  # Run simulation
  cat("Running Supply Chain Model Simulation", "\n")
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
  cat("Saving Supply Chain Model Database", "\n")
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

  cat("Starting Freight Truck Touring Step", "\n")
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_process_inputs.R"))

  # Process inputs
  cat("Processing Freight Truck Touring Inputs", "\n")
  ft_inputs <- new.env()
  AnnualShipments <- ft_sim_process_inputs(envir = ft_inputs)

  # Run simulation
  cat("Running Freight Truck Touring Simulation", "\n")
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
  cat("Saving Freight Truck Touring Database", "\n")
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

  cat("Producing Freight Truck Trip Tables", "\n")
  
  # Load executive functions
  source(file.path(SYSTEM_SCRIPTS_PATH, "tt_build.R"))
  source(file.path(SYSTEM_SCRIPTS_PATH, "tt_process_inputs.R"))

  # Process inputs
  cat("Processing Freight Truck Trip Tables Inputs", "\n")
  tt_inputs <- new.env()
  ft_trips <- tt_process_inputs(envir = tt_inputs)

  # Create trip tables
  cat("Writing Freight Truck Trip Tables to OMX Files", "\n")
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
  cat("Saving Freight Truck Trip Tables Database", "\n")
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
  
  cat("Producing Freight Model Dashboard", "\n")
  
  # Load executive functions
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_build.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "db_build_process_inputs.R"))
  
  # Process inputs
  cat("Processing Freight Model Dashboard Inputs", "\n")
  db_inputs <- new.env()
  db_build_process_inputs(envir = db_inputs)
  
  # Generate dashboard and spreadsheet
  cat("Rendering Freight Model Dashboard and Spreadsheet", "\n")
  dashboardFileLoc <- suppressWarnings(suppressMessages(
    run_sim(FUN = db_build, data = NULL,
            packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH,
            inputEnv = db_inputs
    )
  ))
  
  # Save results to Rdata file
  cat("Saving Dashboard Tabulations Database", "\n")
  save(db_inputs, 
       file = file.path(SCENARIO_OUTPUT_PATH, 
                        SYSTEM_DB_OUTPUTNAME)) 
  
}
