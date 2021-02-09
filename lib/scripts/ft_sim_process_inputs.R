
# This function loads all necessary inputs into envir, after any needed transformations
ft_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c(vehtourpat                = file.path(SYSTEM_DATA_PATH, "model_vehicle_tourpattern.csv"), #Vehicle tour MNL model
                     emp_cbpzone               = file.path(SYSTEM_DATA_PATH, "data_emp_cbpzone.csv"), #Employment by CBP Zone
                     mzemp                     = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), #Industry rankings data by mesozone based on employment -- used here for correspodence between counties and mesozones
                     numberoftours             = file.path(SYSTEM_DATA_PATH, "model_numberoftours.csv"), #Number of tours MNL model
                     mz_centroids              = file.path(SYSTEM_DATA_PATH, "cmap_data_zone_centroids.csv"), #Centroid coordinates of zones
                     mz_skims                  = file.path(SYSTEM_DATA_PATH, "cmap_data_zone_skims.csv"), #zonal skim times
                     stopduration              = file.path(SYSTEM_DATA_PATH, "model_stopduration.csv"), #Stop duration MNL model
                     tod                       = file.path(SYSTEM_DATA_PATH, "model_timeofday.csv"), #TOD MNL model
                     warehouses                = file.path(SYSTEM_DATA_PATH, "cmap_warehouses.csv"), #CMAP Warehouse list
                     zn_totemp                 = file.path(SYSTEM_DATA_PATH, "cmap_data_zone_employment.csv"), #CMAP Zone employment
                     ft_sim_daily_sample       = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_daily_sample.R"),
                     ft_sim_stop_duration      = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_stop_duration.R"),
                     ft_sim_stop_sequence      = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_stop_sequence.R"),
                     ft_sim_summary_render     = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_summary_render.R"),
                     ft_sim_time_of_day        = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_time_of_day.R"),
                     ft_sim_vehicle_choice     = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_vehicle_choice_tour_pattern.R"),
                     ft_sim_warehouse          = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_warehouse_allocation.R"))
  loadInputs(files = project.files, envir = envir)
  
  ### Load scenario input files
  
  ### TODO consider moving some of the above files, e.g., skims and other data files that might vary by scenario
  
  ### Load inputs/outputs from earlier steps
  
  # Firm Synthesis outputs
  load(file.path(SCENARIO_OUTPUT_PATH, "RegionFirms.Rdata"))
  envir[["RegionFirms"]] <- RegionFirms
  
  # Supply Chain outputs
  load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_SCM_OUTPUTNAME))
  gc()
  
  ### Return
  return(sc_sim_results[["BuyerSupplierPairs"]])
  
}
