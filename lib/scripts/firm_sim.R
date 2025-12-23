
# Master function for executing the synthesis of firms.
firm_sim <- function(Establishments) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Firms", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Define run_steps if it is not already in the environment (default to running all steps)
  if(!exists("run_step")) run_step <- rep(TRUE, 2)
  
  if(run_step[1]){
    
    # TAZLandUseCVTM is is in the environment, produced by firm_sim_process_inputs
    
  }
  
  if(run_step[2]){
  
    # Different approach in base and future scenarios: 
    # base year start from unscaled CBP data,
    # future year build on base year scaled firm list
    if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
      
      cat("Creating Base Year Establishment List", "\n")
      
      # Enumerate the domestic firms list
      progressUpdate(prop = 1/11, dir = SCENARIO_LOG_PATH)
      FirmsDomestic <- firm_synthesis_enumerate(Establishments = Establishments,
                                                EstSizeCategories = EstSizeCategories,
                                                TAZEmployment = TAZEmployment,
                                                mzemp = mzemp)
      
      # Allocate SCTG commodities to firms
      progressUpdate(prop = 2/11, dir = SCENARIO_LOG_PATH)
      FirmsDomestic <- firm_synthesis_commodities(Firms = FirmsDomestic,
                                                  c_n6_n6io_sctg = c_n6_n6io_sctg,
                                                  c_n4_sctg_whl = c_n4_sctg_whl)
      
      # Scale the employment in two groups: model region and rest of the US
      progressUpdate(prop = 3/11, dir = SCENARIO_LOG_PATH)
      FirmsRegion <- scaleEstablishmentsTAZEmployment(RegionFirms = FirmsDomestic[modelregion == 1], 
                                                      TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_INTERNAL], 
                                                      NewFirmsProportion = 0,
                                                      MaxBusID = max(FirmsDomestic$BusID),
                                                      EstSizeCategories = EstSizeCategories,
                                                      TAZEmploymentShape = "LONG")
      
      FirmsNational <- scaleEstablishmentsTAZEmployment(RegionFirms = FirmsDomestic[modelregion == 2], 
                                                        TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_NATIONAL], 
                                                        NewFirmsProportion = 0,
                                                        MaxBusID = max(FirmsRegion$BusID),
                                                        EstSizeCategories = EstSizeCategories,
                                                        TAZEmploymentShape = "LONG")
      
      FirmsDomestic <- rbind(FirmsRegion, FirmsNational)
      rm(FirmsRegion, FirmsNational)
      
    } else {
      
      # Future year/alternative scenario
      cat("Updating Base Year Establishment List with Future Control Data", "\n")
        
      # Scale the base year establishments list (FirmsDomesticBase) to future employment
      progressUpdate(prop = 3/11, dir = SCENARIO_LOG_PATH)
      
      FirmsRegion <- scaleEstablishmentsTAZEmployment(RegionFirms = FirmsDomesticBase[modelregion == 1], 
                                                      TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_INTERNAL], 
                                                      MaxBusID = max(FirmsDomesticBase$BusID),
                                                      EstSizeCategories = EstSizeCategories,
                                                      TAZEmploymentShape = "LONG",
                                                      FirmSizeFactors = FirmSizeFactors)
      
      FirmsNational <- scaleEstablishmentsTAZEmployment(RegionFirms = FirmsDomesticBase[modelregion == 2], 
                                                        TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_NATIONAL], 
                                                        MaxBusID = max(FirmsRegion$BusID),
                                                        EstSizeCategories = EstSizeCategories,
                                                        TAZEmploymentShape = "LONG",
                                                        FirmSizeFactors = FirmSizeFactors)
      
      FirmsDomestic <- rbind(FirmsRegion, FirmsNational)
      rm(FirmsRegion, FirmsNational)
      
    }
    
    cat("Adding Employment Group and Spatial Variables", "\n")
    
    # Add employment classifications and spatial fields
    progressUpdate(prop = 4/11, dir = SCENARIO_LOG_PATH)
    
    FirmsDomestic[UEmpCats, 
                  EmpCatGroupedName := i.EmpCatGroupedName,
                  on = "EmpCatName"]
    
    FirmsDomestic[TAZ_System, 
                  c("Mesozone", "CBPZONE", "FAFZONE") := .(i.Mesozone, i.CBPZONE, i.FAFZONE), 
                  on = "TAZ"]
    
    # Create a list of foreign establishments
    cat("Creating Foreign Establishment List", "\n")
    
    progressUpdate(prop = 5/11, dir = SCENARIO_LOG_PATH)
    FirmsForeign <- firm_synthesis_enumerate_foreign(for_prod = for_prod,
                                                     for_cons = for_cons,
                                                     c_n6_n6io_sctg = c_n6_n6io_sctg)
    
    # Add employment classifications to foreign firms
    FirmsForeign[, NAICS2 := as.integer(substr(Industry_NAICS6_Make,1,2))]
    FirmsForeign[c_n2_empcats[,.(NAICS2, EmpCatName)],
                 EmpCatName := i.EmpCatName, on = "NAICS2"]
    FirmsForeign[UEmpCats, 
                  EmpCatGroupedName := i.EmpCatGroupedName,
                  on = "EmpCatName"]
    FirmsForeign[, NAICS2 := NULL]
    
    # Process the Input/Output table
    cat("Processing Input Output Table", "\n")
    
    progressUpdate(prop = 6/11, dir = SCENARIO_LOG_PATH)
    io_list <- firm_synthesis_input_output(io = io,
                                           c_n6_n6io_sctg = c_n6_n6io_sctg,
                                           ProductivityFactors = ProductivityFactors,
                                           FirmsDomestic = FirmsDomestic,
                                           FirmsForeign = FirmsForeign)
    
    # Create the producers table
    cat("Creating Producers Table", "\n")
    
    progressUpdate(prop = 7/11, dir = SCENARIO_LOG_PATH)
    producers_list <- firm_synthesis_producers(io = io_list$io,
                                               fromwhl_make_use = io_list$fromwhl_make_use,
                                               FirmsDomestic = FirmsDomestic,
                                               FirmsForeign = FirmsForeign,
                                               unitcost = unitcost,
                                               EstSizeCategories = EstSizeCategories)
    producers <- producers_list$producers
    
    # Create the consumers table
    cat("Creating Consumers Table", "\n")
    
    progressUpdate(prop = 8/11, dir = SCENARIO_LOG_PATH)
    consumers_list <- firm_synthesis_consumers(io = io_list$io,
                                          FirmsDomestic = FirmsDomestic,
                                          FirmsForeign = FirmsForeign,
                                          c_n6_n6io_sctg = c_n6_n6io_sctg,
                                          unitcost = unitcost,
                                          prefweights = prefweights,
                                          maxbusid = max(producers$SellerID),
                                          writeConsumersIncremental = TRUE)
    
    consumers <- consumers_list$consumers
    
    # Make a list of summaries from the results of firm synthesis
    cat("Creating Firm Synthesis Summaries", "\n")
    
    progressUpdate(prop = 9/11, dir = SCENARIO_LOG_PATH)
    firm_sim_summary <- firm_synthesis_summary(firm_sim_summary = firm_sim_summary,
                                               FirmsDomestic = FirmsDomestic,
                                               TAZEmployment = TAZEmployment, 
                                               c_n2_empcats = c_n2_empcats, 
                                               FirmsDomesticBase = if(exists("FirmsDomesticBase")) {FirmsDomesticBase} else {NULL}, 
                                               FirmSizeFactors = FirmSizeFactors, 
                                               FirmsForeign = FirmsForeign, 
                                               for_cons = for_cons, 
                                               for_prod = for_prod, 
                                               c_n6_n6io_sctg = c_n6_n6io_sctg,
                                               io_list = io_list, 
                                               producers_list = producers_list, 
                                               producers = producers, 
                                               consumers_list = consumers_list, 
                                               consumers = consumers,
                                               prefweights = prefweights)
    
    # Divide the producers and consumers up into markets based on commodities traded
    cat("Writing NAICS Market Groups", "\n")
    
    progressUpdate(prop = 10/11, dir = SCENARIO_LOG_PATH)
    naics_set <- firm_synthesis_sample_groups(firm_sim_summary$firms_sum)
    
    # save naics_set
    save(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set.Rdata"))
    fwrite(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set.csv"))
    
    # Write out the files for the separate market groups
    progressUpdate(prop = 11/11, dir = SCENARIO_LOG_PATH)
    producers_consumers_list <- firm_synthesis_write_groups(producers = producers,
                                                            consumers = consumers,
                                                            naics_set = naics_set)
    
  } # end run step 2
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  if(USER_RUN_MODE == "Calibration"){
    return(get(submodel_results_name))
  } else {
    return(firm_sim_results = c(producers_consumers_list,
                                list(naics_set = naics_set,
                                     Establishments = Establishments,
                                     FirmsDomestic = FirmsDomestic, 
                                     TAZLandUseCVTM = TAZLandUseCVTM,
                                     firm_sim_summary = firm_sim_summary)))
  }
  
}


