
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
  
    # Different approach in base and future scenarios: base year start from start,
    # future year build on base year scaled firm list
    if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
      
      cat("Creating Base Year Establishment List", "\n")
      
      # Run steps
      progressUpdate(prop = 1/12, dir = SCENARIO_LOG_PATH)
      FirmsDomestic <- firm_synthesis_enumerate(Establishments = Establishments,
                                                EstSizeCategories = EstSizeCategories,
                                                TAZEmployment = TAZEmployment,
                                                mzemp = mzemp)
      
      # Allocated SCTG commodities to firms
      progressUpdate(prop = 2/12, dir = SCENARIO_LOG_PATH)
      FirmsDomestic <- firm_synthesis_commodities(Firms = FirmsDomestic,
                                                  c_n6_n6io_sctg = c_n6_n6io_sctg)
      
      # Scale the emplyoment
      progressUpdate(prop = 3/12, dir = SCENARIO_LOG_PATH)
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
      if(file.exists(SCENARIO_BASEFIRMS)){
        
        cat("Updating Base Year Establishment List with Future Control Data", "\n")
        
        # Load the output from the base year firm synthesis model
        progressUpdate(prop = 3/12, dir = SCENARIO_LOG_PATH)
        
        load(SCENARIO_BASEFIRMS)
        FirmsDomestic <- firm_sim_results$FirmsDomestic
        rm(firm_sim_results)
        
        # Scale the emplyoment
        FirmsRegion <- scaleEstablishmentsTAZEmployment(RegionFirms = FirmsDomestic[modelregion == 1], 
                                                        TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_INTERNAL], 
                                                        NewFirmsProportion = BASE_NEW_FIRMS_PROP,
                                                        MaxBusID = max(FirmsDomestic$BusID),
                                                        EstSizeCategories = EstSizeCategories,
                                                        TAZEmploymentShape = "LONG")
        
        FirmsNational <- scaleEstablishmentsTAZEmployment(RegionFirms = FirmsDomestic[modelregion == 2], 
                                                          TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_NATIONAL], 
                                                          NewFirmsProportion = BASE_NEW_FIRMS_PROP,
                                                          MaxBusID = max(FirmsRegion$BusID),
                                                          EstSizeCategories = EstSizeCategories,
                                                          TAZEmploymentShape = "LONG")
        
        FirmsDomestic <- rbind(FirmsRegion, FirmsNational)
        rm(FirmsRegion, FirmsNational)
        
      } else {
        
        stop("No Base Scenario outputs available. Please run the Base Scenario first.")
        
      }
    }
    
    cat("Adding Employment Group and Spatial Variables", "\n")
    
    # Add employment classifications and spatial fields
    progressUpdate(prop = 4/12, dir = SCENARIO_LOG_PATH)
    
    FirmsDomestic[UEmpCats, 
                  EmpCatGroupedName := i.EmpCatGroupedName,
                  on = "EmpCatName"]
    
    FirmsDomestic[TAZ_System, 
                  c("Mesozone", "CBPZONE") := .(i.Mesozone, i.CBPZONE), 
                  on = "TAZ"]
    
    cat("Creating Foreign Establishment List", "\n")
    
    progressUpdate(prop = 5/12, dir = SCENARIO_LOG_PATH)
    FirmsForeign <- firm_synthesis_enumerate_foreign(for_prod = for_prod,
                                                     for_cons = for_cons,
                                                     c_n6_n6io_sctg = c_n6_n6io_sctg,
                                                     EmpBounds = EmpBounds)
    
    # Check on foreign and domestic firms
    SCTGCheck <- merge(FirmsDomestic[, .(FirmsDomestic = .N), by = Commodity_SCTG][order(Commodity_SCTG)],
                       FirmsForeign[, .(FirmsForeign = .N), by = Commodity_SCTG][order(Commodity_SCTG)],
                       by = "Commodity_SCTG")
    
    cat("Processing Input Output Table", "\n")
    
    progressUpdate(prop = 6/12, dir = SCENARIO_LOG_PATH)
    io_list <- firm_synthesis_input_output(io = io,
                                           c_n6_n6io_sctg = c_n6_n6io_sctg)
    
    cat("Creating Producers Table", "\n")
    
    progressUpdate(prop = 7/12, dir = SCENARIO_LOG_PATH)
    producers <- firm_synthesis_producers(io = io_list$io,
                                          fromwhl = io_list$fromwhl,
                                          FirmsDomestic = FirmsDomestic,
                                          FirmsForeign = FirmsForeign,
                                          unitcost = unitcost,
                                          EstSizeCategories = EstSizeCategories)
    
    cat("Creating Consumers Table", "\n")
    
    progressUpdate(prop = 8/12, dir = SCENARIO_LOG_PATH)
    consumers <- firm_synthesis_consumers(io = io_list$io,
                                          wholesalers = producers[ProdType == 3],
                                          FirmsDomestic = FirmsDomestic,
                                          FirmsForeign = FirmsForeign,
                                          c_n6_n6io_sctg = c_n6_n6io_sctg,
                                          unitcost = unitcost,
                                          maxbusid = max(producers$SellerID),
                                          writeConsumersIncremental = TRUE) #change to TRUE for production
    
    cat("Firm Synthesis Summary Dashboard", "\n")
    
    progressUpdate(prop = 9/12, dir = SCENARIO_LOG_PATH)
    firms_sum <- firm_synthesis_summary_render(FirmsDomesticUnscaled = FirmsDomesticUnscaled,
                                               FirmsDomestic = FirmsDomestic,
                                               producers = producers,
                                               consumers = consumers,
                                               io = io_list$io,
                                               prefweights = prefweights,
                                               emp_control = emp_control,
                                               c_n6_labels = c_n6_labels,
                                               c_mz_faf_reg = c_mz_faf_reg)
    
    cat("Writing NAICS Market Groups", "\n")
    
    progressUpdate(prop = 10/12, dir = SCENARIO_LOG_PATH)
    naics_set <- firm_synthesis_sample_groups(firms_sum)
    
    # save naics_set
    save(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set.Rdata"))
    fwrite(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set.csv"))
    
    progressUpdate(prop = 11/12, dir = SCENARIO_LOG_PATH)
    producers_consumers_list <- firm_synthesis_write_groups(producers = producers,
                                                            consumers = consumers)
    
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
                                     SCTGCheck = SCTGCheck)))
  }
  
}
