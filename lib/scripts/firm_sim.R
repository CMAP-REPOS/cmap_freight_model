
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
      
      progressUpdate(prop = 2/12, dir = SCENARIO_LOG_PATH)
      FirmsDomestic <- firm_synthesis_commodities(Firms = FirmsDomestic,
                                                  c_n2017_n2012 = c_n2017_n2012,
                                                  c_n6_n6io_sctg = c_n6_n6io_sctg)
      
      progressUpdate(prop = 4/12, dir = SCENARIO_LOG_PATH)
      # For summaries prior to scaling
      FirmsDomesticUnscaled <- copy(FirmsDomestic)
      
      # Scale the emplyoment
      FirmsDomestic <- firm_synthesis_scaling(Firms = FirmsDomestic,
                                              emp_control = emp_control,
                                              emp_control_taz = emp_control_taz,
                                              c_cbp_faf = c_cbp_faf,
                                              c_cbp_mz = c_cbp_mz,
                                              c_taz_mz = c_taz_mz,
                                              EmpBounds = EmpBounds)
      
      # save complete firms  list after scaling
      save(FirmsDomestic, file = file.path(SCENARIO_OUTPUT_PATH, "BaseYearDomesticFirms.Rdata"))
     
    } else {
      
      # Future year/alternative scenario
      
      if(file.exists(file.path(SCENARIO_BASE_OUTPUT_PATH, "BaseYearDomesticFirms.Rdata"))){
        
        # Load the output from the base year firm synthesis model
        cat("Loading Base Year Establishment List", "\n")
        
        load(file.path(SCENARIO_BASE_OUTPUT_PATH, "BaseYearDomesticFirms.Rdata"))
        
        progressUpdate(prop = 4/12, dir = SCENARIO_LOG_PATH)
        # For summaries prior to scaling
        FirmsDomesticUnscaled <- copy(FirmsDomestic)
        
        # Scale the emplyoment
        FirmsDomestic <- firm_synthesis_scaling(Firms = FirmsDomestic,
                                                emp_control = emp_control,
                                                emp_control_taz = emp_control_taz,
                                                c_cbp_faf = c_cbp_faf,
                                                c_cbp_mz = c_cbp_mz,
                                                c_taz_mz = c_taz_mz,
                                                EmpBounds = EmpBounds)
        
        # save complete firms  list after scaling
        save(FirmsDomestic, file = file.path(SCENARIO_OUTPUT_PATH, "ScenarioDomesticFirms.Rdata"))
        
      } else {
        
        stop("No Base Scenario outputs available. Please run the Base Scenario first.")
        
      }
    }
    
    cat("Saving Regional Establishment List", "\n")
    
    # save regional CBP list for analysis
    RegionFirms <- FirmsDomestic[MESOZONE < 150]
    save(RegionFirms, file = file.path(SCENARIO_OUTPUT_PATH, "RegionFirms.Rdata"))
    
    # save warehouse list for use in truck touring model
    # NAICS 481 air, 482 rail, 483 water, 493 warehouse and storage
    warehouses <- RegionFirms[substr(Industry_NAICS6_Make, 1, 3) %in% c(481, 482, 483, 493)]
    save(warehouses, file = file.path(SCENARIO_OUTPUT_PATH, "warehouses.Rdata"))
    
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
    fwrite(SCTGCheck, file = file.path(SCENARIO_OUTPUT_PATH, "SCTGCheck.csv"))
    
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
                                          unitcost = unitcost)
    
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
                                                            consumers = consumers,
                                                            naics_set = naics_set,
                                                            ScenarioFirms = ScenarioFirms, 
                                                            TAZLandUseCVTM = TAZLandUseCVTM)
    
  } # end run step 2
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  if(USER_RUN_MODE == "Calibration"){
    return(get(submodel_results_name))
  } else {
    return(producers_consumers_list)
  }
  
}
