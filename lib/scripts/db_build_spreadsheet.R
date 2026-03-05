# Master function for preparing and rendering the CV model spreadsheet summary
db_build_spreadsheet <- function(){
  
  # Spreadsheet contains:
  # Summary of current scenario Firms,CV tour and trip list, CV trip tables
  # Summary of comparison scenario outputs or reference data summaries
  # Comparison sheets comparing key metrics between current scenario and comparison scenario or reference data
  # Spreadsheet is saved in the outputs folder of the current scenario
  # Naming convention includes names of current and reference scenarios and date/time for unique naming
  # "Summary_" is summary of the current scenario
  # "Comparison_" is a comparison between the current scenario and a comparison scenario
  # "Validation_" is a comparison between the current scenario and validation data
  
  ### Scenarios to Compare and Settings 

  # Scenarios
  SCENARIO_1 <- SCENARIO_REFERENCE_NAME
  SCENARIO_2 <- SCENARIO_NAME

  # Paths to scenario results
  if(SCENARIO_1 == "Validation"){
    SCENARIO_1_OUTPUT_PATH <- SYSTEM_CALIBRATION_PATH # Preproccessed calibration summmaries 
  } else {
    SCENARIO_1_OUTPUT_PATH <- file.path(SYSTEM_APP_PATH, "scenarios", SCENARIO_REFERENCE_NAME, "outputs")
  }
  SCENARIO_2_OUTPUT_PATH <- SCENARIO_OUTPUT_PATH

  ### Create the summaries 
  
  # 1. TAZ Summaries
  # TAZ Summaries for comparison and current scenarios
  Scenario_2_TAZ_List <- createTAZSummaries(scenario = SCENARIO_2, 
                                            scenario_output_path = SCENARIO_2_OUTPUT_PATH)
  
  if(SCENARIO_1 != "Validation" & SCENARIO_2 != SCENARIO_1){
    Scenario_1_TAZ_List <- createTAZSummaries(scenario = SCENARIO_1, 
                                              scenario_output_path = SCENARIO_1_OUTPUT_PATH,
                                              ref_scenario = TRUE)
  }
  
  # 2. Trip Table Summaries
  # Trip Table Summaries for comparison and current scenarios
  Scenario_2_Truck_Trips_List <- createTripTableSummaries(scenario = SCENARIO_2,
                                                          scenario_output_path = SCENARIO_2_OUTPUT_PATH)

  if(SCENARIO_1 != "Validation" & SCENARIO_2 != SCENARIO_1){
    Scenario_1_Truck_Trips_List <- createTripTableSummaries(scenario = SCENARIO_1,
                                                          scenario_output_path = SCENARIO_1_OUTPUT_PATH,
                                                          ref_scenario = TRUE)
  }
  
  # 3. TAZ Comparison
  if(SCENARIO_1 != "Validation" & SCENARIO_2 != SCENARIO_1) {
    Compare_TAZ_List <- compareTAZ(scenario_1_taz_list = Scenario_1_TAZ_List,
                                   scenario_2_taz_list = Scenario_2_TAZ_List)
  }
  
  # 4. Truck Trip Table Comparison
  if(SCENARIO_1 != "Validation" & SCENARIO_2 != SCENARIO_1) {
    Compare_Truck_Trips_List <- compareTripTable(scenario_1_tt_list = Scenario_1_Truck_Trips_List,
                                               scenario_2_tt_list = Scenario_2_Truck_Trips_List)
  }
  
  # 5. Validation Summaries
  if(SCENARIO_1 == "Validation") {
    
    # Pass the name of the scenario being validated (Scenario 2) and the path to the scenario validation outputs
    # For now, the validation outputs are preprocessed in the calibration scripts 
    # and loaded from cal_results object in db_process_inputs
    Validation_List <- createValidationSummaries(scenario = SCENARIO_2,
                                                 scenario_output_path = SCENARIO_1_OUTPUT_PATH)
  }
    
  ### Create spreadsheet with outputs 

  # Spreadsheet object and formatting
  # Create a summary spreadsheet:
  # Aggregate summaries
  # TAZ summaries
  wb <- createWorkbook()
  introtext1 <- "CMAP Commercial Services Vehicle Model"
  
  # Build the spreadsheet using the defined function
  
  # Comparisons between reference and alternative scenario
  if(SCENARIO_2 != SCENARIO_1 & SCENARIO_1 != "Validation") {
  
    wb <- addScenarioSummarySheet(wb,
                                "Compare_Truck_Trips",
                                Compare_Truck_Trips_List[1:3],
                                tabletitles = Compare_Truck_Trips_List$tabletitles,
                                introtext = list(introtext1,
                                                 "Comparison of Truck Trips",
                                                 paste("Reference Scenario is", Compare_Truck_Trips_List$scenario_1),
                                                 paste("Alternative Scenario is", Compare_Truck_Trips_List$scenario_2)),
                                fmtlist = data.table(table = rep(1:3, each = 3),
                                                     format = rep("nfpct",9),
                                                     col = c(11:13, 11:13, 12:14)))

    wb <- addScenarioSummarySheet(wb,
                                  "Compare_TAZ_Sum",
                                  Compare_TAZ_List[c(1)],
                                  tabletitles = Compare_TAZ_List$tabletitles[c(1)],
                                  introtext = list(introtext1,
                                                   "Comparisons by TAZ",
                                                   paste("Reference Scenario is", Compare_TAZ_List$scenario_1),
                                                   paste("Alternative Scenario is", Compare_TAZ_List$scenario_2)))

    wb <- addScenarioSummarySheet(wb,
                                  "Compare_TAZ_Det",
                                  Compare_TAZ_List[c(3, 5, 7)],
                                  tabletitles = Compare_TAZ_List$tabletitles[c(3, 5, 7)],
                                  introtext = list(introtext1,
                                                   "Comparisons by TAZ",
                                                   paste("Reference Scenario is", Compare_TAZ_List$scenario_1),
                                                   paste("Alternative Scenario is", Compare_TAZ_List$scenario_2)))
    
  }
  
  # Summaries for scenario 1 (when it is not validation data)
  if(SCENARIO_2 != SCENARIO_1 & SCENARIO_1 != "Validation") {
    
    wb <- addScenarioSummarySheet(wb,
                                  paste0(SCENARIO_1, "_Truck_Trips"),
                                  Scenario_1_Truck_Trips_List[1:6],
                                  tabletitles = Scenario_1_Truck_Trips_List$tabletitles,
                                  introtext = list(introtext1,
                                                   "Reference Scenario Truck Trips",
                                                   paste("Reference Scenario is", Scenario_1_Truck_Trips_List$scenario)))

    wb <- addScenarioSummarySheet(wb, 
                                  paste0(SCENARIO_1, "_TAZ"), 
                                  Scenario_1_TAZ_List[c(2, 6, 10)], 
                                  tabletitles = Scenario_1_TAZ_List$tabletitles[c(2, 6, 10)],
                                  introtext = list(introtext1,
                                                   "Reference Scenario TAZ Summaries",
                                                   paste("Reference Scenario is", Scenario_1_TAZ_List$scenario)))
  }
    
  # Summaries for scenario 2 (alternative scenario)
  
  wb <- addScenarioSummarySheet(wb,
                              paste0(SCENARIO_2, "_Truck_Trips"),
                              Scenario_2_Truck_Trips_List[1:6],
                              tabletitles = Scenario_2_Truck_Trips_List$tabletitles,
                              introtext = list(introtext1,
                                               "Alternative Scenario Truck Trips",
                                               paste("Alternative Scenario is", Scenario_2_Truck_Trips_List$scenario)))

  wb <- addScenarioSummarySheet(wb, 
                                paste0(SCENARIO_2, "_TAZ"), 
                                Scenario_2_TAZ_List[c(2, 6, 10)], 
                                tabletitles = Scenario_2_TAZ_List$tabletitles[c(2, 6, 10)],
                                introtext = list(introtext1,
                                                 "Alternative Scenario TAZ Summaries",
                                                 paste("Alternative Scenario is", Scenario_2_TAZ_List$scenario)))
  
  # Validation comparisons
  if(SCENARIO_1 == "Validation"){
    
    wb <- addScenarioSummarySheet(wb,
                                  paste0(SCENARIO_2, "_Val"),
                                  Validation_List[1:(length(Validation_List)-2)],
                                  tabletitles = Validation_List$tabletitles,
                                  introtext = list(introtext1,
                                                   "Alternative Scenario Validation",
                                                   paste("Alternative Scenario is", Validation_List$scenario)))
    
  }
  
  # Save the spreadsheet
  current_time <- gsub("-", "_", gsub(":", "_",gsub(" ", "_", Sys.time())))
  
  if(SCENARIO_2 != SCENARIO_1 & SCENARIO_1 != "Validation") {
    
    spreadsheetpath <- file.path(SCENARIO_2_OUTPUT_PATH, 
                                 paste0("Comparison_", SCENARIO_1, "_", SCENARIO_2, "_", current_time, ".xlsx"))
    
  } else if(SCENARIO_2 != SCENARIO_1 & SCENARIO_1 == "Validation") {
    
    spreadsheetpath <- file.path(SCENARIO_2_OUTPUT_PATH, 
                                 paste0("Validation_", SCENARIO_2, "_", current_time, ".xlsx"))
  
  } else {    
    
    spreadsheetpath <- file.path(SCENARIO_1_OUTPUT_PATH, 
                                 paste0("Summary_", SCENARIO_1, "_", current_time, ".xlsx"))
  }
  
  saveWorkbook(wb, file = spreadsheetpath, overwrite = TRUE)
  
}

