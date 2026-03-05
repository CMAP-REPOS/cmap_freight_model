
# This function loads all necessary inputs into envir, after any needed
# transformations
db_build_process_inputs <- function(envir){
  
  ### Load project input files
  project.files <- list(db_build_graphics_functions    = file.path(SYSTEM_SCRIPTS_PATH, "db_build_graphics_functions.R"),
                        db_build_render                = file.path(SYSTEM_SCRIPTS_PATH, "db_build_render.R"),
                        db_build_spreadsheet           = file.path(SYSTEM_SCRIPTS_PATH, "db_build_spreadsheet.R"),
                        db_build_spreadsheet_functions = file.path(SYSTEM_SCRIPTS_PATH, "db_build_spreadsheet_functions.R"),
                        c_n2_empcats                   = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),
                        c_n6_n6io_sctg                 = file.path(SYSTEM_DATA_PATH, "corresp_naics6_n6io_sctg.csv"),
                        c_sctg                         = file.path(SYSTEM_DATA_PATH, "corresp_sctg_category.csv"),
                        TAZ_System                     = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Load inputs/outputs from earlier steps
  # Preparations for all loadings
  if(any(SCENARIO_DB_FIRMSYN, SCENARIO_DB_FTTM, SCENARIO_DB_TT)) {
    TAZ_System <- envir[["TAZ_System"]]
    if(!(BASE_DASHBOARD_GEOGRAPHY %in% colnames(TAZ_System))){
      stop("BASE_DASHBOARD_GEOGRAPHY must be a column name from the TAZ_System dataset!")
    }
    
    # Convert the DistrictName into a factor
    district_labels <- unique(TAZ_System[, .(DistrictNum, DistrictName)])[order(DistrictNum)]
    district_labels_vec <- district_labels$DistrictName
    TAZ_System[, DistrictName := factor(DistrictName, levels = district_labels_vec)]
    
    # Adjust the order of the counties
    county_order <- unique(TAZ_System[,.(county_state, CountyFIPS, cmap)])[order(-cmap, CountyFIPS)]
    county_order_vec <- unique(county_order$county_state)
    TAZ_System[, CountyName:=factor(county_state, levels = county_order_vec)]
    envir[["TAZ_System"]] <- TAZ_System
    
    # create a simplified TAZ table with unique TAZs (i.e., no county deail outside CMAP)
    TAZ_System_Simple <- unique(TAZ_System[,.(TAZ, Mesozone, FAFZONE, FAFNAME, CBPZONE, 
                                              Country, DistrictName, REGION, SUBREGION,
                                              TAZ_TYPE)])
    envir[["TAZ_System_Simple"]] <- TAZ_System_Simple
    
    #Transform the employment category mapping
    envir[["c_n2_empcats"]] <- unique(envir[["c_n2_empcats"]][,.(EmpCatName, EmpCatDesc,
                                                                 EmpCatLabel, EmpCatGroupedName)])
    setorder(envir[["c_n2_empcats"]],EmpCatName)
    
    # Load the skims for use in the processing steps below
    if(file.exists(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))){
      skims <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "skims_tod.rds"))
      # Put skims in extra-long format
      tod.ranges <- attr(skims, "tod.ranges")
      skims.long <- meltSkimTableByTOD(skims, 
                                       tod_names = names(tod.ranges))
    }
    
    # SCTG/SCTG Groupings
    # convert the short descriptions of SCTG and the SCTG Group Labels to factors
    # ordered based on the numerical order of the SCTG categories
    envir[["c_sctg"]][, Commodity_SCTG_desc_short := factor(Commodity_SCTG_desc_short, 
                                                 levels = envir$c_sctg$Commodity_SCTG_desc_short)]
    envir[["c_sctg"]][, Commodity_SCTG_Group_label := factor(Commodity_SCTG_Group_label, 
                                            levels = unique(envir$c_sctg[,.(Commodity_SCTG_Group, Commodity_SCTG_Group_label)])$Commodity_SCTG_Group_label)]
    
    # Model time periods and labels
    tod_labels <- data.table(TOD = c("EM", "AM","MD", "PM", "NT"),
                             Hours = c("12am-6am","6am-10am", "10am-3pm",
                                       "3pm-7pm", "7pm-12am"))
    envir[["tod_labels"]] <- tod_labels
    
  }
  
  if(SCENARIO_DB_FIRMSYN){
    
    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
    
    # Process domestic firms list for establishment and firm summaries and maps
    ScenarioFirms <- firm_sim_results$FirmsDomestic
    
    # Add dashboard geography
    ScenarioFirms <- merge(ScenarioFirms, TAZ_System_Simple[,.(TAZ, Region = get(BASE_DASHBOARD_GEOGRAPHY), TAZ_TYPE)],
                           by.x = "TAZ", by.y = "TAZ")

    ScenarioFirms[TAZ_TYPE == "MODELREGION", TAZ_TYPE := ifelse(TAZ %in% BASE_TAZ_CMAP, "CMAP MPO Area", "Non-CMAP Part of Model Region")]
    ScenarioFirms[TAZ_TYPE == "NATIONAL", TAZ_TYPE := "Rest of USA"]

    # Convert size to labels
    ScenarioFirms[, esizecat := factor(firm_inputs$EstSizeCategories$Label[esizecat],
                                       levels = firm_inputs$EstSizeCategories$Label)]

    setnames(ScenarioFirms, "Emp", "Employees")
    envir[["ScenarioFirms"]] <- ScenarioFirms

    # SE data for validation of firm synthesis
    envir[["TAZLandUseCVTM"]] <- firm_inputs$TAZLandUseCVTM
    
    # List of summaries creates in firm synthesis
    envir[["firm_sim_summary"]] <- firm_sim_results$firm_sim_summary
    
    # Producers table 
    envir[["producers"]] <- firm_sim_results$producers
    # (consumers table summary is in firm_sim_summary - use that
    # (1) avoids working with the very large consumers table
    # (2) includes the non-simulated consumption to allow for calculating accurate totals
    
    rm(firm_sim_results, firm_inputs, ScenarioFirms)
    gc()
    
  }
  
  if(SCENARIO_DB_SCM){
    
    # load the SC model workspace:
    ### Consider creating a simpler output from the supply chanin model
    ### just containing what is needed for the dashboard as the 
    ### full shipments table takes so long to load andn is large in memory
    ### potential problem with two scenarios
    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_SCM_OUTPUTNAME))
    
    # Shipments table
    shipments <- sc_sim_results$BuyerSupplierPairs
    
    # add sctg labeling/groups
    shipments[envir$c_sctg, 
              c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
              on = "Commodity_SCTG"]
    
    # add trade type
    shipments[, c("Production_Location", "Consumption_Location") := .("Domestic", "Domestic")]
    shipments[Production_zone %in% BASE_MZ_INTERNATIONAL, Production_Location := "Foreign"]
    shipments[Consumption_zone %in% BASE_MZ_INTERNATIONAL, Consumption_Location := "Foreign"]
    shipments[, Trade_Type := "Domestic"]
    shipments[Production_Location == "Foreign", Trade_Type := "Import"]
    shipments[Consumption_Location == "Foreign", Trade_Type := "Export"]
    # Grouped trade type for comparison with targets
    # shipments[, TRADE_TYPE := ifelse(Trade_Type == "Domestic", "DOMESTIC", "EXPORT")]
    
    # add census regions
    census_region <- unique(envir$TAZ_System[,.(Mesozone, STATE = state, REGION)])
    shipments[census_region[, .(Production_zone = Mesozone, STATE, REGION)],
              c("OSTATE", "OREGION") := .(i.STATE, i.REGION),
              on = "Production_zone"]
    
    shipments[census_region[, .(Consumption_zone = Mesozone, STATE, REGION)],
              c("DSTATE", "DREGION") := .(i.STATE, i.REGION),
              on = "Consumption_zone"]
    
    # # Label rail as railroad in the mode list to match with the targets
    # shipments[ModeDomestic == "Rail", ModeDomestic := "Railroad"]
    
    # summary single values and tabulations for dashboard value boxes, tables, chartss
    shipments_summary <- list()
    shipments_summary$trades <- shipments[,.N]
    shipments_summary$producers <- shipments[,uniqueN(SellerID)]
    shipments_summary$consumers <- shipments[,uniqueN(BuyerID)]
    
    shipments_summary$tons_sim <- as.numeric(shipments[,sum(Last.Iteration.Quantity)])
    shipments_summary$tonmilesmllion_sim <- as.numeric(shipments[,sum(Last.Iteration.Quantity * Distance)/1e6])
    
    shipments_summary$trades_tons_type <- shipments[,.(Trades = .N, 
                                                       Tons = sum(Last.Iteration.Quantity),
                                                       TonMilesMillion = sum(Last.Iteration.Quantity * Distance)/1e6),
                                                    keyby = PMG_Incr]
    
    shipments_summary$prod_cons_tons_type <- shipments[,.(Producers = uniqueN(SellerID), 
                                                       Consumers = uniqueN(BuyerID)),
                                                    keyby = PMG_Incr]
    
    shipments_summary$trades_tons_trade_type <- shipments[,.(Trades = .N, 
                                                       Tons = sum(Last.Iteration.Quantity),
                                                       TonMilesMillion = sum(Last.Iteration.Quantity * Distance)/1e6),
                                                    keyby = Trade_Type]
    
    shipments_summary$trades_tons_mode <- shipments[,.(Trades = .N, 
                                                             Tons = sum(Last.Iteration.Quantity),
                                                             TonMilesMillion = sum(Last.Iteration.Quantity * Distance)/1e6),
                                                          keyby = Mode.Domestic]
    
    shipments_summary$trades_tons_sctg_group <- shipments[,.(Trades = .N, 
                                                             Tons = sum(Last.Iteration.Quantity),
                                                             TonMilesMillion = sum(Last.Iteration.Quantity * Distance)/1e6),
                                                          keyby = Commodity_SCTG_Group_label]
    
    shipments_summary$trades_tons_sctg <- shipments[,.(Trades = .N, 
                                                             Tons = sum(Last.Iteration.Quantity),
                                                             TonMilesMillion = sum(Last.Iteration.Quantity * Distance)/1e6),
                                                          keyby = Commodity_SCTG]
    shipments_summary$trades_tons_sctg[envir$c_sctg, 
              Commodity_SCTG_label := i.Commodity_SCTG_desc_short, 
              on = "Commodity_SCTG"]
    
    shipments_summary$prod_cons_tons_sctg_group <- shipments[,.(Producers = uniqueN(SellerID), 
                                                          Consumers = uniqueN(BuyerID)),
                                                       keyby = Commodity_SCTG_Group_label]
    
    shipments_summary$prod_cons_sctg <- shipments[,.(Producers = uniqueN(SellerID), 
                                                     Consumers = uniqueN(BuyerID)),
                                                  keyby = Commodity_SCTG]
    
    shipments_summary$prod_cons_sctg[envir$c_sctg, 
                                       Commodity_SCTG_label := i.Commodity_SCTG_desc_short, 
                                       on = "Commodity_SCTG"]
    
    shipments_summary$trades_buyer_seller <- rbind(shipments[, .(Trades = .N, Type = "Seller"), by = .(id = SellerID)],
                 shipments[, .(Trades = .N, Type = "Buyer"), by = .(id = BuyerID)])
    
    # read in the final naics_set file that has market level summary results
    naics_set <- fread(file = file.path(SCENARIO_OUTPUT_PATH,"naics_set_after_supply_chain.csv"))
    # naics_set_markets <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_markets.csv"))
    # naics_set_buysell <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_buysell.csv"))
    # naics_set_sampling <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_sampling.csv"))
    # naics_set_distchannel <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_distchannel.csv"))
    # naics_set_modechoice <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_modechoice.csv"))
    # naics_set_costs <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_costs.csv"))
    # naics_set_pmg <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_pmg.csv"))
    # naics_set_shipments <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_shipments.csv"))
    
    # read in the group summary csv files from each step
    naics_markets <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_markets.csv"))
    naics_buysell <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_buysell.csv"))
    naics_sampling <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_sampling.csv"))
    naics_distchannel <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_distchannel.csv"))
    naics_modechoice <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_modechoice.csv"))
    naics_costs <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_costs.csv"))
    naics_pmg <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_pmg.csv"))
    naics_shipments <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_shipments.csv"))
    naics_ports <- fread(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_ports.csv"))
    
    # add group labels for charts
    naics_set[envir$c_sctg, 
              c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
              on = "Commodity_SCTG"]
    
    naics_markets[envir$c_sctg, 
                  c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                  on = "Commodity_SCTG"]
    
    naics_buysell[envir$c_sctg, 
                  c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                  on = "Commodity_SCTG"]
    
    naics_sampling[envir$c_sctg, 
                   c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                   on = "Commodity_SCTG"]
    
    naics_distchannel[envir$c_sctg, 
                      c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                      on = "Commodity_SCTG"]
    
    naics_modechoice[envir$c_sctg, 
                     c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                     on = "Commodity_SCTG"]
    
    naics_costs[envir$c_sctg, 
                c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                on = "Commodity_SCTG"]
    
    naics_pmg[envir$c_sctg, 
              c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
              on = "Commodity_SCTG"]
    
    naics_shipments[envir$c_sctg, 
                    c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                    on = "Commodity_SCTG"]
    
    naics_ports[envir$c_sctg, 
                    c("Commodity_SCTG_Group_range", "Commodity_SCTG_Group_label") := .(i.Commodity_SCTG_Group_range, i.Commodity_SCTG_Group_label), 
                    on = "Commodity_SCTG"]
    
    envir[["naics_set"]] <- naics_set
    envir[["naics_markets"]] <- naics_markets
    envir[["naics_buysell"]] <- naics_buysell
    envir[["naics_sampling"]] <- naics_sampling
    envir[["naics_distchannel"]] <- naics_distchannel
    envir[["naics_modechoice"]] <- naics_modechoice
    envir[["naics_costs"]] <- naics_costs
    envir[["naics_pmg"]] <- naics_pmg
    envir[["naics_shipments"]] <- naics_shipments
    envir[["naics_ports"]] <- naics_ports
    
    # Distribution Channel
    # Calibration summary categories
    famesctg <- c(rep("A",3),rep("E",4),rep("K",2),rep("F",3),
                  rep("C",7),rep("B",5),rep("J",6),"C",
                  rep("G",3),"D",rep("I",2),"G","J",rep("H",4))
    famelabels <- c(rep("Ag. Prod.",3),rep("Food Prod.",4),rep("Alcohol, Tob.",2),rep("Stone, Sand",3),
                    rep("Min, Coal, Oil",7),rep("Chemicals",5),rep("Wood, Textiles",6),"Min, Coal, Oil",
                    rep("Metal Prod., Mach.",3),"Electronics",rep("Vehicles",2),"Metal Prod.","Wood, Textiles",rep("Misc., Mixed",4))
    
    naics_distchannel[, Commodity_Category := famesctg[Commodity_SCTG]]
    naics_distchannel[, Commodity_Label := famelabels[Commodity_SCTG]]
    naics_distchannel[, Commodity_Cat_Label := paste0(Commodity_Category, "-", Commodity_Label)]
    
    distchannel_totals_by_fame <-
      naics_distchannel[,
                        .(DistChannel1 = sum(DistChannel1, na.rm = TRUE),
                          DistChannel2 = sum(DistChannel2, na.rm = TRUE),
                          DistChannel3 = sum(DistChannel3, na.rm = TRUE),
                          DistChannel4 = sum(DistChannel4, na.rm = TRUE)),
                        by = .(Commodity_Category)]
    
    distchannel_totals_by_fame[, `0` := DistChannel1 / (DistChannel1 + DistChannel2 + DistChannel3 + DistChannel4)]
    distchannel_totals_by_fame[, `1` := DistChannel2 / (DistChannel1 + DistChannel2 + DistChannel3 + DistChannel4)]
    distchannel_totals_by_fame[, `2+` := (DistChannel3 + DistChannel4) / (DistChannel1 + DistChannel2 + DistChannel3 + DistChannel4)]
    
    distchannel_shares <-  melt.data.table(
      distchannel_totals_by_fame,
      id.vars = c('Commodity_Category'),
      measure.vars = c('0', '1', '2+'),
      variable.name = 'NumberofStops_Choice',
      value.name = 'predicted_share')
    
    distchannel_shares <-  sc_inputs$distchannel_calibration[distchannel_shares,
                                                   on = c('Commodity_Category', 'NumberofStops_Choice')]
    
    distchannel_shares[, Commodity_Label := famelabels[match(Commodity_Category, famesctg)]]
    distchannel_shares[, Commodity_Cat_Label := paste0(Commodity_Category, "-", Commodity_Label)]
    distchannel_shares <- distchannel_shares[,.(Commodity = Commodity_Cat_Label, DistChannel = NumberofStops_Choice,
                                                Target = Target_Share, Model = predicted_share)]
    
    envir[["distchannel_shares"]] <- distchannel_shares
    
    # Mode and Shipment size choice
    # Summary results
    
    ### TODO currently this table is too detailed and too large to work with
    ### Need to simplify somewhat to reduce file size
    # market_summary_mc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, "market_summary_modechoice.fst"),
    #                              as.data.table = TRUE)
    gc()
    
    # Mode, Shipment Size, and joint summary by SCTG, SCTG group, vs. targets
    ### Derive these for now off the shipments tables and the 
    ### share inputs table
    
    # modeshare_calibration <-
    #   mode_ship_targets_choices[TRADE_TYPE == "Domestic",
    #                             .(NAICS2_SCTG = paste0(NAICS2, '_', SCTG_CODE), TARGET,
    #                               mode_ship_name = CHDESC, Mode.Domestic = MODE_GROUP, ShipmentSize = SHIPSIZE_GROUP_MOD,
    #                               NAICS2, SCTG_CODE, SCTG_GROUP, SCTG_GROUP_LABEL)][NAICS2_SCTG %in% unique(market_summary_mc$NAICS2_SCTG)]
    # model vs target
    # modeshare_model <- market_summary_mc[,.(MODEL_PAIRS = sum(N)),
    #                                      keyby = .(mode_ship_name, Mode.Domestic, ShipmentSize, NAICS2_SCTG, NAICS2, SCTG)]
    # modeshare_model[, MODEL := MODEL_PAIRS/sum(MODEL_PAIRS), by = NAICS2_SCTG ]
    # 
    # modeshare_model[modeshare_calibration, TARGET := i.TARGET, on = .(mode_ship_name, NAICS2_SCTG)]
    # 
    # modeshare_model_m <- melt.data.table(modeshare_model[,.(mode_ship_name, NAICS2_SCTG, MODEL, TARGET)],
    #                                      id.vars = c("mode_ship_name", "NAICS2_SCTG"))
    # 
    modeshare_model <- shipments[,.(ShipmentTrades = sum(.N), Weight = sum(Last.Iteration.Quantity)),
                                         keyby = .(Mode.Domestic, Ship_size, Market, NAICS, Commodity_SCTG)]
    modeshare_model[, ModeShareMarket := Weight/sum(Weight), by = Market]
    
    # envir[["market_summary_mc"]] <- market_summary_mc
    # envir[["modeshare_model_m"]] <- modeshare_model_m
    envir[["modeshare_model"]] <- modeshare_model
    
    # ### Create overall and by SCTG group mode choice summaries for comparisons
    # mode_target <- sc_inputs$
    # mode_target[, SCTG := as.integer(SCTG)]
    # mode_target[sctg_group,
    #             c("SCTG_Group_Range", "SCTG_Group_Label") := .(i.SCTG_Group_Range, i.SCTG_Group_Label),
    #             on = "SCTG"]
    # 
    
    # Distance
    distance_bands <- c("<249 miles", "250-499 miles", "500-999 miles", "1000-1999 miles", "2000+ miles")
    shipments[, DIST_BAND := factor(distance_bands[findInterval(Distance, c(0,250,500,1000,2000))],levels = distance_bands)]
    
    # mode_target_all <- mode_target[,.(TargetTons = sum(TONS)), by = .(ModeDomestic = MODE_GROUP, TRADE_TYPE)][, Target := TargetTons/sum(TargetTons), by = TRADE_TYPE]
    # mode_target_sctg <- mode_target[,.(TargetTons = sum(TONS)), by = .(ModeDomestic = MODE_GROUP, SCTG_Group_Label, TRADE_TYPE)][, Target := TargetTons/sum(TargetTons), by = .(SCTG_Group_Label, TRADE_TYPE)]
    
    # mode_target_all <- merge(mode_target_all,
    #                          shipments[!is.na(ModeDomestic),
    #                                    .(ModelTons = sum(AnnualTons)),
    #                                    by = .(ModeDomestic, TRADE_TYPE)][, Model := ModelTons/sum(ModelTons), by = TRADE_TYPE],
    #                          by = c("ModeDomestic", "TRADE_TYPE"),
    #                          all = TRUE)
    
    mode_target_all <- shipments[!is.na(Mode.Domestic),
                                 .(ModelTons = sum(Last.Iteration.Quantity)),
                                       by = .(Mode.Domestic, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = Trade_Type]
                            
    mode_target_all[is.na(mode_target_all)] <- 0
    
    # mode_target_sctg <- merge(mode_target_sctg,
    #                           shipments[!is.na(ModeDomestic),
    #                                     .(ModelTons = sum(AnnualTons)),
    #                                     by = .(ModeDomestic, SCTG_Group_Label, TRADE_TYPE)][, Model := ModelTons/sum(ModelTons), by = .(SCTG_Group_Label, TRADE_TYPE)],
    #                           by = c("ModeDomestic", "SCTG_Group_Label", "TRADE_TYPE"),
    #                           all = TRUE)
    
    mode_target_sctg <-       shipments[!is.na(Mode.Domestic),
                                        .(ModelTons = sum(Last.Iteration.Quantity)),
                                        by = .(Mode.Domestic, Commodity_SCTG_Group_label, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = .(Commodity_SCTG_Group_label, Trade_Type)]
    
    mode_target_sctg[is.na(mode_target_sctg)] <- 0
    
    # shipment size summaries
    # shipsize_target <- mode_ship_targets_list$ship_sctg[TRADE_TYPE == "DOMESTIC"]
    # shipsize_target[, SCTG := as.integer(SCTG)]
    # shipsize_target[sctg_group,
    #                 c("SCTG_Group_Range", "SCTG_Group_Label") := .(i.SCTG_Group_Range, i.SCTG_Group_Label),
    #                 on = "SCTG"]
    # shipsize_target_all <- shipsize_target[,.(TargetTons = sum(TONS)), by = .(shipsize = SHIPSIZE_GROUP)][, Target := TargetTons/sum(TargetTons)]
    # shipsize_target_sctg <- shipsize_target[,.(TargetTons = sum(TONS)), by = .(shipsize = SHIPSIZE_GROUP, SCTG_Group_Label)][, Target := TargetTons/sum(TargetTons), by = SCTG_Group_Label]
    # 
    SHIPSIZE_GROUP <- sc_inputs$shipsize[SCTG == 0]$ShipmentCategory
    shipments[, ship_size_labels := factor(SHIPSIZE_GROUP[Ship_size], levels = SHIPSIZE_GROUP)]
    
    # shipsize_target_all <- merge(shipsize_target_all,
    #                              shipments[!is.na(shipsize) & TRADE_TYPE == "DOMESTIC",
    #                                        .(ModelTons = sum(AnnualTons)),
    #                                        by = shipsize][, Model := ModelTons/sum(ModelTons)],
    #                              by = "shipsize",
    #                              all = TRUE)
    shipsize_target_all <-     shipments[!is.na(ship_size_labels),
                                           .(ModelTons = sum(Last.Iteration.Quantity)),
                                           by = ship_size_labels][, Model := ModelTons/sum(ModelTons)]
    shipsize_target_all[is.na(shipsize_target_all)] <- 0
    
    # shipsize_target_sctg <- merge(shipsize_target_sctg,
    #                               shipments[!is.na(shipsize) & TRADE_TYPE == "DOMESTIC",
    #                                         .(ModelTons = sum(AnnualTons)),
    #                                         by = .(shipsize, SCTG_Group_Label)][, Model := ModelTons/sum(ModelTons), by =  SCTG_Group_Label],
    #                               by = c("shipsize", "SCTG_Group_Label"),
    #                               all = TRUE)
    shipsize_target_sctg <-       shipments[!is.na(ship_size_labels),
                                            .(ModelTons = sum(Last.Iteration.Quantity)),
                                            by = .(ship_size_labels, Commodity_SCTG_Group_label)][, Model := ModelTons/sum(ModelTons), by =  Commodity_SCTG_Group_label]
    shipsize_target_sctg[is.na(shipsize_target_sctg)] <- 0
    
    # summaries by distance band
    # dist_target_all <- mode_target[,.(TargetTons = sum(TONS)), by = .(DIST_BAND, TRADE_TYPE)][, Target := TargetTons/sum(TargetTons), by = TRADE_TYPE]
    # mode_target_dist <- mode_target[,.(TargetTons = sum(TONS)), by = .(ModeDomestic = MODE_GROUP, DIST_BAND, TRADE_TYPE)][, Target := TargetTons/sum(TargetTons), by = .(DIST_BAND, TRADE_TYPE)]
    # dist_target_mode <- mode_target[,.(TargetTons = sum(TONS)), by = .(ModeDomestic = MODE_GROUP, DIST_BAND, TRADE_TYPE)][, Target := TargetTons/sum(TargetTons), by = .(ModeDomestic, TRADE_TYPE)]
    # dist_target_sctg <- mode_target[,.(TargetTons = sum(TONS)), by = .(SCTG, DIST_BAND, TRADE_TYPE)][, Target := TargetTons/sum(TargetTons), by = .(SCTG, TRADE_TYPE)]
    
    # dist_target_all <- merge(dist_target_all,
    #                          shipments[!is.na(DIST_BAND),
    #                                    .(ModelTons = sum(AnnualTons)),
    #                                    by = .(DIST_BAND, TRADE_TYPE)][, Model := ModelTons/sum(ModelTons), by = TRADE_TYPE],
    #                          by = c("DIST_BAND", "TRADE_TYPE"),
    #                          all = TRUE)
    dist_target_all <-       shipments[!is.na(DIST_BAND),
                                       .(ModelTons = sum(Last.Iteration.Quantity)),
                                       by = .(DIST_BAND, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = Trade_Type]
    dist_target_all[is.na(dist_target_all)] <- 0
    
    # mode_target_dist <- merge(mode_target_dist,
    #                           shipments[!is.na(DIST_BAND) & !is.na(ModeDomestic),
    #                                     .(ModelTons = sum(AnnualTons)),
    #                                     by = .(DIST_BAND, ModeDomestic, TRADE_TYPE)][, Model := ModelTons/sum(ModelTons), by = .(DIST_BAND, TRADE_TYPE)],
    #                           by = c("DIST_BAND", "ModeDomestic", "TRADE_TYPE"),
    #                           all = TRUE)
    mode_target_dist <-     shipments[!is.na(DIST_BAND) & !is.na(Mode.Domestic),
                                        .(ModelTons = sum(Last.Iteration.Quantity)),
                                        by = .(DIST_BAND, Mode.Domestic, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = .(DIST_BAND, Trade_Type)]
    mode_target_dist[is.na(mode_target_dist)] <- 0
    
    dist_target_mode <-     shipments[!is.na(DIST_BAND) & !is.na(Mode.Domestic),
                                        .(ModelTons = sum(Last.Iteration.Quantity)),
                                        by = .(DIST_BAND, Mode.Domestic, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = .(Mode.Domestic, Trade_Type)]
    dist_target_mode[is.na(dist_target_mode)] <- 0
    
    # Distance distribution for all SCTGs
    dist_target_sctg <-       shipments[!is.na(DIST_BAND) & !is.na(Mode.Domestic),
                                        .(ModelTons = sum(Last.Iteration.Quantity)),
                                        by = .(DIST_BAND, Commodity_SCTG, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = .(Commodity_SCTG, Trade_Type)]
    dist_target_sctg[is.na(dist_target_sctg)] <- 0
    dist_target_sctg[envir$c_sctg, Commodity_SCTG_label := i.Commodity_SCTG_desc_short, on = "Commodity_SCTG"]
    dist_target_sctg[, Commodity_SCTG_label := paste(Commodity_SCTG, Commodity_SCTG_label)]
    
    dist_target_sctg_group <-       shipments[!is.na(DIST_BAND) & !is.na(Mode.Domestic),
                                        .(ModelTons = sum(Last.Iteration.Quantity)),
                                        by = .(DIST_BAND, Commodity_SCTG_Group_label, Trade_Type)][, Model := ModelTons/sum(ModelTons), by = .(Commodity_SCTG_Group_label, Trade_Type)]
    dist_target_sctg_group[is.na(dist_target_sctg_group)] <- 0
    
    # Tabulation of tons by commodity and trade type
    
    ### TODO add tables to differentiate by trade type (imports/domestic/exports)
    
    # add shipments and summaries to environment
    envir[["shipments_summary"]] <- shipments_summary
    #envir[["shipments"]] <- shipments  #### do we need the full shipments file? Big memory overhead to keep it loaded
    envir[["mode_target_all"]] <- mode_target_all
    envir[["mode_target_sctg"]] <- mode_target_sctg
    envir[["shipsize_target_all"]] <- shipsize_target_all
    envir[["shipsize_target_sctg"]] <- shipsize_target_sctg
    envir[["dist_target_all"]] <- dist_target_all
    envir[["mode_target_dist"]] <- mode_target_dist
    envir[["dist_target_mode"]] <- dist_target_mode
    envir[["dist_target_sctg"]] <- dist_target_sctg
    envir[["dist_target_sctg_group"]] <- dist_target_sctg_group
    
    # Remove any very large objects that we are done with for memory management
    rm(shipments)
    gc()
  
  } 
  
  if(SCENARIO_DB_FTTM){
    
    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FTTM_OUTPUTNAME))
    
    # Do some processing so that fieldnames and variables added are 
    # consistent with the CVTM where possible
    # Use same set of summaries as in the CV dashboard 
    # (with some additions re commodities)
    
    taz_skims <- fread(file.path(SCENARIO_INPUT_PATH, "cmap_data_zone_skims.csv"))
    setnames(taz_skims, c("o", "d", "Peak", "OffPeak", "dist"))
    
    # Add distance for od trip (as opposed to shipment "Distance")
    ft_trips[taz_skims,
              dist := i.dist,
              on = c("o", "d")]
    
    # Add dashboard geography
    ft_trips <- merge(ft_trips, TAZ_System[TAZ %in% BASE_TAZ_INTERNAL,.(TAZ, Region.Origin = get(BASE_DASHBOARD_GEOGRAPHY))],
                      by.x = "o", by.y = "TAZ")
    ft_trips <- merge(ft_trips, TAZ_System[TAZ %in% BASE_TAZ_INTERNAL,.(TAZ, Region.Destination = get(BASE_DASHBOARD_GEOGRAPHY))],
                      by.x = "d", by.y = "TAZ")
    # External stations 
    EXTERNAL_STATIONS <- 3633:3648 # HARDCODED -- SHOULD BE IN BASE_VARIABLES
    ft_trips[o %in% EXTERNAL_STATIONS, Region.Origin := "External"]
    ft_trips[d %in% EXTERNAL_STATIONS, Region.Destination := "External"]
    
    ft_trips <- ft_trips[ft_trips[Trip_ID == 1, .(Tour_ID2, TAZ.Start = o, Region.Start = Region.Origin)],
                     c("TAZ.Start", "Region.Start") := .(i.TAZ.Start, i.Region.Start), on = "Tour_ID2"]

    # add tour start to stop destination distance
    ft_trips[taz_skims[,.(TAZ.Start = o, d, dist)],
              Distance.Start := i.dist,
              on = c("TAZ.Start", "d")]

    # add od segment
    ft_trips[, Movement.Type := add_od_segment(origins = o,
                                               destinations = d,
                                               external_taz = EXTERNAL_STATIONS)]

    # text vehicle type
    ft_trips[,Vehicle:=c("2-axle","3,4 axle","semi/trailer")[Veh_Type]]
    
    envir[["ft_trips"]] <- ft_trips

    # calculate the average cluster distance
    # for multistop tours: distance between all stop combinations excluding the tour start/end
    # In this case include intermediate stops added later to match with the validation data
    # in which scheduled and intermediate stops cannot be distinguished
    
    # code an activity field with levels Pickup, Dropoff, Return 
    ft_trips[is.na(DO), Activity := "Return"]
    ft_trips[DO == 0, Activity := "Pickup"]
    ft_trips[DO == 1, Activity := "Dropoff"]
    
    ft_trips_cluster <- ft_trips[Activity != "Return",.(Tour_ID2, Trip_ID, Region.Start, o, d)]
    ft_trips_cluster <- ft_trips_cluster[, NumStops := .N, by = Tour_ID2][NumStops > 1]
    # Stops are all DTAZs ("d") on the tour
    clusterMeanDist <- function(stop.TAZs, dist.mat){
      idx <- as.character(stop.TAZs)
      dist.mat.subset <- dist.mat[idx, idx, drop = FALSE]
      diag(dist.mat.subset) <- NA
      return(mean(dist.mat.subset, na.rm = TRUE))
    }

    # Convert skims to a matrix
    dist.mat <- as.matrix(dcast.data.table(data = taz_skims[,.(o, d, distance = dist)],
                                           formula = o~d,
                                           value.var = "distance")[, -1])
    rownames(dist.mat) <- colnames(dist.mat)

    ft_trips_cluster[, MeanDist := clusterMeanDist(d, dist.mat), by = Tour_ID2]
    ft_trips_cluster <- ft_trips_cluster[Trip_ID == 1, .(Region.Start, Tour_ID2, NumStops, MeanDist)]
    envir[["ft_trips_cluster"]] <- ft_trips_cluster

    # add a debug summary to look for outliers in the stops by activity by stop to check for high values
    # in absolute terms and per HH/per Emp
    ft_trips_act <- ft_trips[, .(Trips = .N), keyby = .(TAZ = d, Activity)]
    ft_trips_taz <- dcast.data.table(ft_trips_act,
                                     TAZ ~ Activity,
                                     fun.aggregate = sum,
                                     value.var = "Trips")
    #setnames(ft_trips_taz, "Break/Meal","BreakMeal") # should be none unless code updated to add these
    #setnames(ft_trips_taz, "Vehicle Service","VehicleService") # should be none unless code updated to add these
    ft_trips_taz[, Scheduled := Pickup + Dropoff]
    #ft_trips_taz[, Intermediate := BreakMeal + VehicleService + Other]
    #ft_trips_taz[, Stops := Scheduled + Intermediate + Return]
    ft_trips_taz[, Stops := Scheduled + Return]
    ft_trips_taz <- merge(ft_trips_taz,
                          envir$TAZLandUseCVTM,
                          by = "TAZ",
                          all.x = TRUE)

    #ft_trips_taz[, PctIntermediate := Intermediate/Stops]
    ft_trips_taz[, StopPerHH := ifelse(HH > 0, Stops/HH, 0)]
    ft_trips_taz[, StopPerEmp := ifelse(NEmp_Total > 0, Stops/NEmp_Total, 0)]

    envir[["ft_trips_taz"]] <- ft_trips_taz
    
    # Remove any very large objects that we are done with for memory management
    rm(ft_trips)
    gc()
    
  } 
    
  # if(SCENARIO_DB_CVTM){
  #   load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))
  #   
  #   # Add dashboard geography
  #   cv_trips <- cv_sim_results$cv_trips
  #   cv_trips <- merge(cv_trips, TAZ_System[,.(TAZ, Region.Origin = get(BASE_DASHBOARD_GEOGRAPHY))],
  #                     by.x = "OTAZ", by.y = "TAZ")
  #   cv_trips <- merge(cv_trips, TAZ_System[,.(TAZ, Region.Destination = get(BASE_DASHBOARD_GEOGRAPHY))],
  #                     by.x = "DTAZ", by.y = "TAZ")
  #   cv_trips <- cv_trips[cv_trips[TripID == 1, .(TourID, TAZ.Start = OTAZ, Region.Start = Region.Origin)], 
  #                    c("TAZ.Start", "Region.Start") := .(i.TAZ.Start, i.Region.Start), on = "TourID"]
  #   
  #   # add tour start to stop destination distance
  #   cv_trips[skims.long[,.(TAZ.Start = OTAZ, DTAZ, TOD, dist)], 
  #             Distance.Start := i.dist, 
  #             on = c("TAZ.Start", "DTAZ", "TOD")]
  #   
  #   # add od segment
  #   cv_trips[, Movement.Type := add_od_segment(origins = OTAZ, 
  #                                              destinations = DTAZ, 
  #                                              external_taz = NULL)]
  #   
  #   envir[["cv_trips"]] <- cv_trips
  #   
  #   # calculate the average cluster distance 
  #   # for multistop tours: distance between all stop combinations excluding the tour start/end
  #   # In this case include intermediate stops added later to match with the validation data
  #   # in which scheduled and intermediate stops cannot be distinguished
  #   cv_trips_cluster <- cv_trips[Activity != "Return",.(TourID, TripID, Region.Start, OTAZ, DTAZ)]
  #   cv_trips_cluster <- cv_trips_cluster[, NumStops := .N, by = TourID][NumStops > 1]
  #   # Stops are all DTAZs on the tour
  #   clusterMeanDist <- function(stop.TAZs, dist.mat){
  #     idx <- as.character(stop.TAZs)
  #     dist.mat.subset <- dist.mat[idx, idx, drop = FALSE]
  #     diag(dist.mat.subset) <- NA
  #     return(mean(dist.mat.subset, na.rm = TRUE))
  #   }
  #   
  #   # Convert skims to a matrix
  #   dist.mat <- as.matrix(dcast.data.table(data = skims[,.(OTAZ, DTAZ, distance = dist.avg)], 
  #                                          formula = OTAZ~DTAZ, 
  #                                          value.var = "distance")[, -1])
  #   rownames(dist.mat) <- colnames(dist.mat)
  #   
  #   cv_trips_cluster[, MeanDist := clusterMeanDist(DTAZ, dist.mat), by = TourID]
  #   cv_trips_cluster <- cv_trips_cluster[TripID == 1, .(Region.Start, TourID, NumStops, MeanDist)]
  #   envir[["cv_trips_cluster"]] <- cv_trips_cluster
  #   
  #   # add a debug summary to look for outliers in the stops by activity by stop to check for high values
  #   # in absolute terms and per HH/per Emp
  #   cv_trips_act <- cv_trips[, .(Trips = .N), keyby = .(TAZ = DTAZ, Activity, Scheduled)]
  #   cv_trips_taz <- dcast.data.table(cv_trips_act,
  #                                    TAZ ~ Activity,
  #                                    fun.aggregate = sum,
  #                                    value.var = "Trips")
  #   setnames(cv_trips_taz, "Break/Meal","BreakMeal")
  #   setnames(cv_trips_taz, "Vehicle Service","VehicleService")
  #   cv_trips_taz[, Scheduled := Goods + Service]
  #   cv_trips_taz[, Intermediate := BreakMeal + VehicleService + Other]
  #   cv_trips_taz[, Stops := Scheduled + Intermediate + Return]
  #   cv_trips_taz <- merge(cv_trips_taz,
  #                         firm_inputs$TAZLandUseCVTM,
  #                         by = "TAZ",
  #                         all.x = TRUE)
  #   
  #   cv_trips_taz[, PctIntermediate := Intermediate/Stops]
  #   cv_trips_taz[, StopPerHH := ifelse(HH > 0, Stops/HH, 0)]
  #   cv_trips_taz[, StopPerEmp := ifelse(NEmp_Total > 0, Stops/NEmp_Total, 0)]
  #   
  #   envir[["cv_trips_taz"]] <- cv_trips_taz
  # }
  # 
  if(SCENARIO_DB_TT){

    load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_TT_OUTPUTNAME))

    # Create a trip gen summary table
    # extent is all model region TAZs (CMAP MPO and rest of model region)
    # include external stations too
    TAZ_INT_EXT <- c(BASE_TAZ_INTERNAL, EXTERNAL_STATIONS)
    
    tab_template <- data.table(ID = rep(TAZ_INT_EXT,6),
                               Vehicle = rep(c("2-axle", "3,4 axle", "semi/trailer"),
                                             each = length(TAZ_INT_EXT) * 2),
                               Direction = rep(c(rep("O",length(TAZ_INT_EXT)),
                                                 rep("D", length(TAZ_INT_EXT))),3))

    tab_template[, Vehicle := factor(Vehicle, levels = c("2-axle", "3,4 axle", "semi/trailer"))]

    truck_trip_gen <- rbind(tt_list$trip_table[ORI_taz %in% TAZ_INT_EXT,
                                              .(Trips = sum(Trips), Direction = "O"),
                                              keyby = .(ID = ORI_taz, Vehicle)],
                            tt_list$trip_table[DES_taz %in% TAZ_INT_EXT,
                                              .(Trips = sum(Trips), Direction = "D"),
                                              keyby = .(ID = DES_taz, Vehicle)])

    truck_trip_gen <- merge(tab_template,
                            truck_trip_gen,
                            by = c("ID", "Vehicle", "Direction"),
                            all = TRUE)

    truck_trip_gen[is.na(Trips), Trips := 0]
    truck_trip_gen[, Direction := factor(Direction, levels = c("O", "D"))]

    envir[["truck_trip_gen"]] <- truck_trip_gen

    # write a wide version of this table
    truck_trip_gen[, PA := factor(Direction, labels = c("p", "a"))]

    truck_trip_gen_wide <- dcast.data.table(truck_trip_gen,
                     ID ~ Vehicle + PA,
                     fun.aggregate = sum,
                     value.var = "Trips",
                     fill = 0)

    setnames(truck_trip_gen_wide,
             names(truck_trip_gen_wide),
             gsub("_", "Truck", names(truck_trip_gen_wide)))

    fwrite(truck_trip_gen_wide,
           file.path(SCENARIO_OUTPUT_PATH, "FT_Trip_Generation_Summary.csv"))

    # Create a trip and VMT by OD segment, geography, summary

    # Extract TripTable and add time, distance
    TripTable <- tt_list$trip_table[,.(Trips = sum(Trips)),
                                   by = .(o = ORI_taz, d = DES_taz, Vehicle, TimeOfDay)]
    setkey(TripTable, o, d, Vehicle, TimeOfDay)

    TripTable[taz_skims,
              c("dist", "time") := .(i.dist, i.OffPeak),
              on = c("o", "d")]

    # Calculate VMT and VHT
    TripTable[, c("VMT", "VHT") := .(Trips * dist, Trips * time)] # skims are in hours

    # Add grouping variabes (ODSegment, Summary Geography)
    TripTable[, ODSegment := add_od_segment(o, d, external_taz = EXTERNAL_STATIONS)]

    TripTable <- add_od_fields(TripTable, TAZ_System[TAZ %in% BASE_TAZ_INTERNAL],origins = "o", destinations = "d",
                               fieldsToAdd = c("county_state", "DistrictName"))
    TripTable[o %in% EXTERNAL_STATIONS, c("Ocounty_state", "ODistrictName") := "External"]
    TripTable[d %in% EXTERNAL_STATIONS, c("Dcounty_state", "DDistrictName") := "External"]

    TripTable[, OSummaryGeog := ODistrictName]
    TripTable[, DSummaryGeog := DDistrictName]
    
    TripTable[, Vehicle := factor(Vehicle, levels = c("2-axle", "3,4 axle", "semi/trailer"))]
    TripTable[, TOD := factor(TimeOfDay, levels = c("EM", "AM","MD", "PM", "NT"))]
    
    envir[["TripTable"]] <- TripTable

    # Trips, VMT, VHT by Vehicle, TOD, ODSegment
    tmh_vtods <- TripTable[,.(Trips = sum(Trips, na.rm = TRUE),
                              MeanDist = sum(VMT, na.rm = TRUE)/sum(Trips, na.rm = TRUE),
                              MeanTime = (sum(VHT, na.rm = TRUE) * 60)/sum(Trips, na.rm = TRUE), # in minutes
                              MeanSpeed = sum(VMT, na.rm = TRUE)/sum(VHT, na.rm = TRUE), # in miles/hour
                              VMT = sum(VMT, na.rm = TRUE),
                              VHT = sum(VHT, na.rm = TRUE)),
                           keyby = .(Vehicle, TOD = TimeOfDay, ODSegment)]

    tab_template <- data.table(expand.grid(Vehicle = c("2-axle", "3,4 axle", "semi/trailer"),
                                           TOD = c("EM", "AM","MD", "PM", "NT"),
                                           ODSegment = c("II", "IX", "XI", "XX")))

    tab_template[, Vehicle := factor(Vehicle, levels = c("2-axle", "3,4 axle", "semi/trailer"))]
    tab_template[, TOD := factor(TOD, levels = c("EM", "AM","MD", "PM", "NT"))]

    # add some labels for the time periods
    tab_template[tod_labels, Hours := i.Hours, on = "TOD"]

    tmh_vtods <- merge(tab_template,
                       tmh_vtods,
                       by = c("Vehicle", "TOD", "ODSegment"),
                       all = TRUE)

    tmh_vtods[is.na(Trips), c("Trips", "MeanDist", "MeanTime", "MeanSpeed", "VMT", "VHT") := 0]

    tmh_vtods[, Vehicle := factor(Vehicle, levels = c("2-axle", "3,4 axle", "semi/trailer"))]
    tmh_vtods[, TOD := factor(TOD, levels = c("EM", "AM","MD", "PM", "NT"))]
    
    envir[["tmh_vtods"]] <- tmh_vtods
    
    # Write a wide version of this table
    
    tmh_vtods_wide <- dcast.data.table(tmh_vtods,
                                      Vehicle + TOD  ~ ODSegment,
                                      fun.aggregate = sum,
                                      value.var = c("Trips", "VMT", "VHT", "MeanDist", "MeanTime", "MeanSpeed"),
                                      fill = 0)
    
    envir[["tmh_vtods_wide"]] <- tmh_vtods_wide

    fwrite(tmh_vtods_wide,
           file.path(SCENARIO_OUTPUT_PATH, "FT_Trip_VMT_VHT_Summary.csv"))

  }

  if(SCENARIO_DB_SPREADSHEET){

    # Add code here for processing inputs required specifically to build the summary spreadsheet
    # Currently no additional code required beyond that in other sections of this script

  }
   
  # if(SCENARIO_DB_CALIBRATION){
  # 
  #   # Import the calibration results
  #   cal_filepaths <- list.files(SYSTEM_CALIBRATION_PATH, full.names = TRUE)
  #   cal_results <- lapply(cal_filepaths,readRDS)
  #   names(cal_results) <- tools::file_path_sans_ext(list.files(SYSTEM_CALIBRATION_PATH))
  #   envir[["cal_results"]] <- cal_results
  # 
  # }

  if(SCENARIO_DB_REFERENCE){

    # Import the base year dashboard tabulations (or other run scenario)
    # to create comparisons between the current scenario and the reference year scenarios
    REFERENCE_OUTPUT <- file.path(SYSTEM_APP_PATH, "scenarios", SCENARIO_REFERENCE_NAME, "outputs", SYSTEM_DB_OUTPUTNAME)

    if(file.exists(REFERENCE_OUTPUT)){

      db_reference <- new.env()
      load(REFERENCE_OUTPUT, envir = db_reference)

      #extract some of the reference tables and remove the other inputs, functions, etc, that are in the env.
      ref_obj <- ls(db_reference$db_inputs, all.names=TRUE)
      ref_obj <- ref_obj[grep("db_tab_", ref_obj)]
      ref_obj <- c("ScenarioFirms", "TAZLandUseCVTM", "firm_sim_summary", ref_obj)
      for(n in ref_obj) assign(paste0("ref_", n), get(n, db_reference$db_inputs), envir)

    } else {

      SCENARIO_DB_REFERENCE <- FALSE

    }

  }

  ### Create files for mapping
  # Open geographic file of TAZs
  shp <- st_read(file.path(SYSTEM_DATA_PATH, "TAZ_System_Shape.shp"))

  # Add the grouping variable and order it
  shp$DistrictName <- TAZ_System$DistrictName[match(shp$zone17, TAZ_System$TAZ)]
  shp$DistrictName <- factor(shp$DistrictName, levels = district_labels_vec)
  envir[["shp"]] <- shp

  # Generate the bounding box for CMAP region
  envir[["CMAP_BBOX"]] <- st_bbox(shp)

  ### Generate basemaps
  prepTAZPolygons <- envir[["prepTAZPolygons"]]
  prepTAZList <- prepTAZPolygons(shp = shp, group.by = BASE_DASHBOARD_GEOGRAPHY)
  envir[["TAZ.polys"]] <- prepTAZList$shp
  if(exists("district_labels_vec")){
    envir[["TAZ.polys"]]$DistrictName <- factor(envir[["TAZ.polys"]]$DistrictName, levels=district_labels_vec)
    envir[["TAZ.polys"]]$Group <- factor(envir[["TAZ.polys"]]$Group, levels=district_labels_vec)
  }
  envir[["colorFun"]] <- prepTAZList$colorFun

  ### Initialize variables to store common values
  # Create time of day vectors with labels for both 30 and 60-minute divisions
  tod_breaks30 <- seq(from = 0, to = 1440, by = 30)
  tod_labels30 <- c("12:00am - 12:29am", "12:30am - 12:59am", "1:00am - 1:29am", 
                    "1:30am - 1:59am", "2:00am - 2:29am", "2:30am - 2:59am",
                    "3:00am - 3:29am", "3:30am - 3:59am", "4:00am - 4:29am",
                    "4:30am - 4:59am", "5:00am - 5:29am", "5:30am - 5:59am",
                    "6:00am - 6:29am", "6:30am - 6:59am", "7:00am - 7:29am",
                    "7:30am - 7:59am", "8:00am - 8:29am", "8:30am - 8:59am",
                    "9:00am - 9:29am", "9:30am - 9:59am", "10:00am - 10:29am",
                    "10:30am - 10:59am", "11:00am - 11:29am", "11:30am - 11:59am",
                    "12:00pm - 12:29pm", "12:30pm - 12:59pm", "1:00pm - 1:29pm",
                    "1:30pm - 1:59pm", "2:00pm - 2:29pm", "2:30pm - 2:59pm",
                    "3:00pm - 3:29pm", "3:30pm - 3:59pm", "4:00pm - 4:29pm",
                    "4:30pm - 4:59pm", "5:00pm - 5:29pm", "5:30pm - 5:59pm",
                    "6:00pm - 6:29pm", "6:30pm - 6:59pm", "7:00pm - 7:29pm",
                    "7:30pm - 7:59pm", "8:00pm - 8:29pm", "8:30pm - 8:59pm",
                    "9:00pm - 9:29pm", "9:30pm - 9:59pm", "10:00pm - 10:29pm",
                    "10:30pm - 10:59pm", "11:00pm - 11:29pm", "11:30pm - 11:59pm")
  
  tod_breaks60 <- seq(from = 0, to = 1440, by = 60)
  tod_labels60 <- c("12:00am - 12:59am", "1:00am - 1:59am", "2:00am - 2:59am",
                    "3:00am - 3:59am", "4:00am - 4:59am", "5:00am - 5:59am",
                    "6:00am - 6:59am", "7:00am - 7:59am", "8:00am - 8:59am",
                    "9:00am - 9:59am", "10:00am - 10:59am", "11:00am - 11:59am",
                    "12:00pm - 12:59pm", "1:00pm - 1:59pm", "2:00pm - 2:59pm",
                    "3:00pm - 3:59pm", "4:00pm - 4:59pm", "5:00pm - 5:59pm",
                    "6:00pm - 6:59pm", "7:00pm - 7:59pm", "8:00pm - 8:59pm",
                    "9:00pm - 9:59pm", "10:00pm - 10:59pm", "11:00pm - 11:59pm")
  
  envir[["tod_breaks30"]] <- tod_breaks30
  envir[["tod_labels30"]] <- tod_labels30
  envir[["tod_breaks60"]] <- tod_breaks60
  envir[["tod_labels60"]] <- tod_labels60
  
  envir[["theme_db"]] <- theme_db <- theme_bw() + theme(plot.margin = unit(c(10,10,20,10),"pt"))
  
  envir[["rsgcolordf"]] <- data.frame(red=c(246,0,99,186,117,255,82),
                                    green=c(139,111,175,18,190,194,77),
                                    blue=c(31,161,94,34,233,14,133),
                                    colornames=c("orange","marine","leaf","cherry","sky","sunshine","violet"))
  
  # Density value for dot density maps
  envir[["k"]] <- 25
  
  
}