
# This function loads all necessary inputs into envir, after any needed transformations
firm_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c( c_n2_empcats      = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),        # Correspondence between NAICS2 groups and aggregated employment groups
                      c_n4_sctg_whl     = file.path(SYSTEM_DATA_PATH, "corresp_naics4_sctg_wholesale.csv"), # Correspondence between NAICS4 groups and SCTG for wholesalers
                      c_n6_n6io_sctg    = file.path(SYSTEM_DATA_PATH, "corresp_naics6_n6io_sctg.csv"),      # Correspondence between NAICS 6-digit, I/O NAICS, and SCTG
                      cbp               = file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"),                  # CBP data file
                      cbp_ag            = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"),               # CBP data file -- Agriculture records generated separately
                      EstSizeCategories = file.path(SYSTEM_DATA_PATH, "data_est_size_categories.csv"),      # Establishment size categories and labels
                      io                = file.path(SYSTEM_DATA_PATH, "data_2022io.csv"),                   # Input output table
                      unitcost          = file.path(SYSTEM_DATA_PATH, "data_unitcost.csv"),                 # Unit costs 
                      prefweights       = file.path(SYSTEM_DATA_PATH, "data_firm_pref_weights.csv"),        # Preference weights
                      mzemp             = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"),     # Industry rankings data by mesozone based on employment
                      TAZ_System        = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"),                    # TAZ system 
                      firm_sim_commodities       = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_commodities.R"),
                      firm_sim_consumers         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_consumers.R"),
                      firm_sim_enumerate         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate.R"),
                      firm_sim_enumerate_foreign = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate_foreign.R"),
                      firm_sim_input_output      = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_input_output.R"),
                      firm_sim_process_inputs    = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"),
                      firm_sim_producers         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_producers.R"),
                      firm_sim_sample_groups     = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_sample_groups.R"),
                      firm_sim_scaling           = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_scaling.R"),
                      firm_sim_summary           = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_summary.R"),
                      firm_sim_write_groups      = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_write_groups.R"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Process project input files
  
  # Create a unique correspondence between EmpCat and EmpCatGrouped
  envir[["UEmpCats"]]  <- unique(envir[["c_n2_empcats"]][,.(EmpCatName, EmpCatDesc, EmpCatGroupedName)])
  
  # For the base scenario: process the input CBP data into the format required by the model
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    # Combine Domestic Non Ag Firm Records with the Ag Firm Records
    # Remove any ag records in cbp, i.e., NAICS 3 == 112,113 and add the cbp_ag records  
    cbp <- rbind(envir[["cbp_ag"]],
                 envir[["cbp"]][Industry_NAICS6_CBP >= 113000])
    
    # Remove records with missing zones and NAICS codes
    cbp <- cbp[!is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP)]
    
    # Add a field to identify firms as within the CMAP model region (1 in model region, 2 outside)
    cbp[envir$TAZ_System, modelregion := i.modelregion, on = "CBPZONE" ]
    
    # Aggregate by zones, NAICS, and firm size category (should already conform to this grouping)
    cbp <- cbp[,.(establishment = sum(establishment),
                  e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                  e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
               keyby = .(modelregion, CBPZONE, NAICS6 = Industry_NAICS6_CBP)]
    
    # Add 2 digit NAICS and the EmpCatName used in the model
    cbp[, NAICS2 := as.integer(floor(NAICS6/10000))]
    cbp[envir[["c_n2_empcats"]][,.(NAICS2, EmpCatName)], 
        EmpCatName := i.EmpCatName, 
        on = "NAICS2"]
    
    # Account for missing establishments by size category
    cbp[, est_cat_sum := e1+e2+e3+e4+e5+e6+e7+e8]
    cbp[, est_cat_miss := establishment - est_cat_sum]
    cbp[est_cat_miss < 0, est_cat_miss := 0] # should not happen
    
    # Summarize size distributions by NAICS2 by modelregion and draw from for missing est
    cbpn2 <- cbp[,.(e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                    e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
                 keyby = .(modelregion, NAICS2)]
    
    cbpn2 <- melt.data.table(cbpn2,
                             measure.vars = paste0("e",1:8),
                             variable.name ="esizecat",
                             value.name = "est")
    cbpn2[, pest := est/sum(est), by = .(modelregion, NAICS2)]
    cbpn2[, intest := as.integer(gsub("e","", esizecat))]
    
    # add extra establishments for in model region and outside model region
    cbp_extra_r <- list()
    set.seed(BASE_SEED_VALUE)
    
    for(regionnum in 1:2){
    
      cbp_extra <- list()
      cbpn2_r <- cbpn2[modelregion == regionnum]
      
      for(n2 in sort(unique(cbpn2_r$NAICS2))){
        cbp_extra_n2 <- cbp[modelregion == regionnum & NAICS2 == n2 & est_cat_miss > 0,
                            .(NAICS6, CBPZONE, establishment, 
                              est_cat_sum, est_cat_miss, NAICS2, EmpCatName)]
        if(nrow(cbp_extra_n2)>0){
          cbp_extra_n2[, ID := .I]
          cbpn2i = cbpn2_r[NAICS2 == n2]
          cbp_extra_n2_samp <- lapply(1:nrow(cbp_extra_n2), 
                                      function(x) {
                                        samp = sample(x = cbpn2i$intest,
                                                      size = cbp_extra_n2[x]$est_cat_miss,
                                                      replace = TRUE,
                                                      prob = cbpn2i$pest)
                                        tab = data.table(table(samp))})      
          cbp_extra_n2_samp <- rbindlist(cbp_extra_n2_samp, idcol = "ID")
          cbp_extra_n2_samp[, esizecat := paste0("e",samp)]
          setnames(cbp_extra_n2_samp, "N", "est")
          cbp_extra_n2 <- merge(cbp_extra_n2[, .(NAICS6, CBPZONE, EmpCatName, ID)],
                                cbp_extra_n2_samp[,.(ID, est, esizecat)],
                                by = "ID", allow.cartesian = TRUE)
        }
        cbp_extra[[n2]] <- cbp_extra_n2
      }
    
      cbp_extra_r[[regionnum]] <- rbindlist(cbp_extra)
      cbp_extra_r[[regionnum]][, ID := NULL]
    
    }
    
    cbp_extra_r <- rbindlist(cbp_extra_r, idcol = "modelregion")
    
    # Remove unnecessary fields and format to match cbp_extra
    cbp[, c("NAICS2", "establishment", "est_cat_sum", "est_cat_miss") := NULL]
    
    # Melt to create separate rows for each firm size category
    cbp <- melt.data.table(cbp,
                           measure.vars = paste0("e",1:8),
                           variable.name ="esizecat",
                           value.name = "est")
    
    # Combine with cbp extra and summarize
    cbp <- rbind(cbp, cbp_extra_r)
    cbp <- cbp[, .(est = sum(est)), 
               keyby = .(modelregion, CBPZONE, NAICS6, EmpCatName, esizecat)]
    
    # Convert esizecat to an integer (1:8)
    cbp[, esizecat := as.integer(esizecat)]
    
  } else {
    
    # for alternative scenarios, will not be using the CBP data, instead updating base year firms
    cbp <- NULL
    
  }
  
  # remove the CBP data from the environment
  rm(cbp, cbp_ag, envir = envir)
  
  ### Load scenario input files
  scenario.files <- c(CountyEmployment     = file.path(SCENARIO_INPUT_PATH, "data_emp_control_county.csv"),       # Control totals for emmployment by county for all USA
                      TAZEmployment        = file.path(SCENARIO_INPUT_PATH, "data_emp_control_taz.csv"),          # Control totals for emmployment by TAZ
                      FirmSizeFactors      = file.path(SCENARIO_INPUT_PATH, "data_firmsize_factors.csv"),         # Factors to adjust firm sizes by NAICS code 
                      ProductivityFactors  = file.path(SCENARIO_INPUT_PATH, "data_productivity_factors.csv"),     # Factors to scale IO table to account for productivity increases over time
                      TAZHH                = file.path(SCENARIO_INPUT_PATH, "data_hh.csv"),                       # CMAP model region HHs summarized at the TAZ level
                      for_prod             = file.path(SCENARIO_INPUT_PATH, "data_foreign_prod.csv"),             # Foreign producer values by commodity and country (imports)
                      for_cons             = file.path(SCENARIO_INPUT_PATH, "data_foreign_cons.csv"))             # foreign consumer values by commodity and country (exports)
  
  loadInputs(files = scenario.files, envir = envir)
  
  ### Process scenario input files
  
  # If we are using the County employment in the region to scale the TAZ employment controls
  # Do the scaling now (otherwise, the code below just replaces the county data in the region with the unscaled TAZ data)
  if(BASE_MODREG_EMP_CONTROL == "COUNTY"){
    
    CountyEmpModReg <- envir[["CountyEmployment"]][CountyFIPS %in% BASE_FIPS_INTERNAL]
    
    TAZEmpSum <- envir[["TAZEmployment"]][,.(TAZEmp = sum(Employment)), keyby = .(CountyFIPS, NAICS)]
    TAZEmpSum[CountyEmpModReg, CountyEmp := i.Employment, on = .(CountyFIPS, NAICS)]
    TAZEmpSum[is.na(CountyEmp), CountyEmp := 0]
    TAZEmpSum[, TAZScale := ifelse(TAZEmp > 0, CountyEmp/TAZEmp, 1)]
    
    envir[["TAZEmployment"]][TAZEmpSum, TAZScale := i.TAZScale, on = .(CountyFIPS, NAICS)]
    envir[["TAZEmployment"]][, Employment := as.numeric(Employment)]
    envir[["TAZEmployment"]][!is.na(TAZScale), Employment := Employment * TAZScale]
    envir[["TAZEmployment"]][, Employment := bucketRound(Employment), by = .(CountyFIPS, NAICS)]
    envir[["TAZEmployment"]][, TAZScale := NULL]
    
  }
  
  # Aggregate the county employment from outside the model region to CBPZones and add it to the TAZ Employment
  envir[["CountyEmployment"]] <- envir[["CountyEmployment"]][CountyFIPS %in% envir$TAZ_System$CountyFIPS & 
                                                               !CountyFIPS %in% BASE_FIPS_INTERNAL]
  
  envir[["CountyEmployment"]][envir$TAZ_System,
                              c("CBPZONE", "Mesozone", "TAZ") := .(i.CBPZONE, i.Mesozone, i.TAZ), 
                              on = "CountyFIPS"]
  
  envir[["CountyEmployment"]] <- envir[["CountyEmployment"]][,.(Employees.SE = sum(Employment)), 
                              keyby = .(TAZ, Mesozone, CBPZONE, NAICS)]
  
  setnames(envir[["TAZEmployment"]], 
           c("Zone17", "CountyFIPS", "Employment"), 
           c("TAZ", "CBPZONE", "Employees.SE"))
  
  envir[["TAZEmployment"]] <- rbind(envir[["TAZEmployment"]],
                                    envir[["CountyEmployment"]])
  
  rm(CountyEmployment, envir = envir)
  
  # Add employment categories
  envir[["TAZEmployment"]][envir[["c_n2_empcats"]][,.(NAICS = NAICS2, EmpCatName)], 
                           EmpCatName := i.EmpCatName, 
                           on = "NAICS"]
  
  # Summarize by TAZ and employment category
  envir[["TAZEmployment"]] <- envir[["TAZEmployment"]][, .(Employees.SE = sum(Employees.SE)), 
                                                       keyby = .(TAZ, Mesozone, CBPZONE, EmpCatName)]
  
  # Create a summarized version of the CMAP model region employment data with employment grouping categories in wide format
  envir[["TAZLandUseCVTM"]] <- add_totals(dcast.data.table(merge(envir[["TAZEmployment"]][TAZ %in% BASE_TAZ_INTERNAL,
                                                                                          .(TAZ, Mesozone, CountyFIPS = CBPZONE, 
                                                                                              EmpCatName, Employees.SE)],
                                                                 envir[["UEmpCats"]],
                                                                 by = "EmpCatName"),
                                                           TAZ + Mesozone + CountyFIPS ~ EmpCatGroupedName,
                                                           fun.aggregate = sum,
                                                           value.var = "Employees.SE"),
                                          idcols = 3L,
                                          coltotal = FALSE)
  
  setnames(envir[["TAZLandUseCVTM"]], 
           names(envir[["TAZLandUseCVTM"]])[4:ncol(envir[["TAZLandUseCVTM"]])],
           paste("NEmp", names(envir[["TAZLandUseCVTM"]])[4:ncol(envir[["TAZLandUseCVTM"]])], sep = "_"))
  
  envir[["TAZLandUseCVTM"]][envir[["TAZHH"]][,.(TAZ = Zone17, HH)], HH := i.HH, on = "TAZ"]
  
  # Set the names of the FirmSizeFactors employment category field to standard naming
  setnames(envir[["FirmSizeFactors"]], "NAICS", "EmpCatName")
  
  ### Load inputs/outputs from earlier steps
  
  # For alternative scenarios: load the base scenario firm synthesis outputs to use as
  # (1) the starting point for scaling firms to future year employment by TAZ
  # (2) base employments for calculating employment change to develop 
  #     employment growth factors for use in scaling the input output table 
  #     (along with productivity factors)
  
  if(SCENARIO_NAME != BASE_SCENARIO_BASE_NAME){
    
    if(file.exists(SCENARIO_BASEFIRMS)){
      
      # load the base scenario firm synthesis outputs
      load(SCENARIO_BASEFIRMS)
      
      # Extract the base year firms list
      envir$FirmsDomesticBase <- firm_sim_results$FirmsDomestic
      
      # Extract the base year construction factors
      envir$cbp23 <- firm_inputs$cbp23
      
      rm(firm_sim_results, firm_inputs)
      
      # Update the productivity factor for construction: calculate weighted average
      envir$cbp23[envir$ProductivityFactors, ProdFactor := i.ProdFactor, on = "NAICS3"]
      cbp23_prodfactor <- envir$cbp23[,.(NAICS3 = 230L, ProdFactor = sum(N * ProdFactor)/sum(N))]
      envir$ProductivityFactors <- rbind(envir$ProductivityFactors[!NAICS3 %in% 236:238],
                                         cbp23_prodfactor)
      setorder(envir$ProductivityFactors, NAICS3)
      
      # Calculate base year employment for use in creating a set of employment growth scale factors
      EmpBase <- envir$FirmsDomesticBase[,.(Emp = sum(Emp)), keyby = .(NAICS3 = floor(NAICS6/1000))]
      
      envir$ProductivityFactors[EmpBase, EmpBase := i.Emp, on = "NAICS3"]
      envir$ProductivityFactors[is.na(EmpBase), EmpBase := 0]
      
    } else {
      
      stop("No Base Scenario outputs available. Please run the Base Scenario first.")
      
    }
  }
  
  ### Standardize all common and scenario inputs so that NAICS coding is consistent
  
  # This code accounts for any inconsistencies between the Census NAICS and BEA NAICS.
  # The differences are limited to certain industries such as construction, real estate, and government
  
  # Construction (23): recode as 230000 in all datasets
  # This simplifies the complicated many to many relationship between the two coding systems
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
  
    # Create a simplified construction productivity factor (230)
    # Summarize construction establishments by NAICS 3 digit code (and save for use in alternative scenarios)
    cbp23 <- cbp[floor(NAICS6/10000) == 23,.N, keyby = .(NAICS3 = floor(NAICS6/1000))]
    envir$cbp23 <- copy(cbp23)
    
    # Calculate a weighted average construction productivity factor and update the factors table
    # (Generally, the base scenario will have productivity factors = 1, but this leaves the option open for using
    # productivity factors other than 1 in the base scenario)
    cbp23[envir$ProductivityFactors, ProdFactor := i.ProdFactor, on = "NAICS3"]
    cbp23_prodfactor <- cbp23[,.(NAICS3 = 230L, ProdFactor = sum(N * ProdFactor)/sum(N))]
    envir$ProductivityFactors <- rbind(envir$ProductivityFactors[!NAICS3 %in% 236:238],
                                       cbp23_prodfactor)
    setorder(envir$ProductivityFactors, NAICS3)
    
    # Aggregate CBP data so that all businesses are tagged as NAICS = 230000
    cbp[floor(NAICS6/10000) == 23, NAICS6 := 230000L]
    cbp <- cbp[,.(est = sum(est)), 
               keyby = .(modelregion, CBPZONE, NAICS6, EmpCatName, esizecat)]
  
  }
  
  # Aggregate io so that all construction make and use is tagged as NAICS = 230000
  envir$io[substr(Industry_NAICS6_Make,1,2) == 23,
           Industry_NAICS6_Make := "230000"]
  envir$io[substr(Industry_NAICS6_Use,1,2) == 23,
           Industry_NAICS6_Use := "230000"]
  envir$io <- envir$io[,.(ProVal = sum(ProVal)), 
                       keyby = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]
  
  # Simplify the NAICS/SCTG correspondence so that there is a single record for
  # all construction, with NAICS = 230000, SCTG = 0
  envir$c_n6_n6io_sctg <- rbind(envir$c_n6_n6io_sctg[!floor(Industry_NAICS6_CBP/10000) == 23],
                                data.table("230000", "Construction", 230000L, "Construction", 0L, "", 0),
                                use.names = FALSE)
  setorder(envir$c_n6_n6io_sctg, Industry_NAICS6_Make)
  
  # Manufacturing: simplify coding of Alumina refining
  # Codes 331313 (Alumina refining and primary aluminum production) 
  # and 331314 (Secondary smelting and alloying of aluminum)
  # aren't separated consistently in the make and use tables, 
  # collapse into single code across all datasets -- use 331313
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    
    # Aggregate CBP data so that all businesses are tagged as NAICS = 230000
    cbp[NAICS6 %in% c(331313, 331314), NAICS6 := 331313L]
    cbp <- cbp[,.(est = sum(est)), 
               keyby = .(modelregion, CBPZONE, NAICS6, EmpCatName, esizecat)]
  
  }
  
  # Aggregate io so that all Alumina refining make and use is tagged as NAICS = 331313
  envir$io[Industry_NAICS6_Make %in% c(331313, 331314),
           Industry_NAICS6_Make := "331313"]
  envir$io[Industry_NAICS6_Use %in% c(331313, 331314),
           Industry_NAICS6_Use := "331313"]
  envir$io <- envir$io[,.(ProVal = sum(ProVal)), 
                       keyby = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]
  
  # Simplify the NAICS/SCTG correspondence so that there is a single record for
  # Alumina refining, with NAICS = 331313
  envir$c_n6_n6io_sctg <- envir$c_n6_n6io_sctg[!Industry_NAICS6_CBP == 331314]
  
  # Update the foreign trade data so that all Alumina refining is tagged with 331314
  envir$for_cons[Commodity_NAICS6 %in% c(331313, 331314), Commodity_NAICS6 := 331313L]
  envir$for_cons <- envir$for_cons[,.(USExpVal = sum(USExpVal)), 
                                   keyby = .(Country, Commodity_NAICS6, FAFZONE, ctrycod, CBPZONE)]
  
  envir$for_prod[Commodity_NAICS6 %in% c(331313, 331314), Commodity_NAICS6 := 331313L]
  envir$for_prod <- envir$for_prod[,.(USImpVal = sum(USImpVal)), 
                                   keyby = .(Country, Commodity_NAICS6, FAFZONE, ctrycod, CBPZONE)]
  
  # Real Estate (531): record as 531000 in all datasets
  # This simplifies the many to many relationship between the two coding systems
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    
    # Aggregate CBP data so that all businesses are tagged as NAICS = 531000
    cbp[floor(NAICS6/1000) == 531, NAICS6 := 531000L]
    cbp <- cbp[,.(est = sum(est)), 
               keyby = .(modelregion, CBPZONE, NAICS6, EmpCatName, esizecat)]
    
  }
  
  # Aggregate io so that all real estate make and use is tagged as NAICS = 531000
  envir$io[substr(Industry_NAICS6_Make,1,3) == 531,
           Industry_NAICS6_Make := "531000"]
  envir$io[substr(Industry_NAICS6_Use,1,3) == 531,
           Industry_NAICS6_Use := "531000"]
  envir$io <- envir$io[,.(ProVal = sum(ProVal)), 
                       keyby = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]
  
  # Simplify the NAICS/SCTG correspondence so that there is a single record for
  # all real estate, with NAICS = 531000, SCTG = 0
  envir$c_n6_n6io_sctg <- rbind(envir$c_n6_n6io_sctg[!floor(Industry_NAICS6_CBP/1000) == 531],
                                data.table("531000", "Real Estate", 531000L, "Real Estate", 0L, "", 0),
                                use.names = FALSE)
  setorder(envir$c_n6_n6io_sctg, Industry_NAICS6_Make)
  
  # Government: public administration (92) and postal service (491)
  # Add establishments to the CBP to reflect public administration employment
  # Code consistently, converting all datasets to use 920000
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    
    # Synthesize data for missing NAICS/county category 92
    EmpCounty <- envir$TAZEmployment[,.(Emp = sum(Employees.SE)), 
                                     keyby = .(EmpCatName, CBPZONE)]
    EmpCounty[cbp[,.(Est = sum(est)), by = EmpCatName], 
              Est := i.Est, 
              on = c("EmpCatName")]
    EmpCounty[is.na(Est), Est := 0]
    
    EmpCountyPublic <- EmpCounty[EmpCatName == "92"]
    EmpCountyPublic[EmpCounty[EmpCatName != "92", .(Emp = sum(Emp)), by = CBPZONE], 
                    EmpOther := i.Emp, 
                    on = "CBPZONE"]
    EmpCountyPublic[, PctPublic := Emp/EmpOther]
    
    EstablishmentsMiss <- cbp[, .(est = sum(est)), 
                              keyby = .(modelregion, CBPZONE, esizecat)]
    EstablishmentsMiss[EmpCountyPublic, 
                       PctPublic := i.PctPublic, 
                       on = "CBPZONE"]
    EstablishmentsMiss[, estPublic := est * PctPublic]
    EstablishmentsMiss[, estPublic := bucketRound(estPublic)]
    
    cbp <- rbind(cbp,
                 EstablishmentsMiss[, .(modelregion, CBPZONE, NAICS6 = 920000L, 
                                     EmpCatName = 92L, esizecat, est = estPublic)])
    setkey(cbp, modelregion, CBPZONE, NAICS6, EmpCatName, esizecat)
  
  }
  
  # Aggregate io so that all government make and use is tagged as NAICS = 920000
  
  envir$io[substr(Industry_NAICS6_Make,1,2) %in% c("S0", "GS") |
             substr(Industry_NAICS6_Make,1,3) == 491,
           Industry_NAICS6_Make := "920000"]
  envir$io[substr(Industry_NAICS6_Use,1,2) %in% c("S0", "GS") |
             substr(Industry_NAICS6_Use,1,3) == 491,
           Industry_NAICS6_Use := "920000"]
  envir$io <- envir$io[,.(ProVal = sum(ProVal)), 
                       keyby = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]
  
  # Simplify the NAICS/SCTG correspondence so that there is a single record for
  # all government, with NAICS = 920000, SCTG = 0
  envir$c_n6_n6io_sctg <- rbind(envir$c_n6_n6io_sctg[!floor(Industry_NAICS6_CBP/1000) %in% c(491,920)],
                                data.table("920000", "Public Administration", 920000L, "Public Administration", 0L, "", 0),
                                use.names = FALSE)
  setorder(envir$c_n6_n6io_sctg, Industry_NAICS6_Make)
  
  # Update the foreign trade data so that all 9x records are tagged with 92x
  envir$for_cons[floor(Commodity_NAICS6/100000) == 9, Commodity_NAICS6 := 920000L]
  envir$for_cons <- envir$for_cons[,.(USExpVal = sum(USExpVal)), 
                                   keyby = .(Country, Commodity_NAICS6, FAFZONE, ctrycod, CBPZONE)]
  
  envir$for_prod[floor(Commodity_NAICS6/100000) == 9, Commodity_NAICS6 := 920000L]
  envir$for_prod <- envir$for_prod[,.(USImpVal = sum(USImpVal)), 
                                   keyby = .(Country, Commodity_NAICS6, FAFZONE, ctrycod, CBPZONE)]
  
  # Reallocate public production/consumption value within each country 
  # to the remaining privately owned industries in proportion to their prod/cons value
  
  # Foreign Production
  for_prod_sum <- envir$for_prod[, .(USImpVal = as.numeric(sum(USImpVal))), by = .(CBPZONE, FAFZONE)]
  for_prod_sum_no_pub <- envir$for_prod[Commodity_NAICS6 != 920000, 
                                  .(USImpValNoPub = as.numeric(sum(USImpVal))), 
                                  by = .(CBPZONE, FAFZONE)]
  for_prod_sum <- merge(for_prod_sum,
                        for_prod_sum_no_pub,
                        by = c("CBPZONE", "FAFZONE"),
                        all.x = TRUE)
  for_prod_sum[is.na(USImpValNoPub), USImpValNoPub := 0]
  for_prod_sum[, ProdScale := ifelse(USImpValNoPub > 0, USImpVal/USImpValNoPub, 0)]
  
  # account for countries with no non-public production by reallocating within FAF ZONE so FAF ZONE production is conserved
  for_prod_faf <- for_prod_sum[, .(USImpVal = sum(USImpVal)), by = FAFZONE]
  for_prod_faf_no_pub <- for_prod_sum[USImpValNoPub > 0, .(USImpValNoPub = sum(USImpVal)), by = FAFZONE]
  for_prod_faf <- merge(for_prod_faf,
                        for_prod_faf_no_pub,
                        by = "FAFZONE",
                        all.x = TRUE)
  for_prod_faf[, ProdScaleFAF := USImpVal/USImpValNoPub]
  
  # do the scaling for foreign production
  envir$for_prod <- envir$for_prod[Commodity_NAICS6 != 920000]
  envir$for_prod[for_prod_sum, ProdScale := i.ProdScale, on = "CBPZONE"]
  envir$for_prod[for_prod_faf, ProdScaleFAF := i.ProdScaleFAF, on = "FAFZONE"]
  envir$for_prod[, USImpVal := USImpVal * ProdScale * ProdScaleFAF]
  
  # Foreign Consumption
  for_cons_sum <- envir$for_cons[, .(USExpVal = as.numeric(sum(USExpVal))), by = .(CBPZONE, FAFZONE)]
  for_cons_sum_no_pub <- envir$for_cons[Commodity_NAICS6 != 920000, 
                                  .(USExpValNoPub = as.numeric(sum(USExpVal))), 
                                  by = .(CBPZONE, FAFZONE)]
  for_cons_sum <- merge(for_cons_sum,
                        for_cons_sum_no_pub,
                        by = c("CBPZONE", "FAFZONE"),
                        all.x = TRUE)
  for_cons_sum[is.na(USExpValNoPub), USExpValNoPub := 0]
  for_cons_sum[, ConsScale := ifelse(USExpValNoPub > 0, USExpVal/USExpValNoPub, 0)]
  
  # account for counties with no non-public consumption by reallocating within FAF ZONE so FAF ZONE consumption is conserved
  for_cons_faf <- for_cons_sum[, .(USExpVal = sum(USExpVal)), by = FAFZONE]
  for_cons_faf_no_pub <- for_cons_sum[USExpValNoPub > 0, .(USExpValNoPub = sum(USExpVal)), by = FAFZONE]
  for_cons_faf <- merge(for_cons_faf,
                        for_cons_faf_no_pub,
                        by = "FAFZONE",
                        all.x = TRUE)
  for_cons_faf[, ConsScaleFAF := USExpVal/USExpValNoPub]
  
  # do the scaling for foreign consumption
  envir$for_cons <- envir$for_cons[Commodity_NAICS6 != 920000]
  envir$for_cons[for_cons_sum, ConsScale := i.ConsScale, on = "CBPZONE"]
  envir$for_cons[for_cons_faf, ConsScaleFAF := i.ConsScaleFAF, on = "FAFZONE"]
  envir$for_cons[, USExpVal := USExpVal * ConsScale * ConsScaleFAF]
  
  ### Report any missing data/mismatches in coverage across firm synthesis input files
  ### This tabulation is produced in the base scenario only
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
  
    # compare the CBP and trade data (census NAICS) with the IO data (BEA NAICS)
    cbp_sum <- cbp[,.(Est = sum(est)), keyby = .(Industry_NAICS6_CBP = NAICS6)]
    
    for_cons_sum <- envir$for_cons[,.(USExpVal = sum(USExpVal)/1e6), 
                                   keyby = .(Industry_NAICS6_CBP = Commodity_NAICS6)]
    for_prod_sum <- envir$for_prod[,.(USImpVal = sum(USImpVal)/1e6), 
                                   keyby = .(Industry_NAICS6_CBP = Commodity_NAICS6)]
    for_sum <- merge(for_cons_sum, for_prod_sum, by = "Industry_NAICS6_CBP", all = TRUE)
    for_sum[is.na(for_sum)] <- 0
    
    cbp_for_sum <- merge(cbp_sum, for_sum, by = "Industry_NAICS6_CBP", all = TRUE)
    cbp_for_sum[is.na(cbp_for_sum)] <- 0
    
    io_make <- envir$io[,.(ProValMake = sum(ProVal)), keyby = Industry_NAICS6_Make]
    io_use <- envir$io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
                       .(ProValUse = sum(ProVal)), keyby = .(Industry_NAICS6_Make = Industry_NAICS6_Use)]
    io_sum <- merge(io_make, io_use, by = "Industry_NAICS6_Make", all = TRUE)
    io_sum[is.na(io_sum)] <- 0
    
    cbp_for_sum[envir$c_n6_n6io_sctg, 
                Industry_NAICS6_Make := i.Industry_NAICS6_Make, 
                on = "Industry_NAICS6_CBP"]
    cbp_for_io_sum <- cbp_for_sum[,.(Est = sum(Est), USExpVal = sum(USExpVal), USImpVal = sum(USImpVal)),
                                  keyby = Industry_NAICS6_Make]
    
    cbp_for_io_sum <- merge(cbp_for_io_sum, io_sum, by = "Industry_NAICS6_Make", all = TRUE)
    cbp_for_io_sum[is.na(cbp_for_io_sum)] <- 0
    
    cbp_for_io_sum[,.(sum(USExpVal), sum(USImpVal), sum(ProValMake), sum(ProValUse))]
    cbp_for_io_sum[envir$c_n6_n6io_sctg, 
                   Industry_NAICS6_Make_desc := i.Industry_NAICS6_Make_desc, 
                   on = "Industry_NAICS6_Make"]
    
    cbp_for_sum[envir$c_n6_n6io_sctg, 
                Industry_NAICS6_CBP_desc := i.Industry_NAICS6_CBP_desc, 
                on = "Industry_NAICS6_CBP"]
    
    # Reorder the tables with labels first
    setcolorder(cbp_for_sum, c(names(cbp_for_sum)[ncol(cbp_for_sum)], names(cbp_for_sum)[1:ncol(cbp_for_sum)-1]))
    setcolorder(cbp_for_io_sum, c(names(cbp_for_io_sum)[ncol(cbp_for_io_sum)], names(cbp_for_io_sum)[1:ncol(cbp_for_io_sum)-1]))
  
  }
  
  ### objects for intermediate summary results
  
  # create the list
  envir[["firm_sim_summary"]] <- list()
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    # add the base year summary tables comparing CBP, trade, and IO data
    envir$firm_sim_summary$cbp_for_sum <- cbp_for_sum
    envir$firm_sim_summary$cbp_for_io_sum <- cbp_for_io_sum
  }
  
  # Standardize the Trade Data zoning with the model zone system
  # As of 2022 base year model update the countries represented in the 
  # trade data are different than the original implementation of the model
  # The TAZ system file include the new country coding to match the new trade data
  # convert the trade data here to the old country coding to match with skims
  envir$for_prod[envir$TAZ_System[, .(ctrycod = newctrycod, CBPZONE, oldctrycod = ctrycod)],
                 c("ctrycod", "CBPZONE") := .(i.oldctrycod, i.CBPZONE),
                 on = "ctrycod"]
  envir$for_cons[envir$TAZ_System[, .(ctrycod = newctrycod, CBPZONE, oldctrycod = ctrycod)],
                 c("ctrycod", "CBPZONE") := .(i.oldctrycod, i.CBPZONE),
                 on = "ctrycod"]
  
  ### Return the cbp table
  return(cbp)
  
}
