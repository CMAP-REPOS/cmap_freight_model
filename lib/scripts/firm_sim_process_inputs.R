
# This function loads all necessary inputs into envir, after any needed transformations
firm_sim_process_inputs <- function(envir) {
  
  ### Load project input files
  project.files <- c( c_n2_empcats         = file.path(SYSTEM_DATA_PATH, "corresp_naics2_empcats.csv"),    # Correspondence between NAICS2 groups and aggregated employment groups
                      c_n6_n6io_sctg       = file.path(SYSTEM_DATA_PATH, "corresp_naics6_n6io_sctg.csv"),  # Correspondence between NAICS 6-digit, I/O NAICS, and SCTG
                      c_cbp_faf            = file.path(SYSTEM_DATA_PATH, "corresp_fafzone_cbpzone.csv"),   # Correspondence between CBP and FAF zone systems
                      c_cbp_mz             = file.path(SYSTEM_DATA_PATH, "corresp_mesozone_cbpzone.csv"),  # Correspondence between CBP and Mesozone zone systems
                      c_mz_faf_reg         = file.path(SYSTEM_DATA_PATH, "corresp_meso_faf3_region.csv"),  # Correspondence between Mesozones, FAF3, and census regions for summaries
                      c_n6_labels          = file.path(SYSTEM_DATA_PATH, "corresp_naics2007_labels.csv"),  # Correspondence NAICS 2007 at different levels of detail and industry name labels
                      cbp                  = file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"),              # CBP data file
                      cbp_ag               = file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"),           # CBP data file -- Agriculture records generated seperately
                      EstSizeCategories    = file.path(SYSTEM_DATA_PATH, "data_est_size_categories.csv"),  # Establishment size categories and labels
                      io                   = file.path(SYSTEM_DATA_PATH, "data_2010io.csv"),
                      unitcost             = file.path(SYSTEM_DATA_PATH, "data_unitcost.csv"),
                      prefweights          = file.path(SYSTEM_DATA_PATH, "data_firm_pref_weights.csv"),
                      mzemp                = file.path(SYSTEM_DATA_PATH, "data_mesozone_emprankings.csv"), # Industry rankings data by mesozone based on employment
                      TAZ_System           = file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"),                # TAZ system 
                      firm_sim_commodities       = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_commodities.R"),
                      firm_sim_consumers         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_consumers.R"),
                      firm_sim_enumerate         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate.R"),
                      firm_sim_enumerate_foreign = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_enumerate_foreign.R"),
                      firm_sim_input_output      = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_input_output.R"),
                      firm_sim_mesozones         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_mesozones.R"),
                      firm_sim_process_inputs    = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"),
                      firm_sim_producers         = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_producers.R"),
                      firm_sim_sample_groups     = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_sample_groups.R"),
                      firm_sim_scaling           = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_scaling.R"),
                      firm_sim_summary_render    = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_summary_render.R"),
                      firm_sim_write_groups      = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_write_groups.R"))
  
  loadInputs(files = project.files, envir = envir)
  
  ### Process project input files
  envir[["UEmpCats"]]  <- unique(envir[["c_n2_empcats"]][,.(EmpCatName = as.character(EmpCatName), EmpCatDesc, EmpCatGroupedName)])
  
  # For the base scenario need to process the input CBP data into the format required by the model
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    # Combine Domestic Non Ag Firm Records with the Ag Firm Records
    # Remove any ag records in cbp and replacing with cbp_ag  
    cbp <- rbind(envir[["cbp"]][Industry_NAICS6_CBP >= 113110],
                 envir[["cbp_ag"]])
    
    # Identify the region data for the 21 county CMAP region
    # For the 21 counties, CBPZONE == county FIPS code
    # Remove records with missing zones and NAICS codes
    cbp[, CMAP21 := ifelse(CBPZONE %in% BASE_FIPS_INTERNAL,1,0)]
    cbp <- cbp[!is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP)]
    
    # Aggregate by zones, NAICS, and firm size category (should already conform to this grouping)
    cbp <- cbp[,.(establishment = sum(establishment),
                  e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                  e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
               by = .(NAICS6 = Industry_NAICS6_CBP, CBPZONE, CMAP21)]
    
    # Add 2 digit NAICS and the EmpCatName used in the model
    cbp[, NAICS2 := substr(NAICS6, 1, 2)]
    cbp[envir[["c_n2_empcats"]][,.(NAICS2 = as.character(NAICS2), EmpCatName = as.character(EmpCatName))], 
        EmpCatName := i.EmpCatName, on = "NAICS2"]
    
    # Account for missing establishments by size category
    cbp[, est_cat_sum := e1+e2+e3+e4+e5+e6+e7+e8]
    cbp[, est_cat_miss := establishment - est_cat_sum]
    cbp[est_cat_miss < 0, est_cat_miss := 0] # should not happen
    
    # Summarize size distributions by NAISC2 and draw from for missing est
    cbpn2 <- cbp[,.(e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                    e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
                 by = .(NAICS2)]
    
    cbpn2 <- melt.data.table(cbpn2,
                             measure.vars = paste0("e",1:8),
                             variable.name ="esizecat",
                             value.name = "est")
    cbpn2[, pest := est/sum(est), by = NAICS2]
    cbpn2[, intest := as.integer(gsub("e","", esizecat))]
    
    cbp_extra <- list()
    for(n2 in unique(cbpn2$NAICS2)){
      cbp_extra_n2 <- cbp[NAICS2 == n2 & est_cat_miss > 0,
                          .(NAICS6, CBPZONE, CMAP21, establishment, est_cat_sum, est_cat_miss, NAICS2, EmpCatName)]
      if(nrow(cbp_extra_n2)>0){
        cbp_extra_n2[, ID := .I]
        cbpn2i = cbpn2[NAICS2 == n2]
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
        cbp_extra_n2 <- merge(cbp_extra_n2[, .(NAICS6, CBPZONE, CMAP21, EmpCatName, ID)],
                              cbp_extra_n2_samp[,.(ID, est, esizecat)],
                              by = "ID", allow.cartesian = TRUE)
      }
      cbp_extra[[n2]] <- cbp_extra_n2
    }
    
    cbp_extra <- rbindlist(cbp_extra)
    cbp_extra[, ID := NULL]
    
    # Remove unecessary fields and format to match cbp_extra
    cbp[, c("NAICS2", "establishment", "est_cat_sum", "est_cat_miss") := NULL]
    
    # Melt to create separate rows for each firm size category
    cbp <- melt.data.table(cbp,
                           measure.vars = paste0("e",1:8),
                           variable.name ="esizecat",
                           value.name = "est")
    
    # Combine with cbp extra and summarize
    cbp <- rbind(cbp, cbp_extra)
    cbp <- cbp[, .(est = sum(est)), keyby = .(NAICS6, CBPZONE, CMAP21, EmpCatName, esizecat)]
    
    # Convert esizecat to an integer (1:8)
    cbp[, esizecat := as.integer(esizecat)]
    
  } else {
    
    # for alternative scenarios, will not be using the CBP data, instead updating base year firms
    cbp <- NULL
    
  }
  
  # remove the CBP data from the environment
  rm(cbp, cbp_ag, envir = envir)
  
  ### Load scenario input files
  scenario.files <- c(MZEmployment         = file.path(SCENARIO_INPUT_PATH, "data_emp_control_mz.csv"),       #Control totals for emmployment by Mesozone
                      TAZEmployment        = file.path(SCENARIO_INPUT_PATH, "data_emp_control_taz.csv"),      #Control totals for emmployment by TAZ
                      TAZHH                = file.path(SCENARIO_INPUT_PATH, "data_hh.csv"),                   #CMAP model region HHs summarized at the TAZ level
                      for_prod             = file.path(SCENARIO_INPUT_PATH, "data_foreign_prod.csv"),         #Foreign producers
                      for_cons             = file.path(SCENARIO_INPUT_PATH, "data_foreign_cons.csv"))         #foreign consumers                                      
  
  loadInputs(files = scenario.files, envir = envir)
  
  ### Process scenario input files
  
  # Update fieldnames/datatypes for consistency
  
  # Add the MZ Employment from outside the model region to the TAZ Employment
  envir[["TAZEmployment"]] <- rbind(envir[["TAZEmployment"]],
                                    envir[["MZEmployment"]][!Mesozone %in% BASE_MZ_INTERNAL],
                                    fill = TRUE)
  
  # remove the  MZ Employment data from the environment
  rm(MZEmployment, envir = envir)
  
  # Naming and data type of control employment data
  setnames(envir[["TAZEmployment"]], 
           c("Zone17", "NAICS", "Employment"), 
           c("TAZ", "EmpCatName", "Employees.SE"))
  
  envir[["TAZEmployment"]][, EmpCatName := as.character(EmpCatName)]
  
  # Add TAZ field to the external MZs (value of the maximum TAZ + Mesozone)
  envir[["TAZEmployment"]][!Mesozone %in% BASE_MZ_INTERNAL, TAZ := Mesozone + max(BASE_TAZ_INTERNAL)]
  
  # Create a summarized version of the CMAP model region employment data with employment grouping categories in wide format
  envir[["TAZLandUseCVTM"]] <- add_totals(dcast.data.table(merge(envir[["TAZEmployment"]][TAZ %in% BASE_TAZ_INTERNAL,
                                                                                          .(TAZ, Mesozone, CountyFIPS, 
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
  
  ### Return the cbp table
  return(cbp)
  
}
