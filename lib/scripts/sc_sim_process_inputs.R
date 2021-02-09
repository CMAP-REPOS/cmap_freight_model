
# This function loads all necessary inputs into envir, after any needed transformations
sc_sim_process_inputs <- function(envir) {
  
  ### Load and process project input files
  project.files <- c(ModeChoiceParameters       = file.path(SYSTEM_DATA_PATH, "ModeChoiceParameters.rds"),
                     PMGParameters              = file.path(SYSTEM_DATA_PATH, "PMGParameters.rds"), # Input parameters to PMG
                     sctg                       = file.path(SYSTEM_DATA_PATH, "corresp_sctg_category.csv"), #correspondence between SCTG and descriptions
                     mode_availability          = file.path(SYSTEM_DATA_PATH, "data_modepath_availability.csv"), # Modes available by SCTG and ODSegment
                     mode_description           = file.path(SYSTEM_DATA_PATH, "data_mode_description.csv"), # Mode desciption
                     pc_domestic_targets        = file.path(SYSTEM_DATA_PATH, "domestictargets.csv"),
                     pc_international_targets   = file.path(SYSTEM_DATA_PATH, "foreigndomestictargets.csv"),
                     FAF_DISTANCE               = file.path(SYSTEM_DATA_PATH, "data_faf_distance.csv"),
                     mesozone_gcd               = file.path(SYSTEM_DATA_PATH, "data_mesozone_gcd.csv"),  #Mesozone to mesozone gcds
                     shipsize                   = file.path(SYSTEM_DATA_PATH, "data_commodity_shipmentsizes.csv"),
                     skims                      = file.path(SYSTEM_DATA_PATH, "data_modepath_skims.csv"),
                     skims_airports             = file.path(SYSTEM_DATA_PATH, "data_modepath_airports.csv"),
                     skims_ports                = file.path(SYSTEM_DATA_PATH, "data_modepath_ports.csv"),
                     Supplier_Selection_Distribution = file.path(SYSTEM_DATA_PATH, "data_faf_ton_distribution.csv"),
                     distchannel_calibration    = file.path(SYSTEM_DATA_PATH, "model_distchannel_calibration.csv"),
                     distchannel_food           = file.path(SYSTEM_DATA_PATH, "model_distchannel_food.csv"),
                     distchannel_food_cal       = file.path(SYSTEM_DATA_PATH, "model_distchannel_food_cal.csv"),
                     distchannel_mfg            = file.path(SYSTEM_DATA_PATH, "model_distchannel_mfg.csv"),
                     distchannel_mfg_cal        = file.path(SYSTEM_DATA_PATH, "model_distchannel_mfg_cal.csv"),
                     sc_sim_buy_sell            = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_buy_sell.R"),
                     sc_sim_costs               = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_costs.R"),
                     sc_sim_distchannel         = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_distchannel.R"),
                     sc_sim_increment           = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_increment.R"),
                     sc_sim_markets             = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_markets.R"),
                     sc_sim_modalcosts          = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_modalcosts.R"),
                     sc_sim_modechoice          = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_modechoice.R"),
                     sc_sim_pmg                 = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_pmg.R"),
                     sc_sim_ports               = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_ports.R"),
                     sc_sim_results             = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_results.R"),
                     sc_sim_sampling            = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_sampling.R"),
                     sc_sim_shipments           = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_shipments.R")
  )
  
  loadInputs(files = project.files, envir = envir,
             fread.args = list(c_sctg_cat = list(stringsAsFactors = TRUE)))
  
  envir[["famesctg"]] <- c(rep("A",3),rep("E",4),rep("K",2),rep("F",3),
                           rep("C",7),rep("B",5),rep("J",6),"C",
                           rep("G",3),"D",rep("I",2),"G","J",rep("H",4))
  
  envir[["distchan_calcats"]] <- data.table(CHOICE=c("0","1","2+","2+"),CHID=1:4)
  
  envir[["FAF_DISTANCE"]][,Distance_Bin:=findInterval(distance,seq(0,150000,100))]
  setkey(envir[["FAF_DISTANCE"]],oFAFZONE,dFAFZONE)
  
  ### Load and process scenario input files
  
  
  ### Load inputs/outputs from earlier steps
  load(file.path(SCENARIO_OUTPUT_PATH, "naics_set.Rdata"))
  naics_set <- data.table(naics_set)
  
  ### Return
  gc()
  return(naics_set)
  
}
