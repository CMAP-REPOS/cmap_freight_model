
# Master function for executing the synthesis of firms.
firm_sim <- function(cbp) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Firms", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Run steps
  progressUpdate(prop = 1/12, dir = SCENARIO_LOG_PATH)
  FirmsDomestic <- firm_synthesis_enumerate(cbp = cbp,
                                            c_n6_n6io_sctg = c_n6_n6io_sctg,
                                            EmpBounds = EmpBounds,
                                            cbp_ag = cbp_ag)
  
  progressUpdate(prop = 2/12, dir = SCENARIO_LOG_PATH)
  FirmsDomestic <- firm_synthesis_commodities(Firms = FirmsDomestic)
  
  progressUpdate(prop = 3/12, dir = SCENARIO_LOG_PATH)
  FirmsDomestic <- firm_synthesis_mesozones(Firms = FirmsDomestic)
  
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
  
  # save regional CBP list for analysis
  RegionFirms <- FirmsDomestic[MESOZONE < 150]
  save(RegionFirms, file = file.path(SCENARIO_OUTPUT_PATH, "RegionFirms.Rdata"))
  
  # save warehouse list for use in truck touring model
  # NAICS 481 air, 482 rail, 483 water, 493 warehouse and storage
  warehouses <- RegionFirms[substr(Industry_NAICS6_Make, 1, 3) %in% c(481, 482, 483, 493)]
  save(warehouses, file = file.path(SCENARIO_OUTPUT_PATH, "warehouses.Rdata"))
  
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
  
  progressUpdate(prop = 6/12, dir = SCENARIO_LOG_PATH)
  io_list <- firm_synthesis_input_output(io = io,
                                         c_n6_n6io_sctg = c_n6_n6io_sctg)
  
  progressUpdate(prop = 7/12, dir = SCENARIO_LOG_PATH)
  producers <- firm_synthesis_producers(io = io_list$io,
                                        fromwhl = io_list$fromwhl,
                                        FirmsDomestic = FirmsDomestic,
                                        FirmsForeign = FirmsForeign,
                                        unitcost = unitcost)
  
  progressUpdate(prop = 8/12, dir = SCENARIO_LOG_PATH)
  consumers <- firm_synthesis_consumers(io = io_list$io,
                                        wholesalers = producers[ProdType == 3],
                                        FirmsDomestic = FirmsDomestic,
                                        FirmsForeign = FirmsForeign,
                                        c_n6_n6io_sctg = c_n6_n6io_sctg,
                                        unitcost = unitcost,
                                        maxbusid = max(producers$SellerID),
                                        writeConsumersIncremental = TRUE) #change to TRUE for production
  
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
  
  progressUpdate(prop = 10/12, dir = SCENARIO_LOG_PATH)
  naics_set <- firm_synthesis_sample_groups(firms_sum)
  
  # save naics_set
  save(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set.Rdata"))
  fwrite(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set.csv"))
  
  progressUpdate(prop = 11/12, dir = SCENARIO_LOG_PATH)
  producers_consumers_list <- firm_synthesis_write_groups(producers = producers,
                                                          consumers = consumers,
                                                          naics_set = naics_set)
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  return(producers_consumers_list)
  
}
