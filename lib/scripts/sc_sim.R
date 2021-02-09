
# Master function for executing the supply chain simulation.
sc_sim <- function(naics_set) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Supply Chain", dir = SCENARIO_LOG_PATH)
  
  # Calculate model costs, times for each shipment route - mode combination 
  # and prepare other inputs for mode choice based on settings
  ShipmentRoutesCostsList <- sc_sim_modalcosts(skims = skims,
                                               ModeChoiceParameters = ModeChoiceParameters,
                                               mode_description = mode_description,
                                               mode_availability = mode_availability,
                                               sctg = sctg)
  gc()
  
  progressUpdate(prop = 0.2, dir = SCENARIO_LOG_PATH)
  
  # Select the set of markets to run based on the scenario settings
  if(!USER_MARKETS_ALL) naics_set <- naics_set[Commodity_SCTG %in% USER_COMMODITIES_RUN]
  
  # Allocate producers and consumers to groups using sampling to create even sized groups
  naics_set <- sc_sim_markets(naics_set = naics_set)
  naics_set <- sc_sim_markets_check(naics_set = naics_set)
  gc()
  
  progressUpdate(prop = 0.3, dir = SCENARIO_LOG_PATH)
  
  # Write group buy and sell files out
  naics_set <- sc_sim_buy_sell(naics_set = naics_set)
  naics_set <- sc_sim_buy_sell_check(naics_set = naics_set)
  gc()
  
  progressUpdate(prop = 0.4, dir = SCENARIO_LOG_PATH)
  
  # create samples within each group in each market
  naics_set <- sc_sim_sampling(naics_set = naics_set)
  naics_set <- sc_sim_sampling_check(naics_set = naics_set)
  gc()

  progressUpdate(prop = 0.5, dir = SCENARIO_LOG_PATH)

  # Apply the distribution channel model
  naics_set <- sc_sim_distchannel(naics_set = naics_set,
                                  TAZGCD = mesozone_gcd)
  naics_set <- sc_sim_distchannel_check(naics_set = naics_set)
  gc()

  progressUpdate(prop = 0.6, dir = SCENARIO_LOG_PATH)

  # Apply the mode choice model
  naics_set <- sc_sim_modechoice(naics_set = naics_set, 
                                 TAZGCD = mesozone_gcd,
                                 ModeChoiceParameters = ShipmentRoutesCostsList$ModeChoiceParameters,
                                 sctg = ShipmentRoutesCostsList$sctg,
                                 c_path_mode = ShipmentRoutesCostsList$c_path_mode,
                                 mode_availability = ShipmentRoutesCostsList$mode_availability)
  naics_set <- sc_sim_modechoice_check(naics_set = naics_set)
  gc()

  progressUpdate(prop = 0.7, dir = SCENARIO_LOG_PATH)

  # Write group cost files out
  naics_set <- sc_sim_costs(naics_set = naics_set)
  naics_set <- sc_sim_costs_check(naics_set = naics_set)
  gc()

  progressUpdate(prop = 0.8, dir = SCENARIO_LOG_PATH)

  # Run the PMG model
  naics_set <- sc_sim_pmg(naics_set = naics_set)
  naics_set <- sc_sim_pmg_check(naics_set = naics_set)
  gc()

  progressUpdate(prop = 0.9, dir = SCENARIO_LOG_PATH)

  # Combine PMG outputs to create a shipments table
  BuyerSupplierPairs <- sc_sim_shipments(naics_set = naics_set)
  naics_set <- sc_sim_shipments_check(naics_set = naics_set)
  gc()
  
  progressUpdate(prop = 0.93, dir = SCENARIO_LOG_PATH)
  
  # Update shipments table for paths via Ports and Airports
  BuyerSupplierPairs <- sc_sim_ports(BuyerSupplierPairs = BuyerSupplierPairs,
                                     ShipmentRoutesCosts = ShipmentRoutesCostsList$ShipmentRoutesCosts,
                                     skims_airports = skims_airports,
                                     skims_ports = skims_ports, 
                                     ModeChoiceParameters = ShipmentRoutesCostsList$ModeChoiceParameters,
                                     sctg = ShipmentRoutesCostsList$sctg)
  naics_set <- sc_sim_ports_check(naics_set = naics_set,
                                  BuyerSupplierPairs = BuyerSupplierPairs)
  gc()
  
  progressUpdate(prop = 0.96, dir = SCENARIO_LOG_PATH)
  
  # Add in the incremental consumers tables
  BuyerSupplierPairs <- sc_sim_increment(BuyerSupplierPairs = BuyerSupplierPairs)
  naics_set <- sc_sim_increment_check(naics_set = naics_set,
                                      BuyerSupplierPairs = BuyerSupplierPairs)
  gc()
  
  # Save the updated naics_set table as a .csv
  fwrite(naics_set, file = file.path(SCENARIO_OUTPUT_PATH,"naics_set_after_supply_chain.csv"))
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  # Return results
  return(list(BuyerSupplierPairs = BuyerSupplierPairs,
              ShipmentRoutesCostsList = ShipmentRoutesCostsList,
              naics_set = naics_set))
  
}
