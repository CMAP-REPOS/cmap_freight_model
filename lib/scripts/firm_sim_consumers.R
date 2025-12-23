#Create consumers database
firm_synthesis_consumers <- function(io, FirmsDomestic, FirmsForeign, c_n6_n6io_sctg, unitcost, prefweights, maxbusid, writeConsumersIncremental = TRUE){

  # For each firm generate a list of input commodities that need to be purchased (commodity code, amount)
  # All agents that consume some SCTG commodity are potential consumers

  ### Calculate value per employee required
  
  # In the consumers table Use code is the industry code of the consuming firm
  # Domestic consumption is DomConsVal (domestic flows and imports) 
  consval <- merge(io[, .(ProVal = sum(DomConsVal)), by = .(Industry_NAICS6_Make, Industry_NAICS6_Use)],
                   FirmsDomestic[, .(Emp = sum(Emp)), by = .(Industry_NAICS6_Use = Industry_NAICS6_Make)],
                   by = "Industry_NAICS6_Use",
                   all = TRUE,
                   allow.cartesian = TRUE)

  # If there are Industries with no consumption value (i.e., no consumption in IO table):
  consval[is.na(ProVal), ProVal := 0]
  
  # Consumption value per employee (in Million of Dollars)
  consval[, ValEmp := ProVal / Emp]

  # Update the IO table with ValEmp
  io[consval, ValEmp := i.ValEmp, on = c("Industry_NAICS6_Use", "Industry_NAICS6_Make")]

  ### Foreign Consumers
  
  # Foreign consumption is currently described in terms of the commodity being exported
  # Join to the IO table to simulate the types of consuming firms that use those commodities
  # Create a set of appropriately sized individual agents to use in the simulation
  
  io[, PctExpVal := ExpVal / sum(ExpVal), by = Industry_NAICS6_Make]
  io[is.na(PctExpVal), PctExpVal:= DomProdVal/sum(DomProdVal), by = Industry_NAICS6_Make]
  setkey(io,  Industry_NAICS6_Make)
  
  consumers.foreign <- merge(FirmsForeign[FirmType == "ForeignConsumer"],
                             io[, .(Industry_NAICS6_Make, Industry_NAICS6_Use, PctExpVal, IsBelowThreshold)],
                             by = "Industry_NAICS6_Make",
                             allow.cartesian = TRUE)
  
  # Calculate the purchase amount and convert to tons needed - this is consumption value
  # Value was in $M, convert to $
  consumers.foreign[, ConVal := ProVal * PctExpVal * 1000000]
  
  # Convert purchase value from $M to Tons
  consumers.foreign[unitcost, UnitCost := i.UnitCost, on = "Commodity_SCTG"]
  
  # Calculate purchase amount required by weight
  consumers.foreign[, PurchaseAmountTons := ConVal / UnitCost]
  
  # Enumerate large foreign consumers into multiple firms (threshold reduced)
  # Number of firms to create
  consumers.foreign[, est := ceiling(PurchaseAmountTons / BASE_FOREIGN_FIRM_SIZE_LIMIT)]
  
  # Update ProdVal and ConVal
  consumers.foreign[est > 1, c("ProVal", "ConVal") := .(ProVal / est, ConVal / est)]
  
  # Enumerate the foreign consumers using the est variable
  consumers.foreign <- consumers.foreign[rep(seq_len(consumers.foreign[, .N]), est)]
  consumers.foreign[, est := NULL]
  
  # Calculate other fields required in consumers tables
  consumers.foreign[, Mesozone := CBPZONE + 150L]
  
  # Add foreign consumers on after the foreign producers
  consumers.foreign[,BusID:= maxbusid + .I]
  
  # Don't know buyer output SCTG commodity or employment of buying firm
  consumers.foreign[, Buyer.SCTG := 0L]
  consumers.foreign[, Emp := 0L]
  
  # Add output commodity
  consumers.foreign[, OutputCommodity := Industry_NAICS6_Use]
  
  # Remove extra fields
  consumers.foreign[,c("CBPZONE","ProVal","PctExpVal", "UnitCost", "FirmType"):= NULL]
  
  # Rename fields
  setnames(consumers.foreign,
           c("BusID","Mesozone","Industry_NAICS6_Make","Industry_NAICS6_Use","Emp"),
           c("BuyerID","Zone","InputCommodity","NAICS","Size"))
  
  # Split the records into those for simulation in the PMG and those
  # for simpler simulation based on the consumption threshold
  consumers.foreign.pmg <- consumers.foreign[IsBelowThreshold == TRUE][,IsBelowThreshold := NULL]
  consumers.foreign.incremental <- consumers.foreign[IsBelowThreshold == FALSE][,IsBelowThreshold := NULL]
  
  rm(consumers.foreign)
  
  # To simplify simulation, remove very small consumption requirements by bucket rounding the PurchaseAmountTons
  # and removing any resulting zero PurchaseAmountTons
  # Round by NAICS, InputCommodity, Commodity_SCTG, to maintain commodity/NAICS distribution 
  # Round by FAF zone so that spatial distribution by FAF zone is maintained
  consumers.foreign.pmg[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = .(NAICS, InputCommodity, Commodity_SCTG, FAFZONE)]
  consumers.foreign.pmg <- consumers.foreign.pmg[PurchaseAmountTons > 0.5]
  
  consumers.foreign.incremental[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = .(NAICS, InputCommodity, Commodity_SCTG, FAFZONE)]
  consumers.foreign.incremental <- consumers.foreign.incremental[PurchaseAmountTons > 0.5]
  
  ### Domestic Consumers 
  
  # Domestic consumers including wholesalers
  # (they are treated the same as other domestic businesses on the consumption side)
  
  # Some NAICS6-Make industries (NAICS6_Make) make more than one SCTG.
  # Account for this by simulating the SCTG commodity supplied by them based on probability thresholds
  mult_n6make <- unique(c_n6_n6io_sctg[Commodity_SCTG > 0 & Proportion < 1, 
                                       .(Industry_NAICS6_Make, Commodity_SCTG, Proportion)])
  
  assign_mult_sctg <- function(n6m){
    sample(mult_n6make$Commodity_SCTG[mult_n6make$Industry_NAICS6_Make == n6m],
           n6m_samp$N[n6m_samp$Industry_NAICS6_Make == n6m],
           replace = TRUE,
           prob = mult_n6make$Proportion[mult_n6make$Industry_NAICS6_Make == n6m])
  }
  
  # Loop on employment category
  empcats <- sort(unique(FirmsDomestic$EmpCatName))
  consumers_domestic_pmg <- list()
  consumers_domestic_incremental <- list()
  
  for(empcat in empcats){
  
    # Merge consumers.domestic with IO data
    consumers.domestic <- merge(io[,.(Industry_NAICS6_Use, Industry_NAICS6_Make, IsBelowThreshold, ValEmp)],
                                FirmsDomestic[EmpCatName == empcat, 
                                              .(Industry_NAICS6_Use = Industry_NAICS6_Make, 
                                                FAFZONE, Mesozone, 
                                                Buyer.SCTG = Commodity_SCTG,
                                                EmpCatName, EmpCatGroupedName,
                                                Emp, BusID, FirmType)],
                                by = "Industry_NAICS6_Use",
                                allow.cartesian = TRUE)
  
    # Merge in the first matching SCTG code for the commodity being consumed
    consumers.domestic[c_n6_n6io_sctg[!duplicated(Industry_NAICS6_Make), .(Industry_NAICS6_Make, Commodity_SCTG)],
                       Commodity_SCTG := i.Commodity_SCTG,
                       on = "Industry_NAICS6_Make"]
    
    # Simulate SCTG
    setkey(consumers.domestic,Industry_NAICS6_Make)
    
    n6m_samp <- consumers.domestic[unique(mult_n6make$Industry_NAICS6_Make)][, .N, by = Industry_NAICS6_Make]
    
    set.seed(BASE_SEED_VALUE)
    
    for (i in 1:nrow(n6m_samp)){
      consumers.domestic[n6m_samp$Industry_NAICS6_Make[i], 
                         Commodity_SCTG := assign_mult_sctg(n6m_samp$Industry_NAICS6_Make[i])]
    }
    
    # Calculate the purchase amount and convert to tons needed - this is consumption value
    # Value was in $M, convert to $
    consumers.domestic[, ConVal := ValEmp * Emp * 1000000]
    
    # Convert purchase value from $M to Tons
    consumers.domestic[unitcost, UnitCost := i.UnitCost, on = "Commodity_SCTG"]
    
    # Calculate purchase amount required by weight
    consumers.domestic[, PurchaseAmountTons := ConVal / UnitCost]
    
    # Add output commodity
    consumers.domestic[, OutputCommodity := Industry_NAICS6_Use]
    
    # Remove extra fields
    consumers.domestic[, c("ValEmp","UnitCost", "FirmType") := NULL]
    
    # Rename fields
    setnames(consumers.domestic,
             c("BusID","Mesozone","Industry_NAICS6_Make","Industry_NAICS6_Use","Emp"),
             c("BuyerID","Zone","InputCommodity","NAICS","Size"))
    
    # Split the records into those for simulation in the PMG and those
    # for simpler simulation based on the consumption threshold
    consumers.domestic.pmg <- consumers.domestic[IsBelowThreshold == TRUE][,IsBelowThreshold := NULL]
    consumers.domestic.incremental <- consumers.domestic[IsBelowThreshold == FALSE][,IsBelowThreshold := NULL]
    
    # To simplify simulation, remove very small consumption requirements by bucket rounding the PurchaseAmountTons
    # and removing any resulting zero PurchaseAmountTons
    # Round by NAICS, InputCommodity, Commodity_SCTG, to maintain commodity/NAICS distribution 
    # Round by FAF zone so that spatial distribution by FAF zone is maintained
    consumers.domestic.pmg[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = .(NAICS, InputCommodity, Commodity_SCTG, FAFZONE)]
    consumers.domestic.pmg <- consumers.domestic.pmg[PurchaseAmountTons > 0.5]
    
    consumers.domestic.incremental[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = .(NAICS, InputCommodity, Commodity_SCTG, FAFZONE)]
    consumers.domestic.incremental <- consumers.domestic.incremental[PurchaseAmountTons > 0.5]
    
    # Add the tables for this empcat to the lists
    consumers_domestic_pmg[[as.character(empcat)]] <- consumers.domestic.pmg
    consumers_domestic_incremental[[as.character(empcat)]] <- consumers.domestic.incremental
      
    print(paste0("EmpCat=", empcat, 
                 " PMG_Rows=",consumers_domestic_pmg[[as.character(empcat)]][,.N],
                 " Incremental_Rows=",consumers_domestic_incremental[[as.character(empcat)]][,.N]))
  }
  
  rm(consumers.domestic, consumers.domestic.pmg, consumers.domestic.incremental, 
     mult_n6make, n6m_samp)
  gc()
  
  # Create the combined consumers table of those input requirements that will be processed in the PMG -- below threshold
  # and a separate table that contains the remaining above threshold inputs for simpler modeling
  consumers.pmg <- rbind(rbindlist(consumers_domestic_pmg)[, ConsType := 1],
                         consumers.foreign.pmg[, ConsType := 2],
                         use.names = TRUE,
                         fill = TRUE)
  
  rm(consumers_domestic_pmg, consumers.foreign.pmg)
  
  # Combine and save the incremental table of above threshold consumption
  consumers.incremental <- rbind(rbindlist(consumers_domestic_incremental)[, ConsType := 1],
                                   consumers.foreign.incremental[, ConsType := 2],
                                   use.names = TRUE,
                                   fill = TRUE)
    
  rm(consumers_domestic_incremental, consumers.foreign.incremental)
    
  consumers_incremental_summary <- consumers.incremental[, .(Requirements = sum(PurchaseAmountTons)), 
                                                                  keyby = .(Commodity_SCTG, ConsType, 
                                                                            EmpCatName, EmpCatGroupedName,
                                                                            Zone, FAFZONE)]
  
  if(writeConsumersIncremental){
    
    write_fst(consumers.incremental, 
              path = file.path(SCENARIO_OUTPUT_PATH, "consumers_incremental.fst"))

    rm(consumers.incremental)

  }
   
  gc()

  # Add preference weights to pmg consumers
  consumers.pmg[prefweights,
                c("PrefWeight1_UnitCost", "PrefWeight2_ShipTime", "SingleSourceMaxFraction") :=
                  .(i.CostWeight, i.TimeWeight, i.SingleSourceMaxFraction),
                on = "Commodity_SCTG"]

  # Key on input commodity
  setkey(consumers.pmg, InputCommodity)

  # Return the consumers table
  return(list(consumers = consumers.pmg,
              consumers_incremental_summary = consumers_incremental_summary))

}
