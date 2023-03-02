#Create consumers database
firm_synthesis_consumers <- function(io, wholesalers, FirmsDomestic, FirmsForeign, c_n6_n6io_sctg, unitcost, maxbusid, writeConsumersIncremental = TRUE){

  # For each firm generate a list of input commodities that need to be purchased (commodity code, amount)

  # All agents that consume some SCTG commodity are potential consumers
  # Domestic Consumers
  consumers.domestic <- FirmsDomestic[substr(Industry_NAICS6_Make, 1, 2) != "42",]

  # Domestic Wholesalers (previously processed as producers)
  consumers.wholesalers <- wholesalers

  # Foreign Consumers
  consumers.foreign <- FirmsForeign[FirmType == "ForeignConsumer"]

  # Calculate value per employee required
  # In the consumers table Use code is that of the consuming firm
  setnames(consumers.domestic,"Industry_NAICS6_Make","Industry_NAICS6_Use")

  consval <- merge(io[, .(ProVal = sum(ProVal)), by = .(Industry_NAICS6_Make, Industry_NAICS6_Use)],
                   consumers.domestic[, .(Emp = sum(Emp)), by = Industry_NAICS6_Use],
                   by = "Industry_NAICS6_Use",
                   all = TRUE,
                   allow.cartesian = TRUE)

  # If there are Industries with no consumption value (i.e., no consumption in IO table):
  consval[is.na(ProVal), ProVal := 0]
  fwrite(consval[ProVal == 0], file = file.path(SCENARIO_OUTPUT_PATH, "Consumers_ZeroProVal.csv"))
  fwrite(consval[is.na(Emp), .(ProVal = sum(ProVal)), by = Industry_NAICS6_Use], file = file.path(SCENARIO_OUTPUT_PATH, "Consumers_NoEmp.csv"))

  # Consumption value per employee (in Million of Dollars)
  consval[, ValEmp := ProVal / Emp]

  # Update the IO table with ValEmp
  io[consval, ValEmp := i.ValEmp, on = c("Industry_NAICS6_Use", "Industry_NAICS6_Make")]

  # Merge consumers.domestic with IO data
  consumers.domestic <- merge(io[,.(Industry_NAICS6_Use, Industry_NAICS6_Make, IsBelowThreshold, ValEmp)],
                              consumers.domestic[, .(Industry_NAICS6_Use, FAFZONE, Mesozone, Buyer.SCTG = Commodity_SCTG, Emp, BusID, FirmType)],
                              by = "Industry_NAICS6_Use",
                              allow.cartesian = TRUE)

  # Merge in the first matching SCTG code for the commodity being consumed
  consumers.domestic[c_n6_n6io_sctg[!duplicated(Industry_NAICS6_Make), .(Industry_NAICS6_Make, Commodity_SCTG)],
                     Commodity_SCTG := i.Commodity_SCTG,
                     on = "Industry_NAICS6_Make"]

  # Some Naics6-Make industries (NAICS6_Make) make more than one SCTG.
  # Account for this by simulating the SCTG commodity supplied by them based on probability thresholds
  mult_n6make <- unique(c_n6_n6io_sctg[Commodity_SCTG > 0 & Proportion < 1, .(Industry_NAICS6_Make, Commodity_SCTG, Proportion)])

  setkey(consumers.domestic,Industry_NAICS6_Make)

  n6m_samp <- consumers.domestic[unique(mult_n6make$Industry_NAICS6_Make)][, .N, by = Industry_NAICS6_Make]

  assign_mult_sctg <- function(n6m){
    sample(mult_n6make$Commodity_SCTG[mult_n6make$Industry_NAICS6_Make == n6m],
           n6m_samp$N[n6m_samp$Industry_NAICS6_Make == n6m],
           replace = TRUE,
           prob = mult_n6make$Proportion[mult_n6make$Industry_NAICS6_Make == n6m])
  }

  for (i in 1:nrow(n6m_samp)){
    consumers.domestic[n6m_samp$Industry_NAICS6_Make[i], Commodity_SCTG := assign_mult_sctg(n6m_samp$Industry_NAICS6_Make[i])]
  }

  rm(mult_n6make, n6m_samp)

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

  # Foreign consumption
  io[, PctProVal := ProVal / sum(ProVal), by = Industry_NAICS6_Make]
  setkey(io,  Industry_NAICS6_Make)

  consumers.foreign <- merge(consumers.foreign,
                             io[, .(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal, PctProVal, IsBelowThreshold)],
                             by = "Industry_NAICS6_Make",
                             allow.cartesian = TRUE)

  # Calculate the purchase amount and convert to tons needed - this is consumption value
  # Value was in $M, convert to $
  consumers.foreign[, ConVal := ProdVal * PctProVal * 1000000]

  # Convert purchase value from $M to Tons
  consumers.foreign[unitcost, UnitCost := i.UnitCost, on = "Commodity_SCTG"]

  # Calculate purchase amount required by weight
  consumers.foreign[, PurchaseAmountTons := ConVal / UnitCost]

  # Enumerate large foreign consumers into multiple firms (threshold reduced)
  # Number of fims to create
  consumers.foreign[, est := ceiling(PurchaseAmountTons / BASE_FOREIGN_FIRM_SIZE_LIMIT)]

  # Update ProdVal and ConVal
  consumers.foreign[est > 1, c("ProdVal", "ConVal") := .(ProdVal / est, ConVal / est)]

  # Enumerate the foreign consumers using the est variable
  consumers.foreign <- consumers.foreign[rep(seq_len(consumers.foreign[, .N]), est)]
  consumers.foreign[, est := NULL]

  # Calculate other fields required in consumers tables
  consumers.foreign[, Mesozone := CBPZONE + 150L]

  # Add foreign consumers on after the foreign producers
  consumers.foreign[,BusID:= maxbusid + .I]

  # Don't know buyer industry or employment of buying firm
  consumers.foreign[, Buyer.SCTG := 0L]
  consumers.foreign[, Emp := 0L]

  # Add output commodity
  consumers.foreign[, OutputCommodity := Industry_NAICS6_Use]

  # Remove extra fields
  consumers.foreign[,c("CBPZONE","ProdVal","ProVal","PctProVal", "UnitCost", "FirmType"):= NULL]

  # Rename fields
  setnames(consumers.foreign,
           c("BusID","Mesozone","Industry_NAICS6_Make","Industry_NAICS6_Use","Emp"),
           c("BuyerID","Zone","InputCommodity","NAICS","Size"))

  # Process the wholesalers into format for combining with other consumers
  consumers.wholesalers[, ConVal := OutputCapacityTons * NonTransportUnitCost / BASE_WHOLESALE_COST_FACTOR]

  # Remove extra fields
  consumers.wholesalers[, c("NonTransportUnitCost", "FirmType", "TAZ") := NULL]

  # Rename fields
  setnames(consumers.wholesalers, c("SellerID","OutputCommodity","OutputCapacityTons", "ProdType"), c("BuyerID","InputCommodity","PurchaseAmountTons", "ConsType"))

  # Add extra fields: wholesalers are reselling inputs
  consumers.wholesalers[, Buyer.SCTG := Commodity_SCTG]
  consumers.wholesalers[, OutputCommodity := InputCommodity]

  # Create the combined consumers table of those input requirements that will be processed in the PMG -- below threshold
  # and a seperate table that contains the remaining above threshold inputs for simpler modeling
  consumers.pmg <- rbind(consumers.domestic[IsBelowThreshold == TRUE][,IsBelowThreshold := NULL][, ConsType := 1],
                         consumers.foreign[IsBelowThreshold == TRUE][,IsBelowThreshold := NULL][, ConsType := 2],
                         consumers.wholesalers,
                         use.names = TRUE,
                         fill = TRUE)
  gc()
  
  # To simplify simulation, remove very small consumption requirements by bucket rounding the PurchaseAmountTons
  # and removing any resulting PurchaseAmountTons
  # Round by NAICS, InputCommodity, Commodity_SCTG, to maintain commodity/NAICS distribution 
  # Round by FAF zone so that spatial distribution by FAF zone is maintained
  consumers.pmg[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = .(NAICS, InputCommodity, Commodity_SCTG, FAFZONE)]
  consumers.pmg <- consumers.pmg[PurchaseAmountTons > 0.5]
  
  # reduce the size of domestic and then combine with foreign
  # this is optional for testing purposes to avoid waiting for this part to complete
  if(writeConsumersIncremental){

    consumers.incremental <- consumers.domestic
    rm(consumers.domestic)
    gc()

    consumers.incremental <- consumers.incremental[IsBelowThreshold == FALSE][,IsBelowThreshold := NULL][, ConsType := 1]

    consumers.incremental <- rbind(consumers.incremental,
                                   consumers.foreign[IsBelowThreshold == FALSE][,IsBelowThreshold := NULL][, ConsType := 2],
                                   use.names=TRUE)
    
    consumers.incremental[, PurchaseAmountTons := as.numeric(bucketRound(PurchaseAmountTons)), by = .(NAICS, InputCommodity, Commodity_SCTG, FAFZONE)]
    consumers.incremental <- consumers.incremental[PurchaseAmountTons > 0.5]
    
    save(consumers.incremental, file = file.path(SCENARIO_OUTPUT_PATH, "consumers_incremental.Rdata"))
    rm(consumers.incremental)
    gc()

  } else {

    rm(consumers.domestic)
    gc()

  }

  # Add preference weights to pmg consumers
  consumers.pmg[prefweights,
                c("PrefWeight1_UnitCost", "PrefWeight2_ShipTime", "SingleSourceMaxFraction") :=
                  .(i.CostWeight, i.TimeWeight, i.SingleSourceMaxFraction),
                on = "Commodity_SCTG"]

  # Key on input commodity
  setkey(consumers.pmg, InputCommodity)

  # Return the consumers table
  return(consumers.pmg)

}
