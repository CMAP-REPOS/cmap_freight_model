# Create producers/suppliers database
firm_synthesis_producers <- function(io, fromwhl, FirmsDomestic, FirmsForeign, unitcost, EstSizeCategories){

  # All agents that produce some SCTG commodity become potential producers
  # Domestic Producers
  producers.domestic <- FirmsDomestic[Commodity_SCTG > 0 & substr(Industry_NAICS6_Make, 1, 2) != "42",]

  # Domestic Wholesalers
  producers.wholesalers <- FirmsDomestic[substr(Industry_NAICS6_Make, 1, 2) == "42",]

  # Foreigh Producers
  producers.foreign <- FirmsForeign[FirmType == "ForeignProducer"]

  # Summarize wholesalers capacity according to the from wholesale requirements calculated above
  whlval <- fromwhl[,.(ProVal = sum(ProValWhl)), by = .(Industry_NAICS6_Make = NAICS_whl)]

  # Add on the employment by industry
  whlval[producers.wholesalers[, .(Emp = sum(Emp)), by = Industry_NAICS6_Make],
         Emp := i.Emp,
         on = "Industry_NAICS6_Make"]

  # Production value per employee
  whlval[, ValEmp := ProVal / Emp]

  # Merge the value per employee back on to list of wholesalers
  producers.wholesalers[whlval,
              ValEmp := i.ValEmp,
              on = "Industry_NAICS6_Make"]

  # Poduction value for each wholesale establishment
  producers.wholesalers[, ProdVal := Emp * ValEmp]

  # Domestic producers production value
  prodval <- merge(io[, .(ProVal = sum(ProVal)), by = Industry_NAICS6_Make],
                   producers.domestic[, .(Emp = sum(Emp)), by = Industry_NAICS6_Make],
                   by = "Industry_NAICS6_Make",
                   all = TRUE)

  # If there are Industries with no production value (i.e., no production in IO table):
  prodval[is.na(ProVal), ProVal := 0]
  fwrite(prodval[ProVal == 0], file = file.path(SCENARIO_OUTPUT_PATH, "Producers_ZeroProVal.csv"))

  # Production value per employee (in Million of Dollars)
  prodval[, ValEmp := ProVal / Emp]

  # Merge the value per employee back on to producers
  producers.domestic[prodval,
            ValEmp := i.ValEmp,
            on = "Industry_NAICS6_Make"]

  # Production value for each establishment (in Million of Dollars)
  producers.domestic[, ProdVal := Emp * ValEmp]

  # Add on the value per employee based on US firms to foreign firms
  producers.foreign[prodval,
           ValEmp := i.ValEmp,
           on ="Industry_NAICS6_Make"]

  # Remove any missing/zero value industries
  producers.foreign[is.na(ValEmp), ValEmp := 0]

  fwrite(producers.foreign[ValEmp == 0, .(ProdVal = sum(ProdVal), Countries = .N),
                           by = .(FirmType, Industry_NAICS6_Make, Commodity_SCTG)],
         file = file.path(SCENARIO_OUTPUT_PATH, "Producers_Foreign_ZeroValEmp.csv"))

  producers.foreign <- producers.foreign[ValEmp != 0]

  # Update ValEmp using foreign producer adjustment
  # same adjustment applied to unitcost, so assumption is that quantity per employee is the same as domestic production
  producers.foreign[, ValEmp:= ValEmp * BASE_FOREIGN_PROD_COST_FACTOR]

  # Estimate employment and size category
  producers.foreign[, Emp := pmax(round(ProdVal / ValEmp), 1)]
  producers.foreign[, esizecat := findInterval(Emp, EstSizeCategories$LowerBound)]

  # Add on units costs
  producers.foreign[unitcost,
           UnitCost := i.UnitCost,
           on = "Commodity_SCTG"]

  # Update unit cost using foreign producer adjustment
  producers.foreign[, UnitCost := UnitCost * BASE_FOREIGN_PROD_COST_FACTOR]

  # Prduction capacity (ProdVal was in $M)
  producers.foreign[, ProdCap := ProdVal * 1000000 / UnitCost]

  # Enumerate large foreign producers into multiple firms (threshold reduced)
  # Number of fims to create
  producers.foreign[, est := ceiling(ProdCap / BASE_FOREIGN_FIRM_SIZE_LIMIT)]

  # Update ProdVal and ProdCap
  producers.foreign[est > 1, c("ProdVal", "ProdCap") := .(ProdVal / est, ProdCap / est)]

  # Enumerate the foreign producers using the est variable
  producers.foreign <- producers.foreign[rep(seq_len(producers.foreign[, .N]), est)]
  producers.foreign[, est := NULL]

  # calculate other fields required in producers tables
  producers.foreign[, Mesozone := CBPZONE + 150L]
  producers.foreign[, TAZ := as.numeric(NA)]
  producers.foreign[, BusID := max(FirmsDomestic$BusID) + .I]

  # Add Output commodity
  producers.foreign[, OutputCommodity := Industry_NAICS6_Make]

  # Add on unit cost to domestic producers table
  producers.domestic[unitcost,
            UnitCost := i.UnitCost,
            on = "Commodity_SCTG"]

  # Production capacity (ProdVal was in $M)
  producers.domestic[, ProdCap := ProdVal * 1000000 / UnitCost]

  # Add Output commodity
  producers.domestic[, OutputCommodity := Industry_NAICS6_Make]

  # Add on unit cost to wholesalers table
  producers.wholesalers[unitcost,
              UnitCost := i.UnitCost,
              on = "Commodity_SCTG"]

  # factor up unitcost to reflect wholesalers margin
  producers.wholesalers[, UnitCost := UnitCost * BASE_WHOLESALE_COST_FACTOR]

  # Production capacity (ProdVal was in $M)
  producers.wholesalers[, ProdCap := ProdVal * 1000000 / UnitCost]

  # Simulate the single specific NAICS commodity that the wholesaler deals in to simplify
  # (wholesale NAICS are one to many NAICS commodities)
  # Each wholesale firm is identified with a specific NAICS and SCTG
  # Need probabilities for the match with NAICS commodity
  whlnaics <- fromwhl[, .(ProValWhl = sum(ProValWhl)),
                      by = .(Industry_NAICS6_Make, SCTG, NAICS = NAICS_whl)]

  whlnaics[, ProbProValWhl := ProValWhl / sum(ProValWhl), by = .(SCTG, NAICS)]

  setkey(whlnaics, NAICS, SCTG)

  whlnaics[, CumProValWhl := cumsum(ProbProValWhl), by = .(SCTG, NAICS)]

  whlnaicscombs <- unique(whlnaics[, .(NAICS, SCTG)])

  producers.wholesalers[, temprand := runif(.N)]

  for(i in 1:nrow(whlnaicscombs)){

    whlnaicsi <- whlnaics[NAICS == whlnaicscombs$NAICS[i] & SCTG == whlnaicscombs$SCTG[i]]

    producers.wholesalers[Industry_NAICS6_Make == whlnaicscombs$NAICS[i] & Commodity_SCTG == whlnaicscombs$SCTG[i],
                OutputCommodity := whlnaicsi$Industry_NAICS6_Make[1 + findInterval(temprand, whlnaicsi$CumProValWhl)]]

  }

  producers.wholesalers[, temprand := NULL]


  fwrite(producers.wholesalers[is.na(OutputCommodity),
                               .(ProdCap = sum(ProdCap), Firms = .N),
                               by = .(Industry_NAICS6_Make, Commodity_SCTG)],
         file = file.path(SCENARIO_OUTPUT_PATH, "Producers_Wholesalers_NoOutputCommodity.csv"))

  #TODO - clean up correspondences to avoid no matches here
  producers.wholesalers <- producers.wholesalers[!is.na(OutputCommodity)]

  # combine domestic producers, foreign producers, and wholesalers
  # Add tag for producer type - 1 = domestic non-wholesalers, 2 = foreign producer
  producers <- rbind(producers.domestic[, ProdType := 1],
                     producers.foreign[, ProdType := 2],
                     producers.wholesalers[, ProdType := 3],
                     use.names = TRUE,
                     fill = TRUE)
  
  # To simplify simulation, remove very small producers by bucket rounding the ProdCap and removing any resulting 0 tons producers
  # Round by Industry_NAICS6_Make, Commodity_SCTG, and OutputCommodity to maintain commodity/NAICS distribution 
  # Round by FAF zone so that spatial distribution by FAF zone is maintained
  # Remove zero value producers rounding
  producers[, ProdCap := as.numeric(bucketRound(ProdCap)), by = .(Industry_NAICS6_Make, Commodity_SCTG, OutputCommodity, FAFZONE)]
  producers <- producers[ProdCap > 0.5]
  
  # Prepare for Writing out a producers file for each NAICS, with each firm represented by:
  # SellerID (BusID)  Zone (Mesozone)	NAICS (NAICS6_Make)	Size (Emp)	OutputCommodity (SCTG_Make)	OutputCapacityTons (ProdCap)	NonTransportUnitCost (UnitCost)
  producers[, c("CBPZONE", "esizecat", "ProdVal", "ValEmp") := NULL]

  setnames(producers,
           c("BusID", "Mesozone", "Industry_NAICS6_Make", "Emp", "ProdCap", "UnitCost"),
           c("SellerID", "Zone", "NAICS", "Size", "OutputCapacityTons", "NonTransportUnitCost"))

  setkey(producers, OutputCommodity)

  # Return the producers table
  return(producers)

}
