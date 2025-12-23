# Create producers/suppliers database
firm_synthesis_producers <- function(io, fromwhl_make_use, FirmsDomestic, FirmsForeign, unitcost, EstSizeCategories){

  # All agents that produce some SCTG commodity become potential producers
  # Domestic Producers
  producers.domestic <- FirmsDomestic[Commodity_SCTG > 0 & EmpCatName != 42,]

  # Domestic Wholesalers
  producers.wholesalers <- FirmsDomestic[EmpCatName == 42,]

  # Foreign Producers
  producers.foreign <- FirmsForeign[FirmType == "ForeignProducer"]
  setnames(producers.foreign, "ProVal", "ProdVal")
  
  # Summarize wholesalers capacity according to the from wholesale requirements calculated above
  whlval <- fromwhl_make_use[!is.na(ProValWhl),.(ProVal = sum(ProValWhl)), 
                             by = .(Industry_NAICS6_Make = NAICS_Whl)]

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

  # Production value for each wholesale establishment
  producers.wholesalers[, ProdVal := Emp * ValEmp]

  # Domestic producers production value
  prodval <- merge(io[, .(ProVal = sum(DomProdValNoWhl)), 
                      by = Industry_NAICS6_Make],
                   producers.domestic[, .(Emp = sum(Emp)), by = Industry_NAICS6_Make],
                   by = "Industry_NAICS6_Make",
                   all = TRUE)

  # If there are Industries with no production value (i.e., no production in IO table):
  prodval[is.na(ProVal), ProVal := 0]
  
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

  # For any missing industries (in foreign, not in domestic), add an average value
  producers.foreign[is.na(ValEmp), ValEmp := mean(producers.domestic$ValEmp)]

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

  # Production capacity (ProdVal was in $M)
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
  producers.foreign[, modelregion := 3]

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
  whlnaics <- fromwhl_make_use[!is.na(ProValWhl), .(ProValWhl = sum(ProValWhl)),
                      keyby = .(Commodity_SCTG, NAICS_Whl, Industry_NAICS6_Make)]

  whlnaics[, ProbProValWhl := ProValWhl / sum(ProValWhl), 
           by = .(Commodity_SCTG, NAICS_Whl)]

  whlnaics[, CumProValWhl := cumsum(ProbProValWhl), 
           by = .(Commodity_SCTG, NAICS_Whl)]

  whlnaicscombs <- unique(whlnaics[, .(Commodity_SCTG, NAICS_Whl)])

  set.seed(BASE_SEED_VALUE)
  
  producers.wholesalers[, temprand := runif(.N)]

  for(i in 1:nrow(whlnaicscombs)){

    whlnaicsi <- whlnaics[NAICS_Whl == whlnaicscombs$NAICS_Whl[i] 
                          & Commodity_SCTG == whlnaicscombs$Commodity_SCTG[i]]

    producers.wholesalers[Industry_NAICS6_Make == whlnaicscombs$NAICS_Whl[i] 
                          & Commodity_SCTG == whlnaicscombs$Commodity_SCTG[i],
                OutputCommodity := whlnaicsi$Industry_NAICS6_Make[1 + findInterval(temprand, whlnaicsi$CumProValWhl)]]

  }

  producers.wholesalers[, temprand := NULL]

  producers.wholesalers <- producers.wholesalers[!is.na(OutputCommodity)]

  # combine domestic producers, foreign producers, and wholesalers
  # Add tag for producer type - 1 = domestic non-wholesalers, 2 = foreign producer
  producers <- rbind(producers.domestic[, ProdType := 1],
                     producers.foreign[, ProdType := 2],
                     producers.wholesalers[, ProdType := 3],
                     use.names = TRUE,
                     fill = TRUE)
  
  # Copy prior to further processing for review
  producers_all <- copy(producers)
  
  # To simplify simulation, remove very small producers by bucket rounding the ProdCap and removing any resulting 0 tons producers
  # Round by Industry_NAICS6_Make, Commodity_SCTG, and OutputCommodity to maintain commodity/NAICS distribution 
  # Round by FAF zone so that spatial distribution by FAF zone is maintained
  # Remove zero value producers rounding
  producers[, ProdCap := as.numeric(bucketRound(ProdCap)), 
            by = .(Industry_NAICS6_Make, Commodity_SCTG, OutputCommodity, FAFZONE)]
  producers <- producers[ProdCap > 0.5]
  
  # Prepare for Writing out a producers file for each NAICS, with each firm represented by:
  # SellerID (BusID)  Zone (Mesozone)	NAICS (NAICS6_Make)	Size (Emp)	OutputCommodity (SCTG_Make)	
  # OutputCapacityTons (ProdCap)	NonTransportUnitCost (UnitCost)
  producers[, c("CBPZONE", "esizecat", "ProdVal", "ValEmp") := NULL]

  setnames(producers,
           c("BusID", "Mesozone", "Industry_NAICS6_Make", "Emp", "ProdCap", "UnitCost"),
           c("SellerID", "Zone", "NAICS", "Size", "OutputCapacityTons", "NonTransportUnitCost"))

  setkey(producers, OutputCommodity)

  # Return the producers table
  return(list(producers = producers,
              producers_all = producers_all))

}
