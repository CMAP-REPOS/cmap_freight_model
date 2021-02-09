sc_sim_increment <- function(BuyerSupplierPairs){
  
  # Load the file saved during firm synthesis
  load(file = file.path(SCENARIO_OUTPUT_PATH, "consumers_incremental.Rdata"))
 
  setnames(consumers.incremental,
           c("NAICS", "Zone"),
           c("Buyer.NAICS", "Consumption_zone"))
  setkey(consumers.incremental, Buyer.NAICS, Commodity_SCTG, Consumption_zone)
  
  # Summarize categories for Match Category 1:
  # Buyer.NAICS, Commodity_SCTG, Consumption_zone 
  x1 <- BuyerSupplierPairs[, .(Count = .N, 
                               Pctl25 = quantile(as.integer(Last.Iteration.Quantity),c(.25)),
                               Pctl50 = quantile(as.integer(Last.Iteration.Quantity),c(.50)),
                               Pctl75 = quantile(as.integer(Last.Iteration.Quantity),c(.75))),
                           keyby = .(Buyer.NAICS, Commodity_SCTG, Consumption_zone)]
  
  # Summarize categories for Match Category 2:
  # Buyer.NAICS, Commodity_SCTG 
  x2 <- BuyerSupplierPairs[, .(Count = .N,
                               Pctl252 = quantile(as.integer(Last.Iteration.Quantity),c(.25)),
                               Pctl502 = quantile(as.integer(Last.Iteration.Quantity),c(.50)),
                               Pctl752 = quantile(as.integer(Last.Iteration.Quantity),c(.75))), 
                           keyby = .(Buyer.NAICS, Commodity_SCTG)]
  
  # Attach Data to commodity flows
  setkey(BuyerSupplierPairs, Buyer.NAICS, Commodity_SCTG, Consumption_zone)
  BuyerSupplierPairs[, wgtCat := 1 + findInterval(Last.Iteration.Quantity, x1[.BY,.(Pctl25, Pctl50, Pctl75)]),
                     by = c("Buyer.NAICS", "Commodity_SCTG", "Consumption_zone")]
  
  x1b <- BuyerSupplierPairs[, .(Count = .N), keyby = .(Buyer.NAICS, Commodity_SCTG, Consumption_zone, wgtCat)]
  
  consumers.incremental[x1, Count := i.Count]
  consumers.incremental[!is.na(Count), wgtCat := 1 + findInterval(PurchaseAmountTons, x1[.BY,.(Pctl25, Pctl50, Pctl75)]),
                        by = c("Buyer.NAICS", "Commodity_SCTG", "Consumption_zone")]
  consumers.incremental[, Count := NULL]
  
  # check for availability in Pairs data
  setkey(consumers.incremental, Buyer.NAICS, Commodity_SCTG, Consumption_zone, wgtCat)
  consumers.incremental[x1b, Count := i.Count]
  consumers.incremental[!is.na(Count), MatchCat := 1]
  consumers.incremental[, Count := NULL]
  
  # Develop weight categories (Match category 2)
  # Attach Data to commodity flows
  setkey(BuyerSupplierPairs, Buyer.NAICS, Commodity_SCTG)
  BuyerSupplierPairs[, wgtCat2 := 1 + findInterval(Last.Iteration.Quantity, x2[.BY,.(Pctl252, Pctl502, Pctl752)]),
                     by = c("Buyer.NAICS", "Commodity_SCTG")]
  x2b <- BuyerSupplierPairs[, .(Count = .N), by = .(Buyer.NAICS, Commodity_SCTG, wgtCat2)]
  
  # Get all weight categories existing in Pairs data
  x2c <- x2b[, .(Count = sum(Count), wgtCatRep = .(wgtCat2)), keyby = .(Buyer.NAICS, Commodity_SCTG)]	
  
  # Attach to incremental consumers to determine categories
  setkey(consumers.incremental, Buyer.NAICS, Commodity_SCTG)
  consumers.incremental[x2, Count := i.Count]
  consumers.incremental[!is.na(Count) & MatchCat != 1, c("MatchCat", "wgtCat") := .(2, 1 + findInterval(PurchaseAmountTons, x2[.BY,.(Pctl252, Pctl502, Pctl752)]),
                        by = c("Buyer.NAICS", "Commodity_SCTG"))]
  consumers.incremental[, Count := NULL]
  
  # Function to Find Closest Available Weight Category in Buyer Supplier Table
  findClosest <- function(wgt1, field1) {
    vec1 <- unlist(field1, use.names = FALSE)
    wgt1 <- unlist(wgt1, use.names = FALSE)
    yy <- sapply(1:length(wgt1), function(x) which.min(abs(vec1-wgt1[x])))
    x <- vec1[yy]
    return(x)
  }
  
  consumers.incremental[!is.na(wgtCat), 
                        wgtCat2 := findClosest(wgtCat, x2c[.BY, .(wgtCatRep)]), 
                        by = .(Buyer.NAICS, Commodity_SCTG)]
  
  # Build control file for iterating: Categorize consumers.incremental into groups
  cat1 <- consumers.incremental[MatchCat == 1, .(Shp = .N), keyby = .(Buyer.NAICS, Commodity_SCTG, Consumption_zone, wgtCat)][, GroupID := .I]
  cat2 <- consumers.incremental[MatchCat == 2, .(Shp = .N), keyby = .(Buyer.NAICS, Commodity_SCTG, wgtCat2)][, GroupID := .I + max(cat1$GroupID)]
  cntlFile <- rbindlist(list(cat1,cat2), use.names = TRUE, fill = TRUE)		
  
  # Start simple and assume all PurchaseAmountTons moves in 1 shipment (i.e., equal to Last.Iteration.Quantity)
  # Loop through Control File and Select Sample Shipments
  setkey(BuyerSupplierPairs, Buyer.NAICS, Commodity_SCTG, Consumption_zone, wgtCat)
  setkey(consumers.incremental, Buyer.NAICS, Commodity_SCTG, Consumption_zone, wgtCat)
  set.seed(9461)
  
  samp_data <- list()
  for(i in 1:nrow(cntlFile)){
    # Category control values
    NCS <- cntlFile$Buyer.NAICS[i]				# Category Buyer.NAICS 
    cSCTG <- cntlFile$Commodity_SCTG[i]		# Category commodity traded
    cZone <- cntlFile$Consumption_zone[i]	# Category consumption zone
    wCat1 <- cntlFile$wgtCat[i]				# Category weight category 1
    Shp <- cntlFile$Shp[i]					# Number of sampled shipments needed from Category
    grpID <- cntlFile$GroupID[i]			# Category ID
    wCat2 <- cntlFile$wgtCat2[i]			# Category weight category 2
    
    if(!is.na(wCat1)) {
      # wgtCat Selections
      tempPairs <- BuyerSupplierPairs[.(NCS, cSCTG, cZone, wCat1)]	
      tempConsumers <- consumers.incremental[.(NCS, cSCTG, cZone, wCat1)]
    } else {
      tempPairs <- BuyerSupplierPairs[Buyer.NAICS == NCS & Commodity_SCTG == cSCTG & wgtCat2 == wCat2]
      tempConsumers <- consumers.incremental[Buyer.NAICS == NCS & Commodity_SCTG == cSCTG & wgtCat2 == wCat2]
    }
    
    samp_data[[i]] <- tempPairs[sample(1:nrow(tempPairs), Shp, replace = TRUE)]
    
    # Update Attributes of Sampled Commodity Flows with data from incremental consumers
    samp_data[[i]][, BuyerID := tempConsumers$BuyerID]
    samp_data[[i]][, Last.Iteration.Quantity := tempConsumers$PurchaseAmountTons]

  }	
  
  sampled <- rbindlist(samp_data, use.names = TRUE, fill = TRUE)
 
  BuyerSupplierPairs <- rbindlist(list(BuyerSupplierPairs[, PMG_Incr := "PMG"]
                                       , sampled[, PMG_Incr := "Incr"]), 
                                  use.names = TRUE, 
                                  fill = TRUE)
  BuyerSupplierPairs[, c("wgtCat", "wgtCat2") := NULL]
  
  return(BuyerSupplierPairs)
  
}