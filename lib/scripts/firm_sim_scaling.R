# Scale employment by industry and TAZ to match control data including firmsize
scaleEstablishmentsTAZEmployment <- function(RegionFirms, TAZEmployment, MaxBusID, EstSizeCategories, TAZEmploymentShape = "WIDE", NewFirmsProportion = 0, FirmSizeFactors = NULL){
  #Function that scales employees by industry and TAZ
  
  # Summarize employment from synthesized firms by TAZ and EmpCatName
  Employment.Est <- RegionFirms[, .(Employees.Est = sum(Emp)), keyby = .(TAZ, EmpCatName)]
  
  # Melt wide employment data by TAZ and employment category  
  if(toupper(TAZEmploymentShape) == "WIDE"){
    Employment.SE <- melt(TAZEmployment, 
                          id.vars = "TAZ", 
                          variable.name = "EmpCatName", 
                          value.name = "Employees.SE")
  } else {
    Employment.SE = TAZEmployment
  }
  
  # Calculate firm targets if there are firm size factors
  if(!is.null(FirmSizeFactors)) {
    
    # Calculate the firm size requirements
    # Create comparison table:
    # Base firms and employment
    # Target firms and employment
    # Summarize employment from synthesized firms by TAZ and EmpCatName
    FirmSize <- RegionFirms[, .(Firms.Initial = .N, 
                                Employees.Initial = sum(Emp)), 
                            keyby = EmpCatName]
    
    FirmSize[Employment.SE[, .(Employees.SE = sum(Employees.SE, na.rm = TRUE)), keyby = EmpCatName],
             Employees.SE := i.Employees.SE, on = "EmpCatName"]
    FirmSize[, Size.Initial := Employees.Initial/Firms.Initial]
    FirmSize[FirmSizeFactors[,.(EmpCatName, FirmSizeFactor)], 
             FirmSizeFactor := i.FirmSizeFactor, on = "EmpCatName"]
    FirmSize[, Size.Target := Size.Initial * FirmSizeFactor]
    FirmSize[, Firms.Target := round(Employees.SE/Size.Target)]
    
  } # end of if  
  
  # Compare employment between socio-economic input (SE) and synthesized firms (Est)
  Employment.Compare <- merge(Employment.SE, Employment.Est, by = c("TAZ", "EmpCatName"), all = TRUE)
  Employment.Compare[is.na(Employees.SE),   Employees.SE := 0]
  Employment.Compare[is.na(Employees.Est), Employees.Est := 0]
  
  # If both employment sources say there should be no employment, there's
  # nothing to do but drop those records to save on calculations
  Employment.Compare <- Employment.Compare[!(Employees.SE == 0 & Employees.Est == 0)]
  Employment.Compare[, Employees.Difference := Employees.SE - Employees.Est]
  
  # Three cases to deal with:
  # 1. Employment change where there is some Est employment and some SE employment
  # 2. Employment in SE data but none from Est
  # 3. Employment from Est but none in SE data
  #
  # Note: if difference is 0, no action required --> Adjustment factor will be 1: 
  
  # For TAZs with positive growth, only some of the new employment comes from 
  # firm growth, some from new firms -- account for this in the adjustment calculations
  
  # For TAZs with negative growth, only some of the employment reduction comes from 
  # firm decline, some from a change in the number of firms -- account for this in the adjustment calculations
  
  # Positive difference and existing firms, allocate part of the growth to existing firms and part to new firms
  Employment.Compare[Employees.Difference > 0 & Employees.Est > 0, Employees.Growth := Employees.Difference * (1 - NewFirmsProportion)]
  Employment.Compare[Employees.Difference > 0 & Employees.Est > 0, Employees.New := Employees.Difference * NewFirmsProportion]
  
  # Negative difference, shrink existing firms
  Employment.Compare[Employees.Difference <= 0, Employees.Growth := Employees.Difference  * (1 - NewFirmsProportion)]
  Employment.Compare[Employees.Difference <= 0, Employees.New := Employees.Difference * NewFirmsProportion]
  
  # Positive difference and no existing firms, all growth is in new firms
  Employment.Compare[Employees.Difference > 0 & Employees.Est == 0, Employees.Growth := 0]
  Employment.Compare[Employees.Difference > 0 & Employees.Est == 0, Employees.New := as.double(Employees.Difference)]
  
  # Calculate employment scale factors to deal with Case 1 and 3 simultaneously
  Employment.Scaled <- Employment.Compare[Employees.Growth != 0]
  Employment.Scaled[, Adjustment := (Employees.Est + Employees.Growth) / Employees.Est]
  
  # Scale employees in firms table and bucket round
  RegionFirms[Employment.Scaled, Adjustment := i.Adjustment, on = c("TAZ", "EmpCatName")]
  RegionFirms[, Emp := as.numeric(Emp)]
  RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(TAZ, EmpCatName)]
  RegionFirms[, Adjustment := NULL]
  RegionFirms <- RegionFirms[Emp >= 1]
  
  # Add firms to empty TAZ-Employment category combinations to deal with Case 2
  # Also add firms to TAZs that have positive growth to account for gap
  # between growth in existing businesses (scaling above) and the target
  if(nrow(Employment.Compare[Employees.New > 0]) > 0){
    
    FirmsNeeded <- Employment.Compare[Employees.New > 0]
    
    # For each combination:
    # 1. Select a number of firms max of 1 and EMPDIFF/average emp from all of the firms in that EmpCatName
    # 2. Add them to the firms table
    # 3. recalc the EMPADJ to refine the employment to match exactly the SE data employment
    
    # Calculate average employment by EmpCatName
    Employment.Avg <- RegionFirms[, .(Employees.Avg = mean(Emp)), by = EmpCatName]
    FirmsNeeded[Employment.Avg, Employees.Avg := i.Employees.Avg, on = "EmpCatName"]
    
    # Calculate number of firms to be sampled
    FirmsNeeded[, N := round(pmax(1, Employees.New/Employees.Avg))]
    
    # Sample the N firms needed for each TAZ and Employment category
    NewFirms <- FirmsNeeded[, .(N = sum(N)), by = .(TAZ, EmpCatName)]
    set.seed(BASE_SEED_VALUE)
    NewFirms <- NewFirms[, .(BusID = sample(x = RegionFirms[EmpCatName == EmpCatName.temp, BusID], size = N, replace = TRUE)),
                         by = .(TAZ, EmpCatName.temp = EmpCatName)]
    setnames(NewFirms, old = "EmpCatName.temp", new = "EmpCatName")
    
    # Look up the firm attributes for these new firms (from the ones they were created from)
    NewFirms <- merge(NewFirms, RegionFirms[, !c("TAZ", "EmpCatName"), with = FALSE], by = "BusID")
    
    # Check that the employee counts of the new firms matches the SE data and scale/bucket round as needed
    # Need to account for the employment change of existing firms that is contributing to matching SE data
    Employment.New <- NewFirms[, .(Employees.NewFirms = sum(Emp)), by = .(TAZ, EmpCatName)]
    Employment.New[Employment.SE, Employees.SE := i.Employees.SE, on = c("TAZ", "EmpCatName")]
    
    Employment.Region <- RegionFirms[, .(Employees.Region = sum(Emp)), by = .(TAZ, EmpCatName)]
    Employment.New[Employment.Region, Employees.Region := i.Employees.Region, on = c("TAZ", "EmpCatName")]
    Employment.New[is.na(Employees.Region), Employees.Region := 0]
    
    Employment.New[, Employees.New := Employees.SE - Employees.Region]
    Employment.New[, Adjustment := Employees.New / Employees.NewFirms]
    
    NewFirms[Employment.New[, .(TAZ, EmpCatName, Adjustment)], Adjustment := i.Adjustment, on = c("TAZ", "EmpCatName")]
    NewFirms[, Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(TAZ, EmpCatName)]
    NewFirms[, Adjustment := NULL]
    NewFirms <- NewFirms[Emp >= 1]
    
    # Give the new firms new, unique business IDs
    NewFirms[, BusID := .I + MaxBusID]
    
    # Combine the original firms and the new firms
    RegionFirms <- rbind(RegionFirms, NewFirms, use.names = TRUE, fill = TRUE)
    
  }
  
  # Remove firms in cases where the a reduction in the number of firms is necessary 
  # due to firm size changes or employment reductions
  if(nrow(Employment.Compare[Employees.New < 0]) > 0){
    
    FirmsRemove <- Employment.Compare[Employees.New < 0]
    
    # For each combination:
    # 1. Select a number of firms max of 1 and EMPDIFF/average emp from all of the firms in that EmpCatName/TAZ combination
    # 2. Remove them from the firms table
    # 3. recalc the EMPADJ to refine the employment to match exactly the SE data employment
    
    # Calculate average employment by EmpCatName and TAZ
    Employment.Avg <- RegionFirms[, .(NumFirms = .N, Employees.Avg = mean(Emp)), keyby = .(EmpCatName, TAZ)]
    FirmsRemove[Employment.Avg, c("NumFirms", "Employees.Avg") := .(i.NumFirms, i.Employees.Avg), on = c("EmpCatName", "TAZ")]
    
    # Calculate number of firms to be sampled, but don't remove all firms from the TAZ, leave at least 1
    FirmsRemove[, N := round(pmax(1, Employees.New/Employees.Avg))]
    FirmsRemove[NumFirms <= N, N := NumFirms - 1]
    
    # Sample the N firms to remove for each TAZ and Employment category
    set.seed(BASE_SEED_VALUE)
    RegionFirms[FirmsRemove, NumRemove := i.N, on = c("TAZ", "EmpCatName")]
    RegionFirms[!is.na(NumRemove), Draw := runif(.N)]
    RegionFirms[!is.na(NumRemove), DrawRank := frank(Draw), by = .(TAZ, EmpCatName)]
    RegionFirms[!is.na(NumRemove), Remove := ifelse(DrawRank <= NumRemove,1,0)]
    RegionFirms <- RegionFirms[is.na(NumRemove) | Remove == 0][, c("NumRemove", "Draw", "DrawRank", "Remove") := NULL]
    
    # Check that the employee counts of the firms after removals matches the SE data and scale/bucket round as needed
    # Re-summarize employment from synthesized firms by TAZ and EmpCatName
    FirmsRemove[RegionFirms[, .(Employees.Est = sum(Emp)), keyby = .(TAZ, EmpCatName)],
                Employee.Updated := i.Employees.Est, on = .(TAZ, EmpCatName)]
    FirmsRemove[, Employees.AddGrowth := Employees.SE - Employee.Updated]
    
    Employment.Scaled <- FirmsRemove[Employees.AddGrowth != 0]
    Employment.Scaled[, Adjustment := (Employee.Updated + Employees.AddGrowth) / Employee.Updated]
    
    # Scale employees in firms table and bucket round
    RegionFirms[Employment.Scaled, Adjustment := i.Adjustment, on = c("TAZ", "EmpCatName")]
    RegionFirms[, Emp := as.numeric(Emp)]
    RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(TAZ, EmpCatName)]
    RegionFirms[, Adjustment := NULL]
    RegionFirms <- RegionFirms[Emp >= 1]
    
  }
  
  # Now that the employment matches, calculate firm changes to match firm size targets
  if(!is.null(FirmSizeFactors)) {
    
    # Adjust firm requirements to match targets calculated above
    FirmSize[RegionFirms[, .(Firms.Updated = .N), keyby = EmpCatName],
             Firms.Updated := i.Firms.Updated,
             on = "EmpCatName"]
    FirmSize[, Firms.Diff := Firms.Target - Firms.Updated]
    
    # Positive Firms.Diff are additional firms to add to TAZs with existing firms (effectively splitting existing firms)
    # Negative Firms.Diff are firms to remove from TAZs with at least 2 existing firms (effectively consolidating existing firms)
    # Process should not add firms to TAZs without any firms of that type or remove all firms form a TAZ, as employment controls cannot then be matched
    # If firms.diff == 0, no changes needed
    
    if(nrow(FirmSize[Firms.Diff > 0]) > 0){
      
      # Sample the N firms needed for each Employment category
      NewFirms <- FirmSize[Firms.Diff > 0, .(N = Firms.Diff, EmpCatName)]
      set.seed(BASE_SEED_VALUE)
      NewFirms <- NewFirms[, .(BusID = sample(x = RegionFirms[EmpCatName == EmpCatName.temp, BusID], size = N, replace = TRUE)),
                           by = .(EmpCatName.temp = EmpCatName)]
      setnames(NewFirms, old = "EmpCatName.temp", new = "EmpCatName")
      
      # Look up the firm attributes for these new firms (from the ones they were created from)
      NewFirms <- merge(NewFirms, RegionFirms, by = c("BusID", "EmpCatName"))
      
      # Give the new firms new, unique business IDs
      NewFirms[, BusID := .I + max(RegionFirms$BusID)]
      
      # Combine the original firms and the new firms
      RegionFirms <- rbind(RegionFirms, NewFirms, use.names = TRUE, fill = TRUE)
      
      # Re-scale the employment to account for the new firms
      # Need to account for the employment change of existing firms that is contributing to matching SE data
      Employment.Region <- RegionFirms[, .(Employees.Region = sum(Emp)), by = .(TAZ, EmpCatName)]
      Employment.Region[Employment.SE, Employees.SE := i.Employees.SE, on = c("TAZ", "EmpCatName")]
      Employment.Region[is.na(Employees.Region), Employees.Region := 0]
      Employment.Region[is.na(Employees.SE), Employees.SE := 0]
      Employment.Region[, Adjustment := Employees.SE / Employees.Region]
      
      # Scale employees in firms table and bucket round
      RegionFirms[Employment.Region, Adjustment := i.Adjustment, on = c("TAZ", "EmpCatName")]
      RegionFirms[, Emp := as.numeric(Emp)]
      RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(TAZ, EmpCatName)]
      RegionFirms[, Adjustment := NULL]
      
      # Keep as many firms as possible by re-adjusting employment distribution in the TAZ to avoid zero employees
      # Update the zero employee firms to have an employment of 1 and rescale again
      RegionFirms[Emp == 0 , Emp := 1]
      Employment.Region <- RegionFirms[, .(Employees.Region = sum(Emp)), by = .(TAZ, EmpCatName)]
      Employment.Region[Employment.SE, Employees.SE := i.Employees.SE, on = c("TAZ", "EmpCatName")]
      Employment.Region[is.na(Employees.Region), Employees.Region := 0]
      Employment.Region[is.na(Employees.SE), Employees.SE := 0]
      Employment.Region[, Adjustment := Employees.SE / Employees.Region]
      
      # Scale employees in firms table and bucket round
      RegionFirms[Employment.Region, Adjustment := i.Adjustment, on = c("TAZ", "EmpCatName")]
      RegionFirms[, Emp := as.numeric(Emp)]
      RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(TAZ, EmpCatName)]
      RegionFirms[, Adjustment := NULL]
      
      # Remove the remaining zero employee firms as this point and proceed
      RegionFirms <- RegionFirms[Emp >= 1]
      
    }
    
    if(nrow(FirmSize[Firms.Diff < 0]) > 0){
      
      FirmsRemove <- FirmSize[Firms.Diff < 0, .(N = -Firms.Diff, EmpCatName)]
      
      # Sample the N firms to remove for each TAZ and Employment category, but leave at least one in each TAZ
      set.seed(BASE_SEED_VALUE)
      RegionFirms[, BusIDRank := frank(BusID), by = .(TAZ, EmpCatName)]
      RegionFirms[, Consider := ifelse(BusIDRank > 1,1,0)]
      FirmsRemove[RegionFirms[Consider == 1,.N, by = EmpCatName],
                  N := pmin(N, i.N), on = "EmpCatName"]
      FirmsRemove <- FirmsRemove[, .(BusID = sample(x = RegionFirms[EmpCatName == EmpCatName.temp & Consider == 1, BusID], size = N, replace = FALSE)),
                                 by = .(EmpCatName.temp = EmpCatName)]
      setnames(FirmsRemove, old = "EmpCatName.temp", new = "EmpCatName")
      
      RegionFirms[FirmsRemove, Remove := 1, on = "BusID"]
      RegionFirms <- RegionFirms[is.na(Remove)][, c("BusIDRank", "Consider", "Remove") := NULL]
      
      # Re-scale the employment to account for the removed firms
      # Need to account for the employment change of existing firms that is contributing to matching SE data
      Employment.Region <- RegionFirms[, .(Employees.Region = sum(Emp)), by = .(TAZ, EmpCatName)]
      Employment.Region[Employment.SE, Employees.SE := i.Employees.SE, on = c("TAZ", "EmpCatName")]
      Employment.Region[is.na(Employees.Region), Employees.Region := 0]
      Employment.Region[is.na(Employees.SE), Employees.SE := 0]
      Employment.Region[, Adjustment := Employees.SE / Employees.Region]
      
      # Scale employees in firms table and bucket round
      RegionFirms[Employment.Region, Adjustment := i.Adjustment, on = c("TAZ", "EmpCatName")]
      RegionFirms[, Emp := as.numeric(Emp)]
      RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(TAZ, EmpCatName)]
      RegionFirms[, Adjustment := NULL]
      
    }
    
    
  } # end of if(!is.null(FirmSizeFactors)) 
  
  # Recode all employee counts into correct size categories following adjustments
  RegionFirms[, Emp := as.integer(Emp)]
  RegionFirms[, esizecat := cut(x = Emp, breaks = c(EstSizeCategories[["LowerBound"]], Inf),
                                labels = EstSizeCategories[["Label"]], right = FALSE, ordered_result = TRUE)]
  
  # Set table structure
  setkey(RegionFirms, BusID)
  
  return(RegionFirms)
  
}