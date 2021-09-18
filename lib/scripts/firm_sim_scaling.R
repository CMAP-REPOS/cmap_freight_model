# Scale employment by industry and mesozone to match control data
firm_synthesis_scaling <- function(Firms, emp_control, emp_control_taz, c_cbp_faf, c_cbp_mz, c_taz_mz, EmpBounds){

  # Control data are in two levels of details:
  # emp_control:
  # 1. Mesozone 1-273, use for national portion of the model only, >= Mesozone 150
  # 2. NAICS 2 digit (full detail)
  #
  # emp_control_taz:
  # 1. TAZ 1-3632, use for CMAP portion of the model only, <= Mesozone 132
  # 2. NAICS 2 digit (full detail)

  # Update fieldnames/datatypes for consistency
  setnames(emp_control, c("MESOZONE", "n2" , "Employees.SE"))
  
  setnames(emp_control_taz, 
           c("Zone17", "Mesozone", "NAICS", "Employment"), 
           c("TAZ", "MESOZONE", "n2" , "Employees.SE"))
  
  Firms[, n2 := as.integer(n2)]
  
  # Add an initial allocation to the TAZs if there is no TAZ field yet 
  # (this will be the case for base year firm synthesis but not alternative/future scenarios)
  # Allocation proportional to employment
  # Scaling algorithm will deal with any discrepancies at the TAZ level
  if(!"TAZ" %in% names(Firms)){
    
    taz_prob <- emp_control_taz[, .(Employees.SE = sum(Employees.SE)), by = .(TAZ, MESOZONE)]
    taz_prob[, Prob := Employees.SE/sum(Employees.SE), by = MESOZONE]
    
    for (mz in 1:132){
      SampleTAZ <- c_taz_mz[mz == Mesozone]$TAZ
      ProbTAZ <- taz_prob[mz == MESOZONE]$Prob
      if(length(SampleTAZ) == 1){
        Firms[MESOZONE == mz, TAZ := SampleTAZ]
      } else {
        Firms[MESOZONE == mz, TAZ := sample(SampleTAZ, size = .N, replace = TRUE, prob = ProbTAZ)]
      }
    }
  }

  # Scale the employment in the region
  # Leave the upper employment bound open so the 8th group can grow larger than the upper bound set earlier to accomodate large firms
  Firms <- firm_sim_scale_employees(Firms, emp_control, emp_control_taz, c_cbp_faf, c_cbp_mz, c_taz_mz, EmpBounds[1:8])

  ## Remove uncessary fields
  
  # Return the processed cbp table
  return(Firms)

}

# Scale Firms to Employment Forecasts
firm_sim_scale_employees <- function(Firms, emp_control, emp_control_taz, c_cbp_faf, c_cbp_mz, c_taz_mz, EmpBounds) {

  # Scale synthetic firms inside CMAP to employment forecasts
  FirmsCMAP <- Firms[MESOZONE < 150]
  
  # Ensure that the targets and firms are consistent in terms of industrial coverage
  # Function will produce an error if there are zero firms for a category where there is employment
  emp_control_taz <- emp_control_taz[n2 %in% unique(FirmsCMAP$n2)]
  
  # Naming: unit to scale for CMAP region is TAZ, change to being called ZONE
  setnames(emp_control_taz, "TAZ", "ZONE")
  setnames(FirmsCMAP, "TAZ", "ZONE")
  
  FirmsCMAP <- firm_sim_scale_employees_taz(RegionFirms = FirmsCMAP,
                                            Employment.SE = emp_control_taz,
                                            MaxBusID = max(Firms$BusID))
  
  setnames(FirmsCMAP, "ZONE", "TAZ")
  
  # For CMAP firms, those that were sampled might have the wrong MESOZONE
  FirmsCMAP[c_taz_mz, MESOZONE := i.Mesozone, on = "TAZ"]
  
  # Scale synthetic firms outside CMAP to employment forecasts
  FirmsNonCMAP <- Firms[MESOZONE >= 150]
  
  # Ensure that the targets and firms are consistent in terms of industrial coverage
  # Function will produce an error if there are zero firms for a category where there is employment
  emp_control <- emp_control[n2 %in% unique(FirmsNonCMAP$n2)]
  
  # Naming: unit to scale for outside CMAP region is MESOZONE, change to being called ZONE
  setnames(emp_control, "MESOZONE", "ZONE")
  setnames(FirmsNonCMAP, "MESOZONE", "ZONE")
  
  FirmsNonCMAP <- firm_sim_scale_employees_taz(RegionFirms = FirmsNonCMAP,
                                              Employment.SE = emp_control,
                                              MaxBusID = max(c(Firms$BusID, FirmsCMAP$BusID)))

  setnames(FirmsNonCMAP, "ZONE", "MESOZONE")
  
  # Combine to single Firms list
  Firms <- rbind(FirmsCMAP, FirmsNonCMAP)
  
  # Update the County and State FIPS and FAF zones
  Firms[c_cbp_mz, CBPZONE := i.COUNTY, on = "MESOZONE"]
  Firms[MESOZONE >= 150, CBPZONE := MESOZONE - 150L]
  Firms[c_cbp_faf, FAFZONE := i.FAFZONE, on = "CBPZONE"]

  # Recode employee counts into categories
  Firms[, Emp := as.integer(Emp)]
  Firms[, esizecat := findInterval(Emp, EmpBounds)]

  # Set table structure
  setkey(Firms, BusID)

  # Return results
  return(Firms)

}

# sub function called from firm_sim_scale_employees
firm_sim_scale_employees_taz <- function(RegionFirms, Employment.SE, MaxBusID){

  # Keep an unaltered copy of the region firms tables for sampling
  RegionFirmsOriginal <- copy(RegionFirms)
  
  # Summarize employment of synthesized firms by ZONE and Naics 2
  Employment.CBP <- RegionFirms[, .(Employees.CBP = sum(Emp)), by = .(ZONE, n2)]

  # Compare employment between socio-economic input (SE) and synthetized firms (CBP)
  Employment.Compare <- merge(Employment.SE, Employment.CBP, by = c("ZONE", "n2"), all = TRUE)
  Employment.Compare[is.na(Employees.SE), Employees.SE := 0]
  Employment.Compare[is.na(Employees.CBP), Employees.CBP := 0]

  # If both employment sources say there should be no employment, there's
  # nothing to do but drop those records to save on calculations
  Employment.Compare <- Employment.Compare[!(Employees.SE == 0 & Employees.CBP == 0)]
  Employment.Compare[, Employees.Difference := Employees.SE - Employees.CBP]

  # Three cases to deal with:
  # 1. Employment change where there is some CBP employment and some SE employment
  # 2. Employment in SE data but none from CBP
  # 3. Employment from CBP but none in SE data
  #
  # Note: if difference is 0, no action required --> Adjustment factor will be 1:

  # Calculate employment scale factors to deal with Case 1 and 3 simultaneously
  Employment.Scaled <- Employment.Compare[Employees.CBP > 0]
  Employment.Scaled[, Adjustment := Employees.SE / Employees.CBP]

  # Scale employees in firms table and bucket round
  RegionFirms[Employment.Scaled, Adjustment := i.Adjustment, on = c("ZONE", "n2")]
  RegionFirms[, Emp := as.numeric(Emp)]
  RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(ZONE, n2)]
  RegionFirms[, Adjustment := NULL]
  RegionFirms <- RegionFirms[Emp >= 1]

  # Add firms to empty ZONE-NAICS 2 category combinations to deal with Case 2
  FirmsNeeded <- Employment.Compare[Employees.CBP == 0]

  # For each combination:
  # 1. Select a number of firms max of 1 and EMPDIFF/average emp from all of the firms in that industry
  # 2. Add them to the firms table
  # 3. recalc the EMPADJ to refine the employment to match exactly the SE data employment

  # Calculate average employment by NAICS 2 from the original firms list
  Employment.Avg <- RegionFirmsOriginal[, .(Employees.Avg = mean(Emp)), by = n2]
  FirmsNeeded[Employment.Avg, Employees.Avg := i.Employees.Avg, on = "n2"]

  # Calculate number of firms to be sampled
  FirmsNeeded[, N := round(pmax(1, Employees.Difference/Employees.Avg))]

  # Sample the N firms needed for each ZONE and NAICS 2 category
  NewFirms <- FirmsNeeded[, .(N = sum(N)), by = .(ZONE, n2)]
  set.seed(151)
  NewFirms <- NewFirms[, .(BusID = sample(x = RegionFirmsOriginal[n2 == n2.temp,]$BusID, size = N, replace = TRUE)),
                       by = .(ZONE, n2.temp = n2)]
  setnames(NewFirms, old = "n2.temp", new = "n2")

  # Look up the firm attributes for these new firms (from the ones they were created from)
  NewFirms <- merge(NewFirms, RegionFirmsOriginal[, !c("ZONE", "n2"), with = FALSE], by = "BusID")

  # Check that the employee counts of the new firms matches the SE data and scale/bucket round as needed
  Employment.New <- NewFirms[, .(Employees.NewFirms = sum(Emp)), by = .(ZONE, n2)]
  Employment.New[Employment.SE, Employees.SE := i.Employees.SE, on = c("ZONE", "n2")]
  Employment.New[, Adjustment := Employees.SE / Employees.NewFirms]
  NewFirms[Employment.New[, .(ZONE, n2, Adjustment)], Adjustment := i.Adjustment, on = c("ZONE", "n2")]
  NewFirms[, Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(ZONE, n2)]
  NewFirms[, Adjustment := NULL]
  NewFirms <- NewFirms[Emp >= 1]

  # Give the new firms new, unique business IDs
  NewFirms[, BusID := .I + MaxBusID]

  # Combine the original firms and the new firms
  RegionFirms <- rbind(RegionFirms, NewFirms, use.names = TRUE, fill = TRUE)

  return(RegionFirms)

}

# bucket rounding
bucketRound <- function (x, threshold = 0.5) {

  vecf <- floor(x)
  vecd <- x - vecf
  veca <- rep(0, length(x))
  adj <- 0
  for (i in 1:length(x)) {
    adj <- adj + vecd[i]
    if (adj >= threshold) {
      veca[i] <- 1
      adj <- adj - 1
    }
  }
  vecr <- vecf + veca
  return(as.integer(vecr))
}
