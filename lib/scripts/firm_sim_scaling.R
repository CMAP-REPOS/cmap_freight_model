# Scale employment by industry and mesozone to match control data
firm_synthesis_scaling <- function(Firms, emp_control, c_cbp_faf, c_cbp_mz, EmpBounds){

  # Control data are by:
  # 1. Mesozone 1-132
  # 2. NAICS 2 digit (full detail) =
  #    11 21 22 23 31 32 33 42 44 45 48 49 51 52 53 54 55 56 61 62 71 72 81 92

  # Update fieldnames/datatypes for consistency
  setnames(emp_control, c("MESOZONE", "n2" , "Employees.SE"))
  Firms[, n2 := as.integer(n2)]

  # Scale the employment in the region
  # Leave the upper employment bound open so the 8th group can grow larger than the upper bound set earlier to accomodate large firms
  Firms <- firm_sim_scale_employees(Firms, emp_control, c_cbp_faf, c_cbp_mz, EmpBounds[1:8])

  ## Remove uncessary fields
  #Firms[, n2 := NULL]

  # Return the processed cbp table
  return(Firms)

}

# Scale Firms to Employment Forecasts
firm_sim_scale_employees <- function(Firms, Employment.SE, c_cbp_faf, c_cbp_mz, EmpBounds) {

  # Scale synthetic firms to employment forecasts
  Firms <- firm_sim_scale_employees_taz(RegionFirms = Firms,
                                              Employment.SE = Employment.SE,
                                              MaxBusID = max(Firms$BusID))

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

  # Summarize employment of synthesized firms by Mesozone and Naics 2
  Employment.CBP <- RegionFirms[, .(Employees.CBP = sum(Emp)), by = .(MESOZONE, n2)]

  # Compare employment between socio-economic input (SE) and synthetized firms (CBP)
  Employment.Compare <- merge(Employment.SE, Employment.CBP, by = c("MESOZONE", "n2"), all = TRUE)
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
  RegionFirms[Employment.Scaled, Adjustment := i.Adjustment, on = c("MESOZONE", "n2")]
  RegionFirms[, Emp := as.numeric(Emp)]
  RegionFirms[!is.na(Adjustment), Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(MESOZONE, n2)]
  RegionFirms[, Adjustment := NULL]
  RegionFirms <- RegionFirms[Emp >= 1]

  # Add firms to empty MESOZONE-NAICS 2 category combinations to deal with Case 2
  FirmsNeeded <- Employment.Compare[Employees.CBP == 0]

  # For each combination:
  # 1. Select a number of firms max of 1 and EMPDIFF/average emp from all of the firms in that industry
  # 2. Add them to the firms table
  # 3. recalc the EMPADJ to refine the employment to match exactly the SE data employment

  # Calculate average employment by NAICS 2
  Employment.Avg <- RegionFirms[, .(Employees.Avg = mean(Emp)), by = n2]
  FirmsNeeded[Employment.Avg, Employees.Avg := i.Employees.Avg, on = "n2"]

  # Calculate number of firms to be sampled
  FirmsNeeded[, N := round(pmax(1, Employees.Difference/Employees.Avg))]

  # Sample the N firms needed for each MESOZONE and NAICS 2 category
  NewFirms <- FirmsNeeded[, .(N = sum(N)), by = .(MESOZONE, n2)]
  set.seed(151)
  NewFirms <- NewFirms[, .(BusID = sample(x = RegionFirms[n2 == n2.temp,]$BusID, size = N, replace = TRUE)),
                       by = .(MESOZONE, n2.temp = n2)]
  setnames(NewFirms, old = "n2.temp", new = "n2")

  # Look up the firm attributes for these new firms (from the ones they were created from)
  NewFirms <- merge(NewFirms, RegionFirms[, !c("MESOZONE", "n2"), with = FALSE], by = "BusID")

  # Check that the employee counts of the new firms matches the SE data and scale/bucket round as needed
  Employment.New <- NewFirms[, .(Employees.NewFirms = sum(Emp)), by = .(MESOZONE, n2)]
  Employment.New[Employment.SE, Employees.SE := i.Employees.SE, on = c("MESOZONE", "n2")]
  Employment.New[, Adjustment := Employees.SE / Employees.NewFirms]
  NewFirms[Employment.New[, .(MESOZONE, n2, Adjustment)], Adjustment := i.Adjustment, on = c("MESOZONE", "n2")]
  NewFirms[, Emp := as.numeric(bucketRound(Emp * Adjustment)), by = .(MESOZONE, n2)]
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
