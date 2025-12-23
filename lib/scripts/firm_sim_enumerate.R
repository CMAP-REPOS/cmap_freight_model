
#Enumerate firms and merge with correspondences
firm_synthesis_enumerate <- function(Establishments, EstSizeCategories, TAZEmployment, mzemp){

  # Enumerates the agent businesses using the est variable.
  Firms <- Establishments[rep(seq_len(Establishments[, .N]), est),]
  setkey(Firms, modelregion, CBPZONE, NAICS6, EmpCatName, esizecat)

  # Estimate the number of employees
  # Sample from the employment range using probabilities that approximate a declining distribution (1 more likely than 2, etc)
  EmpBounds = c(EstSizeCategories$LowerBound, EstSizeCategories[nrow(EstSizeCategories)]$LowerBound * 2)
  EmpDiff = shift(EmpBounds,-1)-EmpBounds-1
  EmpProb = EstSizeCategories$ProbRatio
  
  set.seed(BASE_SEED_VALUE)
  
  Firms[, Emp := sample(x = EmpBounds[esizecat]:(EmpBounds[esizecat + 1] - 1), 
                         size = .N, 
                         replace = TRUE,
                         prob = seq(EmpProb[esizecat],1, 
                                    by = -(EmpProb[esizecat] - 1)/EmpDiff[esizecat])), 
        by = .(modelregion, esizecat)]
  
  # Add an ID and firm type
  Firms[, BusID := .I]
  Firms[, FirmType := "Domestic"]

  # Remove uncessary fields
  Firms[, est := NULL]
  
  # Assign firms from CMAP Counties to Mesozones
  FirmsMZ <- Firms[CBPZONE %in% BASE_FIPS_INTERNAL, .(CountyFIPS = CBPZONE, BusID, EmpCatName, Emp)]
  
  # Assign specific NAICS categories which would be used to locate businesses to tazs
  FirmsMZ[EmpCatName %in% 31:33, EmpCatName := 3133]
  FirmsMZ[EmpCatName %in% 44:45, EmpCatName := 4445]
  FirmsMZ[EmpCatName %in% 48:49, EmpCatName := 4849]
  
  # Convert the ranking table to long format
  mzemp <- melt.data.table(mzemp,
                           id.vars = c("CountyFIPS", "Mesozone"),
                           variable.name = "EmpCatName",
                           value.name = "EmpRank")
  
  mzemp[, EmpCatName := as.integer(sub("rank", "", as.character(EmpCatName)))]
  
  # Merge the rankings dataset to the firms database based on county
  FirmsMZ <- merge(FirmsMZ,
                     mzemp,
                     by = c("CountyFIPS", "EmpCatName"),
                     allow.cartesian = TRUE,
                     all.x = TRUE)
  
  # Select candidate tazs based on the industry of the firm, firm size, and ranking of that particular industry in a Mesozone
  FirmsMZ[, candidate := 0L]
  FirmsMZ[Emp > 5000 & EmpRank %in% 9:10, candidate := 1L]
  FirmsMZ[Emp > 2000 & Emp <= 5000 & EmpRank %in% 7:10, candidate := 1L]
  FirmsMZ[Emp > 500 & Emp <= 2000 & EmpRank %in% 5:10, candidate := 1L]
  FirmsMZ[Emp > 100 & Emp <= 500 & EmpRank %in% 4:10, candidate := 1L]
  FirmsMZ[Emp > 20 & Emp <= 100 & EmpRank %in% 2:10, candidate := 1L]
  FirmsMZ[Emp <= 20 & EmpRank %in% 1:10, candidate := 1L]
  
  # small number of businesses that did not get a candiate Mesozone -
  # allow those to have some candidates (small error is better than omitting the businesses)
  ZeroCand <- FirmsMZ[,.(Candidates = sum(candidate)), by = BusID][Candidates == 0]$BusID
  FirmsMZ[BusID %in% ZeroCand, candidate := 1L]
  
  # Remove non-candidate Mesozone
  FirmsMZ <- FirmsMZ[candidate == 1,]
  
  # Generate a random number based on which one of the candidate Mesozone would be selected
  set.seed(BASE_SEED_VALUE)
  FirmsMZ[, u := runif(.N)]
  
  # Assign the taz for which the random number generated is the highest among all candidate Mesozone
  FirmsMZ <- FirmsMZ[FirmsMZ[,.I[which.max(u)], by = BusID]$V1,]
  
  # Assign MESOZONES for all firms
  Firms[FirmsMZ, Mesozone := i.Mesozone, on = "BusID"]
  Firms[!CBPZONE %in% BASE_FIPS_INTERNAL, Mesozone := CBPZONE + 150L]
  
  # Add an initial allocation to the TAZs within each Mesozone 
  # Allocation proportional to employment by group
  taz_prob <- TAZEmployment[, .(Employees.SE = sum(Employees.SE)), by = .(TAZ, Mesozone, EmpCatName)]
  taz_prob[, Prob := Employees.SE/sum(Employees.SE), by = .(Mesozone, EmpCatName)]
  # For any mesozones with no employment in that group, replace NaNs with small positive number
  taz_prob[is.na(Prob), Prob := 0.001]
  set.seed(BASE_SEED_VALUE)
    
  for (mz in BASE_MZ_DOMESTIC){
    for (empcat in unique(taz_prob[Mesozone == mz]$EmpCatName)){
      SampleTAZ <- taz_prob[Mesozone == mz & EmpCatName == empcat]$TAZ
      ProbTAZ <- taz_prob[Mesozone == mz & EmpCatName == empcat]$Prob
      if(length(SampleTAZ) == 1){
        Firms[Mesozone == mz & EmpCatName == empcat, TAZ := SampleTAZ]
      } else if(length(SampleTAZ) > 1){
        Firms[Mesozone == mz & EmpCatName == empcat, TAZ := sample(SampleTAZ, size = .N, replace = TRUE, prob = ProbTAZ)]
      } else { # No TAZs?
        
      }
    }
  }
  # Return the enumerated firms table
  return(Firms)

}
