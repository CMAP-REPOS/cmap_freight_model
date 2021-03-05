# Process the IO Table
firm_synthesis_input_output <- function(io, c_n6_n6io_sctg){

  # Clean and format the input output data
  setnames(io, names(io),sub("X", "", names(io)))

  io <- melt.data.table(io,
             id.vars = "Industry_NAICS6_MakeUse",
             variable.name = "Industry_NAICS6_Use",
             value.name = "ProVal")

  setnames(io, "Industry_NAICS6_MakeUse", "Industry_NAICS6_Make")

  io[, Industry_NAICS6_Use := as.character(Industry_NAICS6_Use)]

  # Simplify construction
  # NOTE: some problems/complexity in crosswalk between construction industry types
  # simplify here so that all construction is 230000, so at least production value is retained
  # All construction activity is in place, so it is on the use side, as far as freight movement is concerned
  io[substr(Industry_NAICS6_Make,1,2) == "23", Industry_NAICS6_Make := "230000"]
  io[substr(Industry_NAICS6_Use,1,2) == "23", Industry_NAICS6_Use := "230000"]
  io <- io[, .(ProVal = sum(ProVal)), by = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]

  # Wholesalers: grouped into 42000 in IO tables, but 6 digit NAICS codes in employment data
  # Distribute the IO table wholesaler production and consumption

  # Production with wholesale as an intermediate
  towhl <- io[substr(Industry_NAICS6_Use, 1, 2) == "42" & ProVal > 0 & substr(Industry_NAICS6_Make, 1, 2) != "42"]

  # Consumption with wholesale as an intermediate
  fromwhl <- io[substr(Industry_NAICS6_Make,1,2) == "42" & ProVal > 0 & substr(Industry_NAICS6_Use, 1, 2) != "42"]

  # Production direct to consumption, no wholesale
  nowhl <- io[substr(Industry_NAICS6_Make, 1, 2) != "42" & ProVal > 0 & substr(Industry_NAICS6_Use, 1, 2) != "42"]

  # Correspondence between SCTG and NAICS6 IO codes for wholesalers
  sctgwhl <- data.table(SCTG = c(1:40, 35, 38, 40),
                        NAICS_whl = c(rep("424500",4), rep("424400",3), "424800", "424400", rep("423300",3),
                                      rep("423500",2), "423700", rep("424700",4), "424600", "424200",
                                      rep("424600",2), "423400", rep("423300",2), rep("424100",3), "424300",
                                      rep("423500",2), "423700", "423800", "425100", rep("423100",2),
                                      "425100", "423200", "423900", rep("423600",2), "424900"))

  # WhlProportion is 1/number of different wholesale categories that trade an SCTG
  sctgwhl[, WhlProportion := 1/.N, by = SCTG]

  # Correspondence between SCTG and NAICS6 IO codes for producers
  # MakeProportion is the 1/number of SCTG produced by each NAICS6 IO categories
  c_n6io_sctg <- unique(c_n6_n6io_sctg[Commodity_SCTG > 0,
                                       .(SCTG = Commodity_SCTG, Industry_NAICS6_Make, MakeProportion = Proportion)])

  # Merge to create all possible Make to Wholesale combinations for each SCTG
  sctgwhl <- merge(c_n6io_sctg,
                   sctgwhl,
                   by = "SCTG",
                   allow.cartesian = TRUE)

  # Merge on the Production Value that goes to Wholesalers for each NAICS6 IO
  # This drops anything from towhl that is not making an SCTG commodity
  sctgwhl <- merge(sctgwhl,
                   towhl[,.(Industry_NAICS6_Make, ProValToWhl = ProVal)],
                   by = "Industry_NAICS6_Make")

  # Share out the ProValToWhl by SCTG and then Wholesaler
  sctgwhl[, ProValToWhl := ProValToWhl * MakeProportion * WhlProportion]

  # For from wholesale, add on the ProVal used by those industries direct from producers
  fromwhl <- merge(fromwhl[,.(Industry_NAICS6_Use, ProValFromWhl = ProVal)],
                   nowhl[, .(Industry_NAICS6_Make, Industry_NAICS6_Use, ProValUse = ProVal)],
                   by = "Industry_NAICS6_Use")

  # Which of those commodities can actually be sourced from wholesalers?
  # merge with the sctgwhl table to find which make commodities are in that table
  fromwhl <- merge(fromwhl,
                   sctgwhl,
                   by = "Industry_NAICS6_Make",
                   allow.cartesian = TRUE)

  # Correct the ProValUse value for multiple SCTG and Wholesaler combinations
  fromwhl[, ProValUse := ProValUse * MakeProportion * WhlProportion]

  # Calculate proportion of source for each use industry
  fromwhl[, ProValPctUse := ProValUse / sum(ProValUse), by = Industry_NAICS6_Use]

  # Allocate out the ProValFromWhl
  # First need to scale production amount to consumption
  whlcons <- sum(unique(fromwhl[,.(Industry_NAICS6_Use, ProValFromWhl)])$ProValFromWhl)
  whlprod <- sum(unique(fromwhl[,.(Industry_NAICS6_Make, ProValToWhl)])$ProValToWhl)

  fromwhl[, ProValFact := ProValToWhl * as.numeric(whlcons) / whlprod]

  # Matrix of producers to consumers
  # Row totals are production amounts
  # Column totals are consumption amounts
  # Initial cell values are the proportion of consumption that comes from each producers by industry
  # Then need to IPF to adjust cell values such that row and column totals are conserved
  fromwhl[, CellValue := ProValFact * ProValPctUse]

  for(i in 1:10){
    fromwhl[, ColTotal := sum(CellValue), by = Industry_NAICS6_Use]
    fromwhl[, ColWeight := ProValFromWhl / ColTotal]
    fromwhl[, CellValue := CellValue * ColWeight]
    fromwhl[, RowTotal := sum(CellValue), by = Industry_NAICS6_Make]
    fromwhl[, RowWeight := ProValFact / RowTotal]
    fromwhl[, CellValue := CellValue * RowWeight]
  }

  fromwhl[, CellValue := round(CellValue)]

  fromwhl <- fromwhl[CellValue > 0,
                     .(Industry_NAICS6_Make, Industry_NAICS6_Use, SCTG, NAICS_whl, ProValWhl = CellValue)]

  # Summarize the table into IO records
  iowhl <- fromwhl[, .(ProValWhl = sum(ProValWhl)), by = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]

  # Remove wholesale records from io table
  io <- io[substr(Industry_NAICS6_Use, 1, 2) != "42" & substr(Industry_NAICS6_Make, 1, 2) != "42"]

  # Replace wholesale records by adding the make-use value back to the io table
  # As if it was direct from producer to consumer and not via wholesale
  # So consumption amounts will be correct
  # Note that there are discrepanices between inputs to wholesale and outputs
  # Scaling done above matches to consumption from wholesale
  io <- merge(io,
              iowhl,
              by = c("Industry_NAICS6_Make", "Industry_NAICS6_Use"),
              all.x = TRUE)

  io[is.na(ProValWhl), ProValWhl := 0]

  io[, ProVal := ProVal + ProValWhl]

  # Simplify IO table
  # 1. focus on just producers of transported commodities
  # 2. remove any zero production value records
  io <- io[Industry_NAICS6_Make %in% c_n6_n6io_sctg[Commodity_SCTG > 0]$Industry_NAICS6_Make & ProVal > 0]

  # Calculate Production Value to simulate in the model up to the Production Value threshold
  # Sort on NAICS_USe, ProVal
  setkey(io, Industry_NAICS6_Use, ProVal)

  # Cumulative percent value of the consumption inputs
  io[, CumPctProVal := cumsum(ProVal) / sum(ProVal), by = Industry_NAICS6_Use]

  # Identify the records that are covered by the threshold, including the first above the threshold value
  io[, IsBelowThreshold := ifelse(CumPctProVal > 1 - BASE_PROVALTHRESHOLD, TRUE, FALSE)]

  # Return the processed input ouput table and the wholesale records
  return(io_list = list(io = io,
                        fromwhl = fromwhl))

}
