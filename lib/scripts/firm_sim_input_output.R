# Process the IO Table
firm_synthesis_input_output <- function(io, c_n6_n6io_sctg, ProductivityFactors, 
                                        FirmsDomestic, FirmsForeign){

  # Function processes input output table 
  # Includes scaling to account for scenario specific
  # productivity factors and employment growth
  
  if(SCENARIO_NAME != BASE_SCENARIO_BASE_NAME){
  
    # Alt scenarios only: Update the base year IO table 
    # to future year (scenario specific) IO table
    
    # Replace the import and export data for the future year with the trade data before other scaling 
    # so that domestic scaling accounts for balance of trade
    ExportsSummary <- FirmsForeign[FirmType == "ForeignConsumer",
                                   .(Industry_NAICS6_Use = "Exports", ProVal = sum(ProVal)), 
                                   keyby = Industry_NAICS6_Make]
    ImportsSummary <- FirmsForeign[FirmType == "ForeignProducer",.(Industry_NAICS6_Use = "Imports", ProVal = - sum(ProVal)), 
                                   keyby = Industry_NAICS6_Make]
    
    io <- rbind(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
                   .(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal)],
                ExportsSummary, ImportsSummary)
    
    # Calculate the employment change by 3 digit NAICS sector, base to future
    EmpFuture <- FirmsDomestic[,.(Emp = sum(Emp)), keyby = .(NAICS3 = floor(NAICS6/1000))]
    
    ProductivityFactors[EmpFuture, EmpFuture := i.Emp, on = "NAICS3"]
    ProductivityFactors[is.na(EmpFuture), EmpFuture := 0]
    
    ProductivityFactors[, EmpFactor := EmpFuture/EmpBase]
    ProductivityFactors[is.na(EmpFactor), EmpFactor := 1]
    
    # Calculate the value factor
    ProductivityFactors[, ValueFactor := EmpFactor * ProdFactor]
    
    # Calculate an employment weighted factor to go from NAICS 3 to 6 digit NAICS IO code
    c_n6_n6io_sctg[, NAICS3 := floor(Industry_NAICS6_CBP/1000)]
    c_naicsio_naics3_io_fac <- unique(c_n6_n6io_sctg[,.(Industry_NAICS6_Make, NAICS3)])
    
    c_naicsio_naics3_io_fac <- merge(c_naicsio_naics3_io_fac,
                                     ProductivityFactors,
                                     by = "NAICS3")
    
    c_naicsio_io_fac <- c_naicsio_naics3_io_fac[,.(ValueFactor = sum(ValueFactor * EmpFuture)/sum(EmpFuture)), keyby = Industry_NAICS6_Make]
    c_naicsio_io_fac[is.na(ValueFactor), ValueFactor := 1]
    
    # Use the factors to scale the base IO table to the future year
    io[c_naicsio_io_fac, MakeFactor := i.ValueFactor, on = "Industry_NAICS6_Make"]
    
    io[c_naicsio_io_fac[,.(Industry_NAICS6_Use = Industry_NAICS6_Make, ValueFactor)], 
       UseFactor := i.ValueFactor, 
       on = "Industry_NAICS6_Use"]
    
    io[is.na(MakeFactor), MakeFactor := 1]
    io[is.na(UseFactor), UseFactor := 1]
    
    # For imports and exports, reset the make factor to 1 as they are already scaled values
    io[Industry_NAICS6_Use %in% c("Exports", "Imports"), MakeFactor := 1]
    
    # Develop Future values by IPF to ensure that the two dimensions are grown correctly by industry
    io[, ProdFuture := ProVal * MakeFactor]
    io[, ConsFuture := ProVal * UseFactor]
    io[, ProValFuture := ProVal * MakeFactor * UseFactor]
    
    # Scale the consumption and seed values to match with the sum of ProValFuture
    # Only scale the non-import and export values
    ConsFactor <- as.numeric(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
                                .(ProdFuture = sum(ProdFuture))]/io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
                                                                    .(ConsFuture = sum(ConsFuture))])
    
    ProValFutureFactor <- as.numeric(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
                                        .(ProdFuture = sum(ProdFuture))]/io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
                                                                            .(ProValFuture = sum(ProValFuture))])
    
    io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
       ConsFuture := ConsFuture * ConsFactor]
    
    io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
       ProValFuture := ProValFuture * ProValFutureFactor]
    
    # Doubly Constrain the production value on the 
    # industry level total production and total consumption
    for (i in 1:10){
      
      # Constrain on Consumption
      io_cons <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
                    .(Cons = sum(ConsFuture), PVCons = sum(ProValFuture)), 
                    keyby = Industry_NAICS6_Use]
      
      io_cons[, CAdj := ifelse(PVCons != 0, Cons/PVCons, 1)]
      io[io_cons, CAdj := i.CAdj, on = "Industry_NAICS6_Use"]
      
      io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
         ProValFuture := ProValFuture * CAdj]
      
      # Constrain on Production
      io_prod <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
                    .(Prod = sum(ProdFuture), PVProd = sum(ProValFuture)), 
                    keyby = Industry_NAICS6_Make]
      
      io_prod[, PAdj := ifelse(PVProd != 0, Prod/PVProd, 1)]
      io[io_prod, PAdj := i.PAdj, on = "Industry_NAICS6_Make"]
      
      io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), 
         ProValFuture := ProValFuture * PAdj]
      
    }
    
    io <- io[,.(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal = ProValFuture)]
    
  }  # end of if not base scenario
  
  # copy the scaled IO table for summary (prior to further processing)
  ioscaled <- copy(io)
  
  # Wholesalers: distribute the IO table Production Value with wholesale as an intermediate
  # based on the NAICS IO commodities that the wholesalers trade
  fromwhl <- io[substr(Industry_NAICS6_Make,1,2) == "42"]

  # Translate the production value to NAICS IO codes
  # Using the allocations between wholesale firms and SCTG and NAICSIO and SCTG
  # (1) Allocates the SCTG codes to each record based on share of employment in wholesaling firms
  # (2) Allocates the NAICS IO codes based on share of employment in non-wholesaling firms
  
  naics_sctg_emp <- FirmsDomestic[,.(Emp = sum(Emp)), 
                                  keyby = .(EmpCatName, Industry_NAICS6_Make, Commodity_SCTG)]
  
  naics_sctg_emp[, Prop_SCTG := Emp/sum(Emp), by = Industry_NAICS6_Make]
  naics_sctg_emp[EmpCatName != 42, Prop_NAICS := Emp/sum(Emp), by = Commodity_SCTG]
  
  # Merge with fomwhl to create all possible Producer/Wholesale combinations for each SCTG
  fromwhl_make_use <- merge(fromwhl,
                   naics_sctg_emp[EmpCatName == 42,.(Industry_NAICS6_Make, Commodity_SCTG, Prop_SCTG)],
                   by = "Industry_NAICS6_Make",
                   all.x = TRUE,
                   allow.cartesian = TRUE)
  
  setnames(fromwhl_make_use, "Industry_NAICS6_Make", "NAICS_Whl")
  
  fromwhl_make_use <- merge(fromwhl_make_use,
                   naics_sctg_emp[EmpCatName != 42,.(Industry_NAICS6_Make, Commodity_SCTG, Prop_NAICS)],
                   by = "Commodity_SCTG",
                   all.x = TRUE,
                   allow.cartesian = TRUE)
  
  # Check to see if the created combinations are in the IO table
  fromwhl_make_use[io, InIO := 1, on = c("Industry_NAICS6_Make", "Industry_NAICS6_Use")]
  
  # Also, retain any consumption by Wholesalers to keep the large intra-wholesaler flows
  fromwhl_make_use[substr(Industry_NAICS6_Use,1,2) == 42, InIO := 1]
  
  # scale the totals for combination of NAICS_Whl and Industry_NAICS6_Use
  # so that the totals from the original IO records are preserved
  fromwhl_make_use[, ProValWhl := ProVal * Prop_SCTG * Prop_NAICS] 
  
  fromwhl[fromwhl_make_use[InIO == 1,.(ProValWhl = sum(ProValWhl)), 
                           keyby = .(Industry_NAICS6_Make = NAICS_Whl, Industry_NAICS6_Use)],
          ProValWhl := i.ProValWhl,
          on = c("Industry_NAICS6_Make", "Industry_NAICS6_Use")]
  fromwhl[is.na(ProValWhl), ProValWhl := ProVal]
  
  fromwhl[, WhlScaleMakeUse := ProVal/ProValWhl]
  
  fromwhl_make_use[fromwhl[,.(NAICS_Whl = Industry_NAICS6_Make, Industry_NAICS6_Use, WhlScaleMakeUse)],
                   WhlScaleMakeUse := i.WhlScaleMakeUse, 
                   on = c("NAICS_Whl", "Industry_NAICS6_Use")]
  
  fromwhl_make_use[, ProValWhl := ifelse(InIO == 1, ProValWhl * WhlScaleMakeUse, 0)]
  
  # Scale again for each NAICS Wholesale code to retain exactly the correct amount of value
  fromwhl_make <- merge(fromwhl_make_use[InIO == 1, 
                                         .(ProValWhl = sum(ProValWhl)), 
                                         keyby = .(NAICS_Whl)],
                        fromwhl[,.(ProVal = sum(ProVal)), 
                                keyby = .(NAICS_Whl = Industry_NAICS6_Make)],
                        by = "NAICS_Whl",
                        all = TRUE)
  fromwhl_make[, WhlScaleMake := ProVal/ProValWhl]
  
  fromwhl_make_use[fromwhl_make, WhlScaleMake := i.WhlScaleMake, on = "NAICS_Whl"]
  
  fromwhl_make_use[, ProValWhl := ifelse(InIO == 1, ProValWhl * WhlScaleMake, 0)]
  
  # Summarize the table into IO records
  iowhl <- fromwhl_make_use[InIO == 1, 
                            .(ProValWhl = sum(ProValWhl)), 
                   keyby = .(Industry_NAICS6_Make, Industry_NAICS6_Use)]

  # Remove from wholesale records from the IO table
  io <- io[substr(Industry_NAICS6_Make,1,2) != "42"]

  # Replace wholesale records by adding the make-use value back to the io table
  # As if it was direct from producer to consumer and not via wholesale
  io <- merge(io,
              iowhl,
              by = c("Industry_NAICS6_Make", "Industry_NAICS6_Use"),
              all = TRUE)

  io[is.na(ProVal), ProVal := 0]
  io[is.na(ProValWhl), ProValWhl := 0]
  
  io[, ProValNoWhl := ProVal]
  io[, ProVal := ProValNoWhl + ProValWhl]

  # Simplify IO table to just producers of transported commodities
  nonsctg <- io[!Industry_NAICS6_Make %in% c_n6_n6io_sctg[Commodity_SCTG > 0]$Industry_NAICS6_Make]
  
  io <- io[Industry_NAICS6_Make %in% c_n6_n6io_sctg[Commodity_SCTG > 0]$Industry_NAICS6_Make]
  
  # Restructure the IO table to calculate net domestic production and
  # consumption where:
  # Domestic production = domestic flows + exports
  # Domestic consumption = domestic flows + imports
  # Use domestic production for calculating production value per employee
  # Use domestic consumption for calculating consumption value per employee
  # Total production and consumption = domestic flows + exports + imports
  
  # Calculate fields DomVal, ExpVal, ImpVal, DomProdVal, DomConsVal, TotVal
  # Allocation of export and import value among use industries 
  # is proportional to domestic use
  io_exp <- io[Industry_NAICS6_Use %in% c("Exports")]
  io_imp <- io[Industry_NAICS6_Use %in% c("Imports")]
  
  io <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
           .(Industry_NAICS6_Make, Industry_NAICS6_Use, ProValNoWhl, ProValWhl, DomVal = ProVal)]
  
  io[, PctMake := DomVal/sum(DomVal), by = Industry_NAICS6_Make]
  
  io[io_exp, ExpMake := i.ProVal, on = "Industry_NAICS6_Make"]
  io[, ExpVal := ifelse(is.na(ExpMake), 0, PctMake * ExpMake)]
  
  io[io_imp, ImpMake := i.ProVal, on = "Industry_NAICS6_Make"]
  # In the raw table, imports are negative, change the value here to a positive dollar value
  io[, ImpVal := ifelse(is.na(ImpMake), 0, PctMake * -ImpMake)]
  
  # Calculate two versions of docmestic production, one removing wholesale production
  # This will be used when caclulating production value per employee at non-wholesale businesses
  # so that total production (output by non-wholesalers + wholesalers) is correct
  io[, DomProdVal := DomVal + ExpVal]
  io[, DomProdValNoWhl := DomProdVal - ProValWhl]
  
  io[, DomConsVal := DomVal + ImpVal]
  io[, TotVal := DomVal + ExpVal + ImpVal]

  # Calculate Production Value to simulate in the model up to the Production Value threshold
  setkey(io, Industry_NAICS6_Use, TotVal)

  # Cumulative percent value of the consumption inputs
  io[, CumPctProVal := cumsum(TotVal) / sum(TotVal), by = Industry_NAICS6_Use]

  # Identify the records that are covered by the threshold, including the first above the threshold value
  io[, IsBelowThreshold := ifelse(CumPctProVal > 1 - BASE_PROVALTHRESHOLD, TRUE, FALSE)]

  # Return the processed input output table and the wholesale records
  return(io_list = list(io = io,
                        ioscaled = ioscaled,
                        fromwhl = fromwhl,
                        fromwhl_make = fromwhl_make,
                        fromwhl_make_use = fromwhl_make_use,
                        nonsctg = nonsctg))

}
