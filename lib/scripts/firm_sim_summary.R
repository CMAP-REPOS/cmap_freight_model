# Firm synthesis summary
firm_synthesis_summary <- function(firm_sim_summary, FirmsDomestic, TAZEmployment, c_n2_empcats, 
                                   FirmsDomesticBase, FirmSizeFactors, FirmsForeign, for_cons, for_prod, c_n6_n6io_sctg,
                                   io_list, producers_list, producers, 
                                   consumers_list, consumers, prefweights) {

  ### Run the individual summary functions:
  
  if(SCENARIO_NAME == BASE_SCENARIO_BASE_NAME){
    
    # Base year firm size summary
    firm_sim_summary$FirmSize <- firm_synthesis_summary_firm_size_base(FirmsDomestic, TAZEmployment, c_n2_empcats)
  
  } else {
      
    # Future year firm size summaries
    firm_sim_summary$FirmSize <- firm_synthesis_summary_firm_size_future(FirmsDomesticBase, FirmsDomestic, TAZEmployment, FirmSizeFactors, c_n2_empcats)
    firm_sim_summary$FirmSize3 <- firm_synthesis_summary_firm_size3(FirmsDomesticBase, FirmsDomestic, c_n2_empcats)
    
  }
  
  # Firms
  firm_sim_summary$FirmsDomestic <- firm_synthesis_summary_domestic(FirmsDomestic)
  firm_sim_summary$FirmsForeign <- firm_synthesis_summary_foreign(FirmsForeign, for_cons, for_prod, c_n6_n6io_sctg)
  firm_sim_summary$SCTGCheck <- firm_synthesis_summary_sctg(FirmsDomestic, FirmsForeign)
  
  # IO Tables
  firm_sim_summary$IOProdTradetype <- firm_synthesis_summary_io(io_list$io, c_n6_n6io_sctg)
  firm_sim_summary$io <- io_list$io
  firm_sim_summary$ioscaled <- io_list$ioscaled
  firm_sim_summary$fromwhl <- io_list$fromwhl
  firm_sim_summary$fromwhl_make <- io_list$fromwhl_make
  firm_sim_summary$fromwhl_make_use <- io_list$fromwhl_make_use
  firm_sim_summary$nonsctg <- io_list$nonsctg
  
  # Producers
  firm_sim_summary$producers_all <- producers_list$producers_all
  firm_sim_summary$producers <- firm_synthesis_summary_producers(producers)
  
  # Consumers
  firm_sim_summary$consumers_incremental_summary <- consumers_list$consumers_incremental_summary
  firm_sim_summary$consumers_summary <- firm_synthesis_summary_consumers(consumers, consumers_list$consumers_incremental_summary)
  
  ### Create high level output summaries:
  
  sctgcat <- data.table(Commodity_SCTG = prefweights$Commodity_SCTG,
                        SCTG_Name = prefweights$Commodity_SCTG_desc)

  # Summarize CBP, Producers and Consumers
  firms_sum <- list()

  # CBP
  firms_sum[["firms"]] <- FirmsDomestic[,.N]
  firms_sum[["employment"]] <- FirmsDomestic[,.(Emp = sum(Emp))]$Emp
  firms_sum[["firmsempbysctg"]] <- setcolorder(FirmsDomestic[,.(Establishments = .N, Employment = sum(Emp)), by = Commodity_SCTG][sctgcat, SCTG_Name := i.SCTG_Name, on = "Commodity_SCTG"][order(Commodity_SCTG)],
                                            c("Commodity_SCTG", "SCTG_Name", "Establishments", "Employment"))

  # IO
  firms_sum[["total_value"]] <- io_list$io[,.(ProVal = sum(TotVal))]$ProVal
  firms_sum[["Industry_NAICS_Make"]] <- length(io_list$io[, unique(Industry_NAICS6_Make)])
  firms_sum[["Industry_NAICS_Use"]] <- length(io_list$io[, unique(Industry_NAICS6_Use)])

  # Producers
  firms_sum[["producers"]] <- producers[,.N]
  firms_sum[["producers_emp"]] <- producers[, .(Size = sum(Size))]$Size
  firms_sum[["producers_cap"]] <- producers[, .(OutputCapacityTons = sum(OutputCapacityTons))]$OutputCapacityTons

  producers_summary <- setcolorder(producers[, .(Producers = .N, Employment = sum(Size), OutputCapacity = sum(OutputCapacityTons)),
                                             by = Commodity_SCTG]
                                   [sctgcat, SCTG_Name := i.SCTG_Name, on = "Commodity_SCTG"]
                                   [order(Commodity_SCTG)],
                                   c("Commodity_SCTG", "SCTG_Name", "Producers", "Employment", "OutputCapacity"))

  firms_sum[["producersempbysctg"]] <- producers_summary
  firms_sum[["producersdomforwhole"]] <- producers[, .(Producers = .N, Employment = sum(Size), OutputCapacity = sum(OutputCapacityTons)), by = ProdType]

  # Consumers
  firms_sum[["consumers"]] <- length(consumers[,unique(BuyerID)])
  firms_sum[["consumption_pairs"]] <- consumers[,.N]
  firms_sum[["threshold"]] <- BASE_PROVALTHRESHOLD
  firms_sum[["consumer_inputs"]] <- consumers[, .(PurchaseAmountTons = sum(PurchaseAmountTons))]

  consumers_summary <- setcolorder(consumers[, .(Consumers = .N, InputRequirements = sum(PurchaseAmountTons)),
                                             by = Commodity_SCTG]
                                   [sctgcat, SCTG_Name := i.SCTG_Name, on = "Commodity_SCTG"]
                                   [order(Commodity_SCTG)],
                                   c("Commodity_SCTG", "SCTG_Name", "Consumers", "InputRequirements"))

  firms_sum[["consumersbysctg"]] <- consumers_summary
  firms_sum[["consumersdomforwhole"]] <- consumers[, .(Consumers = .N, ConsumptionValue = sum(ConVal), InputRequirements = sum(PurchaseAmountTons)), by = ConsType]

  # Matching consumers and suppliers -- by SCTG category
  setnames(producers_summary,"Commodity_SCTG","Commodity")
  setnames(consumers_summary,"Commodity_SCTG","Commodity")

  match_summary <- merge(producers_summary[, .(Commodity,SCTG_Name,Producers,OutputCapacity)],
                         consumers_summary[, .(Commodity,Consumers,InputRequirements)],
                         by = "Commodity",
                         all = TRUE)

  setcolorder(match_summary, c("Commodity","SCTG_Name","Producers","Consumers","OutputCapacity","InputRequirements"))

  match_summary[, Ratio_OutputInput := OutputCapacity / InputRequirements]
  match_summary[, Possible_Matches := as.numeric(Producers) * as.numeric(Consumers)]

  firms_sum[["matches"]] <- match_summary

  # Matching consumers and suppliers -- by NAICS codes
  producers_summary_naics <- producers[,.(Producers = .N, Employment = sum(Size), OutputCapacity = sum(OutputCapacityTons)), by = OutputCommodity]
  firms_sum[["producersempbynaics"]] <- data.frame(producers_summary_naics) #so it prints all rows

  consumers_summary_naics <- consumers[,.(Consumers = .N, Employment = sum(Size), InputRequirements = sum(PurchaseAmountTons)), by = InputCommodity]
  firms_sum[["consumersbynaics"]] <- data.frame(consumers_summary_naics) #so it prints all rows

  setnames(producers_summary_naics,"OutputCommodity","NAICS")
  setnames(consumers_summary_naics,"InputCommodity","NAICS")

  match_summary_naics <- merge(producers_summary_naics[, .(NAICS, Producers, OutputCapacity)],
                               consumers_summary_naics[, .(NAICS, Consumers, InputRequirements)],
                               "NAICS",
                               all = TRUE)

  setcolorder(match_summary_naics, c("NAICS","Producers","Consumers","OutputCapacity","InputRequirements"))

  match_summary_naics[, Ratio_OutputInput := OutputCapacity / InputRequirements]
  match_summary_naics[, Possible_Matches := as.numeric(Producers) * as.numeric(Consumers)]

  firms_sum[["matches_naics"]] <- data.frame(match_summary_naics) #so it prints all rows

  # Matching consumers and suppliers -- by NAICS codes AND SCTG
  producers_summary_naics_sctg <- producers[ ,.(Producers = .N,
                                                Employment = sum(Size),
                                                OutputCapacity = sum(OutputCapacityTons)),
                                             by = .(OutputCommodity, Commodity_SCTG)]

  firms_sum[["producersempbynaicssctg"]] <- data.frame(producers_summary_naics_sctg) #so it prints all rows

  consumers_summary_naics_sctg <- consumers[ ,.(Consumers = .N,
                                                Employment = sum(Size),
                                                InputRequirements = sum(PurchaseAmountTons)),
                                             by= .(InputCommodity, Commodity_SCTG)]

  firms_sum[["consumersbynaicssctg"]] <- data.frame(consumers_summary_naics_sctg) #so it prints all rows

  setnames(producers_summary_naics_sctg, "OutputCommodity", "NAICS")
  setnames(consumers_summary_naics_sctg, "InputCommodity", "NAICS")

  match_summary_naics_sctg <- merge(producers_summary_naics_sctg[ ,.(NAICS, Commodity_SCTG, Producers, OutputCapacity)],
                                    consumers_summary_naics_sctg[ ,.(NAICS, Commodity_SCTG, Consumers, InputRequirements)],
                                    c("NAICS", "Commodity_SCTG"),
                                    all = TRUE)

  setcolorder(match_summary_naics_sctg, c("NAICS", "Commodity_SCTG", "Producers", "Consumers", "OutputCapacity", "InputRequirements"))

  match_summary_naics_sctg[ , Ratio_OutputInput := OutputCapacity / InputRequirements]
  match_summary_naics_sctg[ , Possible_Matches := as.numeric(Producers) * as.numeric(Consumers)]

  firms_sum[["matches_naics_sctg"]] <- data.frame(match_summary_naics_sctg) #so it prints all rows

  # Output text file/csv for the high level summaries
  capture.output(print(firms_sum),file=file.path(SCENARIO_OUTPUT_PATH,"firm_syn.txt" ))
  fwrite(match_summary_naics_sctg, file=file.path(SCENARIO_OUTPUT_PATH,"match_summary_naics_sctg.csv" ))

  # Add to the list of summaries and return
  firm_sim_summary$firms_sum <- firms_sum
  
  return(firm_sim_summary)

}

firm_synthesis_summary_firm_size_base <- function(FirmsDomestic, TAZEmployment, c_n2_empcats) {
  
  # Base year firm size summary
  FirmSize <- FirmsDomestic[, .(Firms.Model = .N, 
                                Employees.Model = sum(Emp)), 
                            keyby = EmpCatName]
  FirmSize[, Size.Model := Employees.Model/Firms.Model]
  FirmSize[TAZEmployment[, .(Employees.SE = sum(Employees.SE, na.rm = TRUE)), keyby = EmpCatName],
           Employees.SE := i.Employees.SE, on = "EmpCatName"]
  FirmSize[, Employees.Diff := Employees.Model - Employees.SE]
  
  # Labeling and column order
  FirmSize[c_n2_empcats, EmpCatDesc := i.EmpCatDesc, on = "EmpCatName"]
  setcolorder(FirmSize, c(names(FirmSize)[ncol(FirmSize)], names(FirmSize)[1:(ncol(FirmSize)-1)]))
  
  return(FirmSize)
}

firm_synthesis_summary_firm_size_future <- function(FirmsDomesticBase, FirmsDomestic, TAZEmployment, FirmSizeFactors, c_n2_empcats) {
 
  # Base firms and employment
  # Target firms and employment
  # Summarize employment from synthesized firms by TAZ and EmpCatName
  FirmSize <- FirmsDomesticBase[, .(Firms.Initial = .N, 
                                    Employees.Initial = sum(Emp)), 
                                keyby = EmpCatName]
  
  FirmSize[TAZEmployment[, .(Employees.SE = sum(Employees.SE, na.rm = TRUE)), keyby = EmpCatName],
           Employees.SE := i.Employees.SE, on = "EmpCatName"]
  FirmSize[, Size.Initial := Employees.Initial/Firms.Initial]
  FirmSize[FirmSizeFactors[,.(EmpCatName, FirmSizeFactor)], 
           FirmSizeFactor := i.FirmSizeFactor, on = "EmpCatName"]
  FirmSize[, Size.Target := Size.Initial * FirmSizeFactor]
  FirmSize[, Firms.Target := round(Employees.SE/Size.Target)]
  
  # Future (scaled) firms and employment
  FirmSize[FirmsDomestic[, .(Firms.Updated = .N, 
                             Employees.Updated = sum(Emp)), 
                         keyby = EmpCatName],
           c("Firms.Updated", "Employees.Updated") := .(i.Firms.Updated, i.Employees.Updated),
           on = "EmpCatName"]
  
  # How well did the scaling do?
  FirmSize[, Firms.Diff := Firms.Updated - Firms.Target]
  FirmSize[, Employees.Diff := Employees.Updated - Employees.SE]
  
  # Check percentage error for firms, any outside reasonable tolerance?
  FirmSize[, Firms.PctDiff := Firms.Diff/Firms.Target]
  # check average size
  FirmSize[, Size.Updated := Employees.Updated/Firms.Updated]
  FirmSize[, Size.Diff := Size.Updated - Size.Target]
  
  # Labeling and column order
  FirmSize[c_n2_empcats, EmpCatDesc := i.EmpCatDesc, on = "EmpCatName"]
  setcolorder(FirmSize, c(names(FirmSize)[ncol(FirmSize)], names(FirmSize)[1:(ncol(FirmSize)-1)]))
  
  return(FirmSize)
  
}
  
firm_synthesis_summary_firm_size3 <- function(FirmsDomesticBase, FirmsDomestic, c_n2_empcats) {
  
  # Change in made up by 3 digit firms within 2 digit categories
  FirmSize3 <- FirmsDomesticBase[, .(Firms.Initial = .N, 
                                     Employees.Initial = sum(Emp)), 
                                 keyby = .(EmpCatName, NAICS3 = floor(NAICS6/1000))]
  FirmSize3[FirmsDomestic[, .(Firms.Updated = .N, 
                              Employees.Updated = sum(Emp)), 
                          keyby = .(EmpCatName, NAICS3 = floor(NAICS6/1000))],
            c("Firms.Updated", "Employees.Updated") := .(i.Firms.Updated, i.Employees.Updated),
            on = c("EmpCatName", "NAICS3")]
  
  FirmSize3[, PctEmp3.Initial := Employees.Initial/sum(Employees.Initial), by = EmpCatName]
  FirmSize3[, PctEmp3.Updated := Employees.Updated/sum(Employees.Updated), by = EmpCatName]
  FirmSize3[, PctEmp3.Diff := PctEmp3.Updated - PctEmp3.Initial]
  FirmSize3[,.(EmpCatName, NAICS3, PctEmp3.Initial = round(PctEmp3.Initial,3), 
               PctEmp3.Updated  = round(PctEmp3.Updated,3), 
               PctEmp3.Diff = round(PctEmp3.Diff,3))]
  
  FirmSize3[c_n2_empcats, EmpCatDesc := i.EmpCatDesc, on = "EmpCatName"]
  setcolorder(FirmSize3, c(names(FirmSize3)[ncol(FirmSize3)], names(FirmSize3)[1:(ncol(FirmSize3)-1)]))
  
  return(FirmSize3)
  
  
}

firm_synthesis_summary_domestic <- function(FirmsDomestic){
  
  # Summarize the domestic firms by NAICS make code
  FirmsDomesticSummary <- FirmsDomestic[,.(Est = .N, Emp = sum(Emp)),
                                        keyby = .(EmpCatName, EmpCatGroupedName,  
                                                  Industry_NAICS6_Make, Commodity_SCTG,
                                                  modelregion)]
  FirmsDomesticSummary[, modelregion := factor(modelregion, labels = c("CMAP_Mod_Reg", "Rest_USA"))]
  
  FirmsDomesticSummary <- dcast.data.table(FirmsDomesticSummary,
                                           EmpCatName + EmpCatGroupedName + Industry_NAICS6_Make + Commodity_SCTG ~ modelregion,
                                           value.var = c("Est", "Emp"))
  
  FirmsDomesticSummary[, Est_Total := Est_CMAP_Mod_Reg + Est_Rest_USA]
  FirmsDomesticSummary[, Emp_Total := Emp_CMAP_Mod_Reg + Emp_Rest_USA]
  
  return(FirmsDomesticSummary)
  
}

firm_synthesis_summary_foreign <- function(FirmsForeign, for_cons, for_prod, c_n2_empcats){
  
  # Summarize the enumerated foreign firms with the trade data
  FirmsForeignSummary <- FirmsForeign[,.(Flows = .N, ProVal = sum(ProVal)),
                                      keyby = .(Industry = substr(Industry_NAICS6_Make,1,2), FirmType)]
  FirmsForeignSummary <- add_totals(dcast.data.table(FirmsForeignSummary,
                                          Industry ~ FirmType,
                                          fun.aggregate = sum,
                                          value.var = c("Flows", "ProVal")),
                                    rowtotal = FALSE)
                                    
  for_cons_sum <- add_totals(for_cons[,.(USExpVal  = sum(USExpVal)/1e6), keyby = .(Industry = substr(Commodity_NAICS6,1,2))], rowtotal = FALSE)
  for_prod_sum <- add_totals(for_prod[,.(USImpVal  = sum(USImpVal)/1e6), keyby = .(Industry = substr(Commodity_NAICS6,1,2))], rowtotal = FALSE)
  FirmsForeignSummary[for_cons_sum, USExpVal := i.USExpVal, on = "Industry"]
  FirmsForeignSummary[for_prod_sum, USImpVal := i.USImpVal, on = "Industry"]
  FirmsForeignSummary[, ExportDiff := round(ProVal_ForeignConsumer - USExpVal,2)]
  FirmsForeignSummary[, ImportDiff := round(ProVal_ForeignProducer - USImpVal,2)]
  
  return(FirmsForeignSummary)
   
}

firm_synthesis_summary_sctg <- function(FirmsDomestic, FirmsForeign){
  
  # Check on foreign and domestic firms by SCTG
  SCTGCheck <- merge(FirmsDomestic[, .(FirmsDomestic = .N), by = Commodity_SCTG][order(Commodity_SCTG)],
                     FirmsForeign[, .(FirmsForeign = .N), by = Commodity_SCTG][order(Commodity_SCTG)],
                     all = TRUE, 
                     by = "Commodity_SCTG")
  
  SCTGCheck[is.na(FirmsDomestic), FirmsDomestic := 0]
  SCTGCheck[is.na(FirmsForeign), FirmsForeign := 0]
  
  SCTGCheck[SCTGCodes[,.(Commodity_SCTG = SCTG, Label)], 
            Commodity_SCTG_Label := i.Label,
            on = "Commodity_SCTG"]
  setcolorder(SCTGCheck, c(names(SCTGCheck)[ncol(SCTGCheck)], names(SCTGCheck)[1:(ncol(SCTGCheck)-1)]))
  
  SCTGCheck[, Commodity_SCTG := as.character(Commodity_SCTG)]
  SCTGCheck <- add_totals(SCTGCheck, idcols = 2L)
  
  return(SCTGCheck)
}

firm_synthesis_summary_io <- function(io, c_n6_n6io_sctg){
  
  # Create a summary table from the io table
  io_prod_tradetype <- io[,.(Domestic = sum(DomVal),
                             Exports = sum(ExpVal),
                             Imports = sum(ImpVal),
                             DomProduction = sum(DomProdVal),
                             DomConsumption = sum(DomConsVal),
                             TotalValue = sum(TotVal)),
                         keyby = .(Industry_NAICS6_Make)]
  
  # Add make labels
  io_prod_tradetype[unique(c_n6_n6io_sctg[,.(Industry_NAICS6_Make, Industry_NAICS6_Make_desc)]),
                    Industry_NAICS6_Make_Desc := i.Industry_NAICS6_Make_desc, 
                    on = "Industry_NAICS6_Make"]
  
  setcolorder(io_prod_tradetype, c("Industry_NAICS6_Make_Desc", names(io_prod_tradetype)[1:ncol(io_prod_tradetype)-1]))
  setorder(io_prod_tradetype, Industry_NAICS6_Make)
  
  return(io_prod_tradetype)
  
}

firm_synthesis_summary_producers <- function(producers){
  
  # Create a summary table from the producers table
  # Totals production by SCTG, model region, and type
  prod_sum <- producers[,.(OutputCapacityTons = sum(OutputCapacityTons)),
                        keyby = .(EmpCatName, EmpCatGroupedName, Commodity_SCTG, ProdType, FirmType, modelregion)]
  
  return(prod_sum)
  
}


firm_synthesis_summary_consumers <- function(consumers, consumers_incremental){
  
  # Create a summary table from the consumers table 
  # Add the summary of consumers_incremental to create a complete consumption summary 
  # Totals consumption by SCTG, Industry, ConsType, Zone, and FAFZone
  cons_sum <- consumers[, .(Requirements = sum(PurchaseAmountTons)), 
                                    keyby = .(Commodity_SCTG, ConsType, 
                                              EmpCatName, EmpCatGroupedName,
                                              Zone, FAFZONE)][, ConsSegment := "Simulated in PMG"]
  consumers_incremental[, ConsSegment := "Non-Simulated Consumption"]
  cons_sum <- rbind(cons_sum, consumers_incremental)
  
  return(cons_sum)
  
}
