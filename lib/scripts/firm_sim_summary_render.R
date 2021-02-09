# Firm synthesis summary
firm_synthesis_summary_render <- function(FirmsDomesticUnscaled, FirmsDomestic, producers, consumers, io, prefweights, emp_control, c_n6_labels, c_mz_faf_reg) {

  # output summaries
  sctgcat <- data.table(Commodity_SCTG = prefweights$Commodity_SCTG,
                        SCTG_Name = prefweights$Commodity_SCTG_desc)

  # Sumamrize CBP, Producers and Consumers
  firms_sum <- list()

  # CBP
  firms_sum[["firms"]] <- FirmsDomestic[,.N]
  firms_sum[["employment"]] <- FirmsDomestic[,.(Emp = sum(Emp))]$Emp
  firms_sum[["firmsempbysctg"]] <- setcolorder(FirmsDomestic[,.(Establishments = .N, Employment = sum(Emp)), by = Commodity_SCTG][sctgcat, SCTG_Name := i.SCTG_Name, on = "Commodity_SCTG"][order(Commodity_SCTG)],
                                            c("Commodity_SCTG", "SCTG_Name", "Establishments", "Employment"))

  # IO
  firms_sum[["total_value"]] <- io[,.(ProVal = sum(ProVal))]$ProVal
  firms_sum[["Industry_NAICS_Make"]] <- length(io[, unique(Industry_NAICS6_Make)])
  firms_sum[["Industry_NAICS_Use"]] <- length(io[, unique(Industry_NAICS6_Use)])

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

  #output
  capture.output(print(firms_sum),file=file.path(SCENARIO_OUTPUT_PATH,"firm_syn.txt" ))
  fwrite(match_summary_naics_sctg, file=file.path(SCENARIO_OUTPUT_PATH,"match_summary_naics_sctg.csv" ))

  # Data to summarize in dashboard
  # 1. FirmsDomesticUnscaled -- copy of FirmsDomestic table prior to scaling
  # 2. FirmsDomestic -- FirmsDomestic table after scaling, at end of firm syn script
  # 3. emp_control -- input employment control data

  # Render the dashboard
  rmarkdown::render(input = "./lib/Dashboard/CMAP_FirmSynthesis_Summary.Rmd",
                    output_file = paste0("CMAP_FirmSynthesis_Summary.html"),
                    output_dir = SCENARIO_OUTPUT_PATH)


  return(firms_sum)

}
