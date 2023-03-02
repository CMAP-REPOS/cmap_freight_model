
#Enumerate foreign firms and merge with correspondenses
firm_synthesis_enumerate_foreign <- function(for_prod, for_cons, c_n6_n6io_sctg){

  # Both for_prod and for_cons include foreign public production/consumption value.
  # Reallocate within each country to the remaining privately owned industies in proportion to their prod/cons value
  # In the future, research commodity shares for public production and consumption and allocate in a more detailed way

  # Foreign Production
  for_prod_sum <- for_prod[, .(USImpVal = as.numeric(sum(USImpVal))), by = .(CBPZONE, FAFZONE)]
  for_prod_sum_no_pub <- for_prod[!Commodity_NAICS6 %in% c(910000, 920000, 980000, 990000), .(USImpValNoPub = as.numeric(sum(USImpVal))), by = .(CBPZONE, FAFZONE)]
  for_prod_sum <- merge(for_prod_sum,
                        for_prod_sum_no_pub,
                        by = c("CBPZONE", "FAFZONE"),
                        all.x = TRUE)
  for_prod_sum[is.na(USImpValNoPub), USImpValNoPub := 0]
  for_prod_sum[, ProdScale := ifelse(USImpValNoPub > 0, USImpVal/USImpValNoPub, 0)]

  # account for countries with no non-public production by reallocating within FAF ZONE so FAF ZONE production is conserved
  for_prod_faf <- for_prod_sum[, .(USImpVal = sum(USImpVal)), by = FAFZONE]
  for_prod_faf_no_pub <- for_prod_sum[USImpValNoPub > 0, .(USImpValNoPub = sum(USImpVal)), by = FAFZONE]
  for_prod_faf <- merge(for_prod_faf,
                        for_prod_faf_no_pub,
                        by = "FAFZONE",
                        all.x = TRUE)
  for_prod_faf[, ProdScaleFAF := USImpVal/USImpValNoPub]

  # do the scaling for foreign production
  for_prod <- for_prod[!Commodity_NAICS6 %in% c(910000, 920000, 980000, 990000)]
  for_prod[for_prod_sum, ProdScale := i.ProdScale, on = "CBPZONE"]
  for_prod[for_prod_faf, ProdScaleFAF := i.ProdScaleFAF, on = "FAFZONE"]
  for_prod[, USImpVal := USImpVal * ProdScale * ProdScaleFAF]

  # Foreign Consumption
  for_cons_sum <- for_cons[, .(USExpVal = as.numeric(sum(USExpVal))), by = .(CBPZONE, FAFZONE)]
  for_cons_sum_no_pub <- for_cons[!Commodity_NAICS6 %in% c(910000, 920000, 980000, 990000), .(USExpValNoPub = as.numeric(sum(USExpVal))), by = .(CBPZONE, FAFZONE)]
  for_cons_sum <- merge(for_cons_sum,
                        for_cons_sum_no_pub,
                        by = c("CBPZONE", "FAFZONE"),
                        all.x = TRUE)
  for_cons_sum[is.na(USExpValNoPub), USExpValNoPub := 0]
  for_cons_sum[, ConsScale := ifelse(USExpValNoPub > 0, USExpVal/USExpValNoPub, 0)]

  # account for counties with no non-public consumption by reallocating within FAF ZONE so FAF ZONE consumption is conserved
  for_cons_faf <- for_cons_sum[, .(USExpVal = sum(USExpVal)), by = FAFZONE]
  for_cons_faf_no_pub <- for_cons_sum[USExpValNoPub > 0, .(USExpValNoPub = sum(USExpVal)), by = FAFZONE]
  for_cons_faf <- merge(for_cons_faf,
                        for_cons_faf_no_pub,
                        by = "FAFZONE",
                        all.x = TRUE)
  for_cons_faf[, ConsScaleFAF := USExpVal/USExpValNoPub]

  # do the scaling for foreign consumption
  for_cons <- for_cons[!Commodity_NAICS6 %in% c(910000, 920000, 980000, 990000)]
  for_cons[for_cons_sum, ConsScale := i.ConsScale, on = "CBPZONE"]
  for_cons[for_cons_faf, ConsScaleFAF := i.ConsScaleFAF, on = "FAFZONE"]
  for_cons[, USExpVal := USExpVal * ConsScale * ConsScaleFAF]

  # Enurerate foreign producers and consumers
  # Create one agent per country per commodity
  # Remove any records where the country and FAF zone are not known
  Firms <- rbind(for_prod[!is.na(FAFZONE),.(Industry_NAICS6_CBP = Commodity_NAICS6,
                             ProdVal = USImpVal/1000000,
                             CBPZONE,
                             FAFZONE,
                             FirmType = "ForeignProducer")],
                 for_cons[!is.na(FAFZONE),.(Industry_NAICS6_CBP = Commodity_NAICS6,
                             ProdVal = USExpVal/1000000,
                             CBPZONE,
                             FAFZONE,
                             FirmType = "ForeignConsumer")])

  # Merge in the I/O NAICS codes and SCTG codes
  Firms[c_n6_n6io_sctg,
        c("Industry_NAICS6_Make" , "Commodity_SCTG") := .(i.Industry_NAICS6_Make, i.Commodity_SCTG),
        on  = "Industry_NAICS6_CBP"]

  # There are certain Industry_NAICS6_CBP codes that make multiple SCTG codes - split the Production Value
  # to create one firm/country combination for each of the SCTG codes
  Firms_211111 <- rbind(Firms[Industry_NAICS6_CBP == 211111][, c("Commodity_SCTG", "ProdVal") := .(16L, ProdVal * 0.45)],
                        Firms[Industry_NAICS6_CBP == 211111][, c("Commodity_SCTG", "ProdVal") := .(19L, ProdVal * 0.55)])

  Firms <- rbind(Firms[Industry_NAICS6_CBP != 211111],
                 Firms_211111)

  Firms_324110 <- rbind(Firms[Industry_NAICS6_CBP == 324110][, c("Commodity_SCTG", "ProdVal") := .(17L, ProdVal * 0.25)],
                        Firms[Industry_NAICS6_CBP == 324110][, c("Commodity_SCTG", "ProdVal") := .(18L, ProdVal * 0.25)],
                        Firms[Industry_NAICS6_CBP == 324110][, c("Commodity_SCTG", "ProdVal") := .(19L, ProdVal * 0.5)])

  Firms <- rbind(Firms[Industry_NAICS6_CBP != 324110],
                 Firms_324110)

  # Create and save a diagnostics file -- firms that did not get a match in the IO NAICS/SCTG correspondence
  # Then filter the firms with missing NAICS IO codes out (ideally there will be no firms removed)
  fwrite(Firms[is.na(Industry_NAICS6_Make)], file = file.path(SCENARIO_OUTPUT_PATH, "FirmsForeign_No_Match_NAICSIO.csv"))
  fwrite(Firms[is.na(Industry_NAICS6_Make), .(ProdVal = sum(ProdVal)), by = .(Industry_NAICS6_CBP, FirmType)], file = file.path(SCENARIO_OUTPUT_PATH, "FirmsForeign_No_Match_NAICSIO_Summary.csv"))

  # Summarize and remove any industries with unknown NAICS IO codes
  Firms <- Firms[!is.na(Industry_NAICS6_Make),
                 .(ProdVal = sum(ProdVal)),
                 by = .(Industry_NAICS6_Make, CBPZONE, FAFZONE, Commodity_SCTG, FirmType)]

  # Return the foreign firms table
  return(Firms)

}

