# CMAP Freight Model
# dev script: data_trade.R
#
# Purpose:
# Process the base year and forecast trade data for imports and exports
#
# Outputs:
# Clean trade data (base and forecasts) ready for writing to separate scenario folders
# data_foreign_prod_BaseCase_cleaned.csv
# data_foreign_cons_BaseCase_cleaned.csv
#
# Called as function by cmap_scenarios_build_from_forecasts.R

data_trade <- function(macro_inputs_path){

  ### READ INPUT FILES ==================================================
  
  # Macroeconomic input and interim output files for scenario input development are stored in
  # "dev/FutureScenarios/Macroeconomic Inputs"
  dev_forecasts_dir <- file.path(SYSTEM_DEV_PATH, "FutureScenarios")
  macro_inputs_path <- file.path(dev_forecasts_dir, "Macroeconomic Inputs")
  
  # Read in the trade data
  for_prod_wide <- data.table(read.xlsx(file.path(macro_inputs_path, "Updated_Foreign_Trade_Forecasts_20250827.xlsx"),
                             sheet = "data_foreign_prod_BaseCase"))
  for_cons_wide <- data.table(read.xlsx(file.path(macro_inputs_path, "Updated_Foreign_Trade_Forecasts_20250827.xlsx"),
                             sheet = "data_foreign_cons_BaseCase"))
  
  # TAZ (New file with updated countries to match the trade data)
  TAZ_System <- fread(file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))
  
  # cbp, cbg_ag (new data for 2022)
  cbp <- fread(file.path(SYSTEM_DATA_PATH, "data_emp_cbp.csv"))
  cbp_ag <- fread(file.path(SYSTEM_DATA_PATH, "data_emp_cbp_ag.csv"))
  
  ### PROCESS ==================================================
  
  # Clean up the files
  # replace NA values with 0
  for_prod_wide[is.na(for_prod_wide)] <- 0
  for_cons_wide[is.na(for_cons_wide)] <- 0
  
  # keep list of fieldnames for later after analysis to easily trim the files
  for_prod_names <- copy(names(for_prod_wide))
  for_cons_names <- copy(names(for_cons_wide))
  
  # Check the countries and commodity codes in the foreign production and consumption files
  # Recode as needed
  
  for_cons_wide[NAICS2017[,.(Commodity_Naics6 = as.character(NAICS6))], NAICS6_2017 := 1, on = "Commodity_Naics6"]
  unique(for_cons_wide[is.na(NAICS6_2017),.(Commodity_Naics6)])
  
  for_prod_wide[NAICS2017[,.(Commodity_Naics6 = as.character(NAICS6))], NAICS6_2017 := 1, on = "Commodity_Naics6"]
  unique(for_prod_wide[is.na(NAICS6_2017),.(Commodity_Naics6)])
  
  # NAICS 2017 matches except codes ending in X 
  # -- these are confirmed to be NAICS 2017 codes and 
  # just need to handle these exceptions
  
  # Commodity_Naics6
  # <char>
  # 1:           33641X
  # 2:           1123XX
  # 3:           31131X
  # 4:           31135X
  # 5:           31181X
  # 6:           1121XX
  
  # # Commodity_Naics6
  # Commodity_Naics6
  # <char>
  # 1:           31135X
  # 2:           31181X
  # 3:           31131X
  # 4:           1123XX
  # 5:           1121XX
  
  # Convert the codes with X and XX as the 6th or 5th and 6th characters
  for_cons_wide[grep("X", Commodity_Naics6), 
                c("Commodity_Naics4", "Commodity_Naics5") := 
                  .(substr(Commodity_Naics6,1,4), substr(Commodity_Naics6,1,5))]
  for_cons_wide[!is.na(Commodity_Naics4)]
  
  for_prod_wide[grep("X", Commodity_Naics6), 
                c("Commodity_Naics4", "Commodity_Naics5") := 
                  .(substr(Commodity_Naics6,1,4), substr(Commodity_Naics6,1,5))]
  for_prod_wide[!is.na(Commodity_Naics4)]
  
  # Add matches for NAIC4_2017 and NAICS5_2017
  for_cons_wide[NAICS2017[,.(Commodity_Naics4 = as.character(NAICS4))], NAICS4_2017 := 1, on = "Commodity_Naics4"]
  for_cons_wide[NAICS2017[,.(Commodity_Naics5 = as.character(NAICS5))], NAICS5_2017 := 1, on = "Commodity_Naics5"]
  for_cons_wide[grep("X", Commodity_Naics6), table(NAICS4_2017)]
  for_cons_wide[grep("X", Commodity_Naics6), table(NAICS5_2017)]
  
  for_prod_wide[NAICS2017[,.(Commodity_Naics4 = as.character(NAICS4))], NAICS4_2017 := 1, on = "Commodity_Naics4"]
  for_prod_wide[NAICS2017[,.(Commodity_Naics5 = as.character(NAICS5))], NAICS5_2017 := 1, on = "Commodity_Naics5"]
  for_prod_wide[grep("X", Commodity_Naics6), table(NAICS4_2017)]
  for_prod_wide[grep("X", Commodity_Naics6), table(NAICS5_2017)]
  
  # Calculate some check totals for total value
  base_cons <- for_cons_wide[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_prod <- for_prod_wide[, .(USImpVal_2022 = sum(USImpVal_2022))]
  
  # Most match to a valid 5-digit NAICS, all match to a valid 4-digit NAICS
  # Need to disaggregate proportionally to all of the valid/possible 6 digit NAICS
  # within those 5 and digit categories
  
  # split the table into three sections -- valid 6 digits, valid 5 and valid 4
  # do the disaggregation and then combine to leave a table with all valid 6 digit coding
  
  for_cons_wide_n6 <- for_cons_wide[!is.na(NAICS6_2017) | is.na(NAICS4_2017)]
  for_cons_wide_n5 <- for_cons_wide[!is.na(NAICS5_2017)]
  for_cons_wide_n4 <- for_cons_wide[!is.na(NAICS4_2017) & is.na(NAICS5_2017)]
  
  base_cons_n6 <- for_cons_wide_n6[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_cons_n5 <- for_cons_wide_n5[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_cons_n4 <- for_cons_wide_n4[, .(USExpVal_2022 = sum(USExpVal_2022))]
  
  for_cons_wide_n5[, Commodity_Naics6 := NULL]
  for_cons_wide_n5 <- merge(for_cons_wide_n5,
                            NAICS2017[,.(Commodity_Naics6 = as.character(NAICS6), Commodity_Naics5 = as.character(NAICS5))],
                            by = "Commodity_Naics5",
                            all.x = TRUE,
                            allow.cartesian = TRUE)
  
  for_cons_wide_n4[, Commodity_Naics6 := NULL]
  for_cons_wide_n4 <- merge(for_cons_wide_n4,
                            NAICS2017[,.(Commodity_Naics6 = as.character(NAICS6), Commodity_Naics4 = as.character(NAICS4))],
                            by = "Commodity_Naics4",
                            all.x = TRUE,
                            allow.cartesian = TRUE)
  
  for_prod_wide_n6 <- for_prod_wide[!is.na(NAICS6_2017) | is.na(NAICS4_2017)]
  for_prod_wide_n5 <- for_prod_wide[!is.na(NAICS5_2017)]
  for_prod_wide_n4 <- for_prod_wide[!is.na(NAICS4_2017) & is.na(NAICS5_2017)]
  
  base_prod_n6 <- for_prod_wide_n6[, .(USImpVal_2022 = sum(USImpVal_2022))]
  base_prod_n5 <- for_prod_wide_n5[, .(USImpVal_2022 = sum(USImpVal_2022))]
  base_prod_n4 <- for_prod_wide_n4[, .(USImpVal_2022 = sum(USImpVal_2022))]
  
  for_prod_wide_n5[, Commodity_Naics6 := NULL]
  for_prod_wide_n5 <- merge(for_prod_wide_n5,
                            NAICS2017[,.(Commodity_Naics6 = as.character(NAICS6), Commodity_Naics5 = as.character(NAICS5))],
                            by = "Commodity_Naics5",
                            all.x = TRUE,
                            allow.cartesian = TRUE)
  
  for_prod_wide_n4[, Commodity_Naics6 := NULL]
  for_prod_wide_n4 <- merge(for_prod_wide_n4,
                            NAICS2017[,.(Commodity_Naics6 = as.character(NAICS6), Commodity_Naics4 = as.character(NAICS4))],
                            by = "Commodity_Naics4",
                            all.x = TRUE,
                            allow.cartesian = TRUE)
  
  # Since this is based on 4-5 and 6 digit NAICS codes, do the disaggregation based on employment split
  # in the US CBP data. Obviously this an approximation but means that at least the import export split
  # will be somewhat consistent with the amount of US production/consumption
  # IO codes are simpler so shares from io table won't be that helpful here.
  cbp <- rbind(cbp[Industry_NAICS6_CBP >= 113110], cbp_ag)
  cbp_sum <- cbp[,.(emp = sum(employment)), keyby = .(Commodity_Naics6 = as.character(Industry_NAICS6_CBP))]
  
  for_cons_wide_n5[cbp_sum, emp := i.emp, on = "Commodity_Naics6"]
  for_cons_wide_n5[is.na(emp), emp := 0]
  for_cons_wide_n5[, emp_pct := emp/sum(emp), by = .(Commodity_Naics5, Country)]
  for_cons_wide_n5[is.na(emp_pct)]
  
  for_cons_wide_n4[cbp_sum, emp := i.emp, on = "Commodity_Naics6"]
  for_cons_wide_n4[is.na(emp), emp := 0]
  for_cons_wide_n4[, emp_pct := emp/sum(emp), by = .(Commodity_Naics4, Country)]
  for_cons_wide_n4[is.na(emp_pct)]
  
  cols <- paste("USExpVal", c(2022, seq(2025, 2060, by = 5)), sep = "_")
  for_cons_wide_n5[, (cols) := lapply(.SD,"*", emp_pct), .SDcols = cols]
  for_cons_wide_n4[, (cols) := lapply(.SD,"*", emp_pct), .SDcols = cols]
  
  for_prod_wide_n5[cbp_sum, emp := i.emp, on = "Commodity_Naics6"]
  for_prod_wide_n5[is.na(emp), emp := 0]
  for_prod_wide_n5[, emp_pct := emp/sum(emp), by = .(Commodity_Naics5, Country)]
  for_prod_wide_n5[is.na(emp_pct)]
  
  for_prod_wide_n4[cbp_sum, emp := i.emp, on = "Commodity_Naics6"]
  for_prod_wide_n4[is.na(emp), emp := 0]
  for_prod_wide_n4[, emp_pct := emp/sum(emp), by = .(Commodity_Naics4, Country)]
  for_prod_wide_n4[is.na(emp_pct)]
  
  cols <- paste("USImpVal", c(2022, seq(2025, 2060, by = 5)), sep = "_")
  for_prod_wide_n5[, (cols) := lapply(.SD,"*", emp_pct), .SDcols = cols]
  for_prod_wide_n4[, (cols) := lapply(.SD,"*", emp_pct), .SDcols = cols]
  
  # check the totals by table
  base_cons_n6_up <- for_cons_wide_n6[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_cons_n5_up <- for_cons_wide_n5[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_cons_n4_up <- for_cons_wide_n4[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_prod_n6_up <- for_prod_wide_n6[, .(USImpVal_2022 = sum(USImpVal_2022))]
  base_prod_n5_up <- for_prod_wide_n5[, .(USImpVal_2022 = sum(USImpVal_2022))]
  base_prod_n4_up <- for_prod_wide_n4[, .(USImpVal_2022 = sum(USImpVal_2022))]
  
  identical(base_cons_n6, base_cons_n6_up)
  identical(base_cons_n5, base_cons_n5_up)
  identical(base_cons_n4, base_cons_n4_up)
  identical(base_prod_n6, base_prod_n6_up)
  identical(base_prod_n5, base_prod_n5_up)
  identical(base_prod_n4, base_prod_n4_up)
  
  # recombine the three tables
  for_cons_wide <- rbind(for_cons_wide_n6[,for_cons_names, with = FALSE],
                         for_cons_wide_n5[,for_cons_names, with = FALSE],
                         for_cons_wide_n4[,for_cons_names, with = FALSE])
  
  setorder(for_cons_wide, CtryCod, Commodity_Naics6)
  
  for_prod_wide <- rbind(for_prod_wide_n6[,for_prod_names, with = FALSE],
                         for_prod_wide_n5[,for_prod_names, with = FALSE],
                         for_prod_wide_n4[,for_prod_names, with = FALSE])
  
  setorder(for_prod_wide, CtryCod, Commodity_Naics6)
  
  base_cons_up <- for_cons_wide[, .(USExpVal_2022 = sum(USExpVal_2022))]
  base_prod_up <- for_prod_wide[, .(USImpVal_2022 = sum(USImpVal_2022))]
  
  identical(base_cons, base_cons_up)
  identical(base_prod, base_prod_up)
  
  # Update the TAZs etc in the trade data
  # rename the fields to match TAZ_System naming/case
  setnames(for_cons_wide, c("CtryCod", "FAFZone", "CBPZone"), c("ctrycod", "FAFZONE", "CBPZONE"))
  setnames(for_prod_wide, c("CtryCod", "FAFZone", "CBPZone"), c("ctrycod", "FAFZONE", "CBPZONE"))
  
  for_cons_wide[, c("ctrycod", "FAFZONE", "CBPZONE") := NA]
  for_prod_wide[, c("ctrycod", "FAFZONE", "CBPZONE") := NA]
  
  for_cons_wide[TAZ_System[,.(Country = newCountry, FAFZONE, newCBPZONE, newctrycod)], 
                c("FAFZONE", "ctrycod", "CBPZONE") := 
                  .(i.FAFZONE, i.newctrycod, i.newCBPZONE), 
                on = "Country"]
  
  for_prod_wide[TAZ_System[,.(Country = newCountry, FAFZONE, newCBPZONE, newctrycod)], 
                c("FAFZONE", "ctrycod", "CBPZONE") := 
                  .(i.FAFZONE, i.newctrycod, i.newCBPZONE), 
                on = "Country"]
  
  # check
  for_cons_wide[is.na(ctrycod)]
  for_prod_wide[is.na(ctrycod)]
  
  # sort
  setkey(for_cons_wide, ctrycod, Commodity_Naics6)
  setkey(for_prod_wide, ctrycod, Commodity_Naics6)
  
  ### WRITE ==================================================
  
  # Cleaned trade data
  # Write out the files to the forecast input folder for use in creating the scenario files
  
  fwrite(for_prod_wide, 
         file.path(macro_inputs_path, "data_foreign_prod_BaseCase_cleaned.csv"))
  
  fwrite(for_cons_wide, 
         file.path(macro_inputs_path, "data_foreign_cons_BaseCase_cleaned.csv"))
  
  paste("Finished writing ", file.path(macro_inputs_path, "data_foreign_cons_BaseCase_cleaned.csv"), "and",
        file.path(macro_inputs_path, "data_foreign_prod_BaseCase_cleaned.csv"))
  
}
