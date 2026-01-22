# CMAP Freight Model
# dev script: data_ag
#
# Purpose:
# Create CBP like records from 2022 Ag data
#
# Outputs:
# data_emp_cbp_ag.csv
#
# Called as function by cmap_base_year_update.R

data_ag_2022 <- function(){

  ### READ INPUT FILES ==================================================
  
  # 2022 Ag survey data
  # Table 75 is summary by NAICS
  agdata <- fread(file.path(SYSTEM_DEV_DATA_PATH, "Ag", "USDA Ag Census 2022", "COA2022_Vol1_Ch1_Table75_edited_headers.csv"))
  
  # Complete survey data (includes county data)
  agcty <- fread(file.path(SYSTEM_DEV_DATA_PATH, "Ag", "USDA Ag Census 2022", "qs.census2022.txt"))
  
  # Correspondence
  c_naicsio_naics2017 <- fread(file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017_to NAICS2017io.csv"))
  naics2017io <- fread(file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017io.csv"))
  c_naics6_n6io_sctg <- fread(file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017_to_NAICS2017io_to_SCTG.csv"))
  
  # Read in the old CBP Ag data for format/comparison
  cbp_ag_2017 <- fread(file.path(SYSTEM_DATA_OLD_PATH, "data_emp_cbp_ag.csv"))
  
  # NAICS 2017
  # NAICS2017[NAICS3 %in% c(111, 112)]
  NAICS3_Ag_Labels <- unique(NAICS2017[NAICS3 %in% c(111, 112),.(NAICS3, Label3)])
  NAICS4_Ag_Labels <- unique(NAICS2017[NAICS3 %in% c(111, 112),.(NAICS4, Label4)])
  
  # IO data
  ioxls <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx"),
                                sheet = "2017", startRow = 6))
  
  # TAZ (New file with updated countries to match the trade data)
  TAZ_System <- fread(file.path(SYSTEM_DATA_NEW_PATH, "TAZ_System.csv"))
  
  ### PROCESS =================================================================
  
  # Extract the NAICS detail from the overall dataset for 2022 
  # names(agdata)
  
  # Categories in the NAICS summaries:
  # All farms	Oilseed and grain farming (1111)	
  # Vegetable and melon farming (1112)	
  # Fruit and tree nut farming (1113)	
  # Greenhouse nursery and floriculture production (1114)	
  # Other crop farming (1119) Total	
  # Other crop farming Tobacco (11191)	
  # Other crop farming Cotton (11192)	
  # Other crop farming Sugarcane farming hay farming and all other crop farming (11193 11194 11199)	
  # Beef cattle ranching and farming (112111)	
  # Cattle feedlots (112112)	
  # Dairy cattle and milk production (11212) 	
  # Hog and pig farming (1122)	
  # Poultry and egg production (1123)	
  # Sheep and goat farming (1124)	
  # Animal aquaculture and other animal production (1125 1129)
  
  agdata_old_names <- names(agdata)
  agdata_new_names <- gsub("[0-9]","", agdata_old_names)
  agdata_new_names <- gsub(" (","", agdata_new_names, fixed = TRUE)
  agdata_new_names <- gsub(")","", agdata_new_names, fixed = TRUE)
  agdata_new_names <- gsub(" ", "", agdata_new_names, fixed = TRUE)
  agdata_field <- data.table(old_names = agdata_old_names,
                             NAICS_Codes = c(rep(NA, 11), "1111", "1112", "1113", "1114", 
                                             "1119","11191", "11192", "11193 11194 11199",
                                             "112111", "112112", "11212", "1122", "1123",
                                             "1124", "1125 1129"),
                             new_names = agdata_new_names)
  
  setnames(agdata,
           agdata_field$old_names,
           agdata_field$new_names)
  
  # Select out the different row types
  # Number of operations, i.e. number of farms
  farms_state <- agdata[ShortDesc == "FARM OPERATIONS - NUMBER OF OPERATIONS" & DomainDesc == "TOTAL"]
  # unique(farms_state$StateName) # 50 states and US total row, columns for all farms and NAICS cats
  
  cols <- agdata_field$new_names[c(4,5,11:26)]
  farms_state <- farms_state[,..cols]
  # str(farms_state)
  cols <- agdata_field$new_names[c(11:26)]
  farms_state[,(cols) := lapply(.SD, gsub, pattern = ",", replacement = ""), .SDcols = cols]
  farms_state[,(cols) := lapply(.SD, as.integer), .SDcols = cols]
  farms_state <- melt.data.table(farms_state,
                                 id.vars = c("StateFipsCode", "StateName", "Allfarms"),
                                 variable.name = "NAICSDesc",
                                 value.name = "Farms")
  farms_state <- farms_state[StateFipsCode != 99 & NAICSDesc != "OthercropfarmingTotal"]
  # farms_state[,.(Farms = sum(Farms, na.rm = TRUE))] #1900487
  
  # Number of employees
  farms_emp <- agdata[ShortDesc %in% c("LABOR, HIRED - NUMBER OF WORKERS", "LABOR, UNPAID - NUMBER OF WORKERS")]
  cols <- agdata_field$new_names[c(4,5,7,11:26)]
  farms_emp <- farms_emp[,..cols]
  # str(farms_emp)
  cols <- agdata_field$new_names[c(11:26)]
  farms_emp[,(cols) := lapply(.SD, gsub, pattern = ",", replacement = ""), .SDcols = cols]
  farms_emp[,(cols) := lapply(.SD, gsub, pattern = "(D)", replacement = ""), .SDcols = cols]
  farms_emp[,(cols) := lapply(.SD, as.integer), .SDcols = cols]
  farms_emp <- melt.data.table(farms_emp,
                                 id.vars = c("StateFipsCode", "StateName", "ShortDesc", "Allfarms"),
                                 variable.name = "NAICSDesc",
                                 value.name = "Workers")
  farms_emp <- farms_emp[, .(Allfarms = sum(Allfarms, na.rm = TRUE), 
                             Workers = sum(Workers, na.rm = TRUE)),
                         keyby = .(StateFipsCode, StateName, NAICSDesc)]
  farms_emp <- farms_emp[StateFipsCode != 99 & NAICSDesc != "OthercropfarmingTotal"]
  # farms_emp[,.(Workers = sum(Workers, na.rm = TRUE))] #3663185
  
  # Sales: number of farms by sales amount (for allocating farms to size categories)
  farms_sales <- agdata[ShortDesc == "COMMODITY TOTALS, INCL GOVT PROGRAMS - OPERATIONS WITH RECEIPTS" & DomainDesc == "ECONOMIC CLASS"]
  cols <- agdata_field$new_names[c(4,5,8,11:26)]
  farms_sales <- farms_sales[,..cols]
  # str(farms_sales)
  cols <- agdata_field$new_names[c(11:26)]
  farms_sales[,(cols) := lapply(.SD, gsub, pattern = ",", replacement = ""), .SDcols = cols]
  farms_sales[,(cols) := lapply(.SD, gsub, pattern = "(D)", replacement = ""), .SDcols = cols]
  farms_sales[,(cols) := lapply(.SD, as.integer), .SDcols = cols]
  farms_sales[, DomaincatDesc := gsub("ECONOMIC CLASS: (", "", DomaincatDesc, fixed = TRUE)]
  farms_sales[, DomaincatDesc := gsub(" $)", "", DomaincatDesc, fixed = TRUE)]
  farms_sales <- melt.data.table(farms_sales,
                               id.vars = c("StateFipsCode", "StateName", "DomaincatDesc", "Allfarms"),
                               variable.name = "NAICSDesc",
                               value.name = "Farms")
  farms_sales[,.(Farms = sum(Farms, na.rm = TRUE))]
  # remove US totals and the grouped NAICS rows and the 1,000,000 OR MORE sales cat
  farms_sales <- farms_sales[StateFipsCode != 99 & NAICSDesc != "OthercropfarmingTotal" & DomaincatDesc != "1,000,000 OR MORE"]
  # farms_sales[,.(Farms = sum(Farms, na.rm = TRUE))] #1900487
  # farms_sales[,.(Farms = sum(Farms, na.rm = TRUE)), keyby = DomaincatDesc]
  setnames(farms_sales, "DomaincatDesc", "Sales")
  
  # Extract county level data from the country table: number of farms by NAICS
  farms_cty <- agcty[SHORT_DESC == "FARM OPERATIONS - NUMBER OF OPERATIONS" & DOMAIN_DESC == "TOTAL" & AGG_LEVEL_DESC == "COUNTY"]
  farms_cty <- farms_cty[,.(STATE_FIPS_CODE, STATE_NAME, COUNTY_CODE, COUNTY_NAME, Farms = VALUE)]
  setkey(farms_cty, STATE_FIPS_CODE, COUNTY_CODE)
  # str(farms_cty)
  farms_cty[, Farms := gsub(",", "", Farms, fixed = TRUE)]
  farms_cty[, Farms := as.integer(Farms)]
  # farms_cty[,.(Farms = sum(Farms))] #1900487
  farms_cty[, FIPS_CODE := STATE_FIPS_CODE * 1000 + COUNTY_CODE]
  # uniqueN(farms_cty$FIPS_CODE) # 3078
  
  
  farms_cty_naics <- agcty[SHORT_DESC == "FARM OPERATIONS - NUMBER OF OPERATIONS" & DOMAIN_DESC == "NAICS CLASSIFICATION" & AGG_LEVEL_DESC == "COUNTY"]
  farms_cty_naics <- farms_cty_naics[,.(STATE_FIPS_CODE, STATE_NAME, COUNTY_CODE, COUNTY_NAME, NAICS = DOMAINCAT_DESC, Farms = VALUE)]
  setkey(farms_cty_naics, STATE_FIPS_CODE, COUNTY_CODE, NAICS)
  # str(farms_cty_naics)
  farms_cty_naics[, NAICS := gsub("NAICS CLASSIFICATION: (", "", NAICS, fixed = TRUE)]
  farms_cty_naics[, NAICS := gsub(")", "", NAICS, fixed = TRUE)]
  farms_cty_naics[, NAICS := gsub(" &", "", NAICS, fixed = TRUE)]
  # unique(farms_cty_naics$NAICS)
  farms_cty_naics[, Farms := gsub(",", "", Farms, fixed = TRUE)]
  farms_cty_naics[, Farms := as.integer(Farms)]
  # farms_cty_naics[,.(Farms = sum(Farms)), keyby = NAICS]
  farms_cty_naics <- farms_cty_naics[!NAICS %in% "1119"] # remove the aggregated category
  # farms_cty_naics[,.(Farms = sum(Farms))] #1900487
  farms_cty_naics[, FIPS_CODE := STATE_FIPS_CODE * 1000 + COUNTY_CODE]
  # uniqueN(farms_cty_naics$FIPS_CODE) # 3078
  
  # add midpoint values for the sales ranges
  SalesCategories <- data.table(SalesCategory = unique(farms_sales$Sales), SalesMidPoint = c(500, 1750, 3750, 7500, 17500, 37500, 75000, 175000, 375000, 750000, 1750000, 3750000, 7500000))
  SalesCategories[, SalesRank := rank(SalesMidPoint)]
  farms_sales[SalesCategories[,.(Sales = SalesCategory, SalesMidPoint, SalesRank)], c("MidPoint", "SalesLevel") := .(i.SalesMidPoint, i.SalesRank), on = "Sales"]
  # farms_sales[, .(Farms = sum(Farms, na.rm =  TRUE)), by = .(Sales, SalesLevel)][order(SalesLevel)]
  
  # Split up the combined categories 0f 11193, 11194, 11199 and also 1125, 1129
  # Add 6 digit NAICS code -- use the first one that corresponds to a the shortened NAICS code
  # Need to remember that these farms are representative of the whole group of say 1111 farms and not just 111110
  CorrepAgNAICS_NAICS6 <- data.table(NAICSDesc = agdata_field[!is.na(NAICS_Codes) & NAICS_Codes != "1119"]$new_names,
                                     NAICS = agdata_field[!is.na(NAICS_Codes) & NAICS_Codes != "1119"]$NAICS_Codes,
                                     Industry_NAICS6_CBP = c(111110, 111211, 111310,  111411, 111910, 
                                                             111920, 111930, 112111,  112112, 112120, 
                                                             112210, 112310, 112410, 112511))
  
  farms_sales[CorrepAgNAICS_NAICS6, Industry_NAICS6_CBP := i.Industry_NAICS6_CBP, on = "NAICSDesc"]
  farms_sales[CorrepAgNAICS_NAICS6, NAICS := i.NAICS, on = "NAICSDesc"]
  
  # Enumerate the individual farms
  farms_sales <- farms_sales[!is.na(Farms)]
  farms_sales_num <- farms_sales[rep(seq_len(farms_sales[,.N]),Farms),] 
  farms_sales_num[,BusID:=.I] #Add an ID
  farms_sales_num[, c("Allfarms", "Farms") := NULL]
  
  # corresponding 6 digit NAICS codes in the NAICS code system
  c_naics6_n6io_sctg[Commodity_SCTG %in% 1:3 & Industry_NAICS6_CBP < 112999]
  # Main issue is the split between SCTG 2 and 3 within the 1111 group
  # IO data supports that allocation into two subcategories of 1111A0 and 1111B0
  io1111 <- ioxls[Code %in% c("1111A0", "1111B0")]
  io1111 <- melt(io1111, 
                 id.vars = c("Code", "Commodity.Description"),
                 variable.name = "Industry_NAICS6_Use",
                 value.name = "ProVal")
  setnames(io1111, "Code", "Industry_NAICS6_Make")
  io1111 <- io1111[Industry_NAICS6_Use %in% c(c_naicsio_naics2017$NAICSio, "F04000", "F05000")]
  Shares1111 <- io1111[, .(ProVal = sum(ProVal, na.rm = TRUE)), 
                       by = Industry_NAICS6_Make][, PctProVal := ProVal/sum(ProVal)][]
  
  # if a random uniform draw is greater than the share of 1111A0, change the NAICS6 code to the corresponding NAICS6 code for 1111B0 which is 111130
  # (as before 111130 is representative all of the 1111B0 codes like 111140, 111150 etc)
  set.seed(BASE_SEED_VALUE)
  farms_sales_num[, temprand := runif(.N)]
  farms_sales_num[NAICS == "1111", Industry_NAICS6_CBP := ifelse(temprand < Shares1111[Industry_NAICS6_Make == "1111A0"]$PctProVal, 111110, 111130)]
  farms_sales_num[, temprand := NULL]
  
  # Allocate each farm to a county based on number of farms by NAICS code
  # Enumerate the list of farms for sampling without replacement
  farms_cty_naics <- farms_cty_naics[rep(seq_len(farms_cty_naics[,.N]),Farms),] 
  
  set.seed(BASE_SEED_VALUE)
  for (statefips in unique(farms_sales_num$StateFipsCode)){
    for (naicscode in unique(farms_sales_num[StateFipsCode == statefips]$NAICS)){
      if(nrow(farms_sales_num[StateFipsCode == statefips & NAICS == naicscode]) == 1){
        farms_sales_num[StateFipsCode == statefips & NAICS == naicscode, 
                        CountyFipsCode := farms_cty_naics[STATE_FIPS_CODE == statefips & NAICS == naicscode]$COUNTY_CODE]
      } else {
        farms_sales_num[StateFipsCode == statefips & NAICS == naicscode, 
                      CountyFipsCode := sample(x = farms_cty_naics[STATE_FIPS_CODE == statefips & NAICS == naicscode]$COUNTY_CODE, size = .N, replace = FALSE)]
      }
    }
  }
  
  # Add the County name
  farms_sales_num[unique(farms_cty_naics[,.(StateFipsCode = STATE_FIPS_CODE, CountyFipsCode = COUNTY_CODE, COUNTY_NAME)]), 
                  CountyName := i.COUNTY_NAME, 
                  on = c("StateFipsCode", "CountyFipsCode")]
  
  # Calculate sales per worker and use that to estimate employment at the farm level in proportion to sales
  farms_sales[, SalesTotal := Farms * MidPoint]
  farms_sales_total <- farms_sales[,.(SalesTotal = sum(SalesTotal), Farms = sum(Farms)), keyby = .(StateFipsCode, StateName, NAICSDesc, NAICS)]
  farms_emp[farms_sales_total, c("SalesTotal", "Farms") := .(i.SalesTotal, i.Farms), on = c("StateFipsCode", "NAICSDesc")]
  # farms_emp[!is.na(SalesTotal), sum(SalesTotal)]
  # farms_emp[!is.na(SalesTotal) & Workers == 0, sum(SalesTotal)]/farms_emp[!is.na(SalesTotal), sum(SalesTotal)]
  # farms_emp[!is.na(SalesTotal) & Workers > 0 & Workers < Farms]
  # farms_emp[!is.na(SalesTotal) & Workers > 0 & Workers > Farms]
  # farms_emp[!is.na(SalesTotal) & Workers == 0, Workers := Farms] # Assume 1 emp per farm
  farms_emp[, SalesPerWorker := SalesTotal/Workers]
  
  farms_sales_num[farms_emp, SalesPerWorker := i.SalesPerWorker, on = c("StateFipsCode", "NAICSDesc")]
  farms_sales_num[, Employment := MidPoint/SalesPerWorker]
  farms_sales_num[, EmploymentRound := ifelse(Employment < 1, 1, round(Employment))]
  # farms_sales_num[, .(EmploymentRound = sum(EmploymentRound))]
  # farms_emp[,.(Workers = sum(Workers))]
  # The large number of very small farms with low sales means that rounding all farms to have one
  # employee pushes up the total ag employment significantly, from 3.7 million to 4,8 million
  
  # add an employment size variable, e1 - e8
  # ranges are:
  # 1='1-19',2='20-99',3='100-249',4='250-499',5='500-999',6='1,000-2,499',7='2,500-4,999',8='Over 5,000'
  farms_sales_num[, esizecat := findInterval(EmploymentRound, c(0, 19, 99, 249, 499, 999, 2499, 4999))]
  
  # Add some systems for aggregation
  farms_sales_num[, FipsCode := StateFipsCode * 1000 + CountyFipsCode]
  farms_sales_num[TAZ_System[,.(FipsCode = CountyFIPS, FAFZONE)], FAFZONE := i.FAFZONE, on = c("FipsCode")]
  farms_sales_num[TAZ_System[CBPZONE <= 123], CBPZONE := i.CBPZONE, on = "FAFZONE"]
  farms_sales_num[FipsCode %in% TAZ_System[CBPZONE > 123]$CBPZONE, CBPZONE := FipsCode]
  
  # check for and fix any missing FAFZONE/CBPZONE assignments
  # unique(farms_sales_num[is.na(CBPZONE), .(FipsCode, CountyFipsCode, CountyName, StateFipsCode, StateName)])
  missing_fipscode <- unique(farms_sales_num[is.na(CBPZONE), .(FipsCode, CountyFipsCode, CountyName, StateFipsCode, StateName)]$FipsCode)
  # farms_cty_naics[FIPS_CODE %in% missing_fipscode]
  
  # TAZ_System[state == "AK"]
  farms_sales_num[FipsCode == 2010, FipsCode := 2013] # Aleutians East (2010 split into 2013 and 2016)
  
  # Update the other zone codes
  farms_sales_num[TAZ_System[,.(FipsCode = CountyFIPS, FAFZONE)], FAFZONE := i.FAFZONE, on = c("FipsCode")]
  farms_sales_num[TAZ_System[CBPZONE <= 123], CBPZONE := i.CBPZONE, on = "FAFZONE"]
  farms_sales_num[FipsCode %in% TAZ_System[CBPZONE > 123]$CBPZONE, CBPZONE := FipsCode]
  # unique(farms_sales_num[is.na(CBPZONE), .(FipsCode, CountyFipsCode, CountyName, StateFipsCode, StateName)])
  
  # write out the full list of farms for reference
  fwrite(farms_sales_num, file.path(SYSTEM_DEV_DATA_PATH, "Ag", "data_farms_enumerated_list.csv"))
  
  # Convert the list by county to a list summarized for each CBPZone, NAICS 6 code, and esizecat
  
  # > str(cbp)
  # Classes ‘data.table’ and 'data.frame':	118586 obs. of  13 variables:
  #   $ Industry_NAICS6_CBP: int  113110 113310 114210 115111 115112 115210 115310 211111 212111 212112 ...
  # $ FAFZONE            : int  11 11 11 11 11 11 11 11 11 11 ...
  # $ CBPZONE            : int  1 1 1 1 1 1 1 1 1 1 ...
  # $ employment         : num  0 491 0 0 0 73 160 0 883 0 ...
  # $ establishment      : int  1 80 2 2 1 18 15 4 20 6 ...
  # $ e1                 : int  1 76 1 2 1 18 12 2 5 3 ...
  # $ e2                 : int  0 4 1 0 0 0 1 2 12 1 ...
  # $ e3                 : int  0 0 0 0 0 0 2 0 3 0 ...
  # $ e4                 : int  0 0 0 0 0 0 0 0 0 1 ...
  # $ e5                 : int  0 0 0 0 0 0 0 0 0 1 ...
  # $ e6                 : int  0 0 0 0 0 0 0 0 0 0 ...
  # $ e7                 : int  0 0 0 0 0 0 0 0 0 0 ...
  # $ e8                 : int  0 0 0 0 0 0 0 0 0 0 ...
  # - attr(*, ".internal.selfref")=<externalptr>
  
  data_emp_cbp_ag_size <- farms_sales_num[, .(employment = sum(EmploymentRound), establishment = .N),
                                      by = .(Industry_NAICS6_CBP, FAFZONE, CBPZONE, esizecat)]
  
  # convert the esizecat variable to a set of columns
  data_emp_cbp_ag_size_cast <- dcast.data.table(data_emp_cbp_ag_size, Industry_NAICS6_CBP + FAFZONE + CBPZONE ~ esizecat, fun.aggregate = sum, value.var = "establishment")
  setnames(data_emp_cbp_ag_size_cast, c("1", "2", "3", "4", "5"), c("e1", "e2", "e3", "e4", "e5"))
  data_emp_cbp_ag_size_cast[, paste0("e", 6:8) := 0L]
  data_emp_cbp_ag <- data_emp_cbp_ag_size[, .(employment = sum(employment), establishment = sum(establishment)),
                                          by = .(Industry_NAICS6_CBP, FAFZONE, CBPZONE)]
  data_emp_cbp_ag <- merge(data_emp_cbp_ag, 
                           data_emp_cbp_ag_size_cast, 
                           by = c("Industry_NAICS6_CBP", "FAFZONE", "CBPZONE"))
  setkey(data_emp_cbp_ag, FAFZONE, Industry_NAICS6_CBP)
  
  # sum(data_emp_cbp_ag$establishment)
  # sum(data_emp_cbp_ag$employment)
  
  ### WRITE ==================================================
  
  # CBP formatted version of Ag data
  fwrite(data_emp_cbp_ag, 
         file.path(SYSTEM_DEV_DATA_PATH, "Ag", "data_emp_cbp_ag.csv"))
  
  # Copy the file for use in the model to the folder for new inputs
  file.copy(from = file.path(SYSTEM_DEV_DATA_PATH, "Ag", "data_emp_cbp_ag.csv"),
            to = file.path(SYSTEM_DATA_NEW_PATH, "data_emp_cbp_ag.csv"), overwrite = TRUE)
  
  paste("Finished writing ", file.path(SYSTEM_DATA_NEW_PATH, "data_emp_cbp_ag.csv"))

}