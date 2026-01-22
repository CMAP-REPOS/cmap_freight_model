# CMAP Freight Model
# dev script: data_io.R
#
# Purpose:
# Create IO table input from the spreadsheet from US BEA
#
# Outputs:
#
# Inputs output table scaled to base year production value:
# data_2022io.csv
#
# Called as function by cmap_base_year_update.R

data_io <- function(macro_inputs_path){

### READ INPUT FILES ==================================================

# Read in matrix from spreadsheet
# IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx
# From https://apps.bea.gov/industry/xls/io-annual/IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx

ioxls <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx"),
                   sheet = "2017", startRow = 6))

# Additional sheets in the spreadsheet with correspondences
c_naicsio_naics2017_2_6 <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "BEA_NAICS_2017.xlsx"),
                                                sheet = "Corresp_NACSIO_NAICS2017_2_6"))

naics2017_6 <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "BEA_NAICS_2017.xlsx"),
                                    sheet = "NAICS2017_6"))

# Read in the processed NAICS io codes for 2017 (produced in earlier script: data_naics2017_corresp)
naics2017io <- fread(file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017io.csv"))

# Updated correspondence between NAICS and SCTG for 2017 (produced in earlier script: data_naics2017_corresp)
c_n6_n6io_sctg_2017 <- fread(file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017_to_NAICS2017io_to_SCTG.csv"))

# Sheets from the productivity spreadsheet prepared by EBP
moodys_data <- data.table(read.xlsx(xlsxFile = file.path(macro_inputs_path, "CMAP_Productivity_Data_2060.xlsx"),
                              sheet = "MoodysData(Raw)", rows = 1:178))
prod_factors <- data.table(read.xlsx(xlsxFile = file.path(macro_inputs_path, "CMAP_Productivity_Data_2060.xlsx"),
                                   sheet = "Implied Productivity Scalar", startRow = 5))

# employment forecasts
emp_forecasts <- fread(file.path(macro_inputs_path, "data_emp_control_county_BaseCase_v3.csv"))

# trade forecasts 
# (after processing/clean up)
for_prod_wide <- fread(file.path(macro_inputs_path, "data_foreign_prod_BaseCase_cleaned.csv"))
for_cons_wide <- fread(file.path(macro_inputs_path, "data_foreign_cons_BaseCase_cleaned.csv"))

# cbp, cbg_ag fo comparison with IO and trade data in terms of NAICS coding
cbp <- fread(file.path(SYSTEM_DATA_NEW_PATH, "data_emp_cbp.csv"))
cbp_ag <- fread(file.path(SYSTEM_DATA_NEW_PATH, "data_emp_cbp_ag.csv"))

### PROCESS ==================================================

### IO MATRIX ----------------------------------------------------

# Process the IO matrix
# Remove some of the rows and columns that deal with totals and final consumption

# Matrix columns and rows
# names(ioxls)
# ioxls$Code

# Select the cells from the matrix that are the relevant
# NAICS IO codes referring to industries
# Also keep the import and export fields for accounting for foreign trade

# Remove extra rows:
# Anything not in the NAICS 6 to NAICS IO correspondence
ioxls <- ioxls[Code %in% unique(c_naicsio_naics2017$NAICSio)]
# Remove anything with no output (total intermediate, imports, exports)
ioxls <- ioxls[!(is.na(T001) & is.na(F04000) & is.na(F05000))]

# Remove extra columns:
cols <- names(ioxls)[names(ioxls) %in% c("Code", "Commodity.Description", c_naicsio_naics2017$NAICSio, "F04000", "F05000")]
ioxls <- ioxls[,..cols]

# Relabel the Code, F04000, F05000 fields
setnames(ioxls, c("Code", "Commodity.Description", "F04000", "F05000"), c("Industry_NAICS6_Make", "Industry_NAICS6_Make_Desc", "Exports", "Imports"))
io_prod_tradetype <- ioxls[,.(Industry_NAICS6_Make, Industry_NAICS6_Make_Desc, Exports, Imports)]

# Melt the matrix
io <- melt.data.table(ioxls,
                      id.vars = c("Industry_NAICS6_Make_Desc", "Industry_NAICS6_Make"),
                      variable.name = "Industry_NAICS6_Use",
                      value.name = "ProVal")
# Remove any NA ProVal rows
io <- io[!is.na(ProVal)]
# Remove any zero ProVal rows
io <- io[ProVal != 0]

# Add a domestic summary column to the io_prod_tradetype
io_prod_domestic <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
                       .(Domestic = sum(ProVal)), keyby = .(Industry_NAICS6_Make_Desc, Industry_NAICS6_Make)]

io_prod_tradetype <- merge(io_prod_domestic,
                           io_prod_tradetype, 
                           by  = c("Industry_NAICS6_Make_Desc", "Industry_NAICS6_Make"),
                           all = TRUE)
io_prod_tradetype[is.na(Domestic), Domestic := 0]
io_prod_tradetype[is.na(Exports), Exports := 0]
io_prod_tradetype[is.na(Imports), Imports := 0]
io_prod_tradetype[, NetValue := Domestic + Exports + Imports]
io_prod_tradetype[, DomesticProduction := Domestic + Exports]
io_prod_tradetype[, DomesticConsumption := Domestic - Imports] # Negative as Imports is express as a negative (in terms of domestic production)
setorder(io_prod_tradetype, Industry_NAICS6_Make)

# write out that as the 2017 io summary
io_prod_tradetype_2017_summary <- copy(io_prod_tradetype)

# write out the long 2017 version of the io table
# this is not for use in the model -- will scale to base year to create a base 2022 io table here
io_2017 <- copy(io)

# Deal with any scaling to the base year for the model
# Moodys data has both Employment and GDP rows for each employment sector
# Sectors are 3 digit NAICS, except that: 
# all government is combined as 92
# Ag is just split into two groups FR = FARMS and Fishing; hunting; etc. = FH
# Missing from the dataset are NAICS 491 (postal service)
# Also no NAICS 516 (Internet Publishing and Broadcasting), but there are no records in the CBP data for 516 either
emp_2017_2022 <- moodys_data[ForecastType == "EMP",.(CMAP.Sector, CleanDesc, `2017`, `2022`)]
emp_2017_2022[, emp_fac_2017_2022 := `2022`/`2017`]
emp_2017_2022[prod_factors, prod_fac_2017 := i.2017, on = "CMAP.Sector"]
emp_2017_2022[, value_fac_2017_2022 := emp_fac_2017_2022 * prod_fac_2017]

# Create a correspondence between the CMAP.Sector field in this file and a complete set of 3 digit NAICS codes that
# matches the IO table via the census NAICS codes

c_naicsio_naics3_cmapsector <- copy(c_naicsio_naics2017)
c_naicsio_naics3_cmapsector[, NAICS3 := substr(NAICS, 1, 3)]
c_naicsio_naics3_cmapsector[, CMAP.Sector := NAICS3]

# what are the mismatches with the CMAP.Sector list:
# unique(c_naicsio_naics3_cmapsector[!CMAP.Sector %in% emp_2017_2022$CMAP.Sector]$CMAP.Sector)
#[1] "111" "112" "113" "114" "115" "491" "920"
# unique(emp_2017_2022[!CMAP.Sector %in% c_naicsio_naics3_cmapsector$CMAP.Sector]$CMAP.Sector)
#[1] "FH" "FR" "92"

# unique(NAICS2017[NAICS3 %in% unique(c_naicsio_naics3_cmapsector[!CMAP.Sector %in% emp_2017_2022$CMAP.Sector]$CMAP.Sector),
#                  .(NAICS3, Label3)])
# NAICS3                                          Label3
# 1:    111                                 Crop Production
# 2:    112               Animal Production and Aquaculture
# 3:    113                            Forestry and Logging
# 4:    114                   Fishing, Hunting and Trapping
# 5:    115 Support Activities for Agriculture and Forestry
# 6:    491                                  Postal Service

c_naicsio_naics3_cmapsector[NAICS3 %in% c(111,112,115), CMAP.Sector := "FR"]
c_naicsio_naics3_cmapsector[NAICS3 %in% c(113,114), CMAP.Sector := "FH"]
c_naicsio_naics3_cmapsector[NAICS3 %in% c(920), CMAP.Sector := "92"]
# c_naicsio_naics3_cmapsector[NAICS3 == 491]

# Need to add 491 to the 2017_2022 table (and also to the productivity factors table)
# NAICS2017[NAICS3 == "491"]
emp_2017_2022 <- rbind(emp_2017_2022,
                       data.table("491", "Postal Service", NA, NA, 1,1,1), use.names = FALSE)

# Join the 2017-2022 factors to the correspondence
c_naicsio_naics3_cmapsector[emp_2017_2022, 
                            value_fac_2017_2022 := i.value_fac_2017_2022,
                            on = "CMAP.Sector"]

# The Naicsio categories do not nest perfectly within the NAICS 3 cats -- there is some
# many to many matches here, particularly in construction and also retail.
# c_naicsio_naics3_cmapsector[,.(Num = uniqueN(CMAP.Sector)), keyby = NAICSio][Num > 1]
# c_naicsio_naics3_cmapsector[NAICSio == "230301"]
# c_naicsio_naics3_cmapsector[NAICSio == "48A000"]
# c_naicsio_naics3_cmapsector[NAICSio == "4B0000"]
# c_naicsio_naics3_cmapsector[NAICSio == "52A000"]

c_naicsio_naics3_cmapsector[emp_2017_2022, 
                            emp_2017 := i.2017,
                            on = "CMAP.Sector"]

c_naicsio_naics3_io_fac <- unique(c_naicsio_naics3_cmapsector[,.(NAICSio, NAICS3, CMAP.Sector, value_fac_2017_2022, emp_2017)])
c_naicsio_naics3_io_fac[, emp_2017_pct := emp_2017/sum(emp_2017), by = NAICSio]
c_naicsio_naics3_io_fac[is.na(emp_2017_pct), emp_2017_pct := 1]

# calculate the employment weighted average of the factors for each NAICS IO code
c_naicsio_io_fac <- c_naicsio_naics3_io_fac[,.(value_fac_2017_2022 = sum(value_fac_2017_2022 * emp_2017_pct)/sum(emp_2017_pct)), keyby = NAICSio]

# Replace the import and export data for 2022 with the trade data before other scaling 
# so that domestic scaling is accounts for balance of trade
# io[Industry_NAICS6_Use %in% c("Imports", "Exports"), sum(ProVal), by = Industry_NAICS6_Use]

# First reallocate the public sector production/consumption into other non-public industries
public_sector_naics_codes <- c(910000, 920000, 930000, 980000, 990000)

for_prod_2022 <- for_prod_wide[,.(Imports = sum(USImpVal_2022)/1e6), keyby = .(NAICS = Commodity_Naics6)]
for_prod_no_pub <- for_prod_2022[!NAICS %in% public_sector_naics_codes, .(ImportsNoPub = sum(Imports))]
for_prod_pub <- for_prod_2022[, .(Imports = sum(Imports))]
for_prod_pub_scale = as.numeric(for_prod_pub/for_prod_no_pub)
for_prod_2022 <- for_prod_2022[!NAICS %in% public_sector_naics_codes]
for_prod_2022[, Imports := Imports * for_prod_pub_scale]

for_cons_2022 <- for_cons_wide[,.(Exports = sum(USExpVal_2022)/1e6), keyby = .(NAICS = Commodity_Naics6)]
for_cons_no_pub <- for_cons_2022[!NAICS %in% public_sector_naics_codes, .(ExportsNoPub = sum(Exports))]
for_cons_pub <- for_cons_2022[, .(Exports = sum(Exports))]
for_cons_pub_scale = as.numeric(for_cons_pub/for_cons_no_pub)
for_cons_2022 <- for_cons_2022[!NAICS %in% public_sector_naics_codes]
for_cons_2022[, Exports := Exports * for_cons_pub_scale]

# Convert the NAICS6 codes to NAICS IO codes and then replace the records in the IO data
for_cons_2022[c_naicsio_naics2017, Industry_NAICS6_Make := i.NAICSio, on = "NAICS"]
for_cons_2022 <- for_cons_2022[,.(Industry_NAICS6_Use = "Exports", ProVal = sum(Exports)), keyby = Industry_NAICS6_Make]
for_prod_2022[c_naicsio_naics2017, Industry_NAICS6_Make := i.NAICSio, on = "NAICS"]
for_prod_2022 <- for_prod_2022[,.(Industry_NAICS6_Use = "Imports", ProVal = -sum(Imports)), keyby = Industry_NAICS6_Make]

io <- rbind(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal)],
            for_cons_2022, for_prod_2022)

### Checks on coding ----------------------------------------------------------------------------------

# Check the categorization of IO and trade data compared to the cbp data to look for any mismatches
# anything to change/consolidate before scaling and/or deal with in the code?
# cbp, cbg_ag
cbp <- rbind(cbp[Industry_NAICS6_CBP >= 113110], cbp_ag)
cbp_sum <- cbp[,.(est = sum(establishment), emp = sum(employment)), keyby = .(Industry_NAICS6_CBP = as.numeric(Industry_NAICS6_CBP))]
cbp_sum[c_n6_n6io_sctg_2017, Industry_NAICS6_Make := i.Industry_NAICS6_Make, on = "Industry_NAICS6_CBP"]
# cbp_sum[is.na(Industry_NAICS6_Make)] # nothing missing
# sort(unique(io[!Industry_NAICS6_Make %in% cbp_sum$Industry_NAICS6_Make]$Industry_NAICS6_Make))
#[1] "230301" "230302" "322110" "333242" "482000" "491000" "S00102" "S00203"
# naics2017io[NAICSIO %in% sort(unique(io[!Industry_NAICS6_Make %in% cbp_sum$Industry_NAICS6_Make]$Industry_NAICS6_Make))]
# NAICSIO                                 IndustryName Sector
# 1:  230301        Nonresidential maintenance and repair     23
# 2:  230302           Residential maintenance and repair     23
# 3:  322110                                   Pulp mills   31ND
# 4:  333242        Semiconductor machinery manufacturing   33DG
# 5:  482000                          Rail transportation   48TW
# 6:  491000                               Postal service      G
# 7:  S00102         Other federal government enterprises      G
# 8:  S00203 Other state and local government enterprises      G

# naics2017io[NAICSIO %in% sort(unique(io[!Industry_NAICS6_Use %in% cbp_sum$Industry_NAICS6_Make]$Industry_NAICS6_Use))]

# NAICSIO                                               IndustryName Sector
# 1:  230301                      Nonresidential maintenance and repair     23
# 2:  230302                         Residential maintenance and repair     23
# 3:  233210                                     Health care structures     23
# 4:  233230                                   Manufacturing structures     23
# 5:  233240                         Power and communication structures     23
# 6:  233262                      Educational and vocational structures     23
# 7:  233411                       Single-family residential structures     23
# 8:  233412                         Multifamily residential structures     23
# 9:  322110                                                 Pulp mills   31ND
# 10:  333242                      Semiconductor machinery manufacturing   33DG
# 11:  482000                                        Rail transportation   48TW
# 12:  491000                                             Postal service      G
# 13:  2332A0                           Office and commercial structures     23
# 14:  2332C0         Transportation structures and highways and streets     23
# 15:  2332D0                            Other nonresidential structures     23
# 16:  531HST                                    Tenant-occupied housing     53
# 17:   GSLGE          State and local government (educational services)      G
# 18:   GSLGH State and local government (hospitals and health services)      G
# 19:   GSLGO                State and local government (other services)      G
# 20:  S00101                                 Federal electric utilities      G
# 21:  S00102                       Other federal government enterprises      G
# 22:  S00201               State and local government passenger transit      G
# 23:  S00202              State and local government electric utilities      G
# 24:  S00203               Other state and local government enterprises      G
# 25:  S00500                       Federal general government (defense)      G
# 26:  S00600                    Federal general government (nondefense)      G
# NAICSIO                                               IndustryName Sector

# Check the private manufacturing industries
# Missing from CBP but in IO
# 9:  322110                                                 Pulp mills   31ND
# 10:  333242                      Semiconductor machinery manufacturing   33DG
# io[Industry_NAICS6_Make == 322110] # yes
# io[Industry_NAICS6_Make == 333242] # yes
# io[Industry_NAICS6_Use == 322110] # yes
# io[Industry_NAICS6_Use == 333242] # yes
# cbp_sum[Industry_NAICS6_Make == 322110] # no
# cbp_sum[Industry_NAICS6_Make == 333242] # no

# Difference in coding across datasets:
# See notes in IO table for 331314 Secondary smelting and alloying of aluminum
# It is handled a bit unusually, only appears on the use (industry) side and not on the make (commodity) side
# ‡ Primary output of the "secondary smelting and alloying of aluminum" and 
# "alumina refining and primary aluminum production" industries is treated as 
# being identical and is reported as "alumina refining and primary aluminum production" 
# for both industries.  As a result, "secondary smelting and alloying of aluminum" appears 
# as an industry at the detail level but not as a commodity.						

# naics2017io[NAICSIO == 331314]
# c_n6_n6io_sctg_2017[Industry_NAICS6_Make == 331314]
# io[Industry_NAICS6_Make == 331314] # Only value for imports and exports from the trade data
# io[Industry_NAICS6_Use == 331314] # Lots of records on the use side (industry)
# cbp_sum[Industry_NAICS6_Make == 331314] # 11 est

# Comparable to 331313 Alumina refining and primary aluminum production
# naics2017io[NAICSIO == 331313]
# c_n6_n6io_sctg_2017[Industry_NAICS6_Make == 331313]
# io[Industry_NAICS6_Make == 331313] # Lots of records including trade data
# io[Industry_NAICS6_Use == 331313] # Lots of records on the use side (industry)
# cbp_sum[Industry_NAICS6_Make == 331313] # 3 est

# Should 331314 all be converted to and aggregated with 331313 in all datasets?
# This is done in the code in the input processing script

# Rail Transportation
# 11:  482000                                        Rail transportation   48TW
# naics2017io[NAICSIO == 482000]
# c_n6_n6io_sctg_2017[Industry_NAICS6_Make == 482000]
# io[Industry_NAICS6_Make == 482000] # Lots of records including trade data
# io[Industry_NAICS6_Use == 482000] # Lots of records on the use side (industry)
# io[Industry_NAICS6_Make == 482000, sum(ProVal)] 
# io[Industry_NAICS6_Use == 482000, sum(ProVal)]
# cbp_sum[Industry_NAICS6_Make == 482000] # just no establishments in the CBP
# cbp_sum[substr(Industry_NAICS6_Make,1,2) == "48"] # Lots of other 48x codes, just no rail -- is this due to censoring?

# Construction:
# See notes in IO table for 23 construction 

#† Construction data published by BEA at the detail level do not align with 2017 NAICS industries.  
# In NAICS, industries are classified based on their production processes, 
# whereas BEA construction is classified by type of structure.  
# For example, activity by the 2017 NAICS Roofing contractors industry would be split 
# among many BEA construction categories because roofs are built on many types of structures.						

# naics2017io[substr(NAICSIO,1,2) %in% "23"]$NAICSIO
# dcast.data.table(c_n6_n6io_sctg_2017[substr(Industry_NAICS6_Make,1,2) %in% "23"],
#                  Industry_NAICS6_Make ~ Industry_NAICS6_CBP,
#                  fun.aggregate = length, value.var = "Commodity_SCTG")
# 
# io[substr(Industry_NAICS6_Make,1,2) %in% "23", sum(ProVal), keyby = Industry_NAICS6_Make]
# io[ substr(Industry_NAICS6_Use,1,2) %in% "23", sum(ProVal), keyby = Industry_NAICS6_Use]
# cbp_sum[substr(Industry_NAICS6_Make,1,2) %in% "23"] # 3 est

# Simplify everything to just construction as the crosswalk does not work
# for the reason explained in the io table notes.

# government: 
# 12:  491000                                             Postal service      G
# 17:   GSLGE          State and local government (educational services)      G
# 18:   GSLGH State and local government (hospitals and health services)      G
# 19:   GSLGO                State and local government (other services)      G
# 20:  S00101                                 Federal electric utilities      G
# 21:  S00102                       Other federal government enterprises      G
# 22:  S00201               State and local government passenger transit      G
# 23:  S00202              State and local government electric utilities      G
# 24:  S00203               Other state and local government enterprises      G
# 25:  S00500                       Federal general government (defense)      G
# 26:  S00600                    Federal general government (nondefense)      G

# naics2017io[substr(NAICSIO,1,3) %in% "491"]$NAICSIO
# c_n6_n6io_sctg_2017[substr(Industry_NAICS6_Make,1,3) %in% "491"]
# io[substr(Industry_NAICS6_Make,1,3) %in% "491", sum(ProVal), keyby = Industry_NAICS6_Make]
# io[ substr(Industry_NAICS6_Use,1,3) %in% "491", sum(ProVal), keyby = Industry_NAICS6_Use]
# io[ substr(Industry_NAICS6_Use,1,3) %in% "491"]
# cbp_sum[substr(Industry_NAICS6_Make,1,3) %in% "491"] # just missing from the CBP as it is a government entity

# naics2017io[substr(NAICSIO,1,2) %in% "S0"]$NAICSIO
# c_n6_n6io_sctg_2017[substr(Industry_NAICS6_Make,1,2) %in% "S0"]
# io[substr(Industry_NAICS6_Make,1,2) %in% "S0", sum(ProVal), keyby = Industry_NAICS6_Make]
# io[ substr(Industry_NAICS6_Use,1,2) %in% "S0", sum(ProVal), keyby = Industry_NAICS6_Use]
# io[ substr(Industry_NAICS6_Use,1,2) %in% "S0"]
# cbp_sum[substr(Industry_NAICS6_Make,1,2) %in% "S0"] # just missing from the CBP as it is a government entity
# cbp_sum[substr(Industry_NAICS6_CBP,1,2) %in% "92"] # just missing from the CBP as it is a government entity

# Simulate government businesses in the code
# Use a single category like proposed for construction for government consumption, assume no output
# Trade data has foreign government prod/cons reallocated proportionally to all other codes

# Other codes GS... and also  
# 16:  531HST                                    Tenant-occupied housing     53

# io[substr(Industry_NAICS6_Make,1,2) %in% "GS", sum(ProVal), keyby = Industry_NAICS6_Make] #0 
# io[ substr(Industry_NAICS6_Use,1,2) %in% "GS", sum(ProVal), keyby = Industry_NAICS6_Use]  # significant
 
# io[substr(Industry_NAICS6_Make,1,2) %in% "53", sum(ProVal), keyby = Industry_NAICS6_Make] #0 
# io[ substr(Industry_NAICS6_Use,1,2) %in% "53", sum(ProVal), keyby = Industry_NAICS6_Use]  # significant

# what do these codes get matched with?
# naics2017io[substr(NAICSIO,1,2) %in% "GS"]$NAICSIO
# c_n6_n6io_sctg_2017[substr(Industry_NAICS6_Make,1,2) %in% "GS"] ## matched with 920000

# naics2017io[substr(NAICSIO,1,2) %in% "53"]$NAICSIO
# c_n6_n6io_sctg_2017[substr(Industry_NAICS6_Make,1,2) %in% "53"] ## same codes matched to 531HST, 531ORE

# cbp_sum[substr(Industry_NAICS6_Make,1,2) %in% "GS"] # just missing from the CBP as it is a government entity
# cbp_sum[substr(Industry_NAICS6_CBP,1,2) %in% "53"] # plenty of 531 its just matched with 531ORE instead of 531HST

# Summary:
# Handle mismatches in the firm synthesis input processing

# 1. Construction (23) should be grouped as 230000 in all datasets
# 2. Manufacturing
#    a. combine 331313 and 331314 into single code across all datasets -- use 331313
#    b. look for production value from missing industries -- (example -- pulp mills)
# 3. Other missing non-public codes -- e.g., rail transportation
# 4. 531 appears similar to construction in terms of mismatch between coding methods.
#    Simplify like with construction so single 531000 group
# 5. Government including postal -- approach in the code simulates government consumption
#    Production less important 

### Continue with scaling 2017 to 2022 ----------------------------------------------

# Use the factors to scale the domestic portion of the 2017 IO table to 2022
io[c_naicsio_io_fac[,.(Industry_NAICS6_Make = NAICSio, value_fac_2017_2022)], make_fac := i.value_fac_2017_2022, on = "Industry_NAICS6_Make"]
io[c_naicsio_io_fac[,.(Industry_NAICS6_Use = NAICSio, value_fac_2017_2022)], use_fac := i.value_fac_2017_2022, on = "Industry_NAICS6_Use"]
io[is.na(make_fac), make_fac := 1]
io[is.na(use_fac), use_fac := 1]

# For imports and exports, reset the make factor to 1 as they are already 2022 values
io[Industry_NAICS6_Use %in% c("Exports", "Imports"), make_fac := 1]

# Develop 2022 values by ipf to ensure that the two dimensions are grown correctly by industry
io[, Prod2022 := ProVal * make_fac]
io[, Cons2022 := ProVal * use_fac]
io[, ProVal2022 := ProVal * make_fac * use_fac]

# Scale the consumption and seed values to match with the sum of Prod2022
# Only scale the non-import and export values
io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
   .(ProVal = sum(ProVal), Prod2022 = sum(Prod2022), Cons2022 = sum(Cons2022), ProVal2022 = sum(ProVal2022))]
ConsFactor <- as.numeric(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),.(Prod2022 = sum(Prod2022))]/io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Cons2022 = sum(Cons2022))])
ProVal2022Factor <- as.numeric(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),.(Prod2022 = sum(Prod2022))]/io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(ProVal2022 = sum(ProVal2022))])
io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), Cons2022 := Cons2022 * ConsFactor]
io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ProVal2022 := ProVal2022 * ProVal2022Factor]
io[,.(ProVal = sum(ProVal), Prod2022 = sum(Prod2022), Cons2022 = sum(Cons2022), ProVal2022 = sum(ProVal2022))]

# Doubly Constrain the production value on the industry level total production and total consumption
for (i in 1:10){
  
  # Constrain on Consumption
  io_cons <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Cons = sum(Cons2022), PVCons = sum(ProVal2022)), keyby = Industry_NAICS6_Use]
  io_cons[, CAdj := ifelse(PVCons != 0, Cons/PVCons, 1)]
  io[io_cons, CAdj := i.CAdj, on = "Industry_NAICS6_Use"]
  io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ProVal2022 := ProVal2022 * CAdj]
  
  # Constrain on Production
  io_prod <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Prod = sum(Prod2022), PVProd = sum(ProVal2022)), keyby = Industry_NAICS6_Make]
  io_prod[, PAdj := ifelse(PVProd != 0, Prod/PVProd, 1)]
  io[io_prod, PAdj := i.PAdj, on = "Industry_NAICS6_Make"]
  io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ProVal2022 := ProVal2022 * PAdj]
  
}

# Create a similar 2022 summary table from the factored io table
io[naics2017io[,.(Industry_NAICS6_Make = NAICSIO, IndustryName)], Industry_NAICS6_Make_Desc := i.IndustryName, on = "Industry_NAICS6_Make"]
io_prod_tradetype <- dcast.data.table(io[Industry_NAICS6_Use %in% c("Exports", "Imports"),
                                        .(ProVal2022 = sum(ProVal2022)), keyby = .(Industry_NAICS6_Make_Desc, Industry_NAICS6_Make, Industry_NAICS6_Use)],
                                      Industry_NAICS6_Make_Desc + Industry_NAICS6_Make ~ Industry_NAICS6_Use,
                                      fun.aggregate = sum, value.var = "ProVal2022")

# Add a domestic summary column to the io_prod_tradetype
io_prod_domestic <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
                       .(Domestic = sum(ProVal2022)), keyby = .(Industry_NAICS6_Make_Desc, Industry_NAICS6_Make)]

io_prod_tradetype <- merge(io_prod_domestic,
                           io_prod_tradetype, 
                           by  = c("Industry_NAICS6_Make_Desc", "Industry_NAICS6_Make"),
                           all = TRUE)
io_prod_tradetype[is.na(Domestic), Domestic := 0]
io_prod_tradetype[is.na(Exports), Exports := 0]
io_prod_tradetype[is.na(Imports), Imports := 0]
io_prod_tradetype[, NetValue := Domestic + Exports + Imports]
io_prod_tradetype[, DomesticProduction := Domestic + Exports]
io_prod_tradetype[, DomesticConsumption := Domestic - Imports] # Negative as Imports is express as a negative (in terms of domestic production)
setorder(io_prod_tradetype, Industry_NAICS6_Make)

# write out that as the 2022 io summary
io_prod_tradetype_2022_summary <- copy(io_prod_tradetype)

# write out the long 2022 version of the io table
# this is the version to use in the model pending any revisions after validating against the trade data
io_2022 <- copy(io)
io_2022_for_input <- io_2022[,.(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal = ProVal2022)]

### Future scaling of IO test -----------------------------------------------------
 
# Estimate production/consumption and domestic/imports/exports by year using the 
# productivity factors, employment forecasts

# Need to follow the process used above to grow the IO table out to a future year.
# Function to handle this now in the model code
# Create a prod factors table that is already fully in NAICS 3 codes to avoid need for crosswalk

io <- io_2022[,.(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal = ProVal2022)] # for 2022
# emp_forecasts # 2022 is the base year (the file includes the "Employment" field which is same as 2019
# prod_factors # base of 1 is 2022
# for_cons_wide # future year import values
# for_prod_wide # future year export values

# use the correspondence created above and first remove the 2017-2022 fields
c_naicsio_naics3_cmapsector[, c("value_fac_2017_2022", "emp_2017") := NULL]
# c_naicsio_naics3_cmapsector[NAICS3 == "491"] # complete correspondence between NAICS 6, 3, NAICSIO, and CMAP.Sector used for the prod factors
c_naicsio_naics3_cmapsector[, NAICS2 := as.integer(substr(NAICS,1,2))]

# Need an employment growth factor for each 2 digit NAICS group
# Just do this for base year and one future: 2025 here but will need to use a variable (for both base year and future (scenario) year)
c_naicsio_naics3_cmapsector[emp_forecasts[,.(EmploymentBase = sum(`2022`), 
                                             EmploymentFuture = sum(`2025`)), 
                                          keyby = .(NAICS2 = NAICS)], 
                            c("EmploymentBase", "EmploymentFuture") := .(i.EmploymentBase, i.EmploymentFuture),
                            on = "NAICS2"]

c_naicsio_naics3_cmapsector[, emp_factor := EmploymentFuture/EmploymentBase]
c_naicsio_naics3_cmapsector[is.na(emp_factor), emp_factor := 1]

# extract the productivity factor for the future year (it is relative to 1 for 2022)
c_naicsio_naics3_cmapsector[prod_factors, prod_factor := i.2025, on = "CMAP.Sector"]
c_naicsio_naics3_cmapsector[is.na(prod_factor), prod_factor := 1] # just 491

# calculate the value factor
c_naicsio_naics3_cmapsector[, value_factor := emp_factor * prod_factor]

# simplify to a unique factor for each IO code -- 
# in the model code need to do the employment weighting by 6 digit sector?
# using the sector totals here is just a placeholder, not correct
c_naicsio_naics3_io_fac <- unique(c_naicsio_naics3_cmapsector[,.(NAICSio, NAICS3, CMAP.Sector, EmploymentBase, value_factor)])
c_naicsio_naics3_io_fac[, emp_pct := EmploymentBase/sum(EmploymentBase), by = NAICSio]
c_naicsio_naics3_io_fac[is.na(emp_pct), emp_pct := 1]

# calculate the employment weighted average of the factors for each NAICS IO code
c_naicsio_io_fac <- c_naicsio_naics3_io_fac[,.(value_factor = sum(value_factor * emp_pct)/sum(emp_pct)), keyby = NAICSio]

# Replace the import and export data for the future year with the trade data before other scaling 
# so that domestic scaling is accounts for balance of trade
# io[Industry_NAICS6_Use %in% c("Imports", "Exports"), sum(ProVal), by = Industry_NAICS6_Use]

# First reallocate the public sector production/consumption into other non-public industries
public_sector_naics_codes <- c(910000, 920000, 930000, 980000, 990000)

for_prod_future <- for_prod_wide[,.(Imports = sum(USImpVal_2025)/1e6), keyby = .(NAICS = Commodity_Naics6)]
for_prod_no_pub <- for_prod_future[!NAICS %in% public_sector_naics_codes, .(ImportsNoPub = sum(Imports))]
for_prod_pub <- for_prod_future[, .(Imports = sum(Imports))]
for_prod_pub_scale = as.numeric(for_prod_pub/for_prod_no_pub)
for_prod_future <- for_prod_future[!NAICS %in% public_sector_naics_codes]
for_prod_future[, Imports := Imports * for_prod_pub_scale]

for_cons_future <- for_cons_wide[,.(Exports = sum(USExpVal_2025)/1e6), keyby = .(NAICS = Commodity_Naics6)]
for_cons_no_pub <- for_cons_future[!NAICS %in% public_sector_naics_codes, .(ExportsNoPub = sum(Exports))]
for_cons_pub <- for_cons_future[, .(Exports = sum(Exports))]
for_cons_pub_scale = as.numeric(for_cons_pub/for_cons_no_pub)
for_cons_future <- for_cons_future[!NAICS %in% public_sector_naics_codes]
for_cons_future[, Exports := Exports * for_cons_pub_scale]

# Convert the NAICS6 codes to NAICS IO codes and then replace the records in the IO data
for_cons_future[c_naicsio_naics2017, Industry_NAICS6_Make := i.NAICSio, on = "NAICS"]
for_cons_future <- for_cons_future[,.(Industry_NAICS6_Use = "Exports", ProVal = sum(Exports)), keyby = Industry_NAICS6_Make]
for_prod_future[c_naicsio_naics2017, Industry_NAICS6_Make := i.NAICSio, on = "NAICS"]
for_prod_future <- for_prod_future[,.(Industry_NAICS6_Use = "Imports", ProVal = -sum(Imports)), keyby = Industry_NAICS6_Make]

io <- rbind(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Industry_NAICS6_Make, Industry_NAICS6_Use, ProVal)],
            for_cons_future, for_prod_future)


# Use the factors to scale the 2022 IO table to the future year
io[c_naicsio_io_fac[,.(Industry_NAICS6_Make = NAICSio, value_factor)], make_fac := i.value_factor, on = "Industry_NAICS6_Make"]
io[c_naicsio_io_fac[,.(Industry_NAICS6_Use = NAICSio, value_factor)], use_fac := i.value_factor, on = "Industry_NAICS6_Use"]
io[is.na(make_fac), make_fac := 1]
io[is.na(use_fac), use_fac := 1]

# For imports and exports, reset the make factor to 1 as they are already scaled values
io[Industry_NAICS6_Use %in% c("Exports", "Imports"), make_fac := 1]

# Develop Future values by ipf to ensure that the two dimensions are grown correctly by industry
io[, ProdFuture := ProVal * make_fac]
io[, ConsFuture := ProVal * use_fac]
io[, ProValFuture := ProVal * make_fac * use_fac]


# Scale the consumption and seed values to match with the sum of Prod2022
# Only scale the non-import and export values
# io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),.(ProVal = sum(ProVal), ProdFuture = sum(ProdFuture), ConsFuture = sum(ConsFuture), ProValFuture = sum(ProValFuture))]
ConsFactor <- as.numeric(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),.(ProdFuture = sum(ProdFuture))]/io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(ConsFuture = sum(ConsFuture))])
ProValFutureFactor <- as.numeric(io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),.(ProdFuture = sum(ProdFuture))]/io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(ProValFuture = sum(ProValFuture))])
io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ConsFuture := ConsFuture * ConsFactor]
io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ProValFuture := ProValFuture * ProValFutureFactor]
# io[,.(ProVal = sum(ProVal), ProdFuture = sum(ProdFuture), ConsFuture = sum(ConsFuture), ProValFuture = sum(ProValFuture))]

# Doubly Constrain the production value on the industry level total production and total consumption
for (i in 1:10){
  
  # Constrain on Consumption
  io_cons <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Cons = sum(ConsFuture), PVCons = sum(ProValFuture)), keyby = Industry_NAICS6_Use]
  io_cons[, CAdj := ifelse(PVCons != 0, Cons/PVCons, 1)]
  io[io_cons, CAdj := i.CAdj, on = "Industry_NAICS6_Use"]
  io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ProValFuture := ProValFuture * CAdj]
  
  # Constrain on Production
  io_prod <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), .(Prod = sum(ProdFuture), PVProd = sum(ProValFuture)), keyby = Industry_NAICS6_Make]
  io_prod[, PAdj := ifelse(PVProd != 0, Prod/PVProd, 1)]
  io[io_prod, PAdj := i.PAdj, on = "Industry_NAICS6_Make"]
  io[!Industry_NAICS6_Use %in% c("Exports", "Imports"), ProValFuture := ProValFuture * PAdj]
  
}

# Create a similar future year summary table from the factored io table
io_prod_tradetype <- dcast.data.table(io[Industry_NAICS6_Use %in% c("Exports", "Imports"),
                                         .(ProValFuture = sum(ProValFuture)), keyby = .(Industry_NAICS6_Make, Industry_NAICS6_Use)],
                                      Industry_NAICS6_Make ~ Industry_NAICS6_Use,
                                      fun.aggregate = sum, value.var = "ProValFuture")

# Add a domestic summary column to the io_prod_tradetype
io_prod_domestic <- io[!Industry_NAICS6_Use %in% c("Exports", "Imports"),
                       .(Domestic = sum(ProValFuture)), keyby = .(Industry_NAICS6_Make)]

io_prod_tradetype <- merge(io_prod_domestic,
                           io_prod_tradetype, 
                           by  = "Industry_NAICS6_Make",
                           all = TRUE)
io_prod_tradetype[is.na(Domestic), Domestic := 0]
io_prod_tradetype[is.na(Exports), Exports := 0]
io_prod_tradetype[is.na(Imports), Imports := 0]
io_prod_tradetype[, NetValue := Domestic + Exports + Imports]
io_prod_tradetype[, DomesticProduction := Domestic + Exports]
io_prod_tradetype[, DomesticConsumption := Domestic - Imports] # Negative as Imports is express as a negative (in terms of domestic production)

# add make labels

io_prod_tradetype[naics2017io[,.(Industry_NAICS6_Make = NAICSIO, Industry_NAICS6_Make_Desc = IndustryName)],
                  Industry_NAICS6_Make_Desc := i.Industry_NAICS6_Make_Desc, on = "Industry_NAICS6_Make"]
setcolorder(io_prod_tradetype, c("Industry_NAICS6_Make_Desc", names(io_prod_tradetype)[1:7]))
setorder(io_prod_tradetype, Industry_NAICS6_Make)

# write out that as the future io summary
io_prod_tradetype_future_summary <- copy(io_prod_tradetype)

# future io
io_future <- copy(io)

# Productivity factors in format for use in the model
# Need all of the forecast years (2022-2060) and already in correct 3 digit format
# This will be processed into year specific files in the scenarios_built_from_forecasts.R script

naics3_cmapsector <- unique(c_naicsio_naics3_cmapsector[,.(NAICS2, NAICS3, CMAP.Sector)])
naics3_cmapsector[prod_factors, paste0("prod_factor_", c(2022, seq(2025, 2060, by = 5))) := 
                    .(i.2022, i.2025, i.2030, i.2035, i.2040, i.2045, i.2050, i.2055, i.2060), 
                  on = "CMAP.Sector"]
naics3_cmapsector[is.na(prod_factor_2022), paste0("prod_factor_", c(2022, seq(2025, 2060, by = 5))) := 1] # just 491

### WRITE ==================================================

fwrite(io_prod_tradetype_2017_summary, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_prod_tradetype_2017_summary.csv"))
fwrite(io_2017, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_2017.csv"))
fwrite(io_prod_tradetype_2022_summary, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_prod_tradetype_2022_summary.csv"))
fwrite(io_2022, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_2022_with_processing.csv"))
fwrite(io_2022_for_input, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_2022.csv"))
fwrite(io_prod_tradetype_future_summary, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_prod_tradetype_future_summary.csv"))
fwrite(io_future, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_future.csv"))

# Write out the processed productivity factors associated with the full set of NAICS3 codes
# back to the forecast folder as well as the working dev data folder
fwrite(naics3_cmapsector, 
       file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "data_productivity_factors.csv"))

fwrite(naics3_cmapsector, 
       file.path(macro_inputs_path, "data_productivity_factors.csv"))

# Copy IO data to the new data inputs folder for use in the model
file.copy(from = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "io_2022.csv"),
          to = file.path(SYSTEM_DATA_NEW_PATH, "data_2022io.csv"), overwrite = TRUE)

paste("Finished writing ", file.path(SYSTEM_DATA_NEW_PATH, "data_2022io.csv"))

}