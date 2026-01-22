# CMAP Freight Model
# dev script: data_cbp_2022
#
# Purpose:
# Process new 2022 CBP data for use in firm synthesis
#
# Outputs:
# Processed CBP data
# data_emp_cbp.csv
#
# Called as function by cmap_base_year_update.R

data_cbp_2022 <- function(){

  ### READ INPUT FILES ==================================================
  
  # 2022 CBP data for all US and by county
  # https://www2.census.gov/programs-surveys/cbp/datasets/2022/cbp22us.zip
  # https://www2.census.gov/programs-surveys/cbp/datasets/2022/cbp22st.zip
  # https://www2.census.gov/programs-surveys/cbp/datasets/2022/cbp22co.zip
  cbp_2022_us <- fread(file.path(SYSTEM_DEV_DATA_PATH, "CBP_Emp_HH", "cbp22us.txt"))
  cbp_2022_st <- fread(file.path(SYSTEM_DEV_DATA_PATH, "CBP_Emp_HH", "cbp22st.txt"))
  cbp_2022_co <- fread(file.path(SYSTEM_DEV_DATA_PATH, "CBP_Emp_HH", "cbp22co.txt"))
  
  # 2017 CBP data for correct file format for model input
  cbp_2017 <- fread(file.path(SYSTEM_DATA_OLD_PATH, "data_emp_cbp.csv"))
  
  # NAICS 2017 (in rFreight) -- note that 2022 CBP still uses 2017 NAICS codes, not 2022 codes
  # NAICS2017
  
  # TAZ/County/FAF/CBPZONE coding
  TAZ_System <- fread(file.path(SYSTEM_DATA_NEW_PATH, "TAZ_System.csv"))
  
  ### PROCESS =================================================================
  
  # Extract the 6 digit records in counties (exclude summary geog and naics) and format the data 
  # cbp_2022_co[naics %in% NAICS2017$NAICS6]
  
  # check if there are any 6 digit NAICS not in the list
  other_naics <- sort(unique(cbp_2022_co[!naics %in% NAICS2017$NAICS6]$naics))
  other_naics <- gsub("/", "", other_naics, fixed = TRUE)
  other_naics <- gsub("-", "", other_naics, fixed = TRUE)
  # other_naics[nchar(other_naics)==6]
  # character(0)
  
  # 999 is the code for state totals
  # cbp_2022_co[fipscty != 999]
  
  # select the 6 digit NAICS rows for each county
  cbp_2022_co <- cbp_2022_co[fipscty != 999 & naics %in% NAICS2017$NAICS6]
  
  # add full fips code and check for any mismatches with the TAZ_System
  # length(unique(TAZ_System$CountyFIPS))
  cbp_2022_co[, fipscode := fipstate * 1000 + fipscty]
  # sort(unique(cbp_2022_co[!fipscode %in% TAZ_System$CountyFIPS]$fipscode))
  # TAZ_System[CountyFIPS %in% 2000:2999, .(state, county, CountyFIPS, FAFZONE, FAFNAME, CBPZONE)]
  # TAZ_System[CountyFIPS %in% 9000:9999, .(state, county, CountyFIPS, FAFZONE, FAFNAME, CBPZONE)]
  # state     county CountyFIPS FAFZONE  FAFNAME CBPZONE
  # 1:    CT  FAIRFIELD       9001      92 CT New Y      17
  # 2:    CT   HARTFORD       9003      91 CT Hartf      16
  # 3:    CT LITCHFIELD       9005      92 CT New Y      17
  # 4:    CT  MIDDLESEX       9007      91 CT Hartf      16
  # 5:    CT  NEW HAVEN       9009      92 CT New Y      17
  # 6:    CT NEW LONDON       9011      99   CT rem      18
  # 7:    CT    TOLLAND       9013      91 CT Hartf      16
  # 8:    CT    WINDHAM       9015      91 CT Hartf      16
  
  # Alaska and Connecticut county equivalents coding is different
  # Alaska is a single TAZ, Mesozone, FAFZONE, FAFNAME, CBPZONE
  # Connecticut is 3 TAZ, Mesozone, FAFZONE, FAFNAME, CBPZONE
  
  # Recode the fipscode as best as possible so that it matches the TAZ layer
  # Give Alaska records the old Anchorage FIPS Code
  cbp_2022_co[fipscode %in% 2000:2999,
              fipscode := .(2020)]
  
  # Allocate the CT FIPS code to old county codes
  # Allocation based on overlay map at 
  # https://img.federalregister.gov/EN06JN22.001/EN06JN22.001_original_size.png
  
  # 09110 Capitol Planning Region
  # 09120 Greater Bridgeport Planning Region 
  # 09130 Lower Connecticut River Valley Planning Region
  # 09140 Naugatuck Valley Planning Region
  # 09150 Northeastern Connecticut Planning Region
  # 09160 Northwest Hills Planning Region
  # 09170 South Central Connecticut Planning Region
  # 09180 Southeastern Connecticut Planning Region
  # 09190 Western Connecticut Planning Region
  
  ct_fips <- data.table(fipscode = seq(9110, 9190, by = 10),
                        oldfips =  c(9003, 9001, 9007, 9009, 9015, 9005, 9009, 9011, 9001))
  cbp_2022_co[ct_fips,
              fipscode := i.oldfips,
              on = "fipscode"]
  
  # Add the FAFZONE and CBPZONEs for summarizing the CBP data
  cbp_2022_co[TAZ_System[, .(fipscode = CountyFIPS, FAFZONE, CBPZONE)], 
              c("FAFZONE", "CBPZONE") := .(i.FAFZONE, i.CBPZONE),
              on = "fipscode"]
  # cbp_2022_co[is.na(FAFZONE)] #Empty data.table (0 rows and 26 cols)
  
  # Summarize the table by FAFZONE/CBPZONE and NAICS code and sum up the establishments into size categories
  cols = names(cbp_2022_co)[11:23]
  cbp_2022_co[, (cols) := lapply(.SD, gsub, pattern = "N", replacement = NA), .SDcols = cols]
  cbp_2022_co[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  
  # EstSizeCategories
  # ID       Label LowerBound Midpoint ProbRatio
  # 1:  1        1-19          1     10.0        10
  # 2:  2       20-99         20     59.5         6
  # 3:  3     100-249        100    174.5         4
  # 4:  4     250-499        250    375.0         3
  # 5:  5     500-999        500    749.5         3
  # 6:  6 1,000-2,499       1000   1749.5         3
  # 7:  7 2,500-4,999       2500   3749.5         3
  # 8:  8      5,000+       5000   7500.0         3
  
  # N1000_1         N       Number of Establishments: Employment Size Class:
  #   1,000-1,499 Employees
  # 
  # N1000_2         N       Number of Establishments: Employment Size Class:
  #   1,500-2,499 Employees
  # 
  # N1000_3         N       Number of Establishments: Employment Size Class:
  #   2,500-4,999 Employees
  # 
  # N1000_4         N       Number of Establishments: Employment Size Class:
  #   5,000 or More Employees
  
  data_emp_cbp <- cbp_2022_co[,.(employment = sum(emp, na.rm = TRUE),
                                 establishment = sum(est, na.rm = TRUE),
                                 e1 = sum(`n<5`, na.rm = TRUE) + sum(n5_9, na.rm = TRUE) + sum(n10_19, na.rm = TRUE),
                                 e2 = sum(n20_49, na.rm = TRUE) + sum(n50_99, na.rm = TRUE),
                                 e3 = sum(n100_249, na.rm = TRUE),
                                 e4 = sum(n250_499, na.rm = TRUE),
                                 e5 = sum(n500_999, na.rm = TRUE),
                                 e6 = sum(n1000_1, na.rm = TRUE) + sum(n1000_2, na.rm = TRUE),
                                 e7 = sum(n1000_3, na.rm = TRUE),
                                 e8 = sum(n1000_4, na.rm = TRUE)),
                              keyby = .(Industry_NAICS6_CBP = naics,
                                        FAFZONE, CBPZONE)]
  
  # Compare the total establishments with the sum of the size categories
  # Note that the processing code corrects for this difference by adding extra establishments
  # across the size distribution
  
  data_emp_cbp[, est_cat_sum := e1+e2+e3+e4+e5+e6+e7+e8]
  data_emp_cbp[, est_cat_miss := establishment - est_cat_sum]
  data_emp_cbp[, NAICS2 := substr(Industry_NAICS6_CBP,1,2)]
  data_emp_cbp[NAICS2 %in% 31:33, NAICS2 := "31"]
  data_emp_cbp[NAICS2 %in% 44:45, NAICS2 := "44"]
  data_emp_cbp[NAICS2 %in% 48:49, NAICS2 := "48"]
  # data_emp_cbp[, .(est = sum(establishment), 
  #                  est_cat_sum = sum(est_cat_sum),
  #                  est_cat_miss = sum(est_cat_miss),
  #                  pct_miss = sum(est_cat_miss)/sum(establishment)),
  #              keyby = NAICS2]
  
  # Extract the 6 digit records and higher level summaries from state and us data for comparison
  # Want to ensure that the number of establishments in total 
  # and by state and 2-digit category is correct 
  
  # State totals by 2 digit NAICS
  cbp_2022_st[, naics := gsub("-","", naics, fixed = TRUE)]
  cbp_2022_st[, naics := gsub("/","", naics, fixed = TRUE)]
  # cbp_2022_st[naics %in% 11:81]
  # unique(cbp_2022_st[naics %in% 11:81]$naics)
  # [1] "11" "21" "22" "23" "31" "42" "44" "48" "51" "52" "53" "54" "55" "56" "61" "62" "71" "72" "81"
  # naics codes are the grouped NAICS, i.e., 31 = 31:33, 44 = 44:45, 48 = 48:49
  
  data_emp_state_n2 <- cbp_2022_st[naics %in% 11:81 & lfo == "-",.(employment = sum(emp, na.rm = TRUE),
                                 establishment = sum(est, na.rm = TRUE)),
                              keyby = .(NAICS2 = naics, fipstate)]
  
  # Compare total employment and establishments with the detailed CBP
  data_emp_state_n2[data_emp_cbp[,.(employment = sum(employment, na.rm = TRUE),
                                 establishment = sum(establishment, na.rm = TRUE)),
                                 keyby = .(NAICS2, fipstate = floor(FAFZONE/10))],
                    c("emp.cbp", "est.cbp") := .(i.employment, i.establishment),
                    on = c("NAICS2", "fipstate")]
  
  cols = names(data_emp_state_n2)[3:6]
  # data_emp_state_n2[,.(colSums(.SD, na.rm = TRUE)), .SDcols = cols]
  
  # US totals by 2 digit NAICS
  cbp_2022_us[, naics := gsub("-","", naics, fixed = TRUE)]
  cbp_2022_us[, naics := gsub("/","", naics, fixed = TRUE)]
  # cbp_2022_us[naics %in% 11:81]
  # unique(cbp_2022_us[naics %in% 11:81]$naics)
  # [1] "11" "21" "22" "23" "31" "42" "44" "48" "51" "52" "53" "54" "55" "56" "61" "62" "71" "72" "81"
  # naics codes are the grouped NAICS, i.e., 31 = 31:33, 44 = 44:45, 48 = 48:49
  
  data_emp_us_n2 <- cbp_2022_us[naics %in% 11:81 & lfo == "-",.(employment = sum(emp, na.rm = TRUE),
                                                                   establishment = sum(est, na.rm = TRUE)),
                                   keyby = .(NAICS2 = naics)]
  cols = names(data_emp_us_n2)[2:3]
  # data_emp_us_n2[,.(colSums(.SD, na.rm = TRUE)), .SDcols = cols]
  # The state totals are consistent with the US totals
  # Can scale the county data to match the state totals by 2 digit to account
  # for missing employment and establishments
  
  # Discrepancies by establishment size will be dealt with by input processing code,
  # Discrepancies by NAICS2 and FAFZONE will be dealt with in scaling
  
  data_emp_state_n2[, c("emp.scale", "est.scale") := .(employment/emp.cbp, establishment/est.cbp)]
  # data_emp_state_n2[emp.scale < 1] # zero rows
  # data_emp_state_n2[est.scale < 1] # zero rows
  
  data_emp_cbp[, fipstate := floor(FAFZONE/10)]
  data_emp_cbp[data_emp_state_n2, 
               c("emp.scale", "est.scale") := .(i.emp.scale, i.est.scale),
               on = c("NAICS2", "fipstate")]
  
  data_emp_cbp[, employment := round(employment * emp.scale)]
  data_emp_cbp[, establishment := round(establishment * est.scale)]
  
  cols = names(data_emp_cbp)[4:5]
  # data_emp_cbp[,.(colSums(.SD, na.rm = TRUE)), .SDcols = cols]
  
  # check for any NAs in emp and estab
  # data_emp_cbp[is.na(employment)]
  # data_emp_cbp[is.na(establishment)]
  
  # clean up the file by removing extra columns
  cols = names(cbp_2017)
  data_emp_cbp <- data_emp_cbp[,..cols]
  
  ### WRITE ===================================================================
  
  # write the 2022 file
  fwrite(data_emp_cbp, file = file.path(SYSTEM_DEV_DATA_PATH, "CBP_Emp_HH", "data_emp_cbp_2022.csv"))
  
  # Copy 2022 file to the new data inputs folder for use in the model
  file.copy(from = file.path(SYSTEM_DEV_DATA_PATH, "CBP_Emp_HH", "data_emp_cbp_2022.csv"),
            to = file.path(SYSTEM_DATA_NEW_PATH, "data_emp_cbp.csv"), overwrite = TRUE)
  
  paste("Finished writing ", file.path(SYSTEM_DATA_NEW_PATH, "data_emp_cbp.csv"))

}
