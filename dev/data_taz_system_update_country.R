# CMAP Freight Model
# dev script: data_taz_system_update_country
#
# Purpose:
# Update the country coding in the TAZ_System to reflect the updated trade data for 2022
#
# NOTES: 
# This function is for documentation purposes to show how the current TAZ_System.csv
# was created. It included some exported csv files that were manually processed and read in
# The resulting processed tables are read in in the function from the spreadsheet where that 
# processing took place. Also note the temp code at the end due to the current difference
# between the trade data coding and the skim coding, requiring both old and new TAZ numbering
# for the international zones in the current release of the model (base year 2022)
#
# Outputs:
# TAZ_System.R
#
# Called as function by cmap_base_year_update.R

data_taz_system_update_country <- function(macro_inputs_path){

    ### READ INPUT FILES ==================================================
    
    # Trade Data:
    for_prod_wide <- data.table(read.xlsx(file.path(macro_inputs_path, "Updated_Foreign_Trade_Forecasts_20250827.xlsx"),
                                          sheet = "data_foreign_prod_BaseCase"))
    for_cons_wide <- data.table(read.xlsx(file.path(macro_inputs_path, "Updated_Foreign_Trade_Forecasts_20250827.xlsx"),
                                          sheet = "data_foreign_cons_BaseCase"))
    
    # TAZ
    TAZ_System <- fread(file.path(SYSTEM_DATA_OLD_PATH, "TAZ_System.csv"))
    
    ### PROCESS ==================================================
    
    # Clean up the files
    # replace NA values with 0
    for_prod_wide[is.na(for_prod_wide)] <- 0
    for_cons_wide[is.na(for_cons_wide)] <- 0
    
    # Look for any missing zonal information
    for_prod_wide[FAFZone == 0]
    for_cons_wide[FAFZone == 0]
    
    # Check the country codes in the foreign production and consumption files
    # check for 0s in ctrycode and CBPzone
    
    all_countries <- sort(unique(c(for_prod_wide$Country, for_cons_wide$Country)))
    taz_countries <- toupper(unique(TAZ_System$Country))
    
    length(all_countries) #[1] 232
    length(taz_countries) #[1] 216
    
    all_countries[!all_countries %in% taz_countries]
    
    # [1] "BRITISH INDIAN OCEAN TERRITORIES"    "BRUNEI"                             
    # [3] "BURMA"                               "CABO VERDE"                         
    # [5] "COCOS (KEELING) ISLANDS"             "CONGO (BRAZZAVILLE)"                
    # [7] "CONGO (KINSHASA)"                    "COTE D'IVOIRE"                      
    # [9] "CURACAO"                             "ESWATINI"                           
    # [11] "FALKLAND ISLANDS (ISLAS MALVINAS)"   "FRENCH GUIANA"                      
    # [13] "FRENCH POLYNESIA"                    "FRENCH SOUTHERN AND ANTARCTIC LANDS"
    # [15] "GAZA STRIP ADMINISTERED BY ISRAEL"   "GUADELOUPE"                         
    # [17] "HEARD AND MCDONALD ISLANDS"          "HONG KONG"                          
    # [19] "KOREA, SOUTH"                        "KOSOVO"                             
    # [21] "MACAU"                               "MACEDONIA"                          
    # [23] "MARTINIQUE"                          "MAYOTTE"                            
    # [25] "NEW CALEDONIA"                       "NIUE"                               
    # [27] "PITCAIRN ISLANDS"                    "REUNION"                            
    # [29] "RUSSIA"                              "SINT MAARTEN"                       
    # [31] "SOUTH SUDAN"                         "ST HELENA"                          
    # [33] "ST KITTS AND NEVIS"                  "ST LUCIA"                           
    # [35] "ST PIERRE AND MIQUELON"              "ST VINCENT AND THE GRENADINES"      
    # [37] "SVALBARD, JAN MAYEN ISLAND"          "TAIWAN"                             
    # [39] "VATICAN CITY"                        "WALLIS AND FUTUNA"                  
    # [41] "WEST BANK ADMINISTERED BY ISRAEL"
    
    taz_countries[!taz_countries %in% all_countries]
    
    # [1] "UNITED STATES OF AMERICA"                  "BRITISH INDIAN OCEAN TERRITORY"           
    # [3] "BRUNEI DARUSSALAM"                         "C?TE D'IVOIRE"                            
    # [5] "CAPE VERDE"                                "COCOS ISLANDS"                            
    # [7] "CONGO"                                     "DEMOCRATIC REPUBLIC OF THE CONGO"         
    # [9] "FALKLAND ISLANDS"                          "FRENCH SOUTHERN TERRITORIES"              
    # [11] "HEARD ISLAND AND MCDONALD ISLANDS"         "HOLY SEE"                                 
    # [13] "MYANMAR"                                   "NORTH KOREA"                              
    # [15] "PALESTINIAN TERRITORY"                     "PITCAIRN"                                 
    # [17] "R?UNION"                                   "RUSSIAN FEDERATION"                       
    # [19] "SAINT HELENA"                              "SAINT KITTS AND NEVIS"                    
    # [21] "SAINT LUCIA"                               "SAINT VINCENT AND THE GRENADINES"         
    # [23] "SOUTH KOREA"                               "SWAZILAND"                                
    # [25] "THE FORMER YUGOSLAV REPUBLIC OF MACEDONIA"
    
    # Write out CSVs of lists and make manual matches, update TAZ system labels and then check for remaining no matches
    fwrite(data.table(Country = all_countries[!all_countries %in% taz_countries]),
           file.path(macro_inputs_path, "Countries_Trade_Not_TAZ.csv"))
    
    fwrite(data.table(Country = taz_countries[!taz_countries %in% all_countries]),
           file.path(macro_inputs_path, "Countries_TAZ_Not_Trade.csv"))
    
    # Read in the updated correspondence with manual matching
    c_countries_old_new <- data.table(read.xlsx(file.path(macro_inputs_path, "NewCMAP_Forecasts.xlsx"),sheet = "Country_Clean"))
    
    # Add the matches rows to create a complete correspondence for all countries
    c_countries_old_new <- rbind(data.table(Country = all_countries[all_countries %in% taz_countries],
                                            CountryUpdated = all_countries[all_countries %in% taz_countries]),
                                 c_countries_old_new)
    
    # Update the Country field
    # Upper case to match with the trade data labels
    TAZ_System[, Country := toupper(Country)]
    
    # Create an updated TAZ_System file with corrected/complete country coding
    TAZ_System_Domestic <- TAZ_System[TAZ_TYPE %in% c("MODELREGION", "NATIONAL")]
    TAZ_System_International <- TAZ_System[!TAZ_TYPE %in% c("MODELREGION", "NATIONAL")]
    
    # Keep the old country name, ctrycod, and mesozone for developing a new/old Mesozone
    # correspondence -- important for selecting from skims
    TAZ_System_International[, c("oldMesozone", "oldCountry", "oldctrycod") :=
                               .(Mesozone, Country, ctrycod)]
    
    # Update the country labels based on the correspondence
    TAZ_System_International[c_countries_old_new, Country := i.CountryUpdated, on = "Country"]
    
    # Add new rows for the missing Countries
    # Which old country/mesozone do the new countries correspond to
    # Read in the correspondence with manual matching to best alternatives
    c_countries_new_best_match <- data.table(read.xlsx(file.path(macro_inputs_path, "NewCMAP_Forecasts.xlsx"),sheet = "New_Country_Match"))
    c_countries_new_best_match[, oldCountry := toupper(oldCountry)]
    setnames(c_countries_new_best_match, "CountryUpdated", "Country")
    new_countries_table <- data.table(Country = c_countries_old_new[is.na(Country)]$CountryUpdated)
    new_countries_table <- merge(new_countries_table,
                                 c_countries_new_best_match,
                                 by = "Country")
    
    TAZ_System_International <- rbind(TAZ_System_International,
                                      new_countries_table,
                                      fill = TRUE)
    
    # Set the fields that have identical values for all rows in the international data
    TAZ_System_International[,c("cbd", "chicago", "cmap", "CountyFIPS", "county", "state", "township", "sqmi", "county_state", "DistrictNum",  "DistrictName",  "REGION", "SUBREGION", "modelregion",      "TAZ_TYPE") :=
                               .(   0,         0,      0,           NA,       NA,      NA,         NA,     NA,             NA,            13, "International", "Foreign",   "Foreign",             2, "INTERNATIONAL")]
    
    # Sort on country in alphabetical order and then renumber
    # TAZ starts at 3756, Mesozone at 274, CBPZONE at 124, ctrycod at 1
    
    setorder(TAZ_System_International, Country)
    TAZ_System_International[, c("TAZ", "Mesozone", "CBPZONE", "ctrycod") :=
                                  .(3756:(3755 + .N), 274:(273 + .N), 124:(123 + .N), 1:.N)]
    
    # Code missing FAFZONE and FAFNAME fields
    unique(TAZ_System_International[,.(FAFZONE, FAFNAME)])[order(FAFZONE)]
    TAZ_System_International[is.na(FAFZONE) & Country %in% c("CURACAO", "FRENCH GUIANA", "GUADELOUPE", "MARTINIQUE", "SINT MAARTEN", "ST PIERRE AND MIQUELON"),
                             c("FAFZONE", "FAFNAME") := .(803, "Americas")]
    
    TAZ_System_International[is.na(FAFZONE) & Country %in% c("FRENCH POLYNESIA", "NEW CALEDONIA", "NIUE", "WALLIS AND FUTUNA"),
                             c("FAFZONE", "FAFNAME") := .(808, "SE As Oc")]
    
    TAZ_System_International[is.na(FAFZONE) & Country %in% c("GAZA STRIP ADMINISTERED BY ISRAEL"),
                             c("FAFZONE", "FAFNAME") := .(806, "SWC Asia")]
    
    TAZ_System_International[is.na(FAFZONE) & Country %in% c("HONG KONG", "MACAU", "TAIWAN"),
                             c("FAFZONE", "FAFNAME") := .(807, "E Asia")]
    
    TAZ_System_International[is.na(FAFZONE) & Country %in% c("KOSOVO", "SVALBARD, JAN MAYEN ISLAND"),
                             c("FAFZONE", "FAFNAME") := .(804, "Europe")]
    
    TAZ_System_International[is.na(FAFZONE) & Country %in% c("MAYOTTE", "SOUTH SUDAN"),
                             c("FAFZONE", "FAFNAME") := .(805, "Africa")]
    
    unique(TAZ_System_International[,.(FAFZONE, FAFNAME)])[order(FAFZONE)]
    
    # add the old fields to TAZ_System_Domestic too
    TAZ_System_Domestic[, c("oldMesozone", "oldCountry", "oldctrycod") :=
        .(Mesozone, Country, ctrycod)]
    
    # Recombine
    TAZ_System <- rbind(TAZ_System_Domestic, 
                        TAZ_System_International)
    
    ### WRITE ==================================================
    
    # # TAZ_System
    # fwrite(TAZ_System, 
    #        file.path(SYSTEM_DATA_NEW_PATH, "TAZ_System.csv"))
    
    ### TEMP
    
    # Since the networks are not yet implemented with the new international zones 
    # to match the changes in the trade data, for the current model
    # rename the old fields and the new fields
    # in the model still need to use the old MesoZones and CBPZones
    
    TAZ_System[, oldCBPZONE := CBPZONE]
    TAZ_System[TAZ_TYPE == "INTERNATIONAL", oldCBPZONE := oldMesozone - 150L]
    setnames(TAZ_System, c("Mesozone", "CBPZONE", "Country", "ctrycod"),
             c("newMesozone", "newCBPZONE", "newCountry", "newctrycod"))
    setnames(TAZ_System, c("oldMesozone", "oldCBPZONE", "oldCountry", "oldctrycod"),
             c("Mesozone", "CBPZONE", "Country", "ctrycod"))
    
    fwrite(TAZ_System, 
           file.path(SYSTEM_DATA_NEW_PATH, "TAZ_System.csv"))
    
    paste("Finished writing ", file.path(SYSTEM_DATA_NEW_PATH, "TAZ_System.csv"))

}
