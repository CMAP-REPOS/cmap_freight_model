# CMAP Freight Model
# dev script: data_naics2017_corresp.R
#
# Purpose:
# Create updated correspondence files for NAICS 2017 categories
#
# Outputs:
# Correspondence files using 2017 codes (in IO_Cost_Trade folder):
# NAICS2017io.csv
# NAICS2017_to_NAICS2017io.csv
# NAICS2017_to_NAICS2017io_to_SCTG.csv
# 
# For use in the model (written to the SYSTEM_DATA_NEW_PATH directory):
# corresp_naics6_n6io_sctg.csv
#
# Called as function by cmap_base_year_update.R

data_naics2017_corresp <- function(){

        ### READ INPUT FILES ==================================================
        
        # Read in matrix from spreadsheet
        # IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx
        # From https://apps.bea.gov/industry/xls/io-annual/IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx
        
        ioxls <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx"),
                           sheet = "2017", startRow = 6))
        
        # Read in a clean list of the naics io codes for 2017
        # Made from the first sheet of the spreadsheet
        # Also review correspondence between NAICS 2017 -- NAICS IO -- SCTG codes
        # Correspondences in the model are based on old NAICS versions (2007)
        
        # New 2017 io codes/correspondences built from the https://apps.bea.gov/industry/xls/io-annual/IOUse_Before_Redefinitions_PUR_2017_Detail.xlsx
        # sheet with some manual formatting prior to reading in here.
        naics2017io <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "BEA_NAICS_2017.xlsx"),
                                 sheet = "NAICSIOCodes"))
        
        c_naicsio_naics2017_2_6 <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "BEA_NAICS_2017.xlsx"),
                                 sheet = "Corresp_NACSIO_NAICS2017_2_6"))
        
        naics2017_6 <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "BEA_NAICS_2017.xlsx"),
                                             sheet = "NAICS2017_6"))
        
        # Old 2007 correspondence between NAICS and SCTG
        c_n6_n6io_sctg <- fread(file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "corresp_naics6_n6io_sctg_2007.csv"))
        
        ### PROCESS ==================================================
        
        # Create a complete correspondence between 2017 NAICS IO codes and 6 digit NAICS codes
        naics2017_6[, c("NAICS2017_2", "NAICS2017_3", "NAICS2017_4", "NAICS2017_5") :=
                      .(as.numeric(substr(NAICS2017_6,1,2)),
                        as.numeric(substr(NAICS2017_6,1,3)),
                        as.numeric(substr(NAICS2017_6,1,4)),
                        as.numeric(substr(NAICS2017_6,1,5)))]
        
        # categorize the NAICS fields by length
        c_naicsio_naics2017_2_6[, digits := nchar(NAICS2017_2_6)]
        c_naicsio_naics2017_2_6[, c("NAICS2017_2", "NAICS2017_3", "NAICS2017_4", "NAICS2017_5", "NAICS2017_6") :=
                                  .(as.numeric(ifelse(digits == 2, NAICS2017_2_6, NA)),
                                    as.numeric(ifelse(digits == 3, NAICS2017_2_6, NA)),
                                    as.numeric(ifelse(digits == 4, NAICS2017_2_6, NA)),
                                    as.numeric(ifelse(digits == 5, NAICS2017_2_6, NA)),
                                    as.numeric(ifelse(digits == 6, NAICS2017_2_6, NA)))]
        
        # join on the complete set of 6 digit codes
        c_naicsio_naics2017_2_6 <- merge(c_naicsio_naics2017_2_6,
                                         naics2017_6[,.(NAICS2017_2, NAICS_2017_6_J2 = NAICS2017_6)],
                                         by = "NAICS2017_2",
                                         all.x = TRUE,
                                         allow.cartesian = TRUE)
        
        c_naicsio_naics2017_2_6 <- merge(c_naicsio_naics2017_2_6,
                                         naics2017_6[,.(NAICS2017_3, NAICS_2017_6_J3 = NAICS2017_6)],
                                         by = "NAICS2017_3",
                                         all.x = TRUE,
                                         allow.cartesian = TRUE)
        
        c_naicsio_naics2017_2_6 <- merge(c_naicsio_naics2017_2_6,
                                         naics2017_6[,.(NAICS2017_4, NAICS_2017_6_J4 = NAICS2017_6)],
                                         by = "NAICS2017_4",
                                         all.x = TRUE,
                                         allow.cartesian = TRUE)
        
        c_naicsio_naics2017_2_6 <- merge(c_naicsio_naics2017_2_6,
                                         naics2017_6[,.(NAICS2017_5, NAICS_2017_6_J5 = NAICS2017_6)],
                                         by = "NAICS2017_5",
                                         all.x = TRUE,
                                         allow.cartesian = TRUE)
        
        c_naicsio_naics2017_2_6[is.na(NAICS2017_6), NAICS2017_6 := ifelse(digits == 2, NAICS_2017_6_J2,
                                                                          ifelse(digits == 3, NAICS_2017_6_J3,
                                                                                 ifelse(digits == 4, NAICS_2017_6_J4,
                                                                                        ifelse(digits == 5, NAICS_2017_6_J5, NA))))]
        # remove the extra fields to create a simpler table
        c_naicsio_naics2017 <- c_naicsio_naics2017_2_6[,.(NAICSIO, NAICS2017_6)]
        
        # check the NA matches
        unique(c_naicsio_naics2017[is.na(NAICS2017_6), .(NAICSIO)])
        
        # The 4200ID, 531HSO, GSL*, and S00* codes don't have a match in the industry codes correspondence
        # 3219A0 and 517A00 have some extra incorrect NAICS 2017 6 digit codes includes, remove those NAs
        c_naicsio_naics2017[NAICSIO == "3219A0"]
        c_naicsio_naics2017[NAICSIO == "517A00"]
        c_naicsio_naics2017 <- c_naicsio_naics2017[!(is.na(NAICS2017_6) & NAICSIO %in% c("3219A0", "517A00"))]
        # Leave the other NAs and deal with below
        
        # Public admin codes (GSL*, and S00* codes) add a generic public admin 6 digit, 920000, as 
        # the firm synthesis does not distinguish detail there (public admin is missing from CBP data)
        c_naicsio_naics2017[is.na(NAICS2017_6) & NAICSIO %in% naics2017io[Sector == "G"]$NAICSIO,
                            NAICS2017_6 := 920000]
        
        # Categories left with no matches are non-industry items, remove
        naics2017io[NAICSIO %in% c_naicsio_naics2017[is.na(NAICS2017_6)]$NAICSIO]
        
        c_naicsio_naics2017 <- c_naicsio_naics2017[!is.na(NAICS2017_6)]
        
        # Sort the file, order columns, and name
        setorder(c_naicsio_naics2017, NAICSIO, NAICS2017_6)
        setcolorder(c_naicsio_naics2017, c("NAICS2017_6", "NAICSIO"))
        setnames(c_naicsio_naics2017, c("NAICS", "NAICSio"))
        
        # Write out the 2017 NAICS IO codes as a csv
        # put the full codes and names first
        naics2017io[, NAICS2017Codes := NULL]
        setcolorder(naics2017io, 
                    c("NAICSIO", "IndustryName", "Sector", "SectorName", 
                      "Summary", "SummaryName", "USummary", "USummaryName"))
        
        # Create an updated correspondence with SCTG categories
        
        # What are the changes in IO codes
        NAICS2007io[!NAICSio %in% naics2017io$NAICSIO]
        naics2017io[!NAICSIO %in% NAICS2007io$NAICSio]
        
        # select the unique combinations of NAICSio and SCTG that are in the both 2007 and 2017
        n6io_sctg <- unique(c_n6_n6io_sctg[,.(Industry_NAICS6_Make, Commodity_SCTG, Commodity_SCTG_desc)])
        # these are the 2017 matches
        n6io2017_sctg <- n6io_sctg[Industry_NAICS6_Make %in% naics2017io$NAICSIO]
        # these are the items from 2007 that are not in the 2017 list
        n6io2007_sctg <- n6io_sctg[!Industry_NAICS6_Make %in% naics2017io$NAICSIO]
        # these are 2017 IO codes that do not have a match in the correspondence
        missing_n6io2017 <- naics2017io[!NAICSIO %in% n6io2017_sctg$Industry_NAICS6_Make]
        
        # manually deal with the missing items and then check the correspondence
        # update n6io2007_sctg with missing_n6io2017
        n6io2007_sctg[c_n6_n6io_sctg, Industry_NAICS6_Make_desc := i.Industry_NAICS6_Make_desc, on = "Industry_NAICS6_Make"]
        
        # write these out and create a correspondence to read back in and update...
        fwrite(n6io2007_sctg[,.(Industry_NAICS6_Make, Industry_NAICS6_Make_desc)],
               file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "n6io2007_sctg_not2017.csv"))
        fwrite(missing_n6io2017[,.(NAICSIO, IndustryName)],
               file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "missing_n6io2017_not2007.csv"))
        
        matches_n6io2007_n6io2017 <- data.table(read.xlsx(xlsxFile = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "BEA_NAICS_2017.xlsx"),
                                            sheet = "Matches_2007_2017_NAICSIO"))
        
        # update the correspondence with the new matched rows so that there is a complete set of 2017 io codes
        matches_n6io2007_n6io2017 <- merge(matches_n6io2007_n6io2017,
                                           n6io2007_sctg[,.(NAICSIO_2007 = Industry_NAICS6_Make, Commodity_SCTG, Commodity_SCTG_desc)],
                                           by = "NAICSIO_2007",
                                           all.x = TRUE,
                                           allow.cartesian = TRUE)
        
        n6io2017_sctg_add <- matches_n6io2007_n6io2017[,.(Industry_NAICS6_Make = NAICSIO_2017, Commodity_SCTG, Commodity_SCTG_desc)]
        
        # join on the other records
        n6io2017_sctg <- rbind(n6io2017_sctg,
                               n6io2017_sctg_add)
        setorder(n6io2017_sctg, Industry_NAICS6_Make)
        
        # Remove any records from the file that are missing from the correspondence with NAICS 6 2017
        n6io2017_sctg <- n6io2017_sctg[Industry_NAICS6_Make %in% c_naicsio_naics2017$NAICSio]
        
        # Resolve any remaining NAs in the Commodity SCTG Field
        # 493000, S00101 and S00102 none produce a transportable commodity, so set SCTG == 0
        n6io2017_sctg[is.na(Commodity_SCTG), Commodity_SCTG := 0]
        
        # Check for any duplication created by the process
        n6io2017_sctg[duplicated(n6io2017_sctg)]
        n6io2017_sctg <- n6io2017_sctg[!duplicated(n6io2017_sctg)]
        
        # Calculate the proportion field -- how many SCTG matches for each IO code
        n6io2017_sctg[, Proportion := ifelse(Commodity_SCTG == 0, 0, 1/.N), by = Industry_NAICS6_Make]
        n6io2017_sctg[Proportion > 0 & Proportion < 1]
        
        # Join to the 6 digit NAICS 2017 codes
        c_n6_n6io_sctg_2017 <- merge(c_naicsio_naics2017[,.(NAICS, Industry_NAICS6_Make = NAICSio)],
                                     n6io2017_sctg,
                                     all = TRUE,
                                     allow.cartesian = TRUE,
                                     by = "Industry_NAICS6_Make")
        
        c_n6_n6io_sctg_2017[is.na(NAICS)]
        c_n6_n6io_sctg_2017[is.na(Industry_NAICS6_Make)]
        
        # Add labels for the names of the NAICS and NAICS IO codes and set the name and column order as in the 2007 file
        names(c_n6_n6io_sctg)
        setnames(c_n6_n6io_sctg_2017, "NAICS", "Industry_NAICS6_CBP")
        c_n6_n6io_sctg_2017[naics2017_6[,.(Industry_NAICS6_CBP = NAICS2017_6, NAICS2017_6_Name)], 
                            Industry_NAICS6_CBP_desc := i.NAICS2017_6_Name, on = "Industry_NAICS6_CBP"]
        
        c_n6_n6io_sctg_2017[naics2017io[,.(Industry_NAICS6_Make = NAICSIO, IndustryName)], 
                            Industry_NAICS6_Make_desc := i.IndustryName, on = "Industry_NAICS6_Make"]
        c_n6_n6io_sctg_2017[Industry_NAICS6_CBP == 920000, Industry_NAICS6_CBP_desc := as.character(unique(NAICS2017[NAICS2 == 92]$Label2))]
        
        c_n6_n6io_sctg_2017[is.na(Industry_NAICS6_CBP_desc)] # should be no remaining missing labels
        
        setcolorder(c_n6_n6io_sctg_2017, names(c_n6_n6io_sctg))
        
        # Check for any duplication/errors in the correspondence
        c_n6_n6io_sctg_2017[duplicated(c_n6_n6io_sctg_2017)]
        
        # Check the proportion sums: Should sum to one over each NAICS CBP code
        c_n6_n6io_sctg_2017[Commodity_SCTG > 0,.(Num = .N, Proportion = sum(Proportion)), keyby = .(Industry_NAICS6_CBP)][Num > 1]
        c_n6_n6io_sctg_2017[Commodity_SCTG > 0,.(Num = .N, Proportion = sum(Proportion)), keyby = .(Industry_NAICS6_CBP)][Num > 1][Proportion != 1]
        
        ### WRITE ==================================================
        
        fwrite(c_naicsio_naics2017, 
               file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017_to NAICS2017io.csv"))
        fwrite(naics2017io, 
               file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017io.csv"))
        fwrite(c_n6_n6io_sctg_2017, 
               file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017_to_NAICS2017io_to_SCTG.csv"))
        
        # Copy the files for use in the model to the folder for new inputs
        
        # correspondence NAICS2017_to_NAICS2017io_to_SCTG.csv, write as corresp_naics6_n6io_sctg.csv 
        file.copy(from = file.path(SYSTEM_DEV_DATA_PATH, "IO_Cost_Trade", "NAICS2017_to_NAICS2017io_to_SCTG.csv"),
                  to = file.path(SYSTEM_DATA_NEW_PATH, "corresp_naics6_n6io_sctg.csv"), overwrite = TRUE)
        
        paste("Finished writing ", file.path(SYSTEM_DATA_NEW_PATH, "corresp_naics6_n6io_sctg.csv"))

}