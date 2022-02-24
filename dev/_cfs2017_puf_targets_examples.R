# take a look at the CVS data and see if it matches what is in the documentation

# Load libraries ------------------------------------------
library(data.table)
library(bit64)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(sf)
library(rFreight)

# Define environment variables or functions ---------------
# Define path for different folders
proj_dir <- getwd()
data_dir <- "data"
output_dir <- "output"

cfs_dir <- file.path("survey", "cfs")
faf_dir <- file.path("survey", "faf")

# Define data file names
# CFS Data
#cfs_file <- "cfs-2017-puf-csv.zip"
cfs_file <- "cfs-2017-puf-csv.csv"
cfs_naics_file <- "cfs-2017-puf-uga2.csv"
cfs_sctg_file <- "cfs-2017-puf-uga3.csv"
cfs_mode_file <- "cfs-2017-puf-uga4.csv"
cfs_cv_coef_file <- "cfs-2017-puf-cv-coef.csv"

# FAF Data
# FAF Data
faf_file1 <- "FAF4.5.1_csv.zip"
faf_file2 <- "FAF4.5.1_csv_2013-2018.zip"
faf_zones <- "FAF4_REGIONS.shp"
faf5_file <- "FAF5.1.csv"
faf5_zones <- "2017_CFS_Metro_Areas_with_FAF.shp"

# Correspondence file
segment_sctg_naics_file <- "corresp_modechoice_segments.csv"
corresp_state_region <- "corresp_state_region.csv"

# Target Files
target_dir <- file.path(proj_dir, output_dir, "targets")
sctg_tonnage_file <- file.path(target_dir, "sctg_tonnage.csv")
sctg_dist_trade_file <- file.path(target_dir, "sctg_dist_trade.csv")
sctg_awmps_file <- file.path(target_dir, "sctg_awmps.csv")
mode_tonshare_file <- file.path(target_dir, "mode_tonshare.csv")
mode_tonmileshare_file <- file.path(target_dir, "mode_tonmileshare.csv")
shipsize_tonshare_file <- file.path(target_dir, "shipsize_tonshare.csv")
shipsize_tonmileshare_file <- file.path(target_dir, "shipsize_tonmileshare.csv")
sctg_mode_tonshare_file <- file.path(target_dir, "sctg_mode_tonshare.csv")
sctg_mode_tonmileshare_file <- file.path(target_dir, "sctg_mode_tonmileshare.csv")
sctg_shipsize_tonshare_file <- file.path(target_dir, "sctg_shipsize_tonshare.csv")
sctg_shipsize_tonmileshare_file <- file.path(target_dir, "sctg_shipsize_tonmileshare.csv")
dist_mode_tonshare_file <- file.path(target_dir, "dist_mode_tonshare.csv")
dist_mode_tonmileshare_file <- file.path(target_dir, "dist_mode_tonmileshare.csv")
dist_shipsize_tonshare_file <- file.path(target_dir, "dist_shipsize_tonshare.csv")
dist_shipsize_tonmileshare_file <- file.path(target_dir, "dist_shipsize_tonmileshare.csv")
ind_outsourceshare_file <- file.path(target_dir, "ind_outsourceshare.csv")
ind_nonmodel_outsourceshare_file <- file.path(target_dir, "ind_nonmodel_outsourceshare.csv")
plot_dir <- file.path(proj_dir, output_dir, "summaries", "plots")

target_file <- file.path(proj_dir, output_dir, "targets", "targets.rds")
excel_file <- file.path(proj_dir, output_dir, "targets", "excel.rds")
target_faf5_file <- file.path(proj_dir, output_dir, "targets", "targets_faf5.rds")
faf5_dt_processed <- file.path(proj_dir, output_dir, "targets", "processed_faf5.rds")

# Define constants
LBSTOTONS <- 1/2000
# WEIGHT_BINS <- c(-Inf, 1000, 5000, 20000, 100000, Inf)
WEIGHT_BINS <- c(-Inf, 200, 2000, 20000, 200000, Inf)
WEIGHT_BINS_STRING <- scales::comma(WEIGHT_BINS)
WEIGHT_LABELS <- c(paste0("< ", (WEIGHT_BINS_STRING[2])),
                   paste0(WEIGHT_BINS_STRING[-1], " - ", 
                          shift(WEIGHT_BINS_STRING[-1], type="lead"))[-5])
WEIGHT_LABELS <- gsub("(.*) - Inf", "> \\1", WEIGHT_LABELS)
# WEIGHT_LABELS <- c("< 10", "10 - 50", "50 - 500", "500 - 5000", "> 5000")

DIST_BINS <- c(-Inf, 250, 500, 1000, 2000, Inf)
DIST_BINS_STRING <- scales::comma(DIST_BINS)
DIST_LABELS <- c(paste0("< ", (DIST_BINS_STRING[2])),
                 paste0(DIST_BINS_STRING[-1], " - ", 
                        shift(DIST_BINS_STRING[-1], type="lead"))[-5])
DIST_LABELS <- gsub("(.*) - Inf", "> \\1", DIST_LABELS)
DIST_LABELS <- paste0(DIST_LABELS, " mi")

SHIPSIZE_BINS <- c(-Inf, 1000, 10000, 50000, 100000, Inf)
SHIPSIZE_BINS_STRING <- scales::comma(SHIPSIZE_BINS)
SHIPSIZE_LABELS <- c(paste0("< ", (SHIPSIZE_BINS_STRING[2])),
                     paste0(SHIPSIZE_BINS_STRING[-1], " - ", 
                            shift(SHIPSIZE_BINS_STRING[-1], type="lead"))[-5])
SHIPSIZE_LABELS <- gsub("(.*) - Inf", "> \\1", SHIPSIZE_LABELS)
SHIPSIZE_LABELS <- paste0(SHIPSIZE_LABELS, " lbs")

# Mode order
MODE_ORDER <- c("Truck", "Rail", "Water", "Air", "Pipeline", "Other")

# Plotting helpers
# define color using RSG palette
RSGCOLORDF <- data.frame(
  red = c(246, 0, 99, 186, 117, 255, 82),
  green = c(139, 111, 175, 18, 190, 194, 77),
  blue = c(31, 161, 94, 34, 233, 14, 133),
  colornames = c("orange", "marine", "leaf", "cherry", "sky", "sunshine", "violet")
)

# Define functions
CFSCVCalc <- function(numrec, coef_dt, amps=NULL, type="Tonnage"){
  coef_cv <- coef_dt[Estimate==type, c(aa, bb, cc)]
  cv_ <- NULL
  if(type == "Avgmiles"){
    if(is.null(amps)) stop("CV calculation for Average miles requires APMS.")
    vars_ <- cbind(1, log(numrec), log(amps))
    cv_ <- as.vector(exp(vars_ %*% coef_cv))
  } else {
    vars_ <- cbind(1, log(numrec), log(numrec)^2)
    cv_ <- as.vector(exp(vars_ %*% coef_cv))
  }
  # Change CV to percentage
  if(!is.null(cv_)) cv_ <- cv_/100
  return(cv_)
}

CFSTotalCalc <- function(cfs_dt, group_var, var_name, weight_name="WGT_FACTOR"){
  setnames(cfs_dt, var_name, "total_var")
  setnames(cfs_dt, weight_name, "weight_factor")
  summary_dt <- cfs_dt[, .(Total=sum(total_var * weight_factor), 
                           NumShipments=sum(weight_factor), 
                           NumRec=.N), 
                       keyby=group_var]
  setnames(summary_dt, "Total", paste0("TOTAL_", var_name))
  setnames(cfs_dt, "total_var", var_name)
  setnames(cfs_dt, "weight_factor", weight_name)
  return(summary_dt)
}

CFSAPMSCalc <- function(cfs_dt, group_var, var_name, weight_name="WGT_FACTOR"){
  setnames(cfs_dt, var_name, "total_var")
  setnames(cfs_dt, weight_name, "weight_factor")
  summary_dt <- cfs_dt[, .(Total=weighted.mean(total_var, weight_factor), 
                           NumShipments=sum(weight_factor), 
                           NumRec=.N), 
                       keyby=group_var]
  setnames(summary_dt, "Total", paste0("AVG_", var_name))
  setnames(cfs_dt, "total_var", var_name)
  setnames(cfs_dt, "weight_factor", weight_name)
  return(summary_dt)
}

LongToWideDT <- function(lng_dt, wide_var, value_var){
  if(wide_var %in% names(lng_dt) & all(value_var %in% names(lng_dt)) &
     length(names(lng_dt)) > (length(c(wide_var, value_var)))){
  right_var <- wide_var
  # left_var <- paste0(setdiff(names(lng_dt), c(right_var, value_var)),
  #                    collapse = " + ")
  # dcast_expr <- formula(paste0(left_var, " ~ ", right_var))
  dcast_expr <- formula(paste0("... ~ ", right_var))
  wide_dt <- dcast.data.table(lng_dt, formula=dcast_expr, value.var = c(value_var))
  } else {
    wide_dt <- NULL
  }
  return(wide_dt)
}

OrderSCTG <- function(sctg, first=TRUE,...){
  if(first){
    order(as.integer(gsub("^(\\d{2}).*", "\\1", sctg)) - ifelse(nchar(as.character(sctg))>2,.5,0),...)
  } else {
    order(as.integer(gsub("^.*(\\d{2})$", "\\1", sctg)) + ifelse(nchar(as.character(sctg))>2,.5,0),...)
  }
}

# Load data -----------------------------------------------
# Read the data
model_seg_sctg_naics_dt <- fread(file.path(proj_dir, data_dir, segment_sctg_naics_file))
state_region <- fread(file.path(proj_dir, data_dir, corresp_state_region))

# cfs_dt <- fread(cmd = paste0("unzip -p \"", file.path(proj_dir, data_dir,
#                                                       cfs_dir, cfs_file), "\""))
cfs_dt <- fread(file.path(proj_dir, data_dir, cfs_dir, cfs_file))
cfs_naics_dt <- fread(file.path(proj_dir, data_dir, cfs_dir, cfs_naics_file))
cfs_sctg_dt <- fread(file.path(proj_dir, data_dir, cfs_dir, cfs_sctg_file))
cfs_mode_dt <- fread(file.path(proj_dir, data_dir, cfs_dir, cfs_mode_file))
cfs_cv_coef_dt <- fread(file.path(proj_dir, data_dir, cfs_dir, cfs_cv_coef_file))

list_files <- unzip(file.path(proj_dir, data_dir,
                              faf_dir, faf_file2),list=TRUE)[,1]
# faf_2017_csv_file <- grep("2017.*csv", list_files, value = TRUE, ignore.case = TRUE)
# faf_dt <- fread(cmd = paste0("unzip -cq \"", file.path(proj_dir, data_dir,
#                                                        faf_dir, faf_file2), 
#                              "\" ", faf_2017_csv_file))

faf_dt <- fread(file.path(proj_dir, data_dir, "survey", "faf", "FAF4.5.1.csv"))
faf5_dt <- fread(file.path(proj_dir, data_dir, "survey", "faf", faf5_file))
faf_poly <- read_sf(dsn = file.path(proj_dir, data_dir, "survey", "faf", "FAF4 Zones", faf_zones))
faf5_poly <- read_sf(dsn = file.path(proj_dir, data_dir, "survey", "faf", "FAF5_Zones", faf5_zones))

# Modify or create intermediate dataset---------------------
# Convert lbs to tonnage
cfs_dt[, SHIPMT_WGHT_TON:=SHIPMT_WGHT*LBSTOTONS]
cfs_sctg_dt[, `SCTG Group`:=gsub("'", "", `SCTG Group`)]
cfs_sctg_dt[, SCTG:=str_pad(SCTG, width = 2, pad = "0")]
cfs_sctg_group_dt <- cfs_sctg_dt[SCTG>0, .(Description=paste0(Description, 
                                                              collapse = "\n")),
                                 keyby = `SCTG Group`]

cfs_dt[cfs_sctg_dt, SCTG_GROUP:=`i.SCTG Group`, on=.(SCTG)]
cfs_dt[is.na(SCTG_GROUP), SCTG_GROUP:=SCTG]

# Assign domestic and foreign modes
setkey(cfs_mode_dt, "Mode Code")
cfs_mode_dt <- cfs_mode_dt[CJ(`Mode Code`, EXPORT_YN=c("Y", "N"))][order(EXPORT_YN, `Mode Code`)]
setcolorder(cfs_mode_dt, "EXPORT_YN")
cfs_mode_dt[`Mode Code` %in% c(3,4,5,14), MODE_GROUP:="Truck"]
cfs_mode_dt[`Mode Code` %in% c(3,4,5,14), EXP_MODE_GROUP:="Truck"]
cfs_mode_dt[`Mode Code` %in% c(6,15), MODE_GROUP:="Rail"]
cfs_mode_dt[`Mode Code` %in% c(6,15), EXP_MODE_GROUP:="Rail"]
cfs_mode_dt[`Mode Code` %in% c(7,8,9,10,101), MODE_GROUP:="Water"]
cfs_mode_dt[`Mode Code` %in% c(7,8,9,10,101), EXP_MODE_GROUP:="Water"]
cfs_mode_dt[`Mode Code` %in% c(11), MODE_GROUP:="Air"]
cfs_mode_dt[`Mode Code` %in% c(11), EXP_MODE_GROUP:="Air"]
cfs_mode_dt[`Mode Code` %in% c(16), MODE_GROUP:="Water"]
cfs_mode_dt[`Mode Code` %in% c(16) & EXPORT_YN=="Y", MODE_GROUP:="Truck"]
cfs_mode_dt[`Mode Code` %in% c(16), EXP_MODE_GROUP:="Water"]
cfs_mode_dt[`Mode Code` %in% c(17), MODE_GROUP:="Rail"]
cfs_mode_dt[`Mode Code` %in% c(17), EXP_MODE_GROUP:="Water"]
cfs_mode_dt[is.na(MODE_GROUP), MODE_GROUP:="Other"]
cfs_mode_dt[is.na(MODE_GROUP), MODE_GROUP:="Other"]
cfs_mode_dt[EXPORT_YN=="N", EXP_MODE_GROUP:=NA]
cfs_mode_dt[EXPORT_YN=="Y" & is.na(EXP_MODE_GROUP), EXP_MODE_GROUP:="Other"]
cfs_mode_dt[`Mode Code` %in% c(12), MODE_GROUP:="Pipeline"]
cfs_mode_dt[`Mode Code` %in% c(12) & EXPORT_YN=="Y", MODE_GROUP:=NA]
cfs_mode_dt[`Mode Code` %in% c(12), EXP_MODE_GROUP:=NA]
# cfs_mode_dt[`Mode Code` %in% c(2,19), MODE_GROUP:="Single"]
# cfs_mode_dt[`Mode Code` %in% c(0), MODE_GROUP:="Unknown"]
# cfs_mode_dt[is.na(MODE_GROUP), MODE_GROUP:="Multiple"]
setnames(cfs_mode_dt, "Mode Code", "MODE")
setkey(cfs_mode_dt, "EXPORT_YN", "MODE")
cfs_dt[cfs_mode_dt,MODE_GROUP:=factor(i.MODE_GROUP, levels=MODE_ORDER),
       on=.(EXPORT_YN,MODE)]
cfs_dt[cfs_mode_dt,EXP_MODE_GROUP:=factor(i.EXP_MODE_GROUP, levels = MODE_ORDER),
       on=.(EXPORT_YN,MODE)]

cfs_dt[,SHIPSIZE_GROUP:=cut(SHIPMT_WGHT,
                            breaks = SHIPSIZE_BINS,
                            labels = SHIPSIZE_LABELS)]

faf_dt[,SCTG:=str_pad(sctg2, 2, "left", "0")]
faf_dt[,TONS:=tons_2017*1E3]
faf_dt[,TONMILES:=tmiles_2017*1E6]
faf_dt[cfs_sctg_dt, SCTG_GROUP:=`i.SCTG Group`, on=.(SCTG)]
faf_dt[SCTG=="99", SCTG_GROUP:="39-43"]
faf_dt[,SHIPMENT_TYPE:=c("DOM", "IMP", "EXP")[trade_type]]

faf_pts <-  st_centroid(x = faf_poly)
plot(faf_pts[,c("FAF4_Regio")])
faf_pts_dt <- as.data.table(faf_pts)
faf_pts_dt[, x := st_coordinates(faf_pts)[,1]]
faf_pts_dt[, y := st_coordinates(faf_pts)[,2]]
setnames(faf_pts_dt, c("FAF4_Regio", "x", "y"), c("TAZ", "LONG", "LAT"))
faf_centroids <- faf_pts_dt[,.(TAZ, LONG, LAT)]
faf_dist <- pairsGCD(faf_centroids)

faf_dt[faf_dist[,.(dms_orig = oTAZ, dms_dest = dTAZ, GCD)],
       GCD := i.GCD, on = c("dms_orig", "dms_dest")]

faf_dt[, miles := TONMILES/TONS]
max(faf_dt[!is.na(miles) & !is.infinite(miles) & trade_type == 1]$miles)

dist_bins_100 <- seq(0,15000, by = 100)
faf_dt[,miles_bin := dist_bins_100[findInterval(miles, dist_bins_100)]]

# FAF 5 data and zones
faf5_dt[,SCTG:=str_pad(sctg2, 2, "left", "0")]
faf5_dt[,TONS:=tons_2017*1E3]
# faf5_dt[,TONMILES:=tmiles_2017*1E6] # No tonmiles in FAF5 csv file
faf5_dt[cfs_sctg_dt, SCTG_GROUP:=`i.SCTG Group`, on=.(SCTG)]
faf5_dt[SCTG=="99", SCTG_GROUP:="39-43"]
faf5_dt[,SHIPMENT_TYPE:=c("DOM", "IMP", "EXP")[trade_type]]

faf5_pts <-  st_centroid(x = faf5_poly)
plot(faf5_pts[,c("FAF_Zone")])
faf5_pts_dt <- as.data.table(faf5_pts)
faf5_pts_dt[, x := st_coordinates(faf5_pts)[,1]]
faf5_pts_dt[, y := st_coordinates(faf5_pts)[,2]]
setnames(faf5_pts_dt, c("FAF_Zone", "x", "y"), c("TAZ", "LONG", "LAT"))
faf5_centroids <- faf5_pts_dt[,.(TAZ, LONG, LAT)]
faf5_dist <- pairsGCD(faf5_centroids)
setkey(faf5_dist, oTAZ, dTAZ)

faf5_dt[faf5_dist[,.(dms_orig = as.integer(oTAZ), dms_dest = as.integer(dTAZ), GCD)],
        GCD := i.GCD, on = c("dms_orig", "dms_dest")]

faf_dt[dms_orig == dms_dest, .(miles = mean(miles, na.rm = TRUE)), by = .(dms_orig, dms_dest, SCTG)]
identical(sort(unique(as.integer(faf5_centroids$TAZ))),
          sort(unique(as.integer(faf_centroids$TAZ))))
# update the faf5 miles with intrazonal distance from the FAF4, to replace the zero distance from the centroid to centroid distance

faf5_dt[, miles := GCD]
faf5_dt[faf_dt[dms_orig == dms_dest, .(miles = mean(miles, na.rm = TRUE)), by = .(dms_orig, dms_dest, SCTG)],
        miles := i.miles, on = c("dms_orig", "dms_dest")]
faf5_dt[, miles_bin := dist_bins_100[findInterval(miles, dist_bins_100)]]



model_seg_sctg_naics_dt[, SCTG:=str_pad(sctg_code, 2, "left", "0")]
model_seg_sctg_naics_dt[cfs_sctg_dt, SCTG_GROUP:=`i.SCTG Group`, on=.(SCTG)]

cfs_dt[, NAICS2:=as.integer(substr(NAICS,1,2))]
cfs_dt[model_seg_sctg_naics_dt, MODEL_SEGMENT:=i.SegmentID,
       on=.(NAICS2=naics2_code,
            SCTG)]
cfs_dt[model_seg_sctg_naics_dt, MODEL_SEGMENT:=ifelse(is.na(MODEL_SEGMENT),
                                                      i.SegmentID,
                                                      MODEL_SEGMENT),
       on=.(NAICS2=naics2_code,
            SCTG_GROUP)]
cfs_dt[is.na(MODEL_SEGMENT), MODEL_SEGMENT:=99]
cfs_dt[,INDUSTRY_MODELED:=ifelse(MODEL_SEGMENT!=99, "Model", "Non-Model")]

SCTG_GROUP_LABELS <- unique(model_seg_sctg_naics_dt[,.(SCTG_GROUP, SCTG_LABEL=SCTG_Group)])
NAICS2_LABELS <- unique(model_seg_sctg_naics_dt[order(naics2_code),
                                                .(NAICS2=naics2_code, NAICS2_LABEL=NAICS2_Label)])
MODEL_SEGMENT_LABELS <- unique(model_seg_sctg_naics_dt[order(SegmentID),.(NAICS2=paste0(sort(unique(naics2_code)),
                                                                                        collapse = ", ")),
                                                     keyby=.(MODEL_SEGMENT=SegmentID, NAICS2_LABEL=NAICS2_Label,
                                                       SCTG_GROUP, SCTG_LABEL=SCTG_Group)])

cfs_dt[NAICS2_LABELS, INDUSTRY_GROUP:=i.NAICS2_LABEL,on=.(NAICS2)]
cfs_dt[MODEL_SEGMENT==99, INDUSTRY_GROUP:="Other Industries"]

desc_dt <- data.table(
  DIST_BAND="Domestic distance bin (5 categories) (GCD)",
  DOMAMPS="Domestic average mile per shipment (GCD)",
  DOMTONCFS="Domestic tonnage from CFS",
  DOMTONFAF="Domestic tonnage from FAF",
  EXPTONCFS="Export tonnage from CFS",
  EXPTONFAF="Export tonnage from FAF",
  IMPTONCFS="Import tonnage from CFS",
  IMPTONFAF="Import tonnage from FAF",
  DOMEXPTON="Domestic and export tonnage CFS",
  DOMTON="Domestic tonnage CFS",
  EXPTON="Export tonnage CFS",
  IMPTON="Import tonnage FAF",
  INDUSTRY_GROUP="Industry categories by NAICS2 (7 categories)",
  MISSTONDOMCFS="Domestic tonnage missing from CFS compared to FAF",
  MISSTONEXPCFS="Export tonnage missing from CFS compared to FAF",
  MISINDDOMEXPTON="Domestic and export tonnage FAF additional to CFS tonnage (i.e. FAF-CFS)",
  MODE_GROUP="Domestic Mode choices (5 categories)",
  MODEL_SEGMENT="Model segment as modeled in national freight model (24 categories)",
  NUM_OUTSOURCE_SHIPMENTS="Number of truck shipments that are outsourced",
  OUTSOURCE_SHIPMENT_SHARE="Percentage share of truck shipments that are outsourced",
  SCTG="2 digit commodity code (43 categories)",
  SCTG_GROUP = "Group of 2 digit commodity codes (9 categories)",
  SCTG_GROUP_LABEL = "Description of group of 2 digit commodity codes",
  SHIPMENTS="Number of shipments",
  SHIPSIZE_GROUP="Shipment size bin (4 categories)",
  TON_GROUP="Tonnage bin (5 categories)",
  TONMILES="Ton-miles (domestic)",
  TONMILESHARE="Percentage share of ton-miles (domestic)",
  TONS="Tonnage",
  TONSHARE="Percentage share of tonnage"
)

desc_dt <- data.table(Headers=names(desc_dt),
                      Description=as.vector(t(desc_dt[1,])[,1]))
setkey(desc_dt, Headers)

target_ls <- list()

faf5_ls <- list()

# Write out targets ---------------------------------------
# Annual tonnage by Commodity groups
# domestic and export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG", "SCTG_GROUP"),
                                    var_name = "SHIPMT_WGHT_TON")
tab_dt <- merge(tab_dt,CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG", "SCTG_GROUP"),
                                    var_name = "SHIPMT_WGHT_TON"),
                on=.(SCTG, SCTG_GROUP),
                suffixes = c("_DOM", "_EXP"))

faf_sctg_dt <- faf_dt[,.(TOTAL_SHIPMT_WGHT_TON=
                           sum(TONS)),
                              keyby=.(SCTG, SHIPMENT_TYPE)]
faf_sctg_dt[,SHIPMENT_TYPE:=paste0("TOTAL_SHIPMT_WGHT_TON_", SHIPMENT_TYPE)]
faf_sctg_dt = dcast(faf_sctg_dt, SCTG~SHIPMENT_TYPE, value.var = "TOTAL_SHIPMT_WGHT_TON")
setnames(faf_sctg_dt, paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP")),
         paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP"), "_FAF"))

faf5_sctg_dt <- faf5_dt[,.(TOTAL_SHIPMT_WGHT_TON=
                           sum(TONS)),
                      keyby=.(SCTG, SHIPMENT_TYPE)]
faf5_sctg_dt[,SHIPMENT_TYPE:=paste0("TOTAL_SHIPMT_WGHT_TON_", SHIPMENT_TYPE)]
faf5_sctg_dt = dcast(faf5_sctg_dt, SCTG~SHIPMENT_TYPE, value.var = "TOTAL_SHIPMT_WGHT_TON")
setnames(faf5_sctg_dt, paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP")),
         paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP"), "_FAF"))

faf5_ls$faf5_sctg_dt <- faf5_sctg_dt

#tab_dt = merge(tab_dt, faf_sctg_dt, by="SCTG", all=TRUE)
### Use FAF5
tab_dt = merge(tab_dt, faf5_sctg_dt, by="SCTG", all=TRUE)

# tab_dt[,TOTAL_SHIPMT_WGHT_TON_FAF:=
#          prop.table(TOTAL_DOM_SHIPMT_WGHT_TON+TOTAL_EXP_SHIPMT_WGHT_TON)*
#          TOTAL_SHIPMT_WGHT_TON_FAF,
#                     by=.(SCTG)]
# tab_dt[,MissIndustryTonnage:=TOTAL_SHIPMT_WGHT_TON_FAF-TOTAL_SHIPMT_WGHT_TON_DOM-
#          TOTAL_SHIPMT_WGHT_TON_EXP]
# 
# faf_sctg_imp_dt <- faf_dt[trade_type==2,.(TOTAL_SHIPMT_WGHT_TON=
#                                                     sum(TONS)),
#                                   keyby=SCTG]
# tab_dt[faf_sctg_imp_dt,TOTAL_SHIPMT_WGHT_TON_FAF_IMP:=i.TOTAL_SHIPMT_WGHT_TON,
#                     on=.(SCTG)]
# tab_dt[,TOTAL_SHIPMT_WGHT_TON_FAF_IMP:=prop.table(TOTAL_DOM_SHIPMT_WGHT_TON+TOTAL_EXP_SHIPMT_WGHT_TON)*
#                       TOTAL_SHIPMT_WGHT_TON_FAF_IMP,
#                     by=.(SCTG)]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]




target_ls[["cfs_faf_sctg_tonnage_dt"]] <- tab_dt[,.(SCTG,
                 SCTG_GROUP,
                 SCTG_GROUP_LABEL,
                 DOMTONCFS=TOTAL_SHIPMT_WGHT_TON_DOM,
                 EXPTONCFS=TOTAL_SHIPMT_WGHT_TON_EXP,
                 DOMTONFAF=TOTAL_SHIPMT_WGHT_TON_DOM_FAF,
                 EXPTONFAF=TOTAL_SHIPMT_WGHT_TON_EXP_FAF,
                 IMPTONFAF=TOTAL_SHIPMT_WGHT_TON_IMP_FAF,
                 MISSTONDOMCFS=TOTAL_SHIPMT_WGHT_TON_DOM_FAF-
                   TOTAL_SHIPMT_WGHT_TON_DOM,
                 MISSTONEXPCFS=TOTAL_SHIPMT_WGHT_TON_EXP_FAF-
                   TOTAL_SHIPMT_WGHT_TON_EXP)]

tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt <- merge(tab_dt, CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG_GROUP"),
                       var_name = "SHIPMT_WGHT_TON"),
                by=c("SCTG_GROUP"),
                suffixes = c("_DOM", "_EXP"))

faf_sctg_group_dt <- faf_dt[,.(TOTAL_SHIPMT_WGHT_TON=
                                 sum(TONS)),
                            keyby=.(SCTG_GROUP, SHIPMENT_TYPE)]

faf_sctg_group_dt[,SHIPMENT_TYPE:=paste0("TOTAL_SHIPMT_WGHT_TON_", SHIPMENT_TYPE)]
faf_sctg_group_dt = dcast(faf_sctg_group_dt, SCTG_GROUP~SHIPMENT_TYPE, value.var = "TOTAL_SHIPMT_WGHT_TON")
setnames(faf_sctg_group_dt, paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP")),
         paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP"), "_FAF"))

faf5_sctg_group_dt <- faf5_dt[,.(TOTAL_SHIPMT_WGHT_TON=
                                 sum(TONS)),
                            keyby=.(SCTG_GROUP, SHIPMENT_TYPE)]

faf5_sctg_group_dt[,SHIPMENT_TYPE:=paste0("TOTAL_SHIPMT_WGHT_TON_", SHIPMENT_TYPE)]
faf5_sctg_group_dt = dcast(faf5_sctg_group_dt, SCTG_GROUP~SHIPMENT_TYPE, value.var = "TOTAL_SHIPMT_WGHT_TON")
setnames(faf5_sctg_group_dt, paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP")),
         paste0("TOTAL_SHIPMT_WGHT_TON_", c("DOM", "IMP", "EXP"), "_FAF"))

faf5_ls$faf5_sctg_group_dt <- faf5_sctg_group_dt

#tab_dt = merge(tab_dt, faf_sctg_group_dt, by="SCTG_GROUP", all=TRUE)
### Use FAF5
tab_dt = merge(tab_dt, faf5_sctg_group_dt, by="SCTG_GROUP", all=TRUE)

# tab_dt[,MissIndustryTonnage:=TOTAL_SHIPMT_WGHT_TON_FAF-TOTAL_SHIPMT_WGHT_TON_DOM-
#          TOTAL_SHIPMT_WGHT_TON_EXP]
# 
# faf_sctg_group_imp_dt <- faf_dt[trade_type==2,.(TOTAL_SHIPMT_WGHT_TON=
#                                             sum(TONS)),
#                           keyby=SCTG_GROUP]
# tab_dt[faf_sctg_group_imp_dt,TOTAL_SHIPMT_WGHT_TON_FAF_IMP:=i.TOTAL_SHIPMT_WGHT_TON,
#        on=.(SCTG_GROUP)]
# tab_dt[,TOTAL_SHIPMT_WGHT_TON_FAF_IMP:=prop.table(TOTAL_SHIPMT_WGHT_TON)*
#          TOTAL_SHIPMT_WGHT_TON_FAF_IMP,
#        by=.(SCTG_GROUP)]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]



target_ls[["cfs_faf_sctg_group_tonnage_dt"]] <- tab_dt[,.(SCTG_GROUP,
                                                                         SCTG_GROUP_LABEL,
                                                                         DOMTONCFS=TOTAL_SHIPMT_WGHT_TON_DOM,
                                                                         EXPTONCFS=TOTAL_SHIPMT_WGHT_TON_EXP,
                                                                         DOMTONFAF=TOTAL_SHIPMT_WGHT_TON_DOM_FAF,
                                                                         EXPTONFAF=TOTAL_SHIPMT_WGHT_TON_EXP_FAF,
                                                                         IMPTONFAF=TOTAL_SHIPMT_WGHT_TON_IMP_FAF,
                                                                         MISSTONDOMCFS=TOTAL_SHIPMT_WGHT_TON_DOM_FAF-
                                                                           TOTAL_SHIPMT_WGHT_TON_DOM,
                                                                         MISSTONEXPCFS=TOTAL_SHIPMT_WGHT_TON_EXP_FAF-
                                                                           TOTAL_SHIPMT_WGHT_TON_EXP)]


# Annual trade by Commodity and tonnage bins
# Domestic
tab_dt <- cfs_dt[EXPORT_YN=="N",.(SHIPMENTS=sum(WGT_FACTOR)),keyby=.(SCTG,
                                                               SCTG_GROUP,
                                                               TON_GROUP=
                                                                 cut(SHIPMT_WGHT_TON*WGT_FACTOR,
                                                                     breaks = WEIGHT_BINS,
                                                                     labels = WEIGHT_LABELS))]
tab_dt[,SHIPMENTSHARE:=prop.table(SHIPMENTS),.(SCTG)]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_ton_trades_dt"]] <- tab_dt
# Export
tab_dt <- cfs_dt[EXPORT_YN=="Y",.(SHIPMENTS=sum(WGT_FACTOR)),keyby=.(SCTG,
                                                               SCTG_GROUP,
                                                               TON_GROUP=
                                                                 cut(SHIPMT_WGHT_TON*WGT_FACTOR,
                                                                     breaks = WEIGHT_BINS,
                                                                     labels = WEIGHT_LABELS))]

tab_dt[,SHIPMENTSHARE:=prop.table(SHIPMENTS),.(SCTG)]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_ton_trades_dt"]] <- tab_dt

# Domestic
tab_dt <- cfs_dt[EXPORT_YN=="N",.(SHIPMENTS=sum(WGT_FACTOR)),keyby=.(SCTG_GROUP,
                                                               TON_GROUP=
                                                                 cut(SHIPMT_WGHT_TON*WGT_FACTOR,
                                                                     breaks = WEIGHT_BINS,
                                                                     labels = WEIGHT_LABELS))]

tab_dt[,SHIPMENTSHARE:=prop.table(SHIPMENTS),.(SCTG_GROUP)]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_group_ton_trades_dt"]] <- tab_dt

# Export
tab_dt <- cfs_dt[EXPORT_YN=="Y",.(SHIPMENTS=sum(WGT_FACTOR)),keyby=.(SCTG_GROUP,
                                                               TON_GROUP=
                                                                 cut(SHIPMT_WGHT_TON*WGT_FACTOR,
                                                                     breaks = WEIGHT_BINS,
                                                                     labels = WEIGHT_LABELS))]

tab_dt[,SHIPMENTSHARE:=prop.table(SHIPMENTS),.(SCTG_GROUP)]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_group_ton_trades_dt"]] <- tab_dt

# Annual weighted miles per shipment by commodity group
cfs_dt[, TonWeight:=SHIPMT_WGHT_TON*WGT_FACTOR]

# Domestic
tab_dt <- CFSAPMSCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG", "SCTG_GROUP"),
                      var_name="SHIPMT_DIST_GC")

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_dom_amps_dt"]] <- tab_dt[,.(SCTG, SCTG_GROUP,
                                                 SCTG_GROUP_LABEL, DOMAMPS=AVG_SHIPMT_DIST_GC)]
# Export
tab_dt <- CFSAPMSCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG", "SCTG_GROUP"),
                      var_name="SHIPMT_DIST_GC")

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_dom_amps_dt"]] <- tab_dt[,.(SCTG, SCTG_GROUP,
                                                 SCTG_GROUP_LABEL, DOMAMPS=AVG_SHIPMT_DIST_GC)]


# Domestic
tab_dt <- CFSAPMSCalc(cfs_dt[EXPORT_YN=="N"], group_var = "SCTG_GROUP",
                                var_name="SHIPMT_DIST_GC")

cfs_dt[,TonWeight:=NULL]
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_group_dom_amps_dt"]] <- tab_dt[,.(SCTG_GROUP,
                                                       SCTG_GROUP_LABEL,
                           DOMAMPS=AVG_SHIPMT_DIST_GC)]
# Export
tab_dt <- CFSAPMSCalc(cfs_dt[EXPORT_YN=="Y"], group_var = "SCTG_GROUP",
                                var_name="SHIPMT_DIST_GC")

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_group_dom_amps_dt"]] <- tab_dt[,.(SCTG_GROUP,
                                                       SCTG_GROUP_LABEL,
                           DOMAMPS=AVG_SHIPMT_DIST_GC)]

# Ton and Tonmile share by domestic mode
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("MODE_GROUP"),
                                    var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_dom_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                              TONS=TOTAL_SHIPMT_WGHT_TON,
                              TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON))]

# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("MODE_GROUP"),
                                    var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_exp_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                              TONS=TOTAL_SHIPMT_WGHT_TON,
                              TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON))]

# Domestic
cfs_dt[,SHIPMT_TON_MILES:=SHIPMT_WGHT_TON*SHIPMT_DIST_GC]

tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("MODE_GROUP"),
                                   var_name = "SHIPMT_TON_MILES")

target_ls[["cfs_dom_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                 TONMILES=TOTAL_SHIPMT_TON_MILES,
                                                 TONMILESHARE=prop.table(TOTAL_SHIPMT_TON_MILES))]
# Export
cfs_dt[,SHIPMT_TON_MILES:=SHIPMT_WGHT_TON*SHIPMT_DIST_GC]

tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("MODE_GROUP"),
                                   var_name = "SHIPMT_TON_MILES")

target_ls[["cfs_exp_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                 TONMILES=TOTAL_SHIPMT_TON_MILES,
                                                 TONMILESHARE=prop.table(TOTAL_SHIPMT_TON_MILES))]

# By commodity group
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG", 
                                                               "SCTG_GROUP", 
                                                               "MODE_GROUP"),
                                         var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_dom_sctg_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                   TONS=TOTAL_SHIPMT_WGHT_TON,
                                   TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                keyby=.(SCTG, SCTG_GROUP,
                                        SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG", 
                                                               "SCTG_GROUP", 
                                                               "MODE_GROUP"),
                                         var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_exp_sctg_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                   TONS=TOTAL_SHIPMT_WGHT_TON,
                                   TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                keyby=.(SCTG, SCTG_GROUP,
                                        SCTG_GROUP_LABEL)]

### Similar tables based on FAF5

# Mode coding in FAF5

# Add ton miles for mode summaries
faf5_dt[, TONMILES := TONS * miles]

# Numeric Label	Description
faf_mode_labels <- data.table(mode = 1:8,
                              label = c("Truck", "Rail", 
                                      "Water", "Air (include truck-air)",
                                      "Multiple modes & mail", "Pipeline", 
                                      "Other and unknown", "No domestic mode"))

faf_trade_type_labels <- data.table(trade_type  = 1:3,
                                    label = c("Domestic", "Import", "Export"))


faf5_dt[faf_mode_labels[,.(dms_mode = mode, label)],
        dms_mode_label := i.label,
        on = "dms_mode"]

faf5_dt[faf_mode_labels[,.(fr_inmode = mode, label)],
        fr_inmode_label := i.label,
        on = "fr_inmode"]

faf5_dt[faf_mode_labels[,.(fr_outmode = mode, label)],
        fr_outmode_label := i.label,
        on = "fr_outmode"]

faf5_dt[faf_trade_type_labels,
        trade_type_label := i.label,
        on = "trade_type"]
faf5_dt[, trade_type_label := factor(trade_type_label, levels = faf_trade_type_labels$label)]

faf5_dt[, OSTATEFIPS := dms_orig%/%10]
faf5_dt[, DSTATEFIPS := dms_dest%/%10]

faf5_dt[state_region[,.(OSTATEFIPS = STATEFIPS, STATE, REGION)], 
                     c("OSTATE", "OREGION") := .(i.STATE, i.REGION),
                     on = "OSTATEFIPS"]

faf5_dt[state_region[,.(DSTATEFIPS = STATEFIPS, STATE, REGION)], 
        c("DSTATE", "DREGION") := .(i.STATE, i.REGION),
        on = "DSTATEFIPS"]

# Domestic mode: Tons and Ton share by trade type
dcast.data.table(faf5_dt[,.(TONS = round(sum(TONS))), 
                         keyby = .(dms_mode, dms_mode_label, trade_type_label)][, TONS_SHARE := round(TONS/sum(TONS),3), by = trade_type_label],
                 dms_mode + dms_mode_label ~ trade_type_label, 
                 fun.aggregate = sum, 
                 value.var = c("TONS", "TONS_SHARE"))

faf5_ls$faf5_dom_mode_tt_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                 keyby = .(dms_mode, dms_mode_label, trade_type_label)][, TONS_SHARE := round(TONS/sum(TONS),3), by = trade_type_label]

faf5_ls$faf5_dom_mode_tt_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                 keyby = .(dms_mode, dms_mode_label, trade_type_label)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = trade_type_label]

faf5_ls$faf5_dom_mode_tt_sctg_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                 keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, SCTG)]

faf5_ls$faf5_dom_mode_tt_sctg_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                     keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, SCTG)]

faf5_ls$faf5_dom_mode_tt_region_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                 keyby = .(dms_mode, dms_mode_label, trade_type_label, OREGION, DREGION)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, OREGION, DREGION)]

faf5_ls$faf5_dom_mode_tt_region_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                     keyby = .(dms_mode, dms_mode_label, trade_type_label, OREGION, DREGION)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, OREGION, DREGION)]

faf5_ls$faf5_dom_mode_tt_sctg_region_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                      keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP, OREGION, DREGION)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, SCTG, OREGION, DREGION)]

faf5_ls$faf5_dom_mode_tt_sctg_region_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                          keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP, OREGION, DREGION)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, SCTG, OREGION, DREGION)]

# Foreign mode and foreign/domestic combinations

# Add a variable to seperate out Canada, Mexico, and ROTW
faf5_dt[trade_type_label == "Import", 
        Import_Origin := ifelse(fr_orig == 801, "Canada", 
                                ifelse(fr_orig == 802, "Mexico", "Rest of the World"))]

faf5_dt[trade_type_label == "Export", 
        Export_Destination := ifelse(fr_dest == 801, "Canada", 
                                     ifelse(fr_dest == 802, "Mexico", "Rest of the World"))]

faf5_ls$faf5_fr_mode_import_tons <- faf5_dt[trade_type_label %in% c("Import"),
                         .(TONS = round(sum(TONS))), 
                         keyby = .(fr_inmode, fr_inmode_label)][, TONS_SHARE := round(TONS/sum(TONS),3)]

faf5_ls$faf5_fr_mode_import_tonmiles <- faf5_dt[trade_type_label %in% c("Import"),
                                    .(TONMILES = round(sum(TONMILES))), 
                                    keyby = .(fr_inmode, fr_inmode_label)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3)]

faf5_ls$faf5_fr_mode_export_tons <- faf5_dt[trade_type_label %in% c("Export"),
                                    .(TONS = round(sum(TONS))), 
                                    keyby = .(fr_outmode, fr_outmode_label)][, TONS_SHARE := round(TONS/sum(TONS),3)]

faf5_ls$faf5_fr_mode_export_tonmiles <- faf5_dt[trade_type_label %in% c("Export"),
                                    .(TONMILES = round(sum(TONMILES))), 
                                    keyby = .(fr_outmode, fr_outmode_label)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3)]

faf5_ls$faf5_fr_mode_import_sctg_tons <- faf5_dt[trade_type_label %in% c("Import"),
                                    .(TONS = round(sum(TONS))), 
                                    keyby = .(fr_inmode, fr_inmode_label, SCTG)][, TONS_SHARE := round(TONS/sum(TONS),3), by = SCTG]

faf5_ls$faf5_fr_mode_import_sctg_tonmiles <- faf5_dt[trade_type_label %in% c("Import"),
                                        .(TONMILES = round(sum(TONMILES))), 
                                        keyby = .(fr_inmode, fr_inmode_label, SCTG)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = SCTG]

faf5_ls$faf5_fr_mode_export_sctg_tons <- faf5_dt[trade_type_label %in% c("Export"),
                                    .(TONS = round(sum(TONS))), 
                                    keyby = .(fr_outmode, fr_outmode_label, SCTG)][, TONS_SHARE := round(TONS/sum(TONS),3), by = SCTG]

faf5_ls$faf5_fr_mode_export_sctg_tonmiles <- faf5_dt[trade_type_label %in% c("Export"),
                                        .(TONMILES = round(sum(TONMILES))), 
                                        keyby = .(fr_outmode, fr_outmode_label, SCTG)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = SCTG]

faf5_ls$faf5_fr_mode_import_location_tons <- faf5_dt[trade_type_label %in% c("Import"),
                                    .(TONS = round(sum(TONS))), 
                                    keyby = .(fr_inmode, fr_inmode_label, Import_Origin)][, TONS_SHARE := round(TONS/sum(TONS),3), by = Import_Origin]

faf5_ls$faf5_fr_mode_import_location_tonmiles <- faf5_dt[trade_type_label %in% c("Import"),
                                        .(TONMILES = round(sum(TONMILES))), 
                                        keyby = .(fr_inmode, fr_inmode_label, Import_Origin)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = Import_Origin]

faf5_ls$faf5_fr_mode_export_location_tons <- faf5_dt[trade_type_label %in% c("Export"),
                                    .(TONS = round(sum(TONS))), 
                                    keyby = .(fr_outmode, fr_outmode_label, Export_Destination)][, TONS_SHARE := round(TONS/sum(TONS),3), by = Export_Destination]

faf5_ls$faf5_fr_mode_export_location_tonmiles <- faf5_dt[trade_type_label %in% c("Export"),
                                        .(TONMILES = round(sum(TONMILES))), 
                                        keyby = .(fr_outmode, fr_outmode_label, Export_Destination)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = Export_Destination]

faf5_ls$faf5_fr_mode_import_sctg_location_tons <- faf5_dt[trade_type_label %in% c("Import"),
                                         .(TONS = round(sum(TONS))), 
                                         keyby = .(fr_inmode, fr_inmode_label, SCTG, Import_Origin)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(SCTG, Import_Origin)]

faf5_ls$faf5_fr_mode_import_sctg_location_tonmiles <- faf5_dt[trade_type_label %in% c("Import"),
                                             .(TONMILES = round(sum(TONMILES))), 
                                             keyby = .(fr_inmode, fr_inmode_label, SCTG, Import_Origin)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(SCTG, Import_Origin)]

faf5_ls$faf5_fr_mode_export_sctg_location_tons <- faf5_dt[trade_type_label %in% c("Export"),
                                         .(TONS = round(sum(TONS))), 
                                         keyby = .(fr_outmode, fr_outmode_label, SCTG, Export_Destination)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(SCTG, Export_Destination)]

faf5_ls$faf5_fr_mode_export_sctg_location_tonmiles <- faf5_dt[trade_type_label %in% c("Export"),
                                             .(TONMILES = round(sum(TONMILES))), 
                                             keyby = .(fr_outmode, fr_outmode_label, SCTG, Export_Destination)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(SCTG, Export_Destination)]

dcast.data.table(faf5_ls$faf5_fr_mode_import_location_tons,
                 fr_inmode + fr_inmode_label ~ Import_Origin,
                 value.var = c("TONS", "TONS_SHARE"), 
                 fill = 0)

dcast.data.table(faf5_ls$faf5_fr_mode_export_location_tons,
                 fr_outmode + fr_outmode_label ~ Export_Destination,
                 value.var = c("TONS", "TONS_SHARE"), 
                 fill = 0)


# Mode combinations
faf5_dt[trade_type_label %in% c("Import", "Export"), 
        fr_dms_mode := ifelse(trade_type_label == "Import", 
                              paste(fr_inmode_label, dms_mode_label, sep = " - "),
                              paste(dms_mode_label, fr_outmode_label, sep = " - "))]

faf5_ls$faf5_fr_dom_mode_import_tons <- faf5_dt[trade_type_label %in% c("Import"),
        .(TONS = round(sum(TONS))), 
        keyby = .(fr_dms_mode, fr_inmode_label, dms_mode_label)][, TONS_SHARE := round(TONS/sum(TONS),3)][order(-TONS)]

faf5_ls$faf5_fr_dom_mode_import_tonmiles <- faf5_dt[trade_type_label %in% c("Import"),
                                        .(TONMILES = round(sum(TONMILES))), 
                                        keyby = .(fr_dms_mode, fr_inmode_label, dms_mode_label)][, TONS_SHARE := round(TONMILES/sum(TONMILES),3)][order(-TONMILES)]

faf5_ls$faf5_fr_dom_mode_export_tons <- faf5_dt[trade_type_label %in% c("Export"),
        .(TONS = round(sum(TONS))), 
        keyby = .(fr_dms_mode, dms_mode_label, fr_outmode_label)][, TONS_SHARE := round(TONS/sum(TONS),3)][order(-TONS)]

faf5_ls$faf5_fr_dom_mode_export_tonmiles <- faf5_dt[trade_type_label %in% c("Export"),
                                        .(TONMILES = round(sum(TONMILES))), 
                                        keyby = .(fr_dms_mode, dms_mode_label, fr_outmode_label)][, TONS_SHARE := round(TONMILES/sum(TONMILES),3)][order(-TONMILES)]

# by commodity and origin location/detination locations
faf5_ls$faf5_fr_dom_mode_import_sctG_location_tons <- faf5_dt[trade_type_label %in% c("Import"),
                                        .(TONS = round(sum(TONS))), 
                                        keyby = .(fr_dms_mode, fr_inmode_label, dms_mode_label, SCTG, Import_Origin)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(SCTG, Import_Origin)]

faf5_ls$faf5_fr_dom_mode_import_sctG_location_tonmiles <- faf5_dt[trade_type_label %in% c("Import"),
                                            .(TONMILES = round(sum(TONMILES))), 
                                            keyby = .(fr_dms_mode, fr_inmode_label, dms_mode_label, SCTG, Import_Origin)][, TONS_SHARE := round(TONMILES/sum(TONMILES),3), by = .(SCTG, Import_Origin)]

faf5_ls$faf5_fr_dom_mode_export_sctG_location_tons <- faf5_dt[trade_type_label %in% c("Export"),
                                        .(TONS = round(sum(TONS))), 
                                        keyby = .(fr_dms_mode, dms_mode_label, fr_outmode_label, SCTG, Export_Destination)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(SCTG, Export_Destination)]

faf5_ls$faf5_fr_dom_mode_export_sctG_location_tonmiles <- faf5_dt[trade_type_label %in% c("Export"),
                                            .(TONMILES = round(sum(TONMILES))), 
                                            keyby = .(fr_dms_mode, dms_mode_label, fr_outmode_label, SCTG, Export_Destination)][, TONS_SHARE := round(TONMILES/sum(TONMILES),3), by = .(SCTG, Export_Destination)]




# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG", 
                                                              "SCTG_GROUP", 
                                                              "MODE_GROUP"),
                                        var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                          TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                          TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                       keyby = .(SCTG, SCTG_GROUP,
                                                                 SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG", 
                                                              "SCTG_GROUP", 
                                                              "MODE_GROUP"),
                                        var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                          TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                          TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                       keyby = .(SCTG, SCTG_GROUP,
                                                                 SCTG_GROUP_LABEL)]

# By SCTG Group
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_dom_sctg_group_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                      TONS=TOTAL_SHIPMT_WGHT_TON,
                                                      TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                   keyby=.(SCTG_GROUP,
                                                           SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_exp_sctg_group_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                      TONS=TOTAL_SHIPMT_WGHT_TON,
                                                      TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                   keyby=.(SCTG_GROUP,
                                                           SCTG_GROUP_LABEL)]
# target_ls[["cfs_sctg_group_w_mode_tonshare_dt"]] <- dcast(tab_dt[,.(MODE_GROUP,
#                                                                     TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                                     TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                                  keyby=.(SCTG_GROUP,
#                                                                          SCTG_GROUP_LABEL)],
#                                                     SCTG_GROUP+SCTG_GROUP_LABEL~MODE_GROUP,
#                                                     value.var = c("TONS", "TONSHARE"))

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_group_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                          TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                          TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                       keyby = .(SCTG_GROUP,
                                                                 SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_group_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                          TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                          TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                       keyby = .(SCTG_GROUP,
                                                                 SCTG_GROUP_LABEL)]

# By Model Segment
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("MODEL_SEGMENT", 
                                             "SCTG_GROUP",
                                             "INDUSTRY_GROUP",
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_dom_model_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                      TONS=TOTAL_SHIPMT_WGHT_TON,
                                                      TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                   keyby=.(MODEL_SEGMENT, 
                                                           INDUSTRY_GROUP,
                                                           SCTG_GROUP,
                                                           SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("MODEL_SEGMENT", 
                                             "SCTG_GROUP",
                                             "INDUSTRY_GROUP",
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_exp_model_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                      TONS=TOTAL_SHIPMT_WGHT_TON,
                                                      TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                   keyby=.(MODEL_SEGMENT, 
                                                           INDUSTRY_GROUP,
                                                           SCTG_GROUP,
                                                           SCTG_GROUP_LABEL)]

# target_ls[["cfs_model_mode_w_tonshare_dt"]] <- dcast(tab_dt[,.(MODE_GROUP,
#                                                                TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                                TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                             keyby=.(MODEL_SEGMENT, INDUSTRY_GROUP, SCTG_GROUP,
#                                                                     SCTG_GROUP_LABEL)],
#                                                      MODEL_SEGMENT+INDUSTRY_GROUP+SCTG_GROUP+SCTG_GROUP_LABEL~MODE_GROUP,
#                                                     value.var = c("TONS", "TONSHARE"))

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("MODEL_SEGMENT", 
                                             "SCTG_GROUP", 
                                             "INDUSTRY_GROUP",
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_model_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                          TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                          TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                       keyby = .(MODEL_SEGMENT, 
                                                                 INDUSTRY_GROUP,
                                                                 SCTG_GROUP, SCTG_GROUP_LABEL)]

# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("MODEL_SEGMENT", 
                                             "SCTG_GROUP", 
                                             "INDUSTRY_GROUP",
                                             "MODE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_model_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
                                                          TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                          TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                       keyby = .(MODEL_SEGMENT, 
                                                                 INDUSTRY_GROUP,
                                                                 SCTG_GROUP, SCTG_GROUP_LABEL)]

# # By Non model segments
# # By commodity group
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG", 
#                                              "SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_dom_nonmodel_sctg_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                       TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                       TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                    keyby=.(SCTG, SCTG_GROUP,
#                                                            SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG", 
#                                              "SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_exp_nonmodel_sctg_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                       TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                       TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                    keyby=.(SCTG, SCTG_GROUP,
#                                                            SCTG_GROUP_LABEL)]
# 
# # target_ls[["cfs_nonmodel_sctg_mode_w_tonshare_dt"]] <- dcast(tab_dt[,.(MODE_GROUP,
# #                                                                        TONS=TOTAL_SHIPMT_WGHT_TON,
# #                                                                        TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
# #                                                                     keyby=.(SCTG, SCTG_GROUP,
# #                                                                             SCTG_GROUP_LABEL)],
# #                                                              SCTG+SCTG_GROUP+SCTG_GROUP+SCTG_GROUP_LABEL~MODE_GROUP,
# #                                                      value.var = c("TONS", "TONSHARE"))
# 
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG", 
#                                              "SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_dom_nonmodel_sctg_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                           TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                           TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                        keyby = .(SCTG, SCTG_GROUP,
#                                                                  SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG", 
#                                              "SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_exp_nonmodel_sctg_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                           TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                           TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                        keyby = .(SCTG, SCTG_GROUP,
#                                                                  SCTG_GROUP_LABEL)]
# 
# # By SCTG Group
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_dom_nonmodel_sctg_group_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                             TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                             TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                          keyby=.(SCTG_GROUP,
#                                                                  SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_exp_nonmodel_sctg_group_mode_tonshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                             TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                             TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                          keyby=.(SCTG_GROUP,
#                                                                  SCTG_GROUP_LABEL)]
# # target_ls[["cfs_nonmodel_sctg_group_mode_w_tonshare_dt"]] <- dcast(tab_dt[,.(MODE_GROUP,
# #                                                                              TONS=TOTAL_SHIPMT_WGHT_TON,
# #                                                                              TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
# #                                                                           keyby=.(SCTG_GROUP,
# #                                                                                   SCTG_GROUP_LABEL)],
# #                                                              SCTG_GROUP+SCTG_GROUP+SCTG_GROUP_LABEL~MODE_GROUP,
# #                                                              value.var = c("TONS", "TONSHARE"))
# 
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_dom_nonmodel_sctg_group_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                                 TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                                 TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                              keyby = .(SCTG_GROUP,
#                                                                        SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
#                                              "MODE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_exp_nonmodel_sctg_group_mode_tonmileshare_dt"]] <- tab_dt[,.(MODE_GROUP,
#                                                                 TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                                 TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                              keyby = .(SCTG_GROUP,
#                                                                        SCTG_GROUP_LABEL)]



# by distance band
# Domestic
cfs_dt[, SHIPMT_DIST_GC_BIN:=cut(SHIPMT_DIST_GC,
                                 breaks = DIST_BINS,
                                 labels = DIST_LABELS)]
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                               "MODE_GROUP"),
                                         var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_dom_dist_mode_tonshare_dt"]]  <- tab_dt[,.(MODE_GROUP,
                                   TONS = TOTAL_SHIPMT_WGHT_TON,
                                   TONSHARE = prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]
# Export
cfs_dt[, SHIPMT_DIST_GC_BIN:=cut(SHIPMT_DIST_GC,
                                 breaks = DIST_BINS,
                                 labels = DIST_LABELS)]
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                               "MODE_GROUP"),
                                         var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_exp_dist_mode_tonshare_dt"]]  <- tab_dt[,.(MODE_GROUP,
                                   TONS = TOTAL_SHIPMT_WGHT_TON,
                                   TONSHARE = prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]

# target_ls[["cfs_dist_mode_w_tonshare_dt"]] <- dcast(tab_dt[,.(MODE_GROUP,
#                                                               TONS = TOTAL_SHIPMT_WGHT_TON,
#                                                               TONSHARE = prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                            keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)],
#                                                     DIST_BAND~MODE_GROUP,
#                                                                    value.var = c("TONS", "TONSHARE"))

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                              "MODE_GROUP"),
                                        var_name = "SHIPMT_TON_MILES")


target_ls[["cfs_dom_dist_mode_tonmileshare_dt"]]  <- tab_dt[,.(MODE_GROUP,
                                  TONMILES = TOTAL_SHIPMT_TON_MILES,
                                  TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                               keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                              "MODE_GROUP"),
                                        var_name = "SHIPMT_TON_MILES")


target_ls[["cfs_exp_dist_mode_tonmileshare_dt"]]  <- tab_dt[,.(MODE_GROUP,
                                  TONMILES = TOTAL_SHIPMT_TON_MILES,
                                  TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                               keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]




# Ton and Tonmile share by shipment size
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SHIPSIZE_GROUP"),
                                        var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_dom_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                  TONS=TOTAL_SHIPMT_WGHT_TON,
                                  TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON))]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SHIPSIZE_GROUP"),
                                        var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_exp_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                  TONS=TOTAL_SHIPMT_WGHT_TON,
                                  TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON))]

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SHIPSIZE_GROUP"),
                                       var_name = "SHIPMT_TON_MILES")

target_ls[["cfs_dom_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                 TONMILES = TOTAL_SHIPMT_TON_MILES,
                                 TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES))]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SHIPSIZE_GROUP"),
                                       var_name = "SHIPMT_TON_MILES")

target_ls[["cfs_exp_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                 TONMILES = TOTAL_SHIPMT_TON_MILES,
                                 TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES))]



# By commodity group
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG",
                                                                   "SCTG_GROUP", 
                                                                   "SHIPSIZE_GROUP"),
                                             var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_dom_sctg_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                       TONS=TOTAL_SHIPMT_WGHT_TON,
                                       TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                    keyby=.(SCTG, SCTG_GROUP,
                                            SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG",
                                                                   "SCTG_GROUP", 
                                                                   "SHIPSIZE_GROUP"),
                                             var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_exp_sctg_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                       TONS=TOTAL_SHIPMT_WGHT_TON,
                                       TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                    keyby=.(SCTG, SCTG_GROUP,
                                            SCTG_GROUP_LABEL)]

# target_ls[["cfs_sctg_shipsize_w_tonshare_dt"]] <- dcast(tab_dt[,.(SHIPSIZE_GROUP,
#                                                                   TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                                   TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                                keyby=.(SCTG, SCTG_GROUP,
#                                                                        SCTG_GROUP_LABEL)],
#                                                         SCTG+SCTG_GROUP+SCTG_GROUP_LABEL~SHIPSIZE_GROUP,
#                                                    value.var = c("TONS", "TONSHARE"))

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG", 
                                                                  "SCTG_GROUP", 
                                                                  "SHIPSIZE_GROUP"),
                                            var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                      TONMILES = TOTAL_SHIPMT_TON_MILES,
                                      TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                   keyby = .(SCTG, SCTG_GROUP,
                                             SCTG_GROUP_LABEL)]

# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG", 
                                                                  "SCTG_GROUP", 
                                                                  "SHIPSIZE_GROUP"),
                                            var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                      TONMILES = TOTAL_SHIPMT_TON_MILES,
                                      TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                   keyby = .(SCTG, SCTG_GROUP,
                                             SCTG_GROUP_LABEL)]

# By SCTG group
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_dom_sctg_group_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                           TONS=TOTAL_SHIPMT_WGHT_TON,
                                                           TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                        keyby=.(SCTG_GROUP,
                                                                SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_exp_sctg_group_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                           TONS=TOTAL_SHIPMT_WGHT_TON,
                                                           TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                        keyby=.(SCTG_GROUP,
                                                                SCTG_GROUP_LABEL)]

# target_ls[["cfs_sctg_group_shipsize_w_tonshare_dt"]] <- dcast(tab_dt[,.(SHIPSIZE_GROUP,
#                                                                         TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                                         TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                                      keyby=.(SCTG_GROUP,
#                                                                              SCTG_GROUP_LABEL)],
#                                                         SCTG_GROUP+SCTG_GROUP_LABEL~SHIPSIZE_GROUP,
#                                                         value.var = c("TONS", "TONSHARE"))

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_group_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                               TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                               TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                            keyby = .(SCTG_GROUP,
                                                                      SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_group_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                               TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                               TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                            keyby = .(SCTG_GROUP,
                                                                      SCTG_GROUP_LABEL)]

# By Model Segments
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("MODEL_SEGMENT",
                                             "INDUSTRY_GROUP",
                                             "SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_dom_model_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                           TONS=TOTAL_SHIPMT_WGHT_TON,
                                                           TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                        keyby=.(MODEL_SEGMENT, 
                                                                INDUSTRY_GROUP,
                                                                SCTG_GROUP,
                                                                SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("MODEL_SEGMENT",
                                             "INDUSTRY_GROUP",
                                             "SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_WGHT_TON")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]


target_ls[["cfs_exp_model_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                           TONS=TOTAL_SHIPMT_WGHT_TON,
                                                           TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                                        keyby=.(MODEL_SEGMENT, 
                                                                INDUSTRY_GROUP,
                                                                SCTG_GROUP,
                                                                SCTG_GROUP_LABEL)]

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("MODEL_SEGMENT", 
                                             "INDUSTRY_GROUP",
                                             "SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_dom_model_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                               TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                               TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                            keyby = .(MODEL_SEGMENT, 
                                                                      INDUSTRY_GROUP,
                                                                      SCTG_GROUP,
                                                                      SCTG_GROUP_LABEL)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("MODEL_SEGMENT", 
                                             "INDUSTRY_GROUP",
                                             "SCTG_GROUP", 
                                             "SHIPSIZE_GROUP"),
                       var_name = "SHIPMT_TON_MILES")
tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]

target_ls[["cfs_exp_model_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                                               TONMILES = TOTAL_SHIPMT_TON_MILES,
                                                               TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                                            keyby = .(MODEL_SEGMENT, 
                                                                      INDUSTRY_GROUP,
                                                                      SCTG_GROUP,
                                                                      SCTG_GROUP_LABEL)]


# # By Non-model segments
# # By commodity group
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG",
#                                              "SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_dom_nonmodel_sctg_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                            TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                            TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                         keyby=.(SCTG, SCTG_GROUP,
#                                                                 SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG",
#                                              "SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_exp_nonmodel_sctg_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                            TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                            TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                         keyby=.(SCTG, SCTG_GROUP,
#                                                                 SCTG_GROUP_LABEL)]
# 
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG", 
#                                              "SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_dom_nonmodel_sctg_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                                TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                                TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                             keyby = .(SCTG, SCTG_GROUP,
#                                                                       SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG", 
#                                              "SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_exp_nonmodel_sctg_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                                TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                                TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                             keyby = .(SCTG, SCTG_GROUP,
#                                                                       SCTG_GROUP_LABEL)]
# 
# # By SCTG group
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_dom_nonmodel_sctg_group_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                                  TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                                  TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                               keyby=.(SCTG_GROUP,
#                                                                       SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_WGHT_TON")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# 
# target_ls[["cfs_exp_nonmodel_sctg_group_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                                  TONS=TOTAL_SHIPMT_WGHT_TON,
#                                                                  TONSHARE=prop.table(TOTAL_SHIPMT_WGHT_TON)),
#                                                               keyby=.(SCTG_GROUP,
#                                                                       SCTG_GROUP_LABEL)]
# 
# # Domestic
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="N"], group_var = c("SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_dom_nonmodel_sctg_group_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                                      TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                                      TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                                   keyby = .(SCTG_GROUP,
#                                                                             SCTG_GROUP_LABEL)]
# # Export
# tab_dt <- CFSTotalCalc(cfs_dt[MODEL_SEGMENT==99][EXPORT_YN=="Y"], group_var = c("SCTG_GROUP", 
#                                              "SHIPSIZE_GROUP"),
#                        var_name = "SHIPMT_TON_MILES")
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# 
# target_ls[["cfs_exp_nonmodel_sctg_group_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
#                                                                      TONMILES = TOTAL_SHIPMT_TON_MILES,
#                                                                      TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
#                                                                   keyby = .(SCTG_GROUP,
#                                                                             SCTG_GROUP_LABEL)]

# by distance band
# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                                   "SHIPSIZE_GROUP"),
                                             var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_dom_dist_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                       TONS = TOTAL_SHIPMT_WGHT_TON,
                                       TONSHARE = prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                    keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                                   "SHIPSIZE_GROUP"),
                                             var_name = "SHIPMT_WGHT_TON")

target_ls[["cfs_exp_dist_shipsize_tonshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                       TONS = TOTAL_SHIPMT_WGHT_TON,
                                       TONSHARE = prop.table(TOTAL_SHIPMT_WGHT_TON)),
                                    keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]

# Domestic
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="N"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                                  "SHIPSIZE_GROUP"),
                                            var_name = "SHIPMT_TON_MILES")


target_ls[["cfs_dom_dist_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                      TONMILES = TOTAL_SHIPMT_TON_MILES,
                                      TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                   keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]
# Export
tab_dt <- CFSTotalCalc(cfs_dt[EXPORT_YN=="Y"], group_var = c("SHIPMT_DIST_GC_BIN",
                                                                  "SHIPSIZE_GROUP"),
                                            var_name = "SHIPMT_TON_MILES")


target_ls[["cfs_exp_dist_shipsize_tonmileshare_dt"]]  <- tab_dt[,.(SHIPSIZE_GROUP,
                                      TONMILES = TOTAL_SHIPMT_TON_MILES,
                                      TONMILESHARE = prop.table(TOTAL_SHIPMT_TON_MILES)),
                                   keyby = .(DIST_BAND = SHIPMT_DIST_GC_BIN)]

### FAF5 by distance bin
# add tabulations of FAF5 data by 100 mile distance bins
# distances are domestic miles only, so for import/export represent domestic portion only

faf5_ls$faf5_dom_mode_tt_dist_bin_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                         keyby = .(dms_mode, dms_mode_label, trade_type_label, miles_bin)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, dms_mode)]

faf5_ls$faf5_dom_mode_tt_dist_bin_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                             keyby = .(dms_mode, dms_mode_label, trade_type_label, miles_bin)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, dms_mode)]

faf5_ls$faf5_dom_mode_tt_sctg_dist_bin_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                              keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP, miles_bin)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, SCTG, dms_mode)]

faf5_ls$faf5_dom_mode_tt_sctg_dist_bin_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                                  keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP, miles_bin)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, SCTG, dms_mode)]

# summarize mode shares by the smaller set of 5 dist bins
faf5_dt[, DIST_BAND := DIST_LABELS[findInterval(miles, DIST_BINS)]]
faf5_dt[, DIST_BAND := factor(DIST_BAND, levels = DIST_LABELS)]

faf5_ls$faf5_dom_mode_tt_dist5_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                                  keyby = .(dms_mode, dms_mode_label, trade_type_label, DIST_BAND)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, DIST_BAND)]

faf5_ls$faf5_dom_mode_tt_dist5_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                                      keyby = .(dms_mode, dms_mode_label, trade_type_label, DIST_BAND)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, DIST_BAND)]

faf5_ls$faf5_dom_mode_tt_sctg_dist5_tons <- faf5_dt[,.(TONS = round(sum(TONS))), 
                                                       keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP, DIST_BAND)][, TONS_SHARE := round(TONS/sum(TONS),3), by = .(trade_type_label, SCTG, DIST_BAND)]

faf5_ls$faf5_dom_mode_tt_sctg_dist5_tonmiles <- faf5_dt[,.(TONMILES = round(sum(TONMILES))), 
                                                           keyby = .(dms_mode, dms_mode_label, trade_type_label, SCTG, SCTG_GROUP, DIST_BAND)][, TONMILES_SHARE := round(TONMILES/sum(TONMILES),3), by = .(trade_type_label, SCTG, DIST_BAND)]

dcast.data.table(faf5_ls$faf5_dom_mode_tt_dist5_tons[trade_type_label == "Domestic"],
                 dms_mode + dms_mode_label ~ DIST_BAND,
                 fun.aggregate = sum,
                 value.var = "TONS_SHARE")

# Outsourcing Share of Shippers
cfs_dt[, OutsourcedShipment:=ifelse(MODE==4,1,ifelse(MODE==5,0,NA))]
# Domestic
tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="N",.(Shipments=sum(WGT_FACTOR)), 
                       keyby=.(MODEL_SEGMENT,
                               INDUSTRY_GROUP,
                               SCTG_GROUP,
                               OutsourcedShipment)]

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(MODEL_SEGMENT, SCTG_GROUP)]

target_ls[["cfs_dom_model_sctg_group_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(MODEL_SEGMENT,
                                                                                             INDUSTRY_GROUP,
                                                                                             SCTG_GROUP,
                                                                                             SCTG_GROUP_LABEL,
                                            NUM_OUTSOURCE_SHIPMENTS=Shipments,
                                            OUTSOURCE_SHIPMENT_SHARE=SHIPMENTSHARE)]
# Export
tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="Y",.(Shipments=sum(WGT_FACTOR)), 
                       keyby=.(MODEL_SEGMENT,
                               INDUSTRY_GROUP,
                               SCTG_GROUP,
                               OutsourcedShipment)]

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(MODEL_SEGMENT, SCTG_GROUP)]

target_ls[["cfs_exp_model_sctg_group_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(MODEL_SEGMENT,
                                                                                  INDUSTRY_GROUP,
                                                                                  SCTG_GROUP,
                                                                                  SCTG_GROUP_LABEL,
                                                                                  NUM_OUTSOURCE_SHIPMENTS=Shipments,
                                                                                  OUTSOURCE_SHIPMENT_SHARE=SHIPMENTSHARE)]

# By SCTG Group
# Domestic
tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="N",.(Shipments=sum(WGT_FACTOR)), 
                 keyby=.(SCTG_GROUP,
                         OutsourcedShipment)]

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(SCTG_GROUP)]

target_ls[["cfs_dom_sctg_group_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(SCTG_GROUP,
                                                                                             SCTG_GROUP_LABEL,
                                                                                             NUM_OUTSOURCE_SHIPMENTS=Shipments,
                                                                                             OUTSOURCE_SHIPMENT_SHARE=SHIPMENTSHARE)]
# Export
tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="Y",.(Shipments=sum(WGT_FACTOR)), 
                 keyby=.(SCTG_GROUP,
                         OutsourcedShipment)]

tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(SCTG_GROUP)]

target_ls[["cfs_exp_sctg_group_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(SCTG_GROUP,
                                                                                  SCTG_GROUP_LABEL,
                                                                                  NUM_OUTSOURCE_SHIPMENTS=Shipments,
                                                                                  OUTSOURCE_SHIPMENT_SHARE=SHIPMENTSHARE)]


# # Outsourcing share for Shippers not modeled by model segment
# cfs_dt[is.na(INDUSTRY_GROUP), INDUSTRY_GROUP:=as.character(NAICS2)]
# # Domestic
# tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="N",
#                                 .(Shipments=sum(WGT_FACTOR)), 
#                                 keyby=.(SCTG,
#                                         SCTG_GROUP,
#                                         INDUSTRY_GROUP,
#                                         OutsourcedShipment)]
# 
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(SCTG, INDUSTRY_GROUP)]
# setkey(tab_dt, SCTG, SCTG_GROUP, INDUSTRY_GROUP, OutsourcedShipment)
# target_ls[["cfs_dom_nonmodel_sctg_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(SCTG,
#                                                                                       SCTG_GROUP,
#                                                                                       SCTG_GROUP_LABEL,
#                                                                                       INDUSTRY_GROUP,
#                                                                                       NUM_OUTSOURCE_SHIPMENTS=Shipments,
#                                                                                       OUTSOURCE_SHIPMENT_SHARE=(SHIPMENTSHARE))]
# # Export
# tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="Y",
#                                 .(Shipments=sum(WGT_FACTOR)), 
#                                 keyby=.(SCTG,
#                                         SCTG_GROUP,
#                                         INDUSTRY_GROUP,
#                                         OutsourcedShipment)]
# 
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(SCTG, INDUSTRY_GROUP)]
# setkey(tab_dt, SCTG, SCTG_GROUP, INDUSTRY_GROUP, OutsourcedShipment)
# target_ls[["cfs_exp_nonmodel_sctg_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(SCTG,
#                                                                                       SCTG_GROUP,
#                                                                                       SCTG_GROUP_LABEL,
#                                                                                       INDUSTRY_GROUP,
#                                                                                       NUM_OUTSOURCE_SHIPMENTS=Shipments,
#                                                                                       OUTSOURCE_SHIPMENT_SHARE=(SHIPMENTSHARE))]
# 
# # Domestic
# tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="N",
#                                 .(Shipments=sum(WGT_FACTOR)), 
#                                 keyby=.(SCTG_GROUP,
#                                         INDUSTRY_GROUP,
#                                         OutsourcedShipment)]
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(SCTG_GROUP,
#                                                      SCTG_GROUP_LABEL, INDUSTRY_GROUP)]
# setkey(tab_dt, SCTG_GROUP, INDUSTRY_GROUP, OutsourcedShipment)
# target_ls[["cfs_dom_nonmodel_sctg_group_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(SCTG_GROUP,
#                                                                                             SCTG_GROUP_LABEL,INDUSTRY_GROUP,
#                                                                                       NUM_OUTSOURCE_SHIPMENTS=Shipments,
#                                                                                       OUTSOURCE_SHIPMENT_SHARE=(SHIPMENTSHARE))]
# # Export
# tab_dt <- cfs_dt[!is.na(OutsourcedShipment) & EXPORT_YN=="Y",
#                                 .(Shipments=sum(WGT_FACTOR)), 
#                                 keyby=.(SCTG_GROUP,
#                                         INDUSTRY_GROUP,
#                                         OutsourcedShipment)]
# tab_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL:=i.SCTG_LABEL,on=.(SCTG_GROUP)]
# tab_dt[,SHIPMENTSHARE:=prop.table(Shipments),keyby=.(SCTG_GROUP,
#                                                      SCTG_GROUP_LABEL, INDUSTRY_GROUP)]
# setkey(tab_dt, SCTG_GROUP, INDUSTRY_GROUP, OutsourcedShipment)
# target_ls[["cfs_exp_nonmodel_sctg_group_outsourceshare_dt"]]  <- tab_dt[OutsourcedShipment==1,.(SCTG_GROUP,
#                                                                                             SCTG_GROUP_LABEL,INDUSTRY_GROUP,
#                                                                                       NUM_OUTSOURCE_SHIPMENTS=Shipments,
#                                                                                       OUTSOURCE_SHIPMENT_SHARE=(SHIPMENTSHARE))]


mode_ton_wide_ls <- lapply(target_ls, LongToWideDT, wide_var="MODE_GROUP", value_var=c("TONS", "TONSHARE"))
mode_tonmiles_wide_ls <- lapply(target_ls, LongToWideDT, wide_var="MODE_GROUP", value_var=c("TONMILES", "TONMILESHARE"))
shipsize_ton_wide_ls <- lapply(target_ls, LongToWideDT, wide_var="SHIPSIZE_GROUP", value_var=c("TONS", "TONSHARE"))
shipsize_tonmiles_wide_ls <- lapply(target_ls, LongToWideDT, wide_var="SHIPSIZE_GROUP", value_var=c("TONMILES", "TONMILESHARE"))
sctg_trades_wide_ls <- lapply(target_ls, LongToWideDT, wide_var="TON_GROUP", value_var=c("SHIPMENTS", "SHIPMENTSHARE"))

mode_ton_wide_ls <- mode_ton_wide_ls[unlist(lapply(mode_ton_wide_ls, function(x) !is.null(x)))]
mode_tonmiles_wide_ls <-  mode_tonmiles_wide_ls[unlist(lapply(mode_tonmiles_wide_ls, function(x) !is.null(x)))]
shipsize_ton_wide_ls <-  shipsize_ton_wide_ls[unlist(lapply(shipsize_ton_wide_ls, function(x) !is.null(x)))]
shipsize_tonmiles_wide_ls <-  shipsize_tonmiles_wide_ls[unlist(lapply(shipsize_tonmiles_wide_ls, function(x) !is.null(x)))]
sctg_trades_wide_ls <-  sctg_trades_wide_ls[unlist(lapply(sctg_trades_wide_ls, function(x) !is.null(x)))]

# names(mode_ton_wide_ls) <- gsub("(.*)ton(.*)", "\\1wton\\2", names(mode_ton_wide_ls))
# names(mode_tonmiles_wide_ls) <- gsub("(.*)ton(.*)", "\\1wton\\2", names(mode_tonmiles_wide_ls))
# names(shipsize_ton_wide_ls) <- gsub("(.*)ton(.*)", "\\1wton\\2", names(shipsize_ton_wide_ls))
# names(shipsize_tonmiles_wide_ls) <- gsub("(.*)ton(.*)", "\\1wton\\2", names(shipsize_tonmiles_wide_ls))

# target_ls <- append(target_ls, mode_ton_wide_ls)
# target_ls <- append(target_ls, mode_tonmiles_wide_ls)
# target_ls <- append(target_ls, shipsize_ton_wide_ls)
# target_ls <- append(target_ls, shipsize_tonmiles_wide_ls)

excel_ls <- copy(target_ls)
excel_ls[names(mode_ton_wide_ls)] <- mode_ton_wide_ls
excel_ls[names(mode_tonmiles_wide_ls)] <- mode_tonmiles_wide_ls
excel_ls[names(shipsize_ton_wide_ls)] <- shipsize_ton_wide_ls
excel_ls[names(shipsize_tonmiles_wide_ls)] <- shipsize_tonmiles_wide_ls
excel_ls[names(sctg_trades_wide_ls)] <- sctg_trades_wide_ls

# Reorder target and excel list
target_ls <- lapply(target_ls, function(dt_){
  if("SCTG" %in% names(dt_)){
    if("MODE_GROUP" %in% names(dt_)){
      return(dt_[OrderSCTG(SCTG, first = TRUE, MODE_GROUP)])
    } else {
      return(dt_[OrderSCTG(SCTG)])
    }
  } else {
    return(dt_)
  }
})
excel_ls <- lapply(excel_ls, function(dt_){
  if("SCTG" %in% names(dt_)){
    if("MODE_GROUP" %in% names(dt_)){
      return(dt_[OrderSCTG(SCTG, first = TRUE, MODE_GROUP)])
    } else {
      return(dt_[OrderSCTG(SCTG)])
    }
  } else {
    return(dt_)
  }
})

excel_ls <- lapply(excel_ls, function(dt_) list(table=dt_, description=desc_dt[.(names(dt_))]))
# target_ls <- lapply(target_ls, function(dt_) list(table=dt_, description=desc_dt[.(names(dt_))]))
saveRDS(target_ls, file = target_file)
saveRDS(excel_ls, file=excel_file)

saveRDS(faf5_ls, file = target_faf5_file)

# SCTG group labels
faf5_dt[SCTG_GROUP_LABELS, SCTG_GROUP_LABEL := i.SCTG_LABEL, on = "SCTG_GROUP"]

# Export some summaries for the documentation
# Domestic Flows by Mode from FAF
fwrite(faf5_dt[trade_type_label == "Domestic",
        .(TONS = sum(TONS), TONMILES = sum(TONMILES)), 
        keyby = .(dms_mode, dms_mode_label)],
       file = file.path(proj_dir, output_dir, "summaries", "faf5_domestic_tons_tonmiles_mode.csv"))

#Flows by trade type by SCTG Group
fwrite(dcast.data.table(faf5_dt[,
               .(TONS = sum(TONS), TONMILES = sum(TONMILES)), 
               keyby = .(trade_type_label, SCTG_GROUP, SCTG_GROUP_LABEL)],
               SCTG_GROUP + SCTG_GROUP_LABEL ~ trade_type_label,
               fun.aggregate = sum,
               value.var = "TONS"),
       file = file.path(proj_dir, output_dir, "summaries", "faf5_tons_trade_type.csv"))

# Tabulation of modes by trade type, tons and tons miles
fwrite(dcast.data.table(faf5_dt[,
                         .(TONS = sum(TONS), TONMILES = sum(TONMILES)), 
                         keyby = .(trade_type_label, dms_mode, dms_mode_label)],
                 dms_mode + dms_mode_label ~ trade_type_label,
                 fun.aggregate = sum,
                 value.var = c("TONS", "TONMILES")),
       file = file.path(proj_dir, output_dir, "summaries", "faf5_tons_tonmiles_mode_trade_type.csv"))


# Save the faf 5 table with all labeling etc
saveRDS(faf5_dt, file = faf5_dt_processed)
