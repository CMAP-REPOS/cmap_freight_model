# packages for model and report dashboard
SYSTEM_PKGS <- c("data.table", "rFreight", "rhdf5", "pscl", "TSP", "apollo")

SYSTEM_REPORT_PKGS <- c("DT", "flexdashboard", "leaflet", "plotly", "scales", "rgdal", "rgeos",
                        "pander", "geojsonlint", "stringr", "kableExtra", "openxlsx")

SYSTEM_DEV_PKGS <- c("sf", "lwgeom", "ggrepel", "dplyr",
                     "ggspatial", "bookdown", "leaps")

# combine lists so all install if needed on call to initializeApp
# leave the list of packages used in development seperate, not required for application
SYSTEM_PKGS <- c(SYSTEM_PKGS, SYSTEM_REPORT_PKGS[!SYSTEM_REPORT_PKGS %in% SYSTEM_PKGS])

SYSTEM_APP_PATH       <- getwd()
SYSTEM_RFREIGHT_PATH  <- file.path(SYSTEM_APP_PATH, "lib", "rFreight_0.1-35.zip")
SYSTEM_PKGS_PATH      <- file.path(SYSTEM_APP_PATH, "lib", "pkgs", "library")

# add the library folder to the library search paths -- required for proper install and loading, 
# especially with future
.libPaths(c(SYSTEM_PKGS_PATH, .libPaths())) 
Sys.setenv(R_LIBS = paste(SYSTEM_PKGS_PATH, Sys.getenv("R_LIBS"), sep=.Platform$path.sep))

SYSTEM_PANDOC_PATH      <- file.path(SYSTEM_APP_PATH, "lib", "pkgs", "Pandoc")
SYSTEM_DATA_PATH        <- file.path(SYSTEM_APP_PATH, "lib", "data")
SYSTEM_TEMPLATES_PATH   <- file.path(SYSTEM_APP_PATH, "lib", "templates")
SYSTEM_SCRIPTS_PATH     <- file.path(SYSTEM_APP_PATH, "lib", "scripts")
SYSTEM_DEV_PATH         <- file.path(SYSTEM_APP_PATH, "dev")
SYSTEM_CALIBRATION_PATH <- file.path(SYSTEM_APP_PATH, "dev", "Calibration", "outputs")
SYSTEM_DOCS_PATH        <- file.path(SYSTEM_APP_PATH, "docs")
SYSTEM_PMG_PATH         <- file.path(SYSTEM_APP_PATH, "lib", "PMG")

SYSTEM_FIRMSYN_OUTPUTNAME <- "1.Firms.RData"
SYSTEM_SCM_OUTPUTNAME     <- "2.AnnualShipments.RData"
SYSTEM_FTTM_OUTPUTNAME    <- "3.FreightTruckTrips.RData"
SYSTEM_TT_OUTPUTNAME      <- "4.TripTables.RData"
SYSTEM_DB_OUTPUTNAME      <- "5.DashboardTables.RData"
