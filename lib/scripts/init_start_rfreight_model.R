# initialization script for rFreight applications

# Load global variables
source(file.path("lib", "scripts", "_SYSTEM_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_BASE_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_SCENARIO_VARIABLES.R"))
source(file.path(SYSTEM_SCRIPTS_PATH, "_USER_VARIABLES.R"))

# Install rFreight and any packages not available on CRAN
source(file.path(SYSTEM_SCRIPTS_PATH, "init_install_special_packages.R"))

# Load current rFreight installation
suppressWarnings(suppressMessages(library(rFreight,
                                          lib.loc = SYSTEM_PKGS_PATH)))

# Check for new rFreight version, load rFreight and other packages, create output folder
initializeApp(rFreight.path = SYSTEM_RFREIGHT_PATH,
              output.path = SCENARIO_OUTPUT_PATH,
              lib = SYSTEM_PKGS_PATH,
              packages = c(SYSTEM_PKGS, SYSTEM_REPORT_PKGS),
              reload.rFreight = FALSE)
