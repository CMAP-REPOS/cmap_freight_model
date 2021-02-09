
# This script installs packages for the model which are not readily available on
# CRAN. All packages from CRAN are installed directly by rFreight functions.

# # Set a CRAN mirror
# local({
#   repo <- getOption("repos")
#   repo["CRAN"] <- "https://cran.cnr.berkeley.edu/"
#   options(repos=repo)
# })

# check for data.table -- needs to be installed for rFreight to load
if(!("data,table" %in% installed.packages(lib.loc = SYSTEM_PKGS_PATH)[,1])){
  install.packages("data.table", lib = SYSTEM_PKGS_PATH) 
}

# check for reshape -- needs to be installed for rFreight to load
if(!("reshape" %in% installed.packages(lib.loc = SYSTEM_PKGS_PATH)[,1])){
  install.packages("reshape", lib = SYSTEM_PKGS_PATH) 
}

# Install rFreight if not currently installed
if(!("rFreight" %in% installed.packages(lib.loc = SYSTEM_PKGS_PATH)[,1])){
  rFreightZipName <- list.files(file.path(SYSTEM_APP_PATH, "lib"),
                                pattern = "rFreight")
  if(length(rFreightZipName)<1){
    stop(paste("rFreight package zip file not available at", 
               file.path(SYSTEM_APP_PATH, "lib")))
  } else if(length(rFreightZipName)>1) {
    stop(paste0("More than one rFreight package zip file available at ", 
                file.path(SYSTEM_APP_PATH, "lib"), " . ",
                "Please remove all but the single desired zip file."))
  } else {
    install.packages(file.path(SYSTEM_APP_PATH, "lib", rFreightZipName),
                     lib = file.path(SYSTEM_PKGS_PATH),
                     repos = NULL)
  }
  
}

# Install "rhdf5" and "Rhdf5lib" packages from Bioconductor repository, which
# rFreight cannot do
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}
if (!("rhdf5" %in% installed.packages(SYSTEM_PKGS_PATH))) {
  BiocManager::install("rhdf5", lib = SYSTEM_PKGS_PATH, ask = FALSE)
}
if (!("Rhdf5lib" %in% installed.packages(SYSTEM_PKGS_PATH))) {
  BiocManager::install("Rhdf5lib", lib = SYSTEM_PKGS_PATH, ask = FALSE)
}