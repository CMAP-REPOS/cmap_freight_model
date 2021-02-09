##############################################################################################
# Title:             
# Project:           
# Description:       
# Date:              
# Author:            Resource Systems Group, Inc.
# Copyright:         Copyright 2016 RSG, Inc. - All rights reserved.
##############################################################################################

# 1. Specific scenario arguments from the command line:
#    - Scenario name (defaults to base)
#    - Scenario year (defaults to 2010)
#    - Run Firm Synthesis component (defaults to TRUE)
#    - Run Supply Chain  component  (defaults to TRUE)
#    - Run FTTM component           (defaults to TRUE)
#    - Run Trip Tables component    (defaults to TRUE)
#    - Run Dashboard component      (defaults to TRUE)
SYSTEM_COMMAND_ARGS <- commandArgs(TRUE)

# 2. Run the application
source(file.path("lib", "scripts", "Master.R"))