##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       CMAP_Agent_Run.R calls the model code for the selected scenario
#Date:              December 12, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
############################################################################################## 

#1.Set the base directory (the directory in which the model resides)
basedir <- "E:/cmh/Meso_Freight_PMG_Base_Test_Setup"

#2. Set the scnario to run -- same as the folder name inside the scenarios directory
scenario <- "base"

#3. Run the model 
source(file.path(basedir,"scripts","0_Main.R"))