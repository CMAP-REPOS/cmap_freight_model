##############################################################################################
#Title:             CMAP Agent Based Freight Forecasting Code
#Project:           CMAP Agent-based economics extension to the meso-scale freight model
#Description:       0d_File_Locations.R contains fils paths for inputs, outputs, workspaces
#                   and log files
#Date:              January 28, 2014
#Author:            Resource Systems Group, Inc.
#Copyright:         Copyright 2014 RSG, Inc. - All rights reserved.
############################################################################################## 

#-----------------------------------------------------------------------------------
#Define file paths for inputs, outputs, workspaces and log files
#-----------------------------------------------------------------------------------
print("Defining file names for inputs, outputs, workspaces and log files")

#-----------------------------------------------------------------------------------
#Inputs
#-----------------------------------------------------------------------------------

##1 Firm Synthesis Inputs
firmsyn[["inputs"]] <- within(firmsyn[["inputs"]], {
  c_n6_n6io_sctg                  <-     "corresp_naics6_n6io_sctg.csv"              #Correspondence between NAICS 6-digit, I/O NAICS, and SCTG
  cbp                             <-     "data_emp_cbp.csv"              #CBP data file
  for_prod                        <-     "data_foreign_prod.csv"         #Foreign producers
  for_cons                        <-     "data_foreign_cons.csv"         #foreign consumers
  io                              <-     "data_2010io.csv"
  unitcost                        <-     "data_unitcost.csv"
  prefweights                     <-     "data_firm_pref_weights.csv"
  mzemp                           <-     "data_mesozone_emprankings.csv"      #Industry rankings data by mesozone based on employment
})
  
##2 Procurement Markets
pmg[["inputs"]] <- within(pmg[["inputs"]], {
  sctg                            <-     "corresp_sctg_category.csv"               #correspondence between SCTG and descriptions
  mesozone_gcd                    <-     "data_mesozone_gcd.csv"                   #Mesozone to mesozone gcds
  skims                           <-     "data_modepath_skims.csv"
  distchan_food                   <-     "model_distchannel_food.csv"              #Distribution channel MNL models
  distchan_mfg                    <-     "model_distchannel_mfg.csv"
  distchan_cal                    <-     "model_distchannel_calibration.csv"
  ShipSize_food                   <-     "model_shipsize_food.csv"                 #Shipment Size MNL models
  ShipSize_mfg                    <-     "model_shipsize_mfg.csv"                  #Shipment Size MNL models
  ShipSize_cal                    <-     "model_shipsize_calibration.csv" #Shipment Size Distribution
})

##7 Vehicle Choice and Tour Pattern Inputs
vehtour[["inputs"]] <- within(vehtour[["inputs"]], {
  vehtourpat                      <-     "model_vehicle_tourpattern.csv"           #Vehicle tour MNL model
  emp_cbpzone                     <-     "data_emp_cbpzone.csv"                    #Employment by CBP Zone
  mzemp                           <-     "data_mesozone_emprankings.csv"           #Industry rankings data by mesozone based on employment -- used here for correspodence between counties and mesozones
})

##8 Stop Sequence Inputs
stopseq[["inputs"]] <- within(stopseq[["inputs"]], {
  numberoftours                   <-     "model_numberoftours.csv"                 #Number of tours MNL model
  mz_centroids                    <-     "data_mesozone_centroids.csv"             #Centroid coordinates of mesozones
  mz_skims                        <-     "data_mesozone_skims.csv"                 #Mesozone to mesozone skims
})
  
##9 Stop Duration Inputs
stopdur[["inputs"]] <- within(stopdur[["inputs"]], {
  stopduration                    <-     "model_stopduration.csv"                  #Stop duration MNL model
})

##10 Time of Day Inputs
tourtod[["inputs"]] <- within(tourtod[["inputs"]], {
  tod                             <-     "model_timeofday.csv"                     #TOD MNL model
})

#-----------------------------------------------------------------------------------
#Summary Outputs
#-----------------------------------------------------------------------------------

##7 Vehicle Choice and Tour Pattern Outputs
vehtour[["summary"]] <- within(vehtour[["summary"]], {
  vehtourpatallcommodities                 <-   "vehtourpat_allcommodities.csv"
  vehtourpatfood                           <-   "vehtourpat_food.csv" 
  vehtourpatmfg                            <-   "vehtourpat_mfg.csv" 
})

##8 Stop Sequence Outputs
stopseq[["summary"]] <- within(stopseq[["summary"]], {
  stopseqtourcatbyshipsize                 <-   "stopseq_tourcatbyshipsize.csv"
  stopseqnumstopspertour                   <-   "stopseq_numstopspertour.csv"
})
##9 Stop Duration Outputs
stopdur[["summary"]] <- within(stopdur[["summary"]], {
  stopdurdurationbytourtype                <-   "stopdur_durationbytourtype.csv"
})
  
##10 Time of Day Outputs
tourtod[["summary"]] <- within(tourtod[["summary"]], {
  todbytourtype                            <-   "tod_todbytourtype.csv"
})

#-----------------------------------------------------------------------------------
#Workspaces
#-----------------------------------------------------------------------------------

model[["workspace"]]                        <-   "Model.RData"
firmsyn[["workspace"]]                      <-   "Step1_FirmSynthesis.RData"
pmg[["workspace"]]                          <-   "Step2_ProcurementMarkets.RData"
pmgcon[["workspace"]]                       <-   "Step3_PMGController.RData"
pmgout[["workspace"]]                       <-   "Step4_PMGOutputs.RData"
daysamp[["workspace"]]                      <-   "Step5_DailySample.RData"
whouse[["workspace"]]                       <-   "Step6_WarehouseAllocation.RData"
vehtour[["workspace"]]                      <-   "Step7_VehicleChoiceTourPattern.RData"
stopseq[["workspace"]]                      <-   "Step8_StopSequence.RData"
stopdur[["workspace"]]                      <-   "Step9_StopDuration.RData"
tourtod[["workspace"]]                      <-   "Step10_TimeofDay.RData"
preptt[["workspace"]]                       <-   "Step11_TripTable.RData"

#-----------------------------------------------------------------------------------
#Runtimes and Log File Locations
#-----------------------------------------------------------------------------------
model[["logs"]] <- list()
model[["logs"]] <- within(model[["logs"]], {
  Step_RunTimes                               <-   "RunTimes.csv"
  Main_Log                                    <-   "Main_Log.txt"
  Profile_Log                                 <-   "Profile.out"
  Profile_Summary                             <-   "Profile_Summary.txt"
})


