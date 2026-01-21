# CMAP Freight Model
# dev script: data_unitcost.R
#
# Purpose:
# Create updated unit cost input from FAF data
#
# Outputs:
# data_unitcost_timeseries_2022_2060.csv
# 
# Called as function by cmap_scenarios_build_from_forecasts.R

data_unitcost <- function(macro_inputs_path){

  ### READ INPUT FILES ==================================================
  
  # Read in FAF data (version 5.6.1, latest 2018-2023 database)
  # https://faf.ornl.gov/faf5/data/download_files/FAF5.6.1_2018-2023.zip
  faf <- fread(file.path(macro_inputs_path, "FAF5.6.1_2018-2023.csv"))
  
  # Read in FAF data (verion 5.6.1, latest forecast database)
  # https://faf.ornl.gov/faf5/data/download_files/FAF5.6.1_State.zip
  faffor <- fread(file.path(macro_inputs_path, "FAF5.6.1.csv"))

  ### PROCESS ==================================================
  
  # Summarize the forecast year data (2025-2050)
  # Check if the 2022 data are the same in the forecast file
  unitcost_future <- faffor[,.(Tons2022 = sum(tons_2022)*1e3, Value2022 = sum(value_2022)*1e6,
                         Tons2025 = sum(tons_2025)*1e3, Value2025 = sum(value_2025)*1e6,
                         Tons2030 = sum(tons_2030)*1e3, Value2030 = sum(value_2030)*1e6,
                         Tons2035 = sum(tons_2035)*1e3, Value2035 = sum(value_2035)*1e6,
                         Tons2040 = sum(tons_2040)*1e3, Value2040 = sum(value_2040)*1e6,
                         Tons2045 = sum(tons_2045)*1e3, Value2045 = sum(value_2045)*1e6,
                         Tons2050 = sum(tons_2050)*1e3, Value2050 = sum(value_2050)*1e6),
                      keyby = .(Commodity_SCTG = sctg2)]
  
  unitcost_future[, paste0("UnitCost", c(2022, 2025, 2030, 2035, 2040, 2045, 2050)) := 
                 .(Value2022/Tons2022, Value2025/Tons2025, Value2030/Tons2030, Value2035/Tons2035,
                   Value2040/Tons2040, Value2045/Tons2045, Value2050/Tons2050)] 
  
  # extrapolate the future years to include 2055 and 2060 based on the trend from 2045-2050
  unitcost_future[, UnitCost2055 := UnitCost2050 + (UnitCost2050 - UnitCost2045)]
  unitcost_future[, UnitCost2060 := UnitCost2050 + 2*(UnitCost2050 - UnitCost2045)]
  
  ### WRITE ==================================================
  
  # unit cost time series for year specific inputs
  fwrite(unitcost_future, 
         file.path(macro_inputs_path, "data_unitcost_timeseries_2022_2060.csv"))

  print(paste("Finished writing ", file.path(macro_inputs_path, "data_unitcost_timeseries_2022_2060.csv")))
  
}