
# Simulate the truck touring behavior for freight shipments
ft_sim <- function(AnnualShipments) {
  
  # Begin progress tracking
  progressStart(action = "Simulating...", task = "Freight Truck Movements", dir = SCENARIO_LOG_PATH, subtasks = FALSE)
  
  # Extract a single day of shipments -- in region trips and international thru
  shipments <- ft_sim_daily_sample(pairs = AnnualShipments)
  progressUpdate(prop = 1/7, dir = SCENARIO_LOG_PATH)
  
  # Allocate warehouses
  shipments <- ft_sim_warehouse(shipments = shipments)
  progressUpdate(prop = 2/7, dir = SCENARIO_LOG_PATH)
  
  # Simulate vehicle and tour patter choice
  shipments <- ft_sim_vehicle_choice(shipments = shipments)
  progressUpdate(prop = 3/7, dir = SCENARIO_LOG_PATH)
  
  # Simulate stop sequence
  shipments <- ft_sim_stop_sequence(shipments = shipments)
  progressUpdate(prop = 4/7, dir = SCENARIO_LOG_PATH)
  
  # Simulate stop durations
  shipments <- ft_sim_stop_duration(shipments = shipments)
  progressUpdate(prop = 5/7, dir = SCENARIO_LOG_PATH)
  
  # Simulate tour time of day
  shipments <- ft_sim_time_of_day(shipments = shipments)
  progressUpdate(prop = 6/7, dir = SCENARIO_LOG_PATH)
  
  # Create Summary
  shipments <- ft_sim_summary_render(shipments = shipments)
  progressUpdate(prop = 0.99, dir = SCENARIO_LOG_PATH)
  
  # End progress tracking
  progressEnd(dir = SCENARIO_LOG_PATH)
  
  return(shipments)
  
}
