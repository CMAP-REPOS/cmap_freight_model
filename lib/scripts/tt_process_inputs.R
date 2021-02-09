
# This function loads all necessary inputs
tt_process_inputs <- function(envir){
  
  ### Load project input files
  
  ### Load inputs/outputs from earlier steps
  load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FTTM_OUTPUTNAME))
  
  return(ft_trips)
  
}