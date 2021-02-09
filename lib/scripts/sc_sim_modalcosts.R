
#Calculates modal costs and times for the shipment mode and transfer choice model
sc_sim_modalcosts <- function(skims, ModeChoiceParameters, mode_description, mode_availability, sctg) {
  
  # Update progress log
  progressUpdate(subtaskprogress = 0, subtask = "Calculate Modal Costs", prop = 1/6, dir = SCENARIO_LOG_PATH)
  
  ### Convert parameters from to individual objects for use in formulas
  
  for(i in 1:nrow(ModeChoiceParameters)) assign(ModeChoiceParameters$Variable[i], value = ModeChoiceParameters$Value[i])
  
  # Update progress log
  progressUpdate(subtaskprogress = 0.20, subtask = "Calculate Modal Costs", prop = 1/6, dir = SCENARIO_LOG_PATH)
  
  ### Process skims into format used in mode choice model and write to file for faster loading on seperate processes
  
  setnames(skims, c("Origin", "Destination"), c("Production_zone", "Consumption_zone"))
  
  if(!identical(nModes <- sum(grepl("time",colnames(skims))),
                nCost <- sum(grepl("cost",colnames(skims))))) {
    stop(paste0("Information of time and cost available on ", 
                min(nCost,nModes),
                "/",
                max(nCost,nModes)))
  }
    
  cskims <- skims[, c("Production_zone", "Consumption_zone", 
                      paste0("time", 1:nModes), 
                      paste0("cost", 1:nModes)), 
                  with = FALSE]
  
  numrows <- nrow(cskims)
  
  longcskims <- cbind(melt.data.table(cskims,
                                      id.vars = c("Production_zone","Consumption_zone"), 
                                      measure.vars = paste0("time",1:nModes), 
                                      variable.name = "timepath", 
                                      value.name = "time"),
                      melt.data.table(cskims[,paste0("cost",1:nModes), with=FALSE],
                                      measure.vars = paste0("cost",1:nModes), 
                                      variable.name = "costpath", 
                                      value.name = "cost"))
  
  longcskims[, path := as.integer(rep(1:nModes, each = numrows))]
  longcskims[,c("timepath","costpath"):=NULL]
  longcskims <- longcskims[!(is.na(time) & is.na(cost))]
  
  longcskims[mode_description[, .(path = ModeNumber, Mode.Domestic = LinehaulMode)],
             Mode.Domestic := i.Mode.Domestic, 
             on = "path"]
  
  setkey(longcskims, Production_zone, Consumption_zone)
  
  fwrite(longcskims, file.path(SCENARIO_OUTPUT_PATH, "ShipmentRoutesCosts.csv"))
  
  # Update progress log
  progressUpdate(subtaskprogress = 0.40, subtask = "Calculate Modal Costs", prop = 1/6, dir = SCENARIO_LOG_PATH)
  
  ### Prepare SCTG specific input file
  
  setkey(sctg,Commodity_SCTG)
  
  # Assign values for B2,B3,B5,a, and sdQ paramaters in logistics cost equation
  
  #Assign values for B2,B3,B5,a, and sdQ paramaters in logistics cost equation
  sctg[, c("B2","B3","B5") := 
                    c(ModeChoiceParameters[Variable == "LowDiscRate", Value],
                      ModeChoiceParameters[Variable == "MedDiscRate", Value],
                      ModeChoiceParameters[Variable == "MedDiscRate", Value],
                      ModeChoiceParameters[Variable == "MedDiscRate", Value],
                      ModeChoiceParameters[Variable == "HighDiscRate", Value])
                  [match(Category, c("Bulk natural resource (BNR)", 
                                     "Animals", 
                                     "Intermediate processed goods (IPG)", 
                                     "Other", 
                                     "Finished goods (FG)"))]]
  
  sctg[, a := c(ModeChoiceParameters[Variable == "LowMultiplier", Value],
                ModeChoiceParameters[Variable == "MediumMultiplier", Value],
                ModeChoiceParameters[Variable == "HighMultiplier", Value])
                  [match(Category2,c("Functional",
                                     "Functional/Innovative",
                                     "Innovative"))]]
  
  sctg[, sdQ := c(ModeChoiceParameters[Variable == "LowVariability", Value],
                  ModeChoiceParameters[Variable == "MediumVariability", Value],
                  ModeChoiceParameters[Variable=="HighVariability",Value])
                  [match(Category2,c("Functional",
                                     "Functional/Innovative",
                                     "Innovative"))]]
  
  # For each of the possible paths, define:
  # B0 (logistics cost equation constant) 
  # ls (log savings) value
  
  # These vary depending on the commodity and mode; 
  # extremely high values of B0 are set to indicate a non-available mode-path
  sctg[, paste0("B0", 1:nModes) := 10000]
  sctg[, paste0("ls", 1:nModes) := 1]
  
  # Category='Bulk natural resource (BNR)'
  sctg[Category == "Bulk natural resource (BNR)", paste0("B0",1:2) := 0]
  sctg[Category == "Bulk natural resource (BNR)", paste0("ls",1:2) := 0.5]
  sctg[Category == "Bulk natural resource (BNR)", paste0("B0",3:12) := 
         sctg[Category == "Bulk natural resource (BNR)", paste0("B0",3:12), with = FALSE] * 0.1]
  sctg[Category == "Bulk natural resource (BNR)", paste0("B0",13:45) := 
         sctg[Category == "Bulk natural resource (BNR)", paste0("B0",13:45), with = FALSE] * 10.0]
  sctg[Category == "Bulk natural resource (BNR)",ls3 := 0.5]
  
  ### Set the pipeline (55,56,57) for 16:19 SCTG
  sctg[Commodity_SCTG %in% c(16:19), paste0("B0",55:57) := 0]
  sctg[Commodity_SCTG %in% c(16:19), paste0("ls",55:57) := 0.5]
  
  # Category='Animals'
  sctg[Category == "Animals", paste0("B0",3:12) := 
         sctg[Category == "Animals", paste0("B0",3:12), with = FALSE] * 0.75]
  sctg[Category == "Animals", paste0("B0",c(1:2,13:30)) := 
         sctg[Category == "Animals", paste0("B0",c(1:2,13:30)), with = FALSE] * 10.0]
  sctg[Category == "Animals",B031 := B031 * 0.25]
  
  # Category='Intermediate processed goods (IPG)'
  sctg[Category == "Intermediate processed goods (IPG)" | Category == "Other", paste0("B0",1:12) := 
         sctg[Category == "Intermediate processed goods (IPG)" | Category == "Other", paste0("B0",1:12), with = FALSE] * 2.0]
  sctg[Category == "Finished goods (FG)", paste0("B0",1:12) := 
         sctg[Category == "Finished goods (FG)", paste0("B0",1:12), with = FALSE] * 10.0]
  sctg[Category == "Intermediate processed goods (IPG)" | Category == "Other", paste0("B0",14:30) := 
         sctg[Category == "Intermediate processed goods (IPG)" | Category == "Other",paste0("B0",14:30), with = FALSE] * 0.9]
  sctg[Category == "Intermediate processed goods (IPG)" | Category == "Other",paste0("B0",31:46) := 
         sctg[Category == "Intermediate processed goods (IPG)" | Category == "Other",paste0("B0",31:46), with = FALSE] * 0.5]

  # Category='Finished goods (FG)'
  
  sctg[Category == "Finished goods (FG)", paste0("B0",31:46) := 
         sctg[Category == "Finished goods (FG)", paste0("B0",31:46), with = FALSE] * 0.9]
  sctg[Category == "Finished goods (FG)", paste0("B0",c(32:45,47:50)) := 
         sctg[Category == "Finished goods (FG)", paste0("B0",c(32:45,47:50)), with = FALSE] * 0.25]

  # Update progress log
  progressUpdate(subtaskprogress = 0.60, subtask = "Calculate Modal Costs", prop = 1/6, dir = SCENARIO_LOG_PATH)
  
  ### Update the mode descriptions
  
  c_path_mode <- mode_description[,.(path = ModeNumber, Mode.Domestic = Mode)]
  c_path_mode[Mode.Domestic == "Multiple", Mode.Domestic := "Rail"]
  
  # Update progress log
  progressUpdate(subtaskprogress = 0.80, subtask = "Calculate Modal Costs", prop = 1/6, dir = SCENARIO_LOG_PATH)
  
  ### Update the mode availability table
  
  mode_availability <- melt.data.table(mode_availability,
                                       id.vars = c("ODSegment","Commodity_SCTG","Commodity_SCTG_desc_short"),
                                       variable.factor = FALSE,
                                       value.name = "avail")[,c("path","LinehaulMode") := tstrsplit(variable,"_")][,variable := NULL][,path := as.numeric(path)]
  
  setkey(mode_availability, ODSegment, Commodity_SCTG, path)
  
  # Update progress log
  progressUpdate(subtaskprogress = 1, subtask = "Calculate Modal Costs", prop = 1/6, dir = SCENARIO_LOG_PATH)
  
  # Return list of processed files
  return(list(ShipmentRoutesCosts = longcskims,
              c_path_mode = c_path_mode,
              mode_availability = mode_availability,
              sctg = sctg,
              ModeChoiceParameters = ModeChoiceParameters))
}
