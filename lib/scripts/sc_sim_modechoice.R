sc_sim_modechoice <- function(naics_set, TAZGCD, ModeChoiceParameters, sctg, c_path_mode, mode_availability){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # Only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_MODE_CHOICE_CORES)
  
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_modechoice.txt")
  file.create(log_file_path)
 
  # Apply the mode choice model for each market
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups")), file = log_file_path, append = TRUE)
    
    # Start creating input files for pmg for each group in the naics
    for(g in 1:groups){
      
      taskName <- paste0( "Apply_Mode_Choice_market-", market, "-group-", g, "-of-", groups)
      write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
      
      startAsyncTask(
        taskName, # Name of the task running
        future({ # Start the future processor
          # Write message to the console
          msg <- write(print(paste0(Sys.time(), " Applying Mode Choice Model for: ", market, ",  Group: ", g)), file = log_file_path, append = TRUE)
          
          # Run apply_modechoice function
          output <- capture.output(apply_modechoice(market, g, TAZGCD, ModeChoiceParameters, 
                                                    sctg, c_path_mode, mode_availability))
          return(output) #no need to return anything to future task handler
        }),
        callback = function(asyncResults) {
          # asyncResults is: list(asyncTaskName,
          #                        taskResult,
          #                        startTime,
          #                        endTime,
          #                        elapsedTime,
          #                        caughtError,
          #                        caughtWarning)
          
          #check that cost files was create
          taskName <- asyncResults[["asyncTaskName"]]
          taskInfo <- data.table::data.table(namedCapture::str_match_named(taskName, "^.*market[-](?P<taskMarket>[^-]+)-group-(?P<taskGroup>[^-]+)-of-(?P<taskGroups>.*)$"))[1,]
          taskResult <- asyncResults[["taskResult"]]
          write(print(taskResult), file = log_file_path, append = TRUE)
          marketKey <- paste0("market-", taskInfo$taskMarket)
          groupoutputs <- marketInProcess[[marketKey]]
          if (is.null(groupoutputs)) {
            stop(
              paste0(
                "for taskInfo$taskMarket ",
                taskInfo$taskMarket,
                " marketInProcess[[taskInfo$taskMarket]] (groupoutputs) is NULL! "
              )
            )
          }
          
          groupKey <- paste0("group-", taskInfo$taskGroup)
          groupoutputs[[groupKey]] <- paste0(Sys.time(), ": Finished!")
          
          #don't understand why this is necessary but apparently have to re-store list
          marketInProcess[[marketKey]] <<- groupoutputs
          
          write(print(paste0(Sys.time(),": Finished ",taskName,
                             ", Elapsed time since submitted: ",
                             asyncResults[["elapsedTime"]],
                             " # of group results so far for this market=",
                             length(groupoutputs))),
                file = log_file_path,append = TRUE)
          
          if (length(groupoutputs) == taskInfo$taskGroups) {
            #delete market from tracked outputs
            marketInProcess[[marketKey]] <<- NULL
            write(print(paste0(Sys.time(),": Completed Processing Outputs of all ",
                               taskInfo$taskGroups," groups for market ",
                               taskInfo$taskMarket,". Remaining marketInProcess=",
                               paste0(collapse = ", ", names(marketInProcess)))),
                  file = log_file_path, append = TRUE)
          } #end if all groups in naic are finished
        },
        debug = FALSE
      ) #end call to startAsyncTask
      processRunningTasks(wait = FALSE, debug = TRUE, maximumTasksToResolve = 1)
    }
  } # Finished making inputs to the PMG

  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_modechoice there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketsInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
}

apply_modechoice <- function(market, g, TAZGCD, ModeChoiceParameters, sctg, c_path_mode, mode_availability){
  
  # Load the workspace for this market and group
  load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  # load the processed skims
  ShipmentRoutesCosts <- fread(file.path(SCENARIO_OUTPUT_PATH, "ShipmentRoutesCosts.csv"))
  
  # Apply the mode choice model
  print(paste(Sys.time(), "Applying mode choice model to", market, "group", g))
  
  # Add fields to pc 
  pc_ss <- copy(pc)
  pc_ss[prodcg, 
        c("Production_zone", "Commodity_SCTG", "Seller.Size") := 
          .(i.Production_zone, i.Commodity_SCTG, i.Seller.Size), 
        on = "SellerID"]
  
  pc_ss[conscg, 
        c("Consumption_zone", "ConVal", "Buyer.Size") := 
          .(i.Consumption_zone, i.ConVal, i.Buyer.Size), 
        on = "BuyerID"]
  
  # Add zone to zone distances
  pc_ss[TAZGCD[, .(Production_zone, Consumption_zone, GCD)], 
        Distance := i.GCD,
        on = c("Production_zone", "Consumption_zone")]
  
  pc_ss[, lssbd := 0]
  pc_ss[Seller.Size > 5 & Buyer.Size < 3 & Distance > 300, lssbd := 1]
  
  # Join with shipment sizes
  shipsize_SCTG <- shipsize[SCTG %in% unique(pc_ss$Commodity_SCTG),
                            .(Ship_size = 1:.N, weight = meanWeight, k = 1)]
  
  pc_ss <- merge(pc_ss[, k := 1],
                 shipsize_SCTG,
                 by = "k",
                 allow.cartesian = TRUE)[, k := NULL]
  
  # call the mode choice functions
  pc <- minLogisticsCost(pc_ss, ShipmentRoutesCosts, sctg)
  
  # Save the results
  save(pc, prodcg, conscg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  return(paste("Completed apply_modechoice for market:", market, ", group:", g))
}

# Functions to apply the mode choice model
minLogisticsCost <- function(BuyerSupplierPairs, ShipmentRoutesCosts, sctg, modeChoiceConstants=NULL, runmode = 0){
  
  pass <- 0			### counter for number of passes through function
  
  iSCTG <- unique(BuyerSupplierPairs$Commodity_SCTG)
    
  if(runmode==0)	{
      
      #Direct:
      DirectPairs <- BuyerSupplierPairs[(distchannel==1) & 
                                          Production_zone %in% BASE_TAZ_DOMESTIC & 
                                          Consumption_zone %in% BASE_TAZ_DOMESTIC]
      
      if(!is.null(modeChoiceConstants) & nrow(DirectPairs) > 0){
        DirectPairs <- minLogisticsCostSctgPaths(DirectPairs,
                                                 iSCTG,
                                                 c(3,13,31,46,55:57),
                                                 sctg,
                                                 ShipmentRoutesCosts, 
                                                 modeChoiceConstants = modeChoiceConstants)
      } else if(nrow(DirectPairs) > 0) {
        DirectPairs <- minLogisticsCostSctgPaths(DirectPairs,
                                                 iSCTG,
                                                 c(3,13,31,46,55:57),
                                                 sctg,
                                                 ShipmentRoutesCosts, 
                                                 modeChoiceConstants = NULL)
      } else {
        DirectPairs <- DirectPairs[BuyerID < 0]
      }
      
      if(nrow(DirectPairs)>0) {
        if(pass==0) {
          df_out <- copy(DirectPairs)			### make an actual copy, not just a reference to df2
          pass <- pass + 1
        } else {
          df_out <- rbind(df_out,DirectPairs)
        }
      }
      
      #Indirect and International:
      IndirectPairs <- BuyerSupplierPairs[(distchannel > 1) & 
                                            ((Production_zone %in% BASE_TAZ_INTERNATIONAL | 
                                                Consumption_zone %in% BASE_TAZ_INTERNATIONAL))]
      
      if(!is.null(modeChoiceConstants) & nrow(IndirectPairs) > 0){
        IndirectPairs <- minLogisticsCostSctgPaths(IndirectPairs,
                                                   iSCTG,
                                                   c(1:2,4:12,14:30,32:45,47:54,55:57),
                                                   sctg,
                                                   ShipmentRoutesCosts, 
                                                   modeChoiceConstants = modeChoiceConstants)
      } else if (nrow(IndirectPairs) > 0) {
        IndirectPairs <- minLogisticsCostSctgPaths(IndirectPairs,
                                                   iSCTG,
                                                   c(1:2,4:12,14:30,32:45,47:54,55:57),
                                                   sctg,
                                                   ShipmentRoutesCosts, 
                                                   modeChoiceConstants = NULL)
      } else {
        IndirectPairs <- IndirectPairs[BuyerID < 0]
      }
      
      if(nrow(IndirectPairs)>0) {
        if(pass==0) {
          df_out <- copy(IndirectPairs)
          pass <- pass + 1
        } else {
          df_out <- rbind(df_out,IndirectPairs)
        }
      }
      
    } else {
      
      ###### Runmode!=0 - use for shipments between domestic zone and domestic port --
      
      DomesticShipmentPairs <- copy(BuyerSupplierPairs)
      
      if(is.null(modeChoiceConstants) & nrow(DomesticShipmentPairs) > 0){
        DomesticShipmentPairs <- minLogisticsCostSctgPaths(DomesticShipmentPairs,
                                                           iSCTG,
                                                           c(1:2,4:12,14:30,32:45,55:57),
                                                           sctg,
                                                           ShipmentRoutesCosts, 
                                                           modeChoiceConstants = NULL)			## include inland water
      } else if(nrow(DomesticShipmentPairs) > 0) {
        DomesticShipmentPairs <- minLogisticsCostSctgPaths(DomesticShipmentPairs,
                                                           iSCTG,
                                                           c(1:2,4:12,14:30,32:45,55:57),
                                                           sctg,
                                                           ShipmentRoutesCosts, 
                                                           modeChoiceConstants = modeChoiceConstants)
      } else {
        DomesticShipmentPairs <- DomesticShipmentPairs[BuyerID < 0]
      }
      if(nrow(DomesticShipmentPairs)>0) {if(pass==0) {
        df_out <- copy(DomesticShipmentPairs)
        pass <- pass + 1
      } else {
        df_out <- rbind(df_out,DomesticShipmentPairs)
      }
    }
  }
  return(df_out)
}

minLogisticsCostSctgPaths <- function(BuyerSupplierPairs, iSCTG, paths, sctg,
                                      ShipmentRoutesCosts, modeChoiceConstants = NULL){
  
  s <- sctg[iSCTG]
  
  BuyerSupplierPairs <- merge(BuyerSupplierPairs,
                              ShipmentRoutesCosts[path %in% paths,.(Production_zone, Consumption_zone, 
                                                                    Mode.Domestic, path, time, cost)], 
                              by = c("Production_zone","Consumption_zone"), 
                              #all.x = TRUE, # don't add NA rows for missing skims
                              allow.cartesian = TRUE)
  
  CAP1Carload  <- ModeChoiceParameters[Variable == "CAP1Carload", Value]    #Capacity of 1 Full Railcar
  CAP1FTL      <- ModeChoiceParameters[Variable == "CAP1FTL", Value]		#Capacity of 1 Full Truckload
  CAP1Airplane <- ModeChoiceParameters[Variable == "CAP1Airplane", Value]	#Capacity of 1 Airplane Cargo Hold
  
  BuyerSupplierPairs[, avail := TRUE]
  
  BuyerSupplierPairs[path %in% 1:12 & weight < CAP1Carload, avail := FALSE] #Eliminate Water and Carload from choice set if Shipment Size < 1 Rail Carload
  
  BuyerSupplierPairs[path %in% c(14,19:26,31) & weight < CAP1FTL, avail := FALSE] #Eliminate FTL and FTL-IMX combinations from choice set if Shipment Size < 1 FTL
  
  BuyerSupplierPairs[path %in% c(32:38) & weight < CAP1FTL, avail := FALSE] #Eliminate FTL Linehaul with FTL external drayage choice set if Shipment Size < 1 FTL
  
  BuyerSupplierPairs[path %in% c(15:18,27:30,39:46) & weight > CAP1FTL, avail := FALSE] #Eliminate LTL and its permutations from choice set if Shipment Size > FTL
  
  BuyerSupplierPairs[path %in% 47:50 & weight > CAP1Airplane, avail := FALSE] #Eliminate Air from choice set if Shipment Size > Air Cargo Capacity
  
  BuyerSupplierPairs[path %in% 51:52 & weight < (0.75*CAP1FTL), avail := FALSE] #Eliminate Container-Direct from choice set if Shipment Size < 1 40' Container
  
  BuyerSupplierPairs[path %in% 53:54 & weight < CAP1FTL, avail := FALSE] #Eliminate International Transload-Direct from choice set if Shipment Size < 1 FTL
  
  BuyerSupplierPairs[!(Commodity_SCTG %in% 16:19) & path %in% 55:57, avail := FALSE] #Elimninate pipeline for non-liquid fuels commodities
  
  BuyerSupplierPairs[avail == TRUE, 
                     minc := calcLogisticsCost(.SD[,.(PurchaseAmountTons, weight, ConVal, lssbd, time, cost)], 
                                               s, 
                                               unique(path)), 
                     by = path] 
  
  BuyerSupplierPairs <- BuyerSupplierPairs[avail == TRUE & !is.na(minc)]		## limit to only viable choices before finding minimum path
  
  # Select the modes with minimum cost
  BuyerSupplierPairs <- BuyerSupplierPairs[BuyerSupplierPairs[,.I[which.min(minc)],.(SellerID, BuyerID, Mode.Domestic)][,V1]]
  
  # # Convert the logistics costs to a probability, adjust using constants
  # # iter <- 1 # just once through if in application
  # # Add the constant to the mode choice table
  # if(!is.null(modeChoiceConstants)){
  #   BuyerSupplierPairs[modeChoiceConstants,Constant:=i.Constant, on=c("Commodity_SCTG","ODSegment","path")]
  # } else {
  BuyerSupplierPairs[, Constant := 0]
  # }
  
  # Calculate Probability
  BuyerSupplierPairs[, Prob:= exp(-(minc/(10**floor(log10(max(minc))))) + Constant) / sum(exp(-(minc/(10**floor(log10(max(minc))))) + Constant)), by = .(BuyerID, SellerID)]
  BuyerSupplierPairs[is.nan(Prob), Prob := 1] ### TODO Should this be set to zero?
  BuyerSupplierPairs[is.na(Prob), Prob := 1] ### TODO Should this be set to zero?
  
  BuyerSupplierPairs[,MinCost:=0L]
  BuyerSupplierPairs[BuyerSupplierPairs[,.I[which.max(Prob)],by = .(SellerID, BuyerID)][,V1],MinCost:=1]
  BuyerSupplierPairs <- BuyerSupplierPairs[MinCost==1]
  
  return(BuyerSupplierPairs)
  
} #minLogisticsCostSctgPaths

calcLogisticsCost <- function(dfspi,s,path){
  set.seed(151)
  setnames(dfspi,c("pounds","weight","value","lssbd","time","cost"))
  
  # variables from ModeChoiceParameters
  B1   <- ModeChoiceParameters[Variable == "B1", Value]		#Constant unit per order
  B4   <- ModeChoiceParameters[Variable == "B4", Value]		#Storage costs per unit per year
  j    <- ModeChoiceParameters[Variable == "j", Value]		#Fraction of shipment that is lost/damaged
  sdLT <- ModeChoiceParameters[Variable == "sdLT", Value]	#Standard deviation in lead time
  LT_OrderTime <- ModeChoiceParameters[Variable == "LT_OrderTime", Value]	#Expected lead time for order fulfillment (to be added to in-transit time)
  
  #vars from S
  B2 <- s$B2
  B3 <- s$B3
  B5 <- s$B5
  a  <- s$a
  sdQ<- s$sdQ
  B0 <- s[[paste0("B0",path)]]
  ls <- s[[paste0("ls",path)]]
  
  #adjust factors for B0 and ls based on small buyer,
  if(s$Category=="Bulk natural resource (BNR)" & path %in% 5:12) B0 <- B0 - 0.25*B0*dfspi$lssbd
  if(s$Category=="Finished goods (FG)" & path %in% c(14:30,32:38)) ls <- ls - 0.25*ls*dfspi$lssbd
  if(s$Category=="Finished goods (FG)" & path %in% 39:45) ls <- ls - 0.5*ls*dfspi$lssbd
  
  # Calculate annual transport & logistics cost
  # NEW EQUATION
  dfspi[,minc:= B0 * runif(length(pounds)) + #changed scale of B0 -- too large for small shipment flows
          B1*pounds/weight +
          ls*(pounds/2000)*cost + #since cost is per ton
          B2*j*value +
          B3*time/24*value/365 +
          (B4 + B5*value/(pounds/2000))*weight/(2*2000) +
          (B5*(value/(pounds/2000))*a)*sqrt((LT_OrderTime+time/24)*((sdQ*pounds/2000)^2) + (pounds/2000)^2*(sdLT*LT_OrderTime)^2)]
  
  return(dfspi$minc)
  
}
