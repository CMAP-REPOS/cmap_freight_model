sc_sim_sampling <- function(naics_set){
  
  # Loop over markets -- combinations of naics code and SCTG code
  # Prepare future processors
  # only allocate as many workers as we need (including one for future itself) or to the specified maximum
  plan(multiprocess, workers = USER_COST_CORES) 
  marketInProcess <- list()
  
  # Create a log file for this step
  log_file_path <- file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_sampling.txt")
  file.create(log_file_path)
  
  # Samplng the sets of sellers for each buyer
  for(market_number in 1:nrow(naics_set)){
    market <- as.character(naics_set$Market[market_number])
    groups <- naics_set$groups[market_number]
    
    # Create place to accumulate group results
    marketInProcess[[paste0("market-", market)]] <- list()
    write(print(paste0(Sys.time(), ": Starting Market: ", market, " with ", groups, " groups")), file = log_file_path, append = TRUE)
    
    # Start creating input files for pmg for each group in the naics
    for(g in 1:groups){
      
      taskName <- paste0( "Create_PC_Samples_market-", market, "-group-", g, "-of-", groups)
      write(print(paste0(Sys.time(), ": Submitting task '", taskName, "' to join ", getNumberOfRunningTasks(), " currently running tasks")), file = log_file_path, append = TRUE)
      
      startAsyncTask(
        taskName, # Name of the task running
        future({ # Start the future processor
          # Write message to the console
          msg <- write(print(paste0(Sys.time(), " Creating Sampled Producer and Consumer Combinations for: ", market, ",  Group: ", g)), file = log_file_path, append = TRUE)
          
          # Run create_prod_cons_combinations function
          output <- capture.output(create_prod_cons_combinations(market, g))
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
          } #end if all groups in market are finished
        },
        debug = FALSE
      ) #end call to startAsyncTask
      processRunningTasks(wait = FALSE, debug = TRUE, maximumTasksToResolve = 1)
    }
  } # Finished creating producer/consumer samples
  
  # Wait until all tasks are finished
  processRunningTasks(wait = TRUE, debug = TRUE)
  
  if (length(marketInProcess) != 0) {
    stop(paste(
      "At end of sc_sim_sampling there were still some unfinished markets! Unfinished: ", 
      paste0(collapse = ", ", names(marketsInProcess))))
  }
  
  # Stop the future processors
  future:::ClusterRegistry("stop")
  
  return(naics_set)
}

create_prod_cons_combinations <- function(market, g){
  
  # load the market
  load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  # Rename fields
  setnames(conscg, 
           c("InputCommodity", "NAICS", "Zone", "Size"), 
           c("NAICS", "Buyer.NAICS", "Consumption_zone", "Buyer.Size"))
  setnames(prodcg, 
           c("OutputCommodity", "NAICS", "Zone", "Size"),
           c("NAICS", "Seller.NAICS", "Production_zone", "Seller.Size"))
  
  # Key conscg
  setkey(conscg, FAFZONE)
  
  # seed for sampling
  set.seed(BASE_SEED_VALUE)
  # Set the initial ratioweights to 1. 
  # ratioweights attribute is used to track a metric of production to consumption ratio
  prodcg[, ratioweights := 1]
  
  # what proportion of total output capacity does each producer represent?
  prodcg[, outputprop := OutputCapacityTons/sum(OutputCapacityTons)]
  
  # what proportion of total purchases does each consmer represent?
  conscg[, purchprop := PurchaseAmountTons/sum(PurchaseAmountTons)]
  
  # production and consumption quantiles
  prodcg[, tonquant := findInterval(OutputCapacityTons,
                                    quantile(OutputCapacityTons, probs = seq(0, 1, 0.25), type = 5),
                                    rightmost.closed = TRUE)]
  
  conscg[, tonquant := findInterval(PurchaseAmountTons,
                                    quantile(PurchaseAmountTons, type = 5),
                                    rightmost.closed = TRUE)]
  
  # Assign a sample size (of sellers) to buyers
  if(USER_RUN_TEST != "Sensitivity"){
    # estimate a reasonable number of extra samples to allocate amongst larger consumers
    total_samples <- max(BASE_SUPPLIERS_PER_BUYER * 5 * prodcg[,.N], BASE_SUPPLIERS_PER_BUYER * conscg[,.N])
    conscg[, samplesize := round(total_samples * purchprop)]
    # update the sample size to be minimum BASE_SUPPLIERS_PER_BUYER/2, maximum is the number of producers
    min_samples <- ceiling(BASE_SUPPLIERS_PER_BUYER/2)
  } else {
    # this is a sensitivity test
    conscg[, samplesize := BASE_SUPPLIERS_PER_BUYER]
    min_samples <- ceiling(BASE_SUPPLIERS_PER_BUYER)
  }  
  conscg[, samplesize := ifelse(samplesize < min_samples, min_samples, samplesize)]
  conscg[, samplesize := ifelse(samplesize > prodcg[,.N], prodcg[,.N], samplesize)]
  
  # get the sampling proportions for this markets commodity
  market_sctg <- unique(prodcg$Commodity_SCTG)
  
  FAF_DISTANCE_SCTG <- FAF_DISTANCE[Supplier_Selection_Distribution[SCTG == market_sctg, .(Distance_Bin = DistanceGroup, Proportion)], 
                                    Proportion := i.Proportion, 
                                    on = "Distance_Bin"]
  FAF_DISTANCE_SCTG[is.na(Proportion), Proportion := 1e-6]
  
  # iterative takes samples until there are enough for a feasible solution in PMG
  isInfeasible <- TRUE
  buyeriter <- 0
  
  while(isInfeasible){
    
    # Take the sample
    pc_add <- sample_data_test(prodcg[,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights, outputprop)], 
                               conscg[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
                               FAF_DISTANCE_SCTG)
    if(buyeriter>0){
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("SellerID", "BuyerID"))
    } else {
      pc <- pc_add
    }
    # Checks on sample
    # this is margin of safety to help ensure enough excess capactiy relative to purchase requirements
    buffer <- 0.1
    
    # For buyers, which have sufficient capacity in the sellers that they are paired with to meet their requirements
    Buy.PurRatio <- pc[, .(NumSamples = .N, PurRatio = sum(OutputCapacityTons) * buffer/mean(PurchaseAmountTons)), by = BuyerID]
    # Buy.PurRatio[order(PurRatio)][PurRatio < 1]
    
    # If any buyers have a purchase ratio < 1, i.e. the total output capacity * buffer <  purchase amount 
    # add some records to the sample for those buyers
    if(Buy.PurRatio[PurRatio < 1,.N] > 0){
      conscg.pur <- conscg[BuyerID %in% Buy.PurRatio[PurRatio < 1]$BuyerID]
      pc_add <- sample_data_test(prodcg[,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights, outputprop)], 
                                 conscg.pur[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
                                 FAF_DISTANCE_SCTG) 
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("SellerID", "BuyerID"))
    }
    
    # Which sellers are allocated to buyer samples and how extended are they
    # this is not very meaningful, but in general Sellers (particularly larges ones) should be in many samples 
    Sell.CapRatio <- pc[, .(NumSamples = .N, CapRatio = mean(OutputCapacityTons)/(buffer * sum(PurchaseAmountTons))), by = SellerID]
    # Add some extra cases if there are missing sellers
    # this should generally work to allocate all sellers to at least some buyer sample but don't need to force it
    # not guarenteed in the PMG that all sellers will find a buyer anyway
    Sell.Missing <- prodcg[!SellerID %in% Sell.CapRatio$SellerID]
    
    if(Sell.Missing[,.N] > 0){
      
      # select a reasonable number of buyers to add these sellers too
      avbuyers <- ceiling(pc[,.N]/(prodcg[,.N] - Sell.Missing[,.N]))
      buyerstoadd <- min(avbuyers * Sell.Missing[,.N], conscg[,.N])
      conscg.samp <- conscg[sample.int(conscg[,.N], size = buyerstoadd, replace = FALSE, prob = conscg$purchprop)]
      conscg.samp[, samplesize := 1]
      
      pc_add <- sample_data_test(prodcg[SellerID %in% Sell.Missing$SellerID,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights, outputprop)], 
                                 conscg.samp[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
                                 FAF_DISTANCE_SCTG)
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("SellerID", "BuyerID"))
      
    }
    
    # recalculate Sell cap ratio after dealing with missing
    Sell.CapRatio <- pc[, .(NumSamples = .N, CapRatio = mean(OutputCapacityTons)/(buffer * sum(PurchaseAmountTons))), by = SellerID]
    
    # Check the solution
    system.time(solution <- SolveFlowCLP(pc[,.(SellerID, BuyerID, OutputCapacityTons, PurchaseAmountTons)], 
                                         prodcg, conscg, lp_problems = NULL))
    
    SellersToDo <- solution[[1]]
    BuyersToDo <- solution[[2]]
    lp_problems <- solution[[3]]
    solution <- solution[[4]]
    
    isInfeasible <- (solution > 0)
    totalPairs <- pc[,.N]
    buyeriter <- buyeriter + 1
    
    print(paste0("Is the solution still infeasible:", isInfeasible))
    print(paste0("Iteration:", buyeriter))
    print(paste0("Total number of buyer supplier pairs: ", pc[,.N]))
    
    if(isInfeasible){
      # update the sample size for the next iteration
      # as iterations grow, total sample size will grow quickly to avoid too many iterations
      conscg[, samplesize := samplesize + samplesize * buyeriter]
      # replace the ratio weights with the capaacity ratio to prioritize producers that have not been allocated very often
      # and have in theory a lot of extra capacity
      # give any remaining missing sellers the max ratio weight too
      MaxCapRatio <- max(Sell.CapRatio$CapRatio)
      MinCapRatio <- min(Sell.CapRatio$CapRatio)
      RangeCapRatio <- MaxCapRatio - MinCapRatio
      prodcg[Sell.CapRatio, CapRatio := i.CapRatio, on = "SellerID"]
      prodcg[, ratioweights := ifelse(!is.na(CapRatio), 
                                      CapRatio/RangeCapRatio * 100 + 1, 
                                      MaxCapRatio/RangeCapRatio * 100 + 1)]
    }
  }
  
  delProbCLP(lp_problems)
  rm(lp_problems)
  
  # Save pc, conscg and prodcg into a workspace
  save(pc, conscg, prodcg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  
  return(paste("Completed create_prod_cons_combinations for market:", market, ", group:", g))
  
}

# TODO Old version of function, remove when completed testing new version above
# create_prod_cons_combinations <- function(market, g){
#   
#   # load the market
#   load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
#   
#   # Rename fields ready to merge
#   setnames(conscg, 
#            c("InputCommodity", "NAICS", "Zone", "Size"), 
#            c("NAICS", "Buyer.NAICS", "Consumption_zone", "Buyer.Size"))
#   setnames(prodcg, 
#            c("OutputCommodity", "NAICS", "Zone", "Size"),
#            c("NAICS", "Seller.NAICS", "Production_zone", "Seller.Size"))
#   
#   # merge together to create all of the pairs of firms for this group and commodity
#   set.seed(BASE_SEED_VALUE)
#   # Set the initial ratioweights to 1. 
#   # ratioweights attribute is used to track a metric of production to consumption ratio
#   prodcg[, ratioweights := 1]
#   
#   prodcg[, tonquant := findInterval(OutputCapacityTons,
#                                    quantile(OutputCapacityTons, type = 5),
#                                    rightmost.closed = TRUE)]
#   
#   conscg[, tonquant := findInterval(PurchaseAmountTons,
#                                    quantile(PurchaseAmountTons, type = 5),
#                                    rightmost.closed = TRUE)]
#   
#   # Assign a sample size (of sellers) to buyers
#   conscg[, samplesize := seq(5, BASE_SUPPLIERS_PER_BUYER, length.out = 4)[tonquant]]
#   
#   print("1. First time sampling")
#   
#   system.time(pc <- sample_data(prodcg[,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights)], 
#                     conscg[,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)]))
#   
#   pc[prodcg, OutputCapacityTons := i.OutputCapacityTons, on = "SellerID"]
#   pc[conscg, PurchaseAmountTons := i.PurchaseAmountTons, on = "BuyerID"]
#   
#   solution <- SolveFlowCLP(pc[,.(SellerID, BuyerID, OutputCapacityTons, PurchaseAmountTons)], 
#                            prodcg, conscg, lp_problems = NULL)
#   
#   SellersToDo <- solution[[1]]
#   BuyersToDo <- solution[[2]]
#   lp_problems <- solution[[3]]
#   solution <- solution[[4]]
#   
#   isInfeasible <- (solution > 0)
#   
#   if(isInfeasible){
#     
#     print("Solution for first sample is Infeasible. Adding extra samples until feasible solution found.")
#     buyeriter <- 1
#     totalPairs <- pc[,.N]
#     
#     while(isInfeasible){
#       
#       MarketToDo <- TRUE
#       
#       prodratio <- pc[,.(dfTons = sum(PurchaseAmountTons)),
#                       .(SellerID,OutputCapacityTons)][,.(SellerID, Ratio=dfTons/OutputCapacityTons)]
#       
#       # Update the ratioweights
#       prodcg[prodratio, ratioweights := 1/i.Ratio, on = "SellerID"]
#       prodcg[!prodratio, ratioweights := 100, on = "SellerID"]
#       # increase the sample size step each iteration
#       conscg[, samplesize := samplesize + samplesize * buyeriter]  
#       
#       # First do the sampling for selected sellers
#       prodcg[, availableForSample := TRUE]
#       prodcg[!SellersToDo, availableForSample := FALSE, on = "SellerID"]
#       conscg[, doSample := TRUE]
#       
#       system.time(new_pc <- sample_data(prodcg[availableForSample == TRUE,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights)], 
#                             conscg[doSample == TRUE,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
#                             old_pc = pc,
#                             samplingforSeller = TRUE))
#       
#       new_pc[prodcg, OutputCapacityTons := i.OutputCapacityTons, on = "SellerID"]
#       new_pc[conscg, PurchaseAmountTons := i.PurchaseAmountTons, on = "BuyerID"]
#   
#       pc <- rbindlist(list(pc, new_pc[!pc, on = c("SellerID", "BuyerID")]))
#       
#       
#       if(new_pc[,.N] > 0) MarketToDo <- FALSE
#         
#       # Second do the sampling for selected buyers
#       prodcg[, availableForSample := TRUE]
#       conscg[, doSample := TRUE]
#       conscg[!BuyersToDo, doSample := FALSE, on = "BuyerID"]
#       
#       system.time(new_pc <- sample_data(prodcg[availableForSample == TRUE,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights)], 
#                             conscg[doSample == TRUE,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
#                             old_pc = pc,
#                             samplingforSeller = TRUE))
#       
#       new_pc[prodcg, OutputCapacityTons := i.OutputCapacityTons, on = "SellerID"]
#       new_pc[conscg, PurchaseAmountTons := i.PurchaseAmountTons, on = "BuyerID"]
#       
#       pc <- rbindlist(list(pc, new_pc[!pc, on = c("SellerID", "BuyerID")]))
#       
#       if(new_pc[,.N] > 0) MarketToDo <- FALSE
#       
#       if(MarketToDo){
#         print("Making all the sellers and buyers available for sampling")
#         prodcg[, availableForSample := TRUE]
#         conscg[, doSample := TRUE]
#         
#         new_pc <- sample_data(prodcg[availableForSample == TRUE,.(SellerID, Commodity_SCTG, FAFZONE, OutputCapacityTons, ratioweights)], 
#                               conscg[doSample == TRUE,.(BuyerID, FAFZONE, PurchaseAmountTons, samplesize)],
#                               old_pc = pc,
#                               samplingforSeller = TRUE)
#         
#         new_pc[prodcg, OutputCapacityTons := i.OutputCapacityTons, on = "SellerID"]
#         new_pc[conscg, PurchaseAmountTons := i.PurchaseAmountTons, on = "BuyerID"]
#         
#         pc <- rbindlist(list(pc, new_pc[!pc, on = c("SellerID", "BuyerID")]))
#         
#         if(new_pc[,.N] == 0){
#           stop("No more sampling left")
#         }
#       }
#       
#       print(paste0("Number of new pairs: ", pc[,.N] - totalPairs))
#       
#       totalPairs <- pc[,.N]
#       print(paste0("Total number of pairs: ", pc[,.N]))
#       
#       solution <- SolveFlowCLP(pc[,.(SellerID, BuyerID, OutputCapacityTons, PurchaseAmountTons)], 
#                                prodcg, conscg, lp_problems, firstrun = FALSE)
#       
#       SellersToDo <- solution[[1]]
#       BuyersToDo <- solution[[2]]
#       lp_problems <- solution[[3]]
#       solution <- solution[[4]]
#       
#       isInfeasible <- (solution > 0)
#       buyeriter <- buyeriter + 1
#       
#       print(paste0("Is the solution still infeasible:", isInfeasible))
#       print(paste0("Iteration:", buyeriter))
#       print(paste0("Total number of buyer supplier pairs: ", pc[,.N]))
#     }
#     
#   }
#   
#   delProbCLP(lp_problems)
#   rm(lp_problems)
#  
#   # Save pc, conscg and prodcg into a workspace
#   save(pc, conscg, prodcg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
#   
#   return(paste("Completed create_prod_cons_combinations for market:", market, ", group:", g))
# 
# }

# # Sampling functions
# customSample <- function(SellerID,...){
#   if(length(SellerID)>1){
#     return(sample(SellerID,...))
#   } else {
#     return(SellerID)
#   }
# }

# # Sampling function
# sample_data <- function(prodcg, conscg, 
#                         old_pc = NULL,
#                         fractionOfSupplierperBuyer = 1.0, 
#                         samplingforSeller = FALSE){
#   
#   # Set up inputs for looping through by FAF zone
#   sampled_sellers_list <- list()
#   buyers_list <- list()
#   market_sctg <- unique(prodcg$Commodity_SCTG)
#   prodcg.faf.all <- copy(prodcg) # domestic buyer any suppliers
#   prodcg.faf.dom <- prodcg[FAFZONE < 800] # foreign buyer only domestic suppliers
#   consumers.faf <-  conscg[,.N, by = FAFZONE]
#   buffer <- 0.25
#   samplemultipliers <- rep(c(5L, 2L), c(3L, 7L))
#   samplemultseq <- seq(0,1,.1)
#   
#   # get the sampling proportions for this markets commodity
#   FAF_DISTANCE_SCTG <- FAF_DISTANCE[Supplier_Selection_Distribution[SCTG == market_sctg, .(Distance_Bin = DistanceGroup, Proportion)], 
#                                     Proportion := i.Proportion, 
#                                     on = "Distance_Bin"]
#   FAF_DISTANCE_SCTG[is.na(Proportion), Proportion := 1e-6]
#   
#   # loop on consumer FAF zone
#   for (FAFZONE.buyer in consumers.faf$FAFZONE){
#     
#     #print(FAFZONE.buyer)
#     conscg.faf <- conscg[FAFZONE == FAFZONE.buyer]
#     
#     # number of consumers
#     NumConsumers <- conscg.faf[,.N]
#     
#     if (FAFZONE.buyer < 800) {
#       prodcg.faf <- prodcg.faf.all # domestic buyer any suppliers
#     } else {
#       prodcg.faf <- prodcg.faf.dom # foreign buyer only domestic suppliers 
#     }
#     
#     # number of producers
#     NumProducers <- prodcg.faf[,.N]
#     
#     # add the proportions for this zone
#     prodcg.faf[FAF_DISTANCE_SCTG[dFAFZONE == FAFZONE.buyer, .(FAFZONE = oFAFZONE, Proportion)], 
#                Proportion := i.Proportion, 
#                on = "FAFZONE"]
#     
#     prodcg.faf[is.na(Proportion), Proportion := 1e-6]
#     
#     # calculate sampling probability as a function of product weight, proportions, and ratio weights
#     # prodweights depend on the difference in capacity and purchase amount 
#     system.time(prodweights <- lapply(1:NumConsumers, function(x) 1/(1 + log1p(conscg.faf$PurchaseAmountTons[x]/
#                                                                      prodcg.faf$OutputCapacityTons)^2)))
#                                                                      
#     prodweightsums <- lapply(prodweights, sum)
#     
#     scaledratioweight <- prodcg.faf$ratioweights/sum(prodcg.faf$ratioweights)
#     scaledproportions <- prodcg.faf$Proportion/sum(prodcg.faf$Proportion)
#     
#     system.time(proportions.list <- lapply(1:NumConsumers, 
#                                function(x) prodweights[[x]]/prodweightsums[[x]] * 
#                                  scaledratioweight * scaledproportions))
#     
#     # In the case where we have sampled before, reduce the proprtion to zero for any cases that were sampled previously
#     if(!is.null(old_pc)){
#       # which are tne previously sampled producers for this consumer
#       prod.prev <- lapply(1:NumConsumers,
#                           function(x) match(old_pc[conscg$BuyerID[x] == BuyerID]$SellerID, prodcg.faf$SellerID))
#      
#       lapply(1:NumConsumers, function(x) proportions.list[[x]][prod.prev[[x]]] <- 0)
#       
#     }
#     
#     proportions.sums <- lapply(proportions.list, sum)
#     
#     system.time(proportions.prob <- lapply(1:NumConsumers, 
#                                function(x) proportions.list[[x]]/proportions.sums[[x]]))
#     
#     samplesizes.faf <- pmin(NumProducers, ceiling(conscg.faf$samplesize*fractionOfSupplierperBuyer))
#     
#     # take the first set of samples for each consumer
#     system.time(sampled.sellers <- lapply(1:NumConsumers,
#                               function(x) customSample(1:NumProducers, 
#                                     samplesizes.faf[x], 
#                                     replace = FALSE, 
#                                     prob = proportions.prob[[x]])))
#     
#     if(!samplingforSeller) {
#     
#       resample <- TRUE
#       
#       while(resample){  
#       
#         # check whether the samples each have capacity
#         sampled.sellers.cap <- lapply(1:NumConsumers,
#                                       function(x) sum(prodcg.faf$OutputCapacityTons[sampled.sellers[[x]]])/
#                                         (buffer * conscg.faf$PurchaseAmountTons[x]))
#         
#         resample <- any(sapply(1:NumConsumers,
#                                function(x) ifelse(sampled.sellers.cap[[x]] < 1, TRUE, FALSE)))
#                                                   
#         # scale the sample sizes for those with insufficient capacity
#         samplesizes.faf <- sapply(1:NumConsumers,
#                                      function(x) ifelse(sampled.sellers.cap[[x]] < 1,
#                                      samplesizes.faf[x] * samplemultipliers[findInterval(sampled.sellers.cap[[x]], samplemultseq, rightmost.closed = TRUE)],
#                                      samplesizes.faf[x]))
#         
#         # update the sampled sellers for those with insufficient capacity
#         sampled.sellers <- lapply(1:NumConsumers,
#                                   function(x) if(sampled.sellers.cap[[x]] < 1){
#                                                      customSample(1:NumProducers, 
#                                                            samplesizes.faf[x], 
#                                                            replace = FALSE, 
#                                                            prob = proportions.prob[[x]])
#                                               } else {
#                                                      sampled.sellers[[x]]})
#                                                  
#       }
#     }
#     
#     sampled.sellers.vec <- unlist(sampled.sellers)
#     sampled.sellers.id <- prodcg.faf$SellerID[sampled.sellers.vec]
#     sampled_sellers_list[[FAFZONE.buyer]] <- sampled.sellers.id
#     buyers_list[[FAFZONE.buyer]] <- unlist(lapply(1:NumConsumers,
#                                         function(x) rep(conscg.faf$BuyerID[x], samplesizes.faf[x])))
#     
#   }
#   
#   return(data.table(SellerID = unlist(sampled_sellers_list),
#                     BuyerID = unlist(buyers_list)))
#     # # Merge the pairs with modal weights later used for sampling
#     # pc_split <- merge(modal_weights[modalweights>1e-10],pc_split, by=c("Production_zone","Consumption_zone","ODSegment","SCTG"),allow.cartesian = TRUE)
#     # 
#     # Get the weighted proportion
#     #pc_split[, Proportion := (Proportion/sum(Proportion)) * (modalweights/sum(modalweights)) * (prodweights/sum(prodweights))*(ratioweights/sum(ratioweights)),.(BuyerID)] 
# }

# Sampling function 
sample_data_test <- function(prodcg, 
                             conscg, 
                             FAF_DISTANCE_SCTG){
  
  # Set up inputs for looping through by FAF zone
  prodcg.faf.all <- copy(prodcg) # domestic buyer any suppliers
  prodcg.faf.dom <- prodcg[FAFZONE < 800] # foreign buyer only domestic suppliers
  consumers.faf <-  conscg[,.N, by = FAFZONE]
  samplesizemult <- 50L
  
  pc_list <- list()
  
  for (FAFZONE.buyer in consumers.faf$FAFZONE){
    
    # select consumers
    conscg.faf <- conscg[FAFZONE == FAFZONE.buyer]
    NumConsumers <- conscg.faf[,.N]
    
    # select producers
    if (FAFZONE.buyer < 800) {
      prodcg.faf <- prodcg.faf.all # domestic buyer any suppliers
    } else {
      prodcg.faf <- prodcg.faf.dom # foreign buyer only domestic suppliers 
    }
    NumProducers <- prodcg.faf[,.N]
    
    if(NumProducers > 0){ # is possible depending on how this called that there are no applicable producers
      # add the proportions for this zone and calc probabilities
      prodcg.faf[FAF_DISTANCE_SCTG[dFAFZONE == FAFZONE.buyer, .(FAFZONE = oFAFZONE, Proportion)], 
                 Proportion := i.Proportion, 
                 on = "FAFZONE"]
      prodcg.faf[is.na(Proportion)|Proportion == 0, Proportion := 1e-6]
      
      prodcg.faf[, Prob := Proportion * ratioweights * outputprop]
      prodcg.faf[, Prob := Prob/sum(Prob)]
      
      # create a sample for all of the consumers in one go
      # use the sample size field * 2
      # use the Prob field as prob
      # use replacement and then dedup on the complete table
      samp.faf <- sample.int(NumProducers, size = sum(conscg.faf$samplesize) * samplesizemult, replace = TRUE, prob = prodcg.faf$Prob)
      pc_list[[FAFZONE.buyer]] <- cbind(prodcg.faf[samp.faf, .(SellerID, OutputCapacityTons, Prob)], 
                                        conscg.faf[rep.int(1:NumConsumers, conscg.faf$samplesize * samplesizemult),.(BuyerID, PurchaseAmountTons, samplesize)])
    }
  }
  
  # combine list, deduplicate, return
  pc <- rbindlist(pc_list)
  pc <- unique(pc, by = c("SellerID", "BuyerID"))
  return(pc[,.(SellerID, OutputCapacityTons, BuyerID, PurchaseAmountTons)])   
  
}

SolveFlowCLP <- function(pc_table, prodcg, conscg, lp_problems, firstrun = TRUE){
  # Get the number of variables
  SellerID <- prodcg[, SellerID]
  BuyerID <- conscg[, BuyerID]
  nSellers <- length(SellerID)
  nBuyers <- length(BuyerID)
  flowlength <- pc_table[,.N]
  
  tonquantnum <- length(unique(prodcg$tonquant))
  sellerweights <- c(1,0.75,0.5,0.25)[prodcg[pc_table, tonquant, on = "SellerID"]]
  buyerweigths <- c(1,0.75,0.5,0.25)[conscg[pc_table, tonquant, on = "BuyerID"]]
  objweights <- sellerweights * buyerweigths
  
  # The objective function to minimize is just the sum of flows, 
  # penalty on slack variables for sellers and slack variables on buyers
  ## Slack Variables, Flow Variables
  objective <- c(rep(10, nBuyers), objweights)
  
  # Problem formulation:
  # S: Set of sellers
  # B: Set of buyers
  # f_sb: flow from seller s to buyer b
  # c_sb: cost of flow from seller s to buyer b
  # s_b: unfulfilled demand of buyer b
  # D_b: Demand of buyer b
  # C_s: Production capacity of seller s
  ## min sum_sb c_sb * f_sb + sum_b lambda * s_b
  ## s.t.
  ## 		0 <= sum_b f_sb <= C_s for all s
  ##		D_b <= s_b + sum_s f_sb for all b
  
  # The first constraint is to not oversell
  # Sellers are i and Buyers are j. Columns are flows and slack and first chunk of rows are sellers.
  # Column 1 will be i=1,j=1, Column 2 will be i=2, j=1, until Column=length(SellerID) where i=length(SellerID), j = 1
  
  rowindex1 <- match(pc_table$SellerID, SellerID)
  rhsoutput1 <- pc_table[,.N, .(SellerID)][prodcg, OutputCapacityTons, on = "SellerID"]
  
  rowindex2 <- match(pc_table$BuyerID, BuyerID)
  rhsoutput2 <- pc_table[,.N, .(BuyerID)][conscg, PurchaseAmountTons, on = "BuyerID"]
  rowindex2 <- rowindex2 + nSellers
  
  slackrowindex2 <- 1:nBuyers
  
  rowindex <- integer(length = length(rowindex1) + length(rowindex2) + length(slackrowindex2))
  rowindex[1:nBuyers] <- slackrowindex2 - 1
  rowindex[nBuyers + seq(1,2*flowlength,2)] <- rowindex1 - 1
  rowindex[nBuyers + seq(2,2*flowlength,2)] <- rowindex2 - 1
  
  colindex <- c(1:nBuyers, nBuyers + seq(1,2 * flowlength,2)) - 1
  colindex <- c(colindex, length(rowindex))
  
  clb <- as.double(rep(0, flowlength + nBuyers))
  cub <- as.double(rep(max(rhsoutput1, rhsoutput2), flowlength + nBuyers))
  
  rlb <- as.double(c(rep(0, nSellers), rhsoutput2))
  rub <- as.double(c(rhsoutput1, rep(Inf, nBuyers)))
  
  ar <- double(length = length(rowindex)) + 1
  
  rm(pc_table)
  gc()
  
  if(firstrun){
    
    ## Create a new problem
    tradeproblem <- initProbCLP()
    
    ## Set the direction of the objective function
    setObjDirCLP(tradeproblem, 1)
    
    ## Load model problem
    loadProblemCLP(tradeproblem, 
                   nBuyers + flowlength, 
                   nBuyers + nSellers, 
                   ia = rowindex, 
                   ja = colindex, 
                   ra = ar, 
                   rlb = rlb, 
                   rub = rub, 
                   obj_coef = objective, 
                   lb = clb, 
                   ub = cub)
    
    ## Set Simplex Control Parameters
    setLogLevelCLP(tradeproblem, 1)
    
    ## Solve the problem
    solveInitialPrimalCLP(tradeproblem)
    
    sellers <- SellerID[getRowDualCLP(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColPrimCLP(tradeproblem)[1:nBuyers]
    buyers <- BuyerID[buyersslack > 0]
    ifelse(sum(buyersslack) > 0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(SellerID = sellers), 
                data.table(BuyerID = buyers),
                lp_problems,
                status))
    
  } else {
    
    # Retrieve the original problem
    tradeproblem <- lp_problems
    
    # Retrieve the original number of columns
    num_columns <- getNumColsCLP(tradeproblem)
    
    # Number of new columns
    num_new_columns <- flowlength - (num_columns - nBuyers)
    clb_new <- clb[(num_columns + 1):(length(clb))]
    cub_new <- cub[(num_columns + 1):(length(cub))]
    obj_new <- objective[(num_columns + 1):(length(objective))]
    colst_new <- colindex[(num_columns + 1):(length(colindex))]
    colst_new <- colst_new - min(colst_new)
    row_start_index <- num_columns * 2 - nBuyers + 1
    rows_new <- rowindex[(row_start_index):(length(rowindex))]
    ar_new <- ar[(row_start_index):(length(ar))]
    
    ## Add the new columns
    addColsCLP(tradeproblem, num_new_columns, clb_new, cub_new, obj_new, colst_new, rows_new, ar_new)
    
    ## Solve the problem
    solveInitialPrimalCLP(tradeproblem)
    
    sellers <- SellerID[getRowDualCLP(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColPrimCLP(tradeproblem)[1:nBuyers]
    buyers <- BuyerID[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(SellerID = sellers),
                data.table(BuyerID = buyers),
                lp_problems,
                status))
    
  }
}

