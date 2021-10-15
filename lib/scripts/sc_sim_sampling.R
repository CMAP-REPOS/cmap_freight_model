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
    groups <- naics_set$n_groups[market_number]
    
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
        }, seed = TRUE),
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

create_prod_cons_combinations <- function(market, g, check_feasibility = FALSE){
  
  # load the market
  # load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  conscg <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_consc.fst")),
                     as.data.table = TRUE)
  prodcg <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_prodc.fst")),
                    as.data.table = TRUE)

  # Key conscg
  setkey(conscg, FAF)
  
  # seed for sampling
  set.seed(BASE_SEED_VALUE)
  # Set the initial ratioweights to 1. 
  # ratioweights attribute is used to track a metric of production to consumption ratio
  prodcg[, ratioweights := 1]
  
  # what proportion of total output capacity does each producer represent?
  prodcg[, outputprop := output_capacity_tons/sum(output_capacity_tons)]
  
  # what proportion of total purchases does each consmer represent?
  conscg[, purchprop := purchase_amount_tons/sum(purchase_amount_tons)]
  
  # production and consumption quantiles
  prodcg[, tonquant := findInterval(output_capacity_tons,
                                    quantile(output_capacity_tons, probs = seq(0, 1, 0.25), type = 5),
                                    rightmost.closed = TRUE)]
  
  conscg[, tonquant := findInterval(purchase_amount_tons,
                                    quantile(purchase_amount_tons, type = 5),
                                    rightmost.closed = TRUE)]
  
  # Assign a sample size (of sellers) to buyers
  # estimate a reasonable number of extra samples to allocate amongst larger consumers
  total_samples <- max(BASE_SUPPLIERS_PER_BUYER * 5 * prodcg[,.N], BASE_SUPPLIERS_PER_BUYER * conscg[,.N])
  conscg[, samplesize := round(total_samples * purchprop)]
  # update the sample size to be minimum BASE_SUPPLIERS_PER_BUYER/2, maximum is the number of producers
  min_samples <- ceiling(BASE_SUPPLIERS_PER_BUYER/2)
  conscg[, samplesize := ifelse(samplesize < min_samples, min_samples, samplesize)]
  conscg[, samplesize := ifelse(samplesize > prodcg[,.N], prodcg[,.N], samplesize)]
  
  # get the sampling proportions for this markets commodity
  market_sctg <- unique(prodcg$SCTG)
  
  # distance based sampling for domestic shipments
  FAF_TON_DIST_SCTG <- FAF_TON_DIST[SCTG == market_sctg, .(Distance_Bin = DistanceGroup, Proportion)]
  
  FAF_DISTANCE_SCTG <- FAF_DISTANCE[FAF_TON_DIST_SCTG,
                                    Proportion := i.Proportion,
                                    on = "Distance_Bin"]

  FAF_DISTANCE_SCTG[is.na(Proportion), Proportion := 1e-6]

  # get the trade type proportion for split between domestic and imports for domestic consumers
  FAF_TON_TRADETYPE_SCTG <- FAF_TON_TRADETYPE[SCTG == market_sctg & Trade_Type %in% c("Domestic", "Import")]
  FAF_TON_TRADETYPE_SCTG[, Proportion := Proportion/sum(Proportion, na.rm = TRUE)]
  
  # iterative takes samples until there are enough for a feasible solution in PMG
  isInfeasible <- TRUE
  buyeriter <- 0
  
  while(isInfeasible){
    
    # Take the sample
    pc_add <- sample_data_test(prodcg[,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights, outputprop)], 
                               conscg[,.(buyer_id, FAF, purchase_amount_tons, samplesize)],
                               FAF_DISTANCE_SCTG,
                               FAF_TON_DIST_SCTG,
                               FAF_TON_TRADETYPE_SCTG)
    
    # ### Does the sample have a reasonable distance distribution and trade type split
    # pc_add[prodcg, oFAFZONE := i.FAF, on = "seller_id"]
    # pc_add[conscg, dFAFZONE := i.FAF, on = "buyer_id"]
    # pc_add[FAF_DISTANCE_SCTG, Distance_Bin := i.Distance_Bin, on = c("oFAFZONE", "dFAFZONE")]
    # pc_add[, Trade_Type := "Domestic"]
    # pc_add[dFAFZONE > 800, Trade_Type := "Export"]
    # pc_add[oFAFZONE > 800, Trade_Type := "Import"]
    # 
    # # poportion by trade type
    # pc_add[, .(output_capacity_tons = sum(output_capacity_tons), purchase_amount_tons = sum(purchase_amount_tons))
    #        , by = Trade_Type][, c("Trade_Type_Out_Prop", "Trade_Type_Purch_Prop") := .(output_capacity_tons/sum(output_capacity_tons), purchase_amount_tons/sum(purchase_amount_tons))][]
    # FAF_TON_TRADETYPE[SCTG == market_sctg]
    # 
    # # poportion by distance for domestic
    # pc_add[!is.na(Distance_Bin) & Trade_Type == "Domestic",
    #        .(output_capacity_tons = sum(output_capacity_tons), purchase_amount_tons = sum(purchase_amount_tons)),
    #        keyby = Distance_Bin]
    # FAF_TON_DIST[SCTG == market_sctg][1:10]
    
    
    if(buyeriter>0){
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("seller_id", "buyer_id"))
    } else {
      pc <- pc_add
    }
    # Checks on sample
    # this is margin of safety to help ensure enough excess capacity relative to purchase requirements
    buffer <- 0.1
    
    # For buyers, which have sufficient capacity in the sellers that they are paired with to meet their requirements
    Buy.PurRatio <- pc[, .(NumSamples = .N, PurRatio = sum(output_capacity_tons) * buffer/mean(purchase_amount_tons)), by = buyer_id]
    # Buy.PurRatio[order(PurRatio)][PurRatio < 1]
    
    # If any buyers have a purchase ratio < 1, i.e. the total output capacity * buffer <  purchase amount 
    # add some records to the sample for those buyers
    if(Buy.PurRatio[PurRatio < 1,.N] > 0){
      conscg.pur <- conscg[buyer_id %in% Buy.PurRatio[PurRatio < 1]$buyer_id]
      pc_add <- sample_data_test(prodcg[,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights, outputprop)], 
                                 conscg.pur[,.(buyer_id, FAF, purchase_amount_tons, samplesize)],
                                 FAF_DISTANCE_SCTG,
                                 FAF_TON_DIST_SCTG,
                                 FAF_TON_TRADETYPE_SCTG,
                                 check_distance = FALSE) 
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("seller_id", "buyer_id"))
    }
    
    # Which sellers are allocated to buyer samples and how extended are they
    # this is not very meaningful, but in general Sellers (particularly larges ones) should be in many samples 
    Sell.CapRatio <- pc[, .(NumSamples = .N, CapRatio = mean(output_capacity_tons)/(buffer * sum(purchase_amount_tons))), by = seller_id]
    # Add some extra cases if there are missing sellers
    # this should generally work to allocate all sellers to at least some buyer sample but don't need to force it
    # not guarenteed in the PMG that all sellers will find a buyer anyway
    Sell.Missing <- prodcg[!seller_id %in% Sell.CapRatio$seller_id]
    
    if(Sell.Missing[,.N] > 0){
      
      # select a reasonable number of buyers to add these sellers too
      avbuyers <- ceiling(pc[,.N]/(prodcg[,.N] - Sell.Missing[,.N]))
      buyerstoadd <- min(avbuyers * Sell.Missing[,.N], conscg[,.N])
      conscg.samp <- conscg[sample.int(conscg[,.N], size = buyerstoadd, replace = FALSE, prob = conscg$purchprop)]
      conscg.samp[, samplesize := 1]
      
      pc_add <- sample_data_test(prodcg[seller_id %in% Sell.Missing$seller_id,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights, outputprop)], 
                                 conscg.samp[,.(buyer_id, FAF, purchase_amount_tons, samplesize)],
                                 FAF_DISTANCE_SCTG,
                                 FAF_TON_DIST_SCTG,
                                 FAF_TON_TRADETYPE_SCTG,
                                 check_distance = FALSE)
      pc <- rbind(pc, pc_add)
      pc <- unique(pc, by = c("seller_id", "buyer_id"))
      
    }
    
    if(check_feasibility){
      # recalculate Sell cap ratio after dealing with missing
      Sell.CapRatio <- pc[, .(NumSamples = .N, CapRatio = mean(output_capacity_tons)/(buffer * sum(purchase_amount_tons))), by = seller_id]
    
      # Check the solution
      system.time(solution <- SolveFlowCLP(pc[,.(seller_id, buyer_id, output_capacity_tons, purchase_amount_tons)], 
                                         prodcg, conscg, lp_problems = NULL))
    
      SellersToDo <- solution[[1]]
      BuyersToDo <- solution[[2]]
      lp_problems <- solution[[3]]
      solution <- solution[[4]]
      
      isInfeasible <- (solution > 0)
    } else {
      
      # force to stop after single iteration
      isInfeasible <- FALSE
    }  
    
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
      prodcg[Sell.CapRatio, CapRatio := i.CapRatio, on = "seller_id"]
      prodcg[, ratioweights := ifelse(!is.na(CapRatio), 
                                      CapRatio/RangeCapRatio * 100 + 1, 
                                      MaxCapRatio/RangeCapRatio * 100 + 1)]
    }
  }
  
  if(check_feasibility){
    delProbCLP(lp_problems)
    rm(lp_problems)
  }
  
  # Save pc, conscg and prodcg into a workspace
  # save(pc, conscg, prodcg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
  write_fst(conscg, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_consc.fst")))
  write_fst(prodcg, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_prodc.fst")))
  write_fst(pc, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")))
  
  return(paste("Completed create_prod_cons_combinations for market:", market, ", group:", g))
  
}

# create_prod_cons_combinations <- function(market, g){
#   
#   # load the market
#   load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
#   
#   # Rename fields ready to merge
#   setnames(conscg, 
#            c("input_commodity", "NAICS", "Zone", "size"), 
#            c("NAICS", "Buyer.NAICS", "Consumption_zone", "Buyer.Size"))
#   setnames(prodcg, 
#            c("output_commodity", "NAICS", "Zone", "size"),
#            c("NAICS", "Seller.NAICS", "Production_zone", "Seller.Size"))
#   
#   ### TODO figure out this section of code and update for NFM
#   # longcskims_sctg <- mode_availability[avail==TRUE & SCTG %in% unique(prodcg$SCTG)][,.(SCTG,ODSegment,path)][longcskims,on=.(path),allow.cartesian=TRUE][!is.na(ODSegment)]
#   # 
#   # distance_bins <- model$scenvars$distance_bins
#   # nSupplierPerBuyer <- model$scenvars$nSupplierPerBuyer
#   # distBased <- model$scenvars$distBased
#   # 
#   # # Bin the distances
#   # mesozone_gcd[,Distance_Bin:=findInterval(GCD,distance_bins)]
#   # 
#   # if(model$scenvars$distBased){
#   #   supplier_selection_distribution <- fread(file.path(model$basedir,"scenarios","base","inputs","DistanceDistribution.csv"),key=c("SCTG","DistanceGroup"))
#   # } else {
#   #   supplier_selection_distribution <- fread(file.path(model$basedir,"scenarios","base","inputs","TonnageDistribution.csv"),key=c("SCTG","DistanceGroup"))
#   # }
#   # 
#   # # Get modal availability
#   # zonemodeavailability <- dcast.data.table(longcskims_sctg,SCTG+Production_zone+Consumption_zone~Mode.Domestic, value.var = "Mode.Domestic", fun.aggregate = function(x) ifelse(length(x)>0,TRUE,FALSE))
#   # 
#   # # Get the modal cost data
#   # modal_targets <- pc_domestic_targets[SCTG %in% unique(prodcg$SCTG)]
#   # modal_targets <- modal_targets[mode_availability[SCTG%in%unique(prodcg$SCTG)&avail==TRUE,.N,.(Mode.Domestic=LinehaulMode,ODSegment,SCTG)],on=.(SCTG,ODSegment,Mode.Domestic)][,N:=NULL]
#   # modal_targets[is.na(Target), Target:= 0]
#   # 
#   # # Get mode specific weights for zone pairs and commodity type
#   # modal_weights <- longcskims_sctg[,.(weights=1/min(cost)),.(Production_zone,Consumption_zone,Mode.Domestic,ODSegment,SCTG)]
#   # 
#   # # Merge modal targets
#   # modal_weights <- merge(modal_weights,modal_targets,by=c("Mode.Domestic","ODSegment","SCTG"))
#   # 
#   # 
#   # # Get the weights according to the target
#   # modal_weights <- modal_weights[,.(modalweights=sum(weights*Target)),.(Production_zone,Consumption_zone,ODSegment,SCTG)]
#   # modal_weights[is.na(modalweights), modalweights:=1e-6]
#   
#   # merge together to create all of the pairs of firms for this group and commodity
#   set.seed(BASE_SEED_VALUE)
#   # Set the initial ratioweights to 1. 
#   # ratioweights attribute is used to track a metric of production to consumption ratio
#   prodcg[, ratioweights := 1]
#   
#   prodcg[, tonquant := findInterval(output_capacity_tons,
#                                    quantile(output_capacity_tons, type = 5),
#                                    rightmost.closed = TRUE)]
#   
#   conscg[, tonquant := findInterval(purchase_amount_tons,
#                                    quantile(purchase_amount_tons, type = 5),
#                                    rightmost.closed = TRUE)]
#   
#   # Assign a sample size (of sellers) to buyers
#   conscg[, samplesize := seq(5, BASE_SUPPLIERS_PER_BUYER, length.out = 4)[tonquant]]
#   
#   print("1. First time sampling")
#   
#   pc <- sample_data(prodcg[,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights)], 
#                     conscg[,.(buyer_id, FAF, purchase_amount_tons, samplesize)])
#   
#   pc[prodcg, output_capacity_tons := i.output_capacity_tons, on = "seller_id"]
#   pc[conscg, purchase_amount_tons := i.purchase_amount_tons, on = "buyer_id"]
#   
#   solution <- SolveFlowCLP(pc[,.(seller_id, buyer_id, output_capacity_tons, purchase_amount_tons)], 
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
#       prodratio <- pc[,.(dfTons = sum(purchase_amount_tons)),
#                       .(seller_id,output_capacity_tons)][,.(seller_id, Ratio=dfTons/output_capacity_tons)]
#       
#       # Update the ratioweights
#       prodcg[prodratio, ratioweights := 1/i.Ratio, on = "seller_id"]
#       prodcg[!prodratio, ratioweights := 100, on = "seller_id"]
#       #conscg[, samplesize := samplesize + samplesize / buyeriter] 
#       
#       # First do the sampling for selected sellers
#       prodcg[, availableForSample := TRUE]
#       prodcg[!SellersToDo, availableForSample := FALSE, on = "seller_id"]
#       conscg[, doSample := TRUE]
#       
#       new_pc <- sample_data(prodcg[availableForSample == TRUE,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights)], 
#                             conscg[doSample == TRUE,.(buyer_id, FAF, purchase_amount_tons, samplesize)],
#                             old_pc = pc,
#                             samplingforSeller = TRUE)
#       
#       new_pc[prodcg, output_capacity_tons := i.output_capacity_tons, on = "seller_id"]
#       new_pc[conscg, purchase_amount_tons := i.purchase_amount_tons, on = "buyer_id"]
#   
#       pc <- rbindlist(list(pc, new_pc[!pc, on = c("seller_id", "buyer_id")])) # removing any dupes that made it
#       
#       
#       if(new_pc[,.N] > 0) MarketToDo <- FALSE
#         
#       # Second do the sampling for selected buyers
#       prodcg[, availableForSample := TRUE]
#       conscg[, doSample := TRUE]
#       conscg[!BuyersToDo, doSample := FALSE, on = "buyer_id"]
#       
#       new_pc <- sample_data(prodcg[availableForSample == TRUE,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights)], 
#                             conscg[doSample == TRUE,.(buyer_id, FAF, purchase_amount_tons, samplesize)],
#                             old_pc = pc,
#                             samplingforSeller = TRUE)
#       
#       new_pc[prodcg, output_capacity_tons := i.output_capacity_tons, on = "seller_id"]
#       new_pc[conscg, purchase_amount_tons := i.purchase_amount_tons, on = "buyer_id"]
#       
#       pc <- rbindlist(list(pc, new_pc[!pc, on = c("seller_id", "buyer_id")]))
#       
#       if(new_pc[,.N] > 0) MarketToDo <- FALSE
#       
#       if(MarketToDo){
#         print("Making all the sellers and buyers available for sampling")
#         prodcg[, availableForSample := TRUE]
#         conscg[, doSample := TRUE]
#         
#         new_pc <- sample_data(prodcg[availableForSample == TRUE,.(seller_id, SCTG, FAF, output_capacity_tons, ratioweights)], 
#                               conscg[doSample == TRUE,.(buyer_id, FAF, purchase_amount_tons, samplesize)],
#                               old_pc = pc,
#                               samplingforSeller = TRUE)
#         
#         new_pc[prodcg, output_capacity_tons := i.output_capacity_tons, on = "seller_id"]
#         new_pc[conscg, purchase_amount_tons := i.purchase_amount_tons, on = "buyer_id"]
#         
#         pc <- rbindlist(list(pc,new_pc[!pc, on = c("seller_id", "buyer_id")]))
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
#       solution <- SolveFlowCLP(pc[,.(seller_id, buyer_id, output_capacity_tons, purchase_amount_tons)], 
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
#   ### TODO figure out this section of code and update for NFM
#   # # Start sampling to match modal target share
#   # prodcg[, availableForSample := TRUE]
#   # conscg[, doSample := TRUE]
#   # 
#   # pc_proportion <- zonemodeavailability[pc[,.(Production_zone, Consumption_zone, SCTG, buyer_id, purchase_amount_tons, ODSegment)], on = .(Production_zone, Consumption_zone, SCTG)]
#   # 
#   # modenames <- intersect(c("Truck", "Rail", "Water", "Air", "Pipeline"), colnames(pc_proportion))
#   # 
#   # pc_proportion <- melt.data.table(pc_proportion[,lapply(.SD, function(x) sum(x * purchase_amount_tons)/sum(purchase_amount_tons)), .SDcols = modenames, by = .(ODSegment, SCTG)], id.vars = c("ODSegment", "SCTG"), variable.name = "Mode.Domestic", variable.factor = FALSE, value.name = "Proportion")
#   # 
#   # print(pc_proportion)
#   # 
#   # # Make a copy to compare improvements from further iterations
#   # pc_proportion_prior <- copy(pc_proportion)
#   # 
#   # # Make a copy to track changes to the target share
#   # modal_targets_orig <- copy(modal_targets)
#   # 
#   # # Only sample pairs from those that have observed share less than target share
#   # modal_targets[pc_proportion, Target := ifelse(Target < Proportion, 1e-10, Target), on = .(ODSegment, SCTG, Mode.Domestic)]
#   # 
#   # # Assign modal weights based on the cost between zones
#   # modal_weights <- longcskims_sctg[,.(weights= 1/min(cost)), .(Production_zone, Consumption_zone, Mode.Domestic, ODSegment, SCTG)]
#   # 
#   # # Weight the modal weights by targets
#   # modal_weights <- merge(modal_weights, modal_targets[Target > 1e-10], by = c("Mode.Domestic", "ODSegment", "SCTG"), allow.cartesian = TRUE)
#   # 
#   # modal_weights <- modal_weights[,.(modalweights = sum(weights * Target)), .(Production_zone, Consumption_zone, ODSegment, SCTG)]
#   # 
#   # 
#   # resampleModal <- TRUE
#   # sampleModes <- 1L
#   # prior_diff <- 1
#   # limitBuyersResampling <- 10L
#   # 
#   # resampleModal <- modal_targets[, any(Target > 1e-10)]
#   # 
#   # print("4. Sampling to match modal share targets")
#   # while(resampleModal & (prior_diff >= 1e-3) & sampleModes < (limitBuyersResampling + 1L)){
#   #   
#   #   new_pc <- rbindlist(lapply(1:n_splits, sample_data, prodcg, conscg, fractionOfSupplierperBuyer = 1.0, samplingforSeller = resampleModal, pc_data = pc))
#   #   
#   #   pc <- rbindlist(list(pc, new_pc))
#   #   rm(new_pc)
#   #   gc()
#   #   
#   #   pc_proportion <- zonemodeavailability[pc[,.(Production_zone,Consumption_zone,SCTG,buyer_id,purchase_amount_tons,ODSegment)],on=.(Production_zone,Consumption_zone,SCTG)]
#   #   
#   #   modenames <- intersect(c("Truck","Rail","Water","Air","Pipeline"),colnames(pc_proportion))
#   #   
#   #   pc_proportion <- melt.data.table(pc_proportion[,lapply(.SD,function(x) sum(x*purchase_amount_tons)/sum(purchase_amount_tons)),.SDcols=modenames,by=.(ODSegment,SCTG)],id.vars = c("ODSegment","SCTG"), variable.name = "Mode.Domestic", variable.factor = FALSE, value.name = "Proportion")
#   #   
#   #   print(pc_proportion)
#   #   
#   #   print(prior_diff <- pc_proportion_prior[pc_proportion,sum(abs(Proportion-i.Proportion)),on=.(ODSegment,SCTG,Mode.Domestic)])
#   #   # print(pc_proportion)
#   #   pc_proportion_prior <- copy(pc_proportion)
#   #   
#   #   modal_targets <- pc_proportion[modal_targets_orig,.(Target=ifelse(Target<Proportion,1e-10,Target),ODSegment,SCTG,Mode.Domestic),on=.(ODSegment,SCTG,Mode.Domestic)]#[modal_targets,.(Target=ifelse(Target>1e-10,Target+i.Target,1e-10),ODSegment,SCTG,Mode.Domestic),on=.(ODSegment,SCTG,Mode.Domestic)]
#   #   
#   #   resampleModal <- modal_targets[,any(Target>1e-10)]
#   #   
#   #   modal_weights <- longcskims_sctg[,.(weights=1/min(cost)),.(Production_zone,Consumption_zone,Mode.Domestic,ODSegment,SCTG)]
#   #   
#   #   modal_weights <- merge(modal_weights,modal_targets[Target>1e-10],by=c("Mode.Domestic","ODSegment","SCTG"),allow.cartesian = TRUE)
#   #   
#   #   modal_weights <- modal_weights[,.(modalweights=sum(weights*Target)),.(Production_zone,Consumption_zone,ODSegment,SCTG)]
#   #   print(modal_targets)
#   #   
#   #   print(sampleModes <- sampleModes + 1L)
#   #   
#   #   print(paste0("Number of Combinations after ", sampleModes-1, " iteration: ",pc[,.N]))
#   #   
#   # }
#   
#   # Save pc, conscg and prodcg into a workspace
#   save(pc, conscg, prodcg, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
#   
#   return(paste("Completed create_prod_cons_combinations for market:", market, ", group:", g))
# 
# }

# # Sampling functions
# customSample <- function(seller_id,...){
#   if(length(seller_id)>1){
#     return(sample(seller_id,...))
#   } else {
#     return(seller_id)
#   }
# }
# 
# # Sampling function
# sample_data <- function(prodcg, conscg, 
#                         old_pc = NULL,
#                         fractionOfSupplierperBuyer = 1.0, 
#                         samplingforSeller = FALSE){
#   
#   # Set up inputs for looping through by FAF zone
#   sampled_sellers_list <- list()
#   buyers_list <- list()
#   market_sctg <- unique(prodcg$SCTG)
#   prodcg.faf.all <- copy(prodcg) # domestic buyer any suppliers
#   prodcg.faf.dom <- prodcg[FAF < 800] # foreign buyer only domestic suppliers
#   consumers.faf <-  conscg[,.N, by = FAF]
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
#   for (FAF.buyer in consumers.faf$FAF){
#     
#     print(FAF.buyer)
#     conscg.faf <- conscg[FAF == FAF.buyer]
#     
#     # number of consumers
#     NumConsumers <- conscg.faf[,.N]
#     
#     if (FAF.buyer < 800) {
#       prodcg.faf <- prodcg.faf.all # domestic buyer any suppliers
#     } else {
#       prodcg.faf <- prodcg.faf.dom # foreign buyer only domestic suppliers 
#     }
#     
#     # number of producers
#     NumProducers <- prodcg.faf[,.N]
#     
#     # continue for this FAF zone if there are producers available to sample
#     
#     if(NumProducers > 0){
#       # add the proportions for this zone
#       prodcg.faf[FAF_DISTANCE_SCTG[dFAFZONE == FAF.buyer, .(FAF = oFAFZONE, Proportion)], 
#                  Proportion := i.Proportion, 
#                  on = "FAF"]
#       
#       prodcg.faf[is.na(Proportion), Proportion := 1e-6]
#       
#       prodweights <- lapply(1:NumConsumers, function(x) 1/(1 + log1p(conscg.faf$purchase_amount_tons[x]/
#                                                                        prodcg.faf$output_capacity_tons)^2))
#       
#       prodweightsums <- lapply(prodweights, sum)
#       
#       scaledratioweight <- prodcg.faf$ratioweights/sum(prodcg.faf$ratioweights)
#       scaledproportions <- prodcg.faf$Proportion/sum(prodcg.faf$Proportion)
#       
#       proportions.list <- lapply(1:NumConsumers, 
#                                  function(x) prodweights[[x]]/prodweightsums[[x]] * 
#                                    scaledratioweight * scaledproportions)
#       
#       # In the case where we have sampled before, reduce the proprtion to zero for any cases that were sampled previously
#       if(!is.null(old_pc)){
#         # which are tne previously sampled producers for this consumer
#         prod.prev <- lapply(1:NumConsumers,
#                             function(x) match(old_pc[conscg$buyer_id[x] == buyer_id]$seller_id, prodcg.faf$seller_id))
#        
#         lapply(1:NumConsumers, function(x) proportions.list[[x]][prod.prev[[x]]] <- 0)
#         
#       }
#       
#       proportions.sums <- lapply(proportions.list, sum)
#       
#       proportions.prob <- lapply(1:NumConsumers, 
#                                  function(x) proportions.list[[x]]/proportions.sums[[x]])
#       
#       samplesizes.faf <- pmin(NumProducers, ceiling(conscg.faf$samplesize*fractionOfSupplierperBuyer))
#       
#       # take the first set of samples for each consumer
#       sampled.sellers <- lapply(1:NumConsumers,
#                                 function(x) customSample(1:NumProducers, 
#                                       samplesizes.faf[x], 
#                                       replace = FALSE, 
#                                       prob = proportions.prob[[x]]))
#       
#       if(!samplingforSeller) {
#       
#         resample <- TRUE
#         
#         while(resample){  
#         
#           # check whether the samples each have capacity
#           sampled.sellers.cap <- lapply(1:NumConsumers,
#                                         function(x) sum(prodcg.faf$output_capacity_tons[sampled.sellers[[x]]])/
#                                           (buffer * conscg.faf$purchase_amount_tons[x]))
#           
#           resample <- any(sapply(1:NumConsumers,
#                                  function(x) ifelse(sampled.sellers.cap[[x]] < 1, TRUE, FALSE)))
#                                                     
#           # scale the sample sizes for those with insufficient capacity
#           samplesizes.faf <- sapply(1:NumConsumers,
#                                        function(x) ifelse(sampled.sellers.cap[[x]] < 1,
#                                        samplesizes.faf[x] * samplemultipliers[findInterval(sampled.sellers.cap[[x]], samplemultseq, rightmost.closed = TRUE)],
#                                        samplesizes.faf[x]))
#           
#           # update the sampled sellers for those with insufficient capacity
#           sampled.sellers <- lapply(1:NumConsumers,
#                                     function(x) if(sampled.sellers.cap[[x]] < 1){
#                                                        customSample(1:NumProducers, 
#                                                              samplesizes.faf[x], 
#                                                              replace = FALSE, 
#                                                              prob = proportions.prob[[x]])
#                                                 } else {
#                                                        sampled.sellers[[x]]})
#                                                    
#         }
#       }
#     
#       sampled.sellers.vec <- unlist(sampled.sellers)
#       sampled.sellers.id <- prodcg.faf$seller_id[sampled.sellers.vec]
#       sampled_sellers_list[[FAF.buyer]] <- sampled.sellers.id
#       buyers_list[[FAF.buyer]] <- unlist(lapply(1:NumConsumers,
#                                           function(x) rep(conscg.faf$buyer_id[x], samplesizes.faf[x])))
#     }  
#   }
#   
#   return(data.table(seller_id = unlist(sampled_sellers_list),
#                     buyer_id = unlist(buyers_list)))
#     # # Merge the pairs with modal weights later used for sampling
#     # pc_split <- merge(modal_weights[modalweights>1e-10],pc_split, by=c("Production_zone","Consumption_zone","ODSegment","SCTG"),allow.cartesian = TRUE)
#     # 
#     # Get the weighted proportion
#     #pc_split[, Proportion := (Proportion/sum(Proportion)) * (modalweights/sum(modalweights)) * (prodweights/sum(prodweights))*(ratioweights/sum(ratioweights)),.(buyer_id)] 
# }

# Sampling function 
sample_data_test <- function(prodcg, 
                             conscg, 
                             FAF_DISTANCE_SCTG,
                             FAF_TON_DIST_SCTG,
                             FAF_TON_TRADETYPE_SCTG,
                             check_distance = TRUE){
  
  # Set up inputs for looping through by FAF zone
  prodcg.faf.all <- copy(prodcg) # domestic buyer any suppliers
  prodcg.faf.dom <- prodcg[FAF < 800] # foreign buyer only domestic suppliers
  consumers.faf <-  conscg[,.N, by = FAF]
  samplesizemult <- 2L
  
  pc_list <- list()
  
  for (FAFZONE.buyer in consumers.faf$FAF){
    
    # select consumers
    conscg.faf <- conscg[FAF == FAFZONE.buyer]
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
      
      ### Distance factors for domestic producers
      prodcg.faf[FAF_DISTANCE_SCTG[dFAFZONE == FAFZONE.buyer, .(FAF = oFAFZONE, Distance_Bin)], 
                 Distance_Bin := i.Distance_Bin, 
                 on = "FAF"]
      
      if("Output_Prop" %in% names(FAF_TON_DIST_SCTG)) FAF_TON_DIST_SCTG[, Output_Prop := NULL]
      
      FAF_TON_DIST_SCTG[prodcg.faf[!is.na(Distance_Bin) & FAF < 800,
                 .(Output_Prop = sum(outputprop)), 
                 keyby = Distance_Bin],
                 Output_Prop := i.Output_Prop,
                 on = "Distance_Bin"]
      
      FAF_TON_DIST_SCTG[, Distance_Factor := Proportion/Output_Prop]           
      
      prodcg.faf[FAF_TON_DIST_SCTG,
                 Distance_Factor := i.Distance_Factor, 
                 on = "Distance_Bin"]
      
      prodcg.faf[is.na(Distance_Factor), Distance_Factor := 1e-6]
      prodcg.faf[FAF > 800, Distance_Factor := 1]
      
      ### Factors for trade type shares
      if(FAFZONE.buyer < 800){ 
        
        prodcg.faf[, Trade_Type := "Domestic"]
        prodcg.faf[FAF > 800, Trade_Type := "Import"]
        
        if("Output_Prop" %in% names(FAF_TON_TRADETYPE_SCTG)) FAF_TON_TRADETYPE_SCTG[, Output_Prop := NULL]
        
        FAF_TON_TRADETYPE_SCTG[prodcg.faf[,.(Output_Prop = sum(outputprop * Distance_Factor)), 
                                          keyby = Trade_Type],
                               Output_Prop := i.Output_Prop,
                               on = "Trade_Type"]
        
        FAF_TON_TRADETYPE_SCTG[, Trade_Type_Factor := Proportion/Output_Prop]           
        
        prodcg.faf[FAF_TON_TRADETYPE_SCTG,
                   Trade_Type_Factor := i.Trade_Type_Factor, 
                   on = "Trade_Type"]
      } else {
        # All export
        prodcg.faf[, Trade_Type_Factor := 1]
      }
      
      prodcg.faf[, Prob := ratioweights * outputprop * Distance_Factor * Trade_Type_Factor]
      prodcg.faf[, Prob := Prob/sum(Prob)]
      
      #prodcg.faf[,round(sum(Prob),3), keyby = Distance_Bin]
      
      # create a sample for all of the consumers in one go
      # use the sample size field * 2
      # use the Prob field as prob
      # use replacement and then dedup on the complete table
      samp.faf <- sample.int(NumProducers, size = sum(conscg.faf$samplesize) * samplesizemult, replace = TRUE, prob = prodcg.faf$Prob)
      
      pc.FAFZONE.buyer <- cbind(prodcg.faf[samp.faf, .(seller_id, output_capacity_tons, Prob, Distance_Bin)], 
                                conscg.faf[rep.int(1:NumConsumers, conscg.faf$samplesize * samplesizemult),.(buyer_id, purchase_amount_tons, samplesize)])
      
      # check whether the deduped list for this sample has a reasonable number of local producers
      if(FAFZONE.buyer < 800 & check_distance){
        pc.dist <- unique(pc.FAFZONE.buyer[,.(seller_id, buyer_id)])
      
        pc.dist[pc.FAFZONE.buyer, 
                c("purchase_amount_tons", "Distance_Bin") := .(i.purchase_amount_tons, i.Distance_Bin), 
                on = c("seller_id", "buyer_id")]
        
        pc.dist.sum <- pc.dist[,.(purchase_amount_tons = sum(purchase_amount_tons)), keyby = Distance_Bin]
        
        pc.dist.sum[, Prop := purchase_amount_tons/sum(purchase_amount_tons)]
        
        pc.dist.sum[FAF_TON_DIST_SCTG, Target := i.Proportion, on = "Distance_Bin"]
        
        pc.dist.sum[, Diff := Prop - Target]
        
        if(nrow(pc.dist.sum[Distance_Bin == 1 & Diff < 0])>0){
          
          # add additional short distance producers to the sample
          pc.FAFZONE.buyer.short <- merge(prodcg.faf[Distance_Bin == 1, .(seller_id, output_capacity_tons, Prob, Distance_Bin, k=1)], 
                                          conscg.faf[,.(buyer_id, purchase_amount_tons, samplesize, k = 1)],
                                          by = "k",
                                          allow.cartesian = TRUE)[, k := NULL]
          
          pc.FAFZONE.buyer <- rbind(pc.FAFZONE.buyer,
                                    pc.FAFZONE.buyer.short)
          
          # trim down the sample to meet the sample threshold size by removing the most distant suppliers
          setkey(pc.FAFZONE.buyer, buyer_id, Distance_Bin)
          pc.FAFZONE.buyer[, seller_rank := 1:.N, by = buyer_id]
          pc.FAFZONE.buyer <- pc.FAFZONE.buyer[seller_rank <= samplesizemult * samplesize]
          pc.FAFZONE.buyer[, seller_rank := NULL]
          
        }
        
      }
      
      pc_list[[FAFZONE.buyer]] <- pc.FAFZONE.buyer[, Distance_Bin := NULL]
      
    }
  }
  
  # combine list, deduplicate, return
  pc <- rbindlist(pc_list)
  pc <- unique(pc, by = c("seller_id", "buyer_id"))
  
  return(pc[,.(seller_id, output_capacity_tons, buyer_id, purchase_amount_tons)])   
  
}

# Checking if flows can happen function
SolveFlowGLPK <- function(pc_table, prodcg, conscg, firstrun=TRUE){
  # Get the number of variables
  seller_id <- prodcg[, seller_id]
  buyer_id <- conscg[, buyer_id]
  nSellers <- length(seller_id)
  nBuyers <- length(buyer_id)
  flowlength <- pc_table[,.N]
  sellerweights <- c(1,0.75,0.5,0.25)[prodcg[pc_table, tonquant, on = "seller_id"]]
  buyerweigths <- c(1,0.75,0.5,0.25)[conscg[pc_table, tonquant, on = "buyer_id"]]
  objweights <- sellerweights * buyerweigths
  
  # The objective function to minimize is just the sum of flows, penalty on slack variables for sellers and slack variables on buyers
  ## Slack Variables, Flow Variables
  objective <- c(rep(10, nBuyers), objweights) 
  
  # The first constraint is to not oversell
  # Sellers are i and Buyers are j. Columns are flows and slack and first chunk of rows are sellers.
  # Column 1 will be i=1,j=1, Column 2 will be i=2, j=1, until Column=length(seller_id) where i=length(seller_id), j = 1
  
  rowindex1 <- match(pc_table$seller_id, seller_id)
  colindex1 <- 1:flowlength + nBuyers
  rhsoutput1 <- pc_table[,.N,.(seller_id)][prodcg, output_capacity_tons,on = "seller_id"]
  
  slackrowindex1 <- 1:nSellers
  slackcolindex1 <- 1:nSellers + nBuyers
  
  rowindex2 <- match(pc_table$buyer_id,buyer_id)
  colindex2 <- 1:flowlength + nBuyers
  rhsoutput2 <- pc_table[,.N,.(buyer_id)][conscg, purchase_amount_tons, on = "buyer_id"]
  rowindex2 <- rowindex2 + nSellers
  
  slackrowindex2 <- 1:nBuyers
  slackcolindex2 <- 1:nBuyers
  
  constraintmatrix <- slam::as.simple_triplet_matrix(Matrix::sparseMatrix(i=c(rowindex1, rowindex2, slackrowindex2, slackrowindex1), j=c(colindex1, colindex2, slackcolindex2, slackcolindex1), x = 1, dimnames = list(c(seller_id,buyer_id), seq_len(flowlength + nBuyers + nSellers))))
  
  rm(pc_table)
  gc()
  
  if(firstrun){

    ## Create a new problem
    tradeproblem <- initProbGLPK()
    
    ## Set the direction of the objective function
    setObjDirGLPK(tradeproblem, GLP_MIN)
    
    ## Add the number of rows and columns
    addRowsGLPK(tradeproblem, nBuyers+nSellers)
    addColsGLPK(tradeproblem, nBuyers)
    addColsGLPK(tradeproblem, nSellers)
    addColsGLPK(tradeproblem, flowlength)
    
    ## Set the objective coefficients
    setObjCoefsGLPK(tradeproblem, seq_len(flowlength + nBuyers + nSellers), as.double(objective))
    
    ## Set the columns and rows bounds
    setColsBndsGLPK(tradeproblem,
                    seq_len(flowlength + nBuyers + nSellers), 
                    lb = as.double(rep(0, flowlength + nBuyers + nSellers)), 
                    ub = as.double(rep(max(rhsoutput1, rhsoutput2), flowlength + nBuyers + nSellers)))
    
    setRowsBndsGLPK(tradeproblem,
                    seq_len(nSellers + nBuyers),
                    lb = as.double(c(rep(0, nSellers), rhsoutput2)), 
                    ub = as.double(c(rhsoutput1, rep(Inf, nBuyers))))
    
    ## Load constraint matrix
    loadMatrixGLPK(tradeproblem, 
                   as.integer(length(constraintmatrix$i)), 
                   as.integer(constraintmatrix$i), 
                   as.integer(constraintmatrix$j), 
                   as.double(constraintmatrix$v))
    
    ## Set the row names
    setRowsNamesGLPK(tradeproblem,
                     as.integer(seq_len(nSellers + nBuyers + nSellers)),
                     c(seller_id, buyer_id))
    
    
    ## Set Simplex Control Parameters
    
    ## Solve the problem
    solveSimplexGLPK(tradeproblem)
    
    sellers <- seller_id[getRowsDualGLPK(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColsPrimGLPK(tradeproblem)[1:nBuyers]
    buyers <- buyer_id[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(seller_id = sellers),
                data.table(buyer_id = buyers),
                status))
    
  } else {
    
    # Retrieve the original problem
    tradeproblem <- lp_problems
    
    # Retrieve the original number of columns
    num_columns <- getNumColsGLPK(tradeproblem)
    
    ## Add the new columns
    addColsGLPK(tradeproblem, flowlength - (num_columns - nBuyers))
    
    ## Set the objective coefficients
    setObjCoefsGLPK(tradeproblem, colindex1, as.double(objective))
    
    ## Set the columns bounds
    setColsBndsGLPK(tradeproblem, 
                    colindex1, 
                    lb = as.double(rep(0, flowlength)), 
                    ub = as.double(rep(Inf, flowlength)))
    
    ## Load constraint matrix
    loadMatrixGLPK(tradeproblem, 
                   as.integer(length(constraintmatrix$i)), 
                   as.integer(constraintmatrix$i), 
                   as.integer(constraintmatrix$j), 
                   as.double(constraintmatrix$v))
    
    ## Set Simplex Control Parameters
    
    ## Solve the problem
    solveSimplexGLPK(tradeproblem)
    
    sellers <- seller_id[getRowsDualGLPK(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColsPrimGLPK(tradeproblem)[1:nBuyers]
    buyers <- buyer_id[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(seller_id = sellers),
                data.table(buyer_id = buyers),
                status))
  }
}

SolveFlowCLP <- function(pc_table, prodcg, conscg, lp_problems, firstrun = TRUE){
  # Get the number of variables
  seller_id <- prodcg[, seller_id]
  buyer_id <- conscg[, buyer_id]
  nSellers <- length(seller_id)
  nBuyers <- length(buyer_id)
  flowlength <- pc_table[,.N]
  sellerweights <- c(1,0.75,0.5,0.25)[prodcg[pc_table, tonquant, on = "seller_id"]]
  buyerweigths <- c(1,0.75,0.5,0.25)[conscg[pc_table, tonquant, on = "buyer_id"]]
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
  # Column 1 will be i=1,j=1, Column 2 will be i=2, j=1, until Column=length(seller_id) where i=length(seller_id), j = 1
  
  rowindex1 <- match(pc_table$seller_id, seller_id)
  rhsoutput1 <- pc_table[,.N, .(seller_id)][prodcg, output_capacity_tons, on = "seller_id"]
  
  rowindex2 <- match(pc_table$buyer_id, buyer_id)
  rhsoutput2 <- pc_table[,.N, .(buyer_id)][conscg, purchase_amount_tons, on = "buyer_id"]
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
    
    sellers <- seller_id[getRowDualCLP(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColPrimCLP(tradeproblem)[1:nBuyers]
    buyers <- buyer_id[buyersslack > 0]
    ifelse(sum(buyersslack) > 0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(seller_id = sellers), 
                data.table(buyer_id = buyers),
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
    
    sellers <- seller_id[getRowDualCLP(tradeproblem)[1:nSellers] == 0]
    buyersslack <- getColPrimCLP(tradeproblem)[1:nBuyers]
    buyers <- buyer_id[buyersslack > 0]
    ifelse(sum(buyersslack)>0, status <- 1, status <- 0)
    
    lp_problems <- tradeproblem
    
    return(list(data.table(seller_id = sellers),
                data.table(buyer_id = buyers),
                lp_problems,
                status))
    
  }
}

