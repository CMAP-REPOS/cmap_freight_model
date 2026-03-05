sc_sim_pmg <- function(naics_set){

  t0 <- Sys.time()
  
  # Run the PMGs:
  # Three methods available.
  # (1) C++ code BASE_PMG_TYPE == 1
  # (2) R code BASE_PMG_TYPE == 2
  # (3) R code with simplified/fast function, BASE_PMG_TYPE == 3
  
  # Write out the PMG ini file
  fwrite(PMGParameters[variable != "pmglogging"], file = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
         sep = "=", row.names = FALSE, col.names = FALSE)
  pmgparameters.writelog <- ifelse(PMGParameters[PMGParameters$variable == "pmglogging"]$value == 1, TRUE, FALSE)

  # Expanded set of market-group combinations
  naics_set_expanded <- data.table(NAICS = rep(naics_set$NAICS, naics_set$groups),
                                   SCTG = rep(naics_set$SCTG, naics_set$groups),
                                   Market = rep(naics_set$Market, naics_set$groups),
                                   Group = unlist(lapply(naics_set$groups, seq, from=1)))
  naics_set_expanded[, Market_Group := paste(Market, Group, sep = "_")]
  
  if(USER_PMG_CORES > 1){
    require(parallel)
    
    clust <- makeCluster(USER_PMG_CORES)
    
    clusterCall(clust, 
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH)
    
    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
    clusterExport(clust, 
                  c("runPMG", "pmg_simple", "pmg_start"), 
                  envir = environment())
    
    naicslist <- parLapplyLB(clust, 
                             1:nrow(naics_set_expanded), 
                             function(x){
                               
                               # Remove old outputs and log files if they exist
                               market <- as.character(naics_set_expanded$Market[x]) 
                               g <- naics_set_expanded$Group[x]
                               
                               if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))){
                                 file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))
                               }
                               if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market,"_g", g, ".txt")))){
                                 file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".txt")))
                               }
                               
                               # Apply the pmg function
                               if(BASE_PMG_TYPE == 1){
                                 runPMG(market = as.character(naics_set_expanded$Market[x]), 
                                      groupnum = naics_set_expanded$Group[x], 
                                      writelog = pmgparameters.writelog, 
                                      wait = TRUE, 
                                      pmgexe = file.path(SYSTEM_PMG_PATH,"pmg.exe"),
                                      inipath = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
                                      inpath = SCENARIO_OUTPUT_PATH, 
                                      outpath = SCENARIO_OUTPUT_PATH,
                                      logpath = SCENARIO_OUTPUT_PATH)
                               } else if(BASE_PMG_TYPE == 2){
                                 pmg_start(market = as.character(naics_set_expanded$Market[x]), 
                                          g = naics_set_expanded$Group[x]) 
                               } else {
                                 pmg_simple(market = as.character(naics_set_expanded$Market[x]), 
                                           g = naics_set_expanded$Group[x]) 
                               }
                             },
                             chunk.size = 1)
    stopCluster(clust)
    
  } else {
    
    naicslist <- lapply(1:nrow(naics_set_expanded), 
                        function(x){
                          
                          print(paste(x,
                                      as.character(naics_set_expanded$Market[x]),
                                      naics_set_expanded$Group[x]))
                          
                          # Remove old outputs and log files if they exist
                          market <- as.character(naics_set_expanded$Market[x]) 
                          g <- naics_set_expanded$Group[x]
                          
                          if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))){
                            file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".out.csv")))
                          }
                          if(file.exists(file.path(SCENARIO_OUTPUT_PATH, paste0(market,"_g", g, ".txt")))){
                            file.remove(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".txt")))
                          }
                          
                          # Apply the pmg function
                          if(BASE_PMG_TYPE == 1){
                            runPMG(market = as.character(naics_set_expanded$Market[x]), 
                                   groupnum = naics_set_expanded$Group[x], 
                                   writelog = pmgparameters.writelog, 
                                   wait = TRUE, 
                                   pmgexe = file.path(SYSTEM_PMG_PATH,"pmg.exe"),
                                   inipath = file.path(SYSTEM_PMG_PATH,"PMG.ini"), 
                                   inpath = SCENARIO_OUTPUT_PATH, 
                                   outpath = SCENARIO_OUTPUT_PATH,
                                   logpath = SCENARIO_OUTPUT_PATH)
                          } else if(BASE_PMG_TYPE == 2){
                            pmg_start(market = as.character(naics_set_expanded$Market[x]), 
                                      g = naics_set_expanded$Group[x]) 
                          } else {
                            pmg_simple(market = as.character(naics_set_expanded$Market[x]), 
                                       g = naics_set_expanded$Group[x]) 
                          }
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_pmg.csv"))
  naics_missing <- naics_set_expanded[!Market_Group %in% naics_completed]$Market_Group
  if(length(naics_missing) > 0) cat("Market-Group combinations missing from market simulation in sc_sim_pmg: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  return(naics_set)
}

#' Builds the system call to the PMG application and runs the application
#'
#' Builds the systems call including the command line options to run the PMG
#' application for a particular NAICs market and group sample from within the
#' full set of buyers and sellers in that NAICS market.
#' 
#' @param naics_io_code BEA io code for the commodity to be run, i.e., that matches with the filenaming used for the buy/sell/costs files (character string).
#' @param groupnum is the sample group numbers for the group to be run, i.e., that matches with the numbering used for the buy/sell/costs files (integer).
#' @param writelog TRUE/FALSE to indicate whether to capture standard output from the PMG application is a text file.
#' @param invisible TRUE/FALSE to indicate whether to show the command window or not.
#' @param wait TRUE/FALSE to indicate whether to R should wait for the PMG application to finish, or (if false) should run the PMG application asynchronously.
#' @param pmgexe Path to the pmg executable, defaults to "./PMG/pmg.exe"
#' @param inipath Path to the ini file, defaults to "./PMG/PMG.ini"
#' @param inpath Path to the PMG inputs folder, defaults to "./outputs"
#' @param outpath Path to the PMG outputs folder, defaults to "./outputs"
#' @param logpath Path to the log file folder, defaults to "./outputs"
#' @keywords PMG
#' @export
#' @examples
#' \dontrun{
#' runPMG(naics,g,writelog=FALSE,wait=TRUE)
#' }

runPMG <- function(market,groupnum=NA,writelog=FALSE,invisible=TRUE,wait=FALSE,
                   pmgexe="./PMG/pmg.exe",inipath="./PMG/pmg.ini",inpath="./outputs",outpath="./outputs",
                   logpath="./outputs"){
  
  #location of PMG executable: in the PMG folder, called PMG.exe
  pmgexe <- gsub("/","\\",pmgexe,fixed=TRUE)
  
  #command line options
  # 1.  Specify ini file path:
  #     -i C:\path\to\file\pmg.ini 
  inipath <- gsub("/","\\",inipath,fixed=TRUE)
  
  # 2. specify data input and output file name prefixes 
  # -p market
  ioprefix <- market
  if(!is.na(groupnum)) ioprefix <- paste0(market,"_g",groupnum)
  
  # 3. specify data directory path for input files files 
  # location of market.buy.csv, market.sell.csv and  market.costs.csv
  # -d C:\path\to\inputs
  inpath <- gsub("/","\\",inpath,fixed=TRUE)
  
  # 4. specify directory path for output file
  # locations of naics_io_code.out.csv
  # -o C:\path\to\outputs
  outpath <- gsub("/","\\",outpath,fixed=TRUE)
  #
  # divert stout to log for this run
  logcall <- ""
  logpath <- gsub("/","\\",logpath,fixed=TRUE)
  if(writelog) logcall <- paste0(logpath,"\\",market,"_g",groupnum,".txt")
  
  # build system call:
  system2(pmgexe,
          args = paste("-i",inipath,"-p",ioprefix,"-d",inpath,"-o",outpath), 
          stdout = logcall,  
          invisible = invisible, 
          wait = wait)
  
  return(paste(market, groupnum, sep = "_"))
  
}

pmg_simple <- function(market, g){
  
 # Simplified PMG function to create a direct allocation set of trades
 # Use the sampled trade groups (costs) and buy/sell files to very simply
 # Simulate a reasonably uniform set of trades that meet purchase requirements
 # And are drawn from the sampled trade groups in a way that approximate distance
 # And mode distribution. The purpose of this approach is for fast, approximated 
 # Scenario runs and to provide a reasonable point of comparison to understand 
 # Variability introduced by running the PMG in different ways/with different settings
  
  # read the buy, sell, and costs files for this market
  buy <- fread(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".buy.csv")))
  sell <- fread(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".sell.csv")))
  costs <- fread(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".costs.csv")))
  
  # Naming
  setnames(costs, c("sellerid", "buyerid"), c("SellerID", "BuyerID"))
  
  # Initialize fields
  buy[, PurchaseAmountTons_Rem := PurchaseAmountTons]
  sell[, OutputCapacityTons_Rem := OutputCapacityTons]
  
  # Create Trades table
  trades <- costs[,.(SellerID, BuyerID, Attribute1_UnitCost, Attribute2_ShipTime)]
  trades[sell, c("OutputCapacityTons", "OutputCapacityTons_Rem", "NonTransportUnitCost") := 
           .(i.OutputCapacityTons, i.OutputCapacityTons_Rem, i.NonTransportUnitCost), 
         on = "SellerID"]
  trades[buy, c("PurchaseAmountTons", "PurchaseAmountTons_Rem", "PrefWeight1_UnitCost", "PrefWeight2_ShipTime", "SingleSourceMaxFraction") := 
           .(i.PurchaseAmountTons, i.PurchaseAmountTons_Rem, i.PrefWeight1_UnitCost, i.PrefWeight2_ShipTime, i.SingleSourceMaxFraction), 
         on = "BuyerID"]
  trades[, MaxQuantity := pmax(pmin(PurchaseAmountTons, PurchaseAmountTons_Rem, OutputCapacityTons_Rem),0)]
  trades[, PurchaseCost := MaxQuantity * NonTransportUnitCost]
  trades[, LogisticsCost := MaxQuantity * Attribute1_UnitCost]
  
  # create a utility and select the best trade for the buyer
  # index the logistics cost and shipping time by buyer and calculate a weighted utility
  # include preference for larger capacity suppliers
  trades[, c("IndexUnitCost", "IndexShipTime", "IndexOutputCapacity") := .(Attribute1_UnitCost/mean(Attribute1_UnitCost),
                                                                           Attribute2_ShipTime/mean(Attribute2_ShipTime),
                                                                           OutputCapacityTons_Rem/mean( OutputCapacityTons_Rem)), 
         by = BuyerID]
  
  trades[, Utility := - (PrefWeight1_UnitCost * IndexUnitCost + 
                           PrefWeight2_ShipTime * IndexShipTime)]
  
  # Rank the utilities for each buyer, order trades, and calculate the cumulative trade quantity
  trades[, BRank := frank(-Utility), by = BuyerID]
  setorder(trades, BuyerID, BRank)
  trades[, CumQuantity := cumsum(MaxQuantity), by = BuyerID]
  trades[, StartQuantity := shift(CumQuantity, 1, 0), by = BuyerID]
  trades[StartQuantity < PurchaseAmountTons_Rem, Trade := 1]
    
  # Pick the best trades for each buyer up to their total purchase requirement
  trades[Trade == 1, PurchaseQuantity := ifelse(CumQuantity < PurchaseAmountTons_Rem, 
                                                MaxQuantity, 
                                                PurchaseAmountTons_Rem - StartQuantity)]
    
  # # check
  # trades[Trade == 1, sum(PurchaseQuantity)]
  # buy[, sum(PurchaseAmountTons)]
  
  # Create the formatted table for output
  setnames(trades, c("SellerID", "BuyerID"), c("SellerId", "BuyerId"))
  trades[, Quantity.Traded := ifelse(is.na(Trade), 0, PurchaseQuantity)]
  trades[, Number.of.Trades := ifelse(is.na(Trade), 0, 1)]
  trades[, Last.Iteration.Quantity := Quantity.Traded]
  
  # Write the out.csv with the iteration results and trade quantities
  fwrite(trades[order(BuyerId, SellerId),
                .(BuyerId, SellerId, Quantity.Traded, Number.of.Trades, Last.Iteration.Quantity)],
         file = file.path(SCENARIO_OUTPUT_PATH,paste0(market, "_g", g, ".out.csv")))
  
  return(paste(market, g, sep = "_")) 
  
}
  


pmg_start <- function(market, g){
  
  # ### check the output from the run_pmg function
  # pmgout <- fread(file = file.path(SCENARIO_OUTPUT_PATH, 
  #                                  paste0(market, "_g", g, ".out.csv")))
  # pmgoutb <- unique(pmgout$BuyerId)
  # pmgouts <- unique(pmgout$SellerId)
  # pmgoutt <- pmgout[Last.Iteration.Quantity > 0,.(Traded = sum(Last.Iteration.Quantity)), keyby = BuyerId]
  # 
  # compbuy <- merge(buy[,.(BuyerID, PurchaseAmountTons)],
  #                  pmgoutt[,.(BuyerID = BuyerId, Traded)],
  #                  by = "BuyerID", all = TRUE)
  # 
  # buynotsat <- compbuy[is.na(Traded),.(PurchaseAmountTons = sum(PurchaseAmountTons))]
  # buysat <- compbuy[!is.na(Traded),.(PurchaseAmountTons = sum(PurchaseAmountTons))]
  # buyreq <- compbuy[,.(PurchaseAmountTons = sum(PurchaseAmountTons))]
  # sellcap <- sell[,.(OutputCapacityTons = sum(OutputCapacityTons))]
  # buyreq/sellcap
  # buysat/sellcap
  # buynotsat/sellcap
  # # Lots of capacity by high amount not traded
  
  # PMG parameters object in memory
  # PMGParameters
  pmg.iter.max <- PMGParameters[variable == "IMax"]$value
  
  # Weight to guide the trades towards large suppliers to reduce overselling adjustments needed
  pmg.supplier.size.beta <- 0.00
  
  # load the pc table for this market and group
  # pc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")),
  #                as.data.table = TRUE)
  
  # read the buy, sell, and costs files for this market
  buy <- fread(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".buy.csv")))
  sell <- fread(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".sell.csv")))
  costs <- fread(file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".costs.csv")))
  
  # Iterative trading games
  # costs file is structured with the set of sampled seller options for each buyer
  # Attribute1_UnitCost = unit logistics cost
  # Attribute2_ShipTime = shiptime in days
  # cost is unit transportation cost only
  
  # Iteration:
  # Calculate a utility for each offer from seller to buyer
  # Select the buyers preferred trade
  
  # Naming
  setnames(costs, c("sellerid", "buyerid"), c("SellerID", "BuyerID"))
  
  # Initialize fields
  buy[, PurchaseAmountTons_Rem := PurchaseAmountTons]
  
  buy[, SingleSourceMaxFraction := 1.00]
  
  sell[, OutputCapacityTons_Rem := OutputCapacityTons]
  costs[, TradedFlag := 0]
  costs[, SoldOutFlag := 0]
  
  # List to hold iteration trades:
  trades.iter <- list()
  
  # Stop conditions for the iterations:
  # iter = max iterations
  # all purchase requirements met
  purchase_requirements_rem <- sum(buy$PurchaseAmountTons_Rem)
  output_capacity_rem <- sum(sell$OutputCapacityTons_Rem)
  iter = 1
  print(paste("Iteration:", iter, ", Purchase Amount Remaining:", purchase_requirements_rem, "Output Capacity Remaining:", output_capacity_rem))
  
  while(iter <= pmg.iter.max * 100 & purchase_requirements_rem > 0){
    
    # Create Trades table
    trades <- costs[TradedFlag == 0,.(SellerID, BuyerID, Attribute1_UnitCost, Attribute2_ShipTime, TradedFlag, SoldOutFlag)]
    trades[sell, c("OutputCapacityTons", "OutputCapacityTons_Rem", "NonTransportUnitCost") := 
             .(i.OutputCapacityTons, i.OutputCapacityTons_Rem, i.NonTransportUnitCost), 
           on = "SellerID"]
    trades[buy, c("PurchaseAmountTons", "PurchaseAmountTons_Rem", "PrefWeight1_UnitCost", "PrefWeight2_ShipTime", "SingleSourceMaxFraction") := 
             .(i.PurchaseAmountTons, i.PurchaseAmountTons_Rem, i.PrefWeight1_UnitCost, i.PrefWeight2_ShipTime, i.SingleSourceMaxFraction), 
           on = "BuyerID"]
    trades[, MaxQuantity := pmax(pmin(round(PurchaseAmountTons * SingleSourceMaxFraction), PurchaseAmountTons_Rem, OutputCapacityTons_Rem),0)]
    trades[, PurchaseCost := MaxQuantity * NonTransportUnitCost]
    trades[, LogisticsCost := MaxQuantity * Attribute1_UnitCost]
    
    # create a utility and select the best trade for the buyer
    # index the logistics cost and shipping time by buyer and calculate a weighted utility
    # include preference for larger capacity suppliers
    trades[, c("IndexUnitCost", "IndexShipTime", "IndexOutputCapacity") := .(Attribute1_UnitCost/mean(Attribute1_UnitCost),
                                                    Attribute2_ShipTime/mean(Attribute2_ShipTime),
                                                    OutputCapacityTons_Rem/mean( OutputCapacityTons_Rem)), 
               by = BuyerID]
    
    trades[, Utility := - (PrefWeight1_UnitCost * IndexUnitCost + 
                             PrefWeight2_ShipTime * IndexShipTime - 
                             pmg.supplier.size.beta * IndexOutputCapacity + 
                             1e6 * TradedFlag)]
    
    # iterate here through a series of buy and sell rankings and selections of trades
    trades_list <- list()
    #iter_buy_sell <- 1
    max_iter_buy_sell <- 20
    allow_sold_out_sellers <- FALSE # will dynamically change to true if needed to clear the market
    
    ### change this to a while loop unitl the market clears...
    for(iter_buy_sell in 1:max_iter_buy_sell){
    
    # Rank the utilities for each buyer, order trades, and calculate the cumulative trade quantity
    trades[, BRank := frank(-Utility), by = BuyerID]
    setorder(trades, BuyerID, BRank)
    trades[, CumQuantity := cumsum(MaxQuantity), by = BuyerID]
    trades[, StartQuantity := shift(CumQuantity, 1, 0), by = BuyerID]
    trades[StartQuantity < PurchaseAmountTons_Rem, Trade := 1]
    
    # Pick the best trades for each buyer up to their total purchase requirement
    trades[Trade == 1, PurchaseQuantity := ifelse(CumQuantity < PurchaseAmountTons_Rem, 
                                                  MaxQuantity, 
                                                  PurchaseAmountTons_Rem - StartQuantity)]

    # # update the remaining capacity and requirements after the trades
    # trades.iter.buyer <- trades[Trade == 1, .(NumTrades = .N, Quantity.Iter = sum(PurchaseQuantity)), keyby = BuyerID]
    # trades.iter.seller <- trades[Trade == 1, .(NumTrades = .N, Quantity.Iter = sum(PurchaseQuantity)), keyby = SellerID]
    # 
    # buy[trades.iter.buyer, PurchaseAmountTons_Rem := PurchaseAmountTons_Rem - i.Quantity.Iter, on = "BuyerID"]
    # sell[trades.iter.seller, OutputCapacityTons_Rem := OutputCapacityTons_Rem - i.Quantity.Iter, on = "SellerID"]
    # costs[trades[Trade == 1], TradedFlag := TradedFlag + i.Trade, on = c("SellerID", "BuyerID")]
    # 
    # check for buyers who have all of their requirements fulfilled,
    # and for any that don't. For the buy iteration, all should be fulfilled
    # check for any oversold sellers (can happen in an iteration if make trades to multiple buyers)
    # buy[PurchaseAmountTons_Rem <= 0]
    # buy[PurchaseAmountTons_Rem > 0]
    # sell[OutputCapacityTons_Rem <= 0][order(OutputCapacityTons_Rem)]
    
    # buy[PurchaseAmountTons_Rem < 0]
    # sell[OutputCapacityTons_Rem < 0][order(-OutputCapacityTons_Rem)]
    # sell[OutputCapacityTons_Rem > 0][order(-OutputCapacityTons_Rem)]
    # sell[,.(OutputCapacityTons = sum(OutputCapacityTons),
    #         OutputCapacityTons_Rem  = sum(OutputCapacityTons_Rem))]
    
    # there are a lot of oversold sellers
    # need to add code here to adjust the markets that those sellers are in
    # can the oversold sellers be replaced by sellers with capacity?
    # find the oversold suppliers and order their trades by the markets with the most
    # capacity by undersold suppliers, and switch to undersold suppliers the have the highest utility
    # do this iteratively to equilibrate on minimum amount of overselling
  
  
    run_this <- FALSE
    
    if(run_this){
      for(iter_adj in 1:10){
      
    # Add the output capacity after the trades to a new copy of the trades table
    trades.sell.adj <- copy(trades)
    trades.sell.adj[sell, OutputCapacityTons_Rem := i.OutputCapacityTons_Rem, on = "SellerID"]
    
    # Calculate the shortfall set of trades for each Buyer, i.e., those from sellers who are oversold
    #trades.sell.adj[Trade == 1, .(CapacityUsed = sum(PurchaseQuantity)), by = BuyerID]
    trades.shortfall.buyer <- trades.sell.adj[Trade == 1 & OutputCapacityTons_Rem < 0, .(CapacityShortfall = sum(PurchaseQuantity)), by = BuyerID]
    trades.capacity.avail <- trades.sell.adj[is.na(Trade) & OutputCapacityTons_Rem > 0, .(CapacityAvailable = sum(OutputCapacityTons_Rem)), by = BuyerID]
    # for the buyers with some shortfalls, is there enough capacity in their market
    trades.shortfall.buyer[trades.capacity.avail, CapacityAvailable := i.CapacityAvailable, on = "BuyerID"]
    trades.shortfall.buyer[, IsCapacity := CapacityAvailable - CapacityShortfall]
    trades.shortfall.buyer[IsCapacity < 0]  ## could just ignore these -- allow suppliers to go slightly over cap?)
    
    # iteration: 
    # if the buyer has a trade from an overcapacity seller,
    # remove the lowest utility trade from an overcapacity seller
    # add the highest utility trade from a capacity available seller (or trades as required to get to purhcase requirements)
    
    # move a shipment, recalculate....with a limit?
    
    # Tag the shipments from overcap sellers
    trades.sell.adj[Trade == 1 & OutputCapacityTons_Rem < 0, TradesOverCap := BRank]
    # Which is the worst ranked in each Buyer market?
    trades.sell.adj[trades.sell.adj[, .I[TradesOverCap < max(TradesOverCap, na.rm = TRUE)], by = BuyerID]$V1,
                    TradesOverCap := NA]
    # Add that as the required quantity
    trades.sell.adj[trades.sell.adj[!is.na(TradesOverCap), 
                                    .(TransferQuantity = sum(PurchaseQuantity)),
                                    keyby = BuyerID],
                    TransferQuantityRequired := i.TransferQuantity, on = "BuyerID"]
    
    # which are the best ranked shipments on sellers with capacity needed to reach the TransferQuantity
    # Tag the shipments from undercap sellers
    trades.sell.adj[is.na(Trade) & OutputCapacityTons_Rem > 0, TradesUnderCap := BRank]
    # Calculate the quantity required to meet the Transfer Quantity
    trades.sell.adj[!is.na(TradesUnderCap), CumTransferQuantity := cumsum(MaxQuantity), by = BuyerID]
    trades.sell.adj[!is.na(TradesUnderCap), StartTransferQuantity := shift(CumTransferQuantity, 1, 0), by = BuyerID]
    trades.sell.adj[StartTransferQuantity < TransferQuantityRequired, TransferTrade := 1]
    
    trades.sell.adj[TransferTrade == 1, TransferQuantity := ifelse(CumTransferQuantity < TransferQuantityRequired, 
                                                  MaxQuantity, 
                                                  TransferQuantityRequired - StartTransferQuantity)]
    
    # Create adjusted trade fields and purchase quantity fields with the updated shipments
    trades.sell.adj[, TradeAdj := Trade]
    trades.sell.adj[!is.na(TradesOverCap), TradeAdj := NA]
    trades.sell.adj[!is.na(TransferTrade), TradeAdj := 1]
    trades.sell.adj[, PurchaseQuantityAdj := ifelse(!is.na(TransferQuantity), 
                                                   TransferQuantity,
                                                   PurchaseQuantity * TradeAdj)]
    # check if the adjustments mean that the market is not cleared (all requirements fulfilled)
    # if it is revert to the original trades
    trades.sell.adj[trades.sell.adj[,.(PurchaseQuantityAdj = sum(PurchaseQuantityAdj,na.rm = TRUE)), by = BuyerID],
                    PurchaseAmountTons_RemAdj := PurchaseAmountTons - i.PurchaseQuantityAdj,
                    on = "BuyerID"]
    trades.sell.adj[PurchaseAmountTons_RemAdj > 0, TradeAdj := Trade]
    trades.sell.adj[PurchaseAmountTons_RemAdj > 0, PurchaseQuantityAdj := PurchaseQuantity * TradeAdj]
    
    # Is overselling reduced?
    trades.iter.buyer.adj <- trades.sell.adj[TradeAdj == 1, .(NumTrades = .N, Quantity.Iter = sum(PurchaseQuantityAdj)), keyby = BuyerID]
    trades.iter.seller.adj <- trades.sell.adj[TradeAdj == 1, .(NumTrades = .N, Quantity.Iter = sum(PurchaseQuantityAdj)), keyby = SellerID]
    
    buy[trades.iter.buyer.adj, PurchaseAmountTons_RemAdj := PurchaseAmountTons - i.Quantity.Iter, on = "BuyerID"]
    sell[, OutputCapacityTons_RemAdj := as.numeric(OutputCapacityTons)]
    sell[trades.iter.seller.adj, OutputCapacityTons_RemAdj := OutputCapacityTons_RemAdj - i.Quantity.Iter, on = "SellerID"]
    trades.sell.adj[sell, OutputCapacityTons_RemAdj := i.OutputCapacityTons_RemAdj, on = "SellerID"]
    
    # Calculate the shortfall set of trades for each Buyer, i.e., those from sellers who are oversold
    trades.shortfall.buyer.adj <- trades.sell.adj[TradeAdj == 1 & OutputCapacityTons_RemAdj < 0, .(CapacityShortfall = sum(PurchaseQuantityAdj)), by = BuyerID]
    trades.capacity.avail.adj <- trades.sell.adj[is.na(TradeAdj) & OutputCapacityTons_RemAdj > 0, .(CapacityAvailable = sum(OutputCapacityTons_RemAdj)), by = BuyerID]
    
    trades.sell.adj[,sum(PurchaseQuantity, na.rm = TRUE)]
    trades.sell.adj[,sum(PurchaseQuantityAdj, na.rm = TRUE)]
    
    # sell[,.(OutputCapacityTons = sum(OutputCapacityTons),
    #         OutputCapacityTons_Rem  = sum(OutputCapacityTons_Rem),
    #         OutputCapacityTons_RemAdj = sum(OutputCapacityTons_RemAdj))]
    
    over_cap_last <- sell[OutputCapacityTons_Rem < 0,.(OutputCapacityTons = sum(OutputCapacityTons),
            OutputCapacityTons_Rem  = sum(OutputCapacityTons_Rem))]$OutputCapacityTons_Rem
    over_cap_this <- sell[OutputCapacityTons_RemAdj < 0,.(OutputCapacityTons = sum(OutputCapacityTons),
                                      OutputCapacityTons_RemAdj  = sum(OutputCapacityTons_RemAdj))]$OutputCapacityTons_RemAdj
    
    sell[, OutputCapacityTons_Rem := OutputCapacityTons_RemAdj]
    sell[, OutputCapacityTons_RemAdj := NULL]

    # update the trades table with the adjusted trades and iterate
    trades[trades.sell.adj, c("Trade", "PurchaseQuantity") := .(i.TradeAdj, i.PurchaseQuantityAdj), on = c("BuyerID", "SellerID")]
    print(paste0(over_cap_last, over_cap_this, over_cap_last - over_cap_this))
    
    }
    }
      
    ### Iterative buyer market adjustments to seller overselling are not converging n no overselling
    # Alternative approach?
    
    # seller by seller adjustment? (could be slow)
    # could prioritize the trades to check the cumulative capacity available
    # and allocate capacity only up to being at capacity?
    # The just need to shuffle a bit when some capacity needed to meet requirements?
    
    # at this point we have a set of potential trades
    # as well as ordered markets with the most preferred trades
    # check for overselling by seller and select the set of achievable trades to meet
    # the output capacity
    
    # buy[PurchaseAmountTons_Rem > 0]
    # sell[OutputCapacityTons_Rem <= 0][order(OutputCapacityTons_Rem)]
    # sell[OutputCapacityTons_Rem > 0][order(OutputCapacityTons_Rem)]
    # sell[OutputCapacityTons_Rem <= 0, sum(OutputCapacityTons_Rem)]
    # sell[OutputCapacityTons_Rem > 0, sum(OutputCapacityTons_Rem)]
    
    trades[, SRank := frank(-Utility), by = SellerID]
    setorder(trades, SellerID, SRank)
    trades[, SellQuantity := ifelse(!is.na(PurchaseQuantity), PurchaseQuantity, 0)]
    trades[, CumQuantitySeller := cumsum(SellQuantity), by = SellerID]
    trades[, StartQuantitySeller := shift(CumQuantitySeller, 1, 0), by = SellerID]
    if(!allow_sold_out_sellers){
    trades[StartQuantitySeller < OutputCapacityTons_Rem & SellQuantity > 0, TradeSeller := 1]
    } else {
      ### test this just effectively resets the seller capacity but doesn't allow 
      # for them to supply shipments larger that their total output
      trades[StartQuantitySeller < OutputCapacityTons & SellQuantity > 0, TradeSeller := 1]  
    }
      
    # Trades for each buyer and seller
    # trades[Trade == 1, .(NumTrades = .N, PurchaseQuantity = sum(PurchaseQuantity)), by = TradeSeller]
    
    ### Trades that are trade == 1 and TradeSeller == 1 are ok
    ### both preferred for meeting purchase requirements AND
    ### within seller capacity
    
    # what next? reset the purchase reqs and seller capacities after those trades
    # and select the next set of trades?
    
    # do this iteratively.
    # update the sell and buy tables with capacities and purchase requirements
    # put the selected trades into a list
    # create a new trades table and do it again
    trades[Trade == 1 & TradeSeller == 1, TradedFlag := 1] 
    
    # update the remaining capacity and requirements after the trades
    trades.iter.buyer <- trades[TradedFlag == 1, .(NumTrades = .N, Quantity.Iter = sum(PurchaseQuantity)), keyby = BuyerID]
    trades.iter.seller <- trades[TradedFlag == 1, .(NumTrades = .N, Quantity.Iter = sum(PurchaseQuantity)), keyby = SellerID]
    
    buy[trades.iter.buyer, PurchaseAmountTons_Rem := PurchaseAmountTons_Rem - i.Quantity.Iter, on = "BuyerID"]
    sell[trades.iter.seller, OutputCapacityTons_Rem := OutputCapacityTons_Rem - i.Quantity.Iter, on = "SellerID"]
    costs[trades[TradedFlag == 1], TradedFlag := TradedFlag + i.Trade, on = c("SellerID", "BuyerID")]
    
    trades_list[[iter_buy_sell]] <- trades[TradedFlag == 1]
    
    # remove the accepted trades and reset the trades table
    trades <- trades[TradedFlag == 0]
    trades[, c("BRank", "CumQuantity", "StartQuantity", "Trade",                  
               "PurchaseQuantity", "SRank", "CumQuantitySeller",
               "SellQuantity", "StartQuantitySeller", "TradeSeller") := NULL]
    ### should just do this once at the top of iteration...
    trades[sell, c("OutputCapacityTons", "OutputCapacityTons_Rem", "NonTransportUnitCost") := 
             .(i.OutputCapacityTons, i.OutputCapacityTons_Rem, i.NonTransportUnitCost), 
           on = "SellerID"]
    trades[buy, c("PurchaseAmountTons", "PurchaseAmountTons_Rem", "PrefWeight1_UnitCost", "PrefWeight2_ShipTime", "SingleSourceMaxFraction") := 
             .(i.PurchaseAmountTons, i.PurchaseAmountTons_Rem, i.PrefWeight1_UnitCost, i.PrefWeight2_ShipTime, i.SingleSourceMaxFraction), 
           on = "BuyerID"]
    
    # remove entirely any buyers who have their purchase requirements met
    trades <- trades[PurchaseAmountTons_Rem > 0]
    
    if(!allow_sold_out_sellers){
      
      # update fields
      trades[, MaxQuantity := pmax(pmin(round(PurchaseAmountTons * SingleSourceMaxFraction), PurchaseAmountTons_Rem, OutputCapacityTons_Rem),0)]
      trades[, PurchaseCost := MaxQuantity * NonTransportUnitCost]
      trades[, LogisticsCost := MaxQuantity * Attribute1_UnitCost]
      
      # For any sellers who are sold out, reduce the utility of their offers -- they
      # can be picked if they are the only option but strong preference for other sellers unlikely
      trades[OutputCapacityTons_Rem <= 0, SoldOutFlag := 1]
      trades[, Utility := Utility - 1e6 * SoldOutFlag]
    }  
    # check if there are any trades left SoldoutFlag == 0
    # if not, update the soldoutflag, revert utilty
    if(nrow(trades[SoldOutFlag == 0]) == 0){
      
      trades[, SoldOutFlag := 0]
      allow_sold_out_sellers <- TRUE
      trades[, Utility := - (PrefWeight1_UnitCost * IndexUnitCost + 
                               PrefWeight2_ShipTime * IndexShipTime - 
                               pmg.supplier.size.beta * IndexOutputCapacity + 
                               1e6 * TradedFlag)]
    }
      
    if(allow_sold_out_sellers){
      
      # update fields
      trades[, MaxQuantity := pmax(pmin(round(PurchaseAmountTons * SingleSourceMaxFraction), PurchaseAmountTons_Rem),0)]
      trades[, PurchaseCost := MaxQuantity * NonTransportUnitCost]
      trades[, LogisticsCost := MaxQuantity * Attribute1_UnitCost]
      
    }  
    
    purchase_requirements_rem <- sum(buy$PurchaseAmountTons_Rem)
    output_capacity_rem <- sum(sell$OutputCapacityTons_Rem)
    print(paste("Iteration:", iter_buy_sell, ", Purchase Amount Remaining:", purchase_requirements_rem, "Output Capacity Remaining:", output_capacity_rem, "Allow Sold Out Sellers:", allow_sold_out_sellers))
    
    } # end of iter_buy_sell
    
    # what is left after the for iter_buy_sell
    trades
    buy[PurchaseAmountTons_Rem > 0][order(-PurchaseAmountTons_Rem)]
    uniqueN(buy[PurchaseAmountTons_Rem > 0]$BuyerID)
    uniqueN(buy[PurchaseAmountTons_Rem == 0]$BuyerID)
    sell[OutputCapacityTons_Rem > 0][order(-OutputCapacityTons_Rem)]
    uniqueN(sell[OutputCapacityTons_Rem > 0]$SellerID)
    uniqueN(sell[OutputCapacityTons_Rem < 0]$SellerID)
    sell[OutputCapacityTons_Rem < 0][order(OutputCapacityTons_Rem)][1:50]
    
    buy[,.(PurchaseAmountTons = sum(PurchaseAmountTons),
           PurchaseAmountTons_Rem = sum(PurchaseAmountTons_Rem))][,.(PurchaseAmountTons_Rem/PurchaseAmountTons)]
    sell[,.(OutputCapacityTons = sum(OutputCapacityTons),
            OutputCapacityTons_Rem = sum(OutputCapacityTons_Rem))][,.(OutputCapacityTons_Rem/OutputCapacityTons)]
    
    # where is the overcapacity and undercapacity located
    sell[,.(OutputCapacityTons = sum(OutputCapacityTons),
         OutputCapacityTons_Rem = sum(OutputCapacityTons_Rem)), keyby = FAFZONE][order(-OutputCapacityTons_Rem)]
    
    TAZ_System <- fread(file.path(SYSTEM_DATA_PATH, "TAZ_System.csv"))
    FAF_Zones <- unique(TAZ_System[,.(FAFZONE, FAFNAME)])
    FAF_Zones[FAFZONE == 20]
    
    sell[FAFZONE == 20]
    tradesfinal <- rbindlist(trades_list, idcol = "iternum")
    tradesfinal[,.N, by = iternum]
    buy[BuyerID %in% tradesfinal[SellerID == 461727]$BuyerID]
    costs[BuyerID == 10779777]
    tradesfinal[BuyerID == 10779777 & SellerID == 461727]
    tradesfinal[BuyerID == 10779777][order(-PurchaseQuantity)]
    buy[BuyerID == 10779777]
    costs[BuyerID == 10779777 & TradedFlag == 1]
    
    
    #-----------------------------------------
    
    # #### TEMP
    buy[PurchaseAmountTons_Rem <= 0, PurchaseAmountTons_Rem := 0]
    sell[OutputCapacityTons_Rem <= 0, OutputCapacityTons_Rem := 0]
    costs[BuyerID %in% buy[PurchaseAmountTons_Rem == 0]$BuyerID, TradedFlag := 1]
    costs[SellerID %in% sell[OutputCapacityTons_Rem == 0]$SellerID, TradedFlag := 1]
    
    # save the trades from this iteration
    trades.iter.costs <- trades[Trade == 1, .(SellerID, BuyerID, Quantity.Iter = MaxQuantity)]
    trades.iter[[iter]] <- trades.iter.costs
    
    # update the iteration conditions:
    purchase_requirements_rem <- sum(buy$PurchaseAmountTons_Rem)
    output_capacity_rem <- sum(sell$OutputCapacityTons_Rem)
    print(paste("Iteration:", iter, ", Purchase Amount Remaining:", purchase_requirements_rem, "Output Capacity Remaining:", output_capacity_rem))
    iter = iter + 1
  }
  
  ### problem is that the buyers samples run out....
  ### can we add sellers when that happens? plenty of capacity in the market
  ### end of each iteration add sellers for buyers who still have purchase requirements but no sellers left?
  ### pick sellers based on how overcapacity they are? Those in the sets with the lowest purchase requirements relative to capacity?
  ### problem is we don't know the costs for that new pairing!
  
  ### TODO:
  # what is the correct utility structure?
  # game behavior?
  # constraint parameters: what does each one do? How to implement?
  # checks on full clearing of market so all purchase amounts satisfied
  
  # Write the out.csv with the iteration results and trade quantities
  fwrite(trades[order(BuyerId, SellerId),
                .(BuyerId, SellerId, Quantity.Traded, Number.of.Trades, Last.Iteration.Quantity)],
         file = file.path(SCENARIO_OUTPUT_PATH,paste0(market, "_g", g, ".out.csv")))
  
  return(paste(market, g, sep = "_"))
}
