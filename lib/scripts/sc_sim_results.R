# Functions to check results of sc_sim components

### Markets --------------------------------------------------------------------------

sc_sim_markets_check <- function(naics_set){
  
  # parse the market script
  log.market <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_market.txt"))))
  close(con)
  log.market.com <- log.market[grep("Finished Create_Markets_market", LogLine)]
  
  # Add market, groups, and elapsed time fields
  split_market_line <- function(finished_text){
    splits <- c("market-", ", Elapsed", " ")
    splittextvec <- gsub(",", "", finished_text)
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(5,10,11)]) 
  }
  
  log.market.com[, c("Market", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_market_line)))]
  log.market.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  
  # cycle through the completed markets and grabs some stats
  # for all of the groups in that have completed, cycle through and check
  naics_set[log.market.com, MarketRT := i.RuntimeSec, on = "Market"]
  naics_set[, MarketStep := ifelse(is.na(MarketRT), FALSE, TRUE)]
  
  # check the outputs for each market/group with each market
  # paricularly, check total output cap, purchase amount, ratio, by group
  # domestic and international amounts and check the balance between those has been resolved correctly
  group_summary <- list()
  
  for(market in naics_set$Market){
    
    print(market)
    market_completed_market <- naics_set[Market == market]$MarketStep
    #should contain: consc, prodc
    if(market_completed_market){
      
      load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
      
      # check the results in the market file
      naics_set[Market == market, c("MarketProdN", "MarketProdOCT") := .(prodc[, .N], sum(prodc$OutputCapacityTons))]
      naics_set[Market == market, c("MarketConsN", "MarketConsPAT") := .(consc[, .N], sum(consc$PurchaseAmountTons))]
      
      naics_set[Market == market, 
                c("MarketConsProdRatio", "MarketOCTPATRatio") := 
                  .(MarketConsN/MarketProdN, MarketProdOCT/MarketConsPAT)]
      
      # update the number of groups based on what was created by the market step after rounding
      naics_set[Market == market, groups := max(consc$numgroups, na.rm = TRUE)]
      
      # repeat the checks by group
      group_summary[[market]] <- prodc[, .(MarketProdN = .N, MarketProdOCT = sum(OutputCapacityTons)), by = group]
      group_summary[[market]][consc[, .(MarketConsN = .N, MarketConsPAT = sum(PurchaseAmountTons)), by = group],
                    c("MarketConsN", "MarketConsPAT") := .(i.MarketConsN, i.MarketConsPAT), on = "group"]
      
      # summarize domestic and foreign 
      group_summary[[market]][prodc[DomFor == "Domestic", .(OCTDom = sum(OutputCapacityTons)), by = group],
                    OCTDom := i.OCTDom, on = "group"]
      group_summary[[market]][prodc[DomFor == "Foreign", .(OCTFor = sum(OutputCapacityTons)), by = group],
                    OCTFor := i.OCTFor, on = "group"]
      group_summary[[market]][consc[DomFor == "Domestic", .(PATDom = sum(PurchaseAmountTons)), by = group],
                    PATDom := i.PATDom, on = "group"]
      group_summary[[market]][consc[DomFor == "Foreign", .(PATFor = sum(PurchaseAmountTons)), by = group],
                    PATFor := i.PATFor, on = "group"]
      
      group_summary[[market]][is.na(group_summary[[market]])] <- 0
      
      group_summary[[market]][, CDomRatio := ifelse(PATDom != 0, (OCTDom + OCTFor - PATFor)/PATDom, 999)]
      group_summary[[market]][, CForRatio := ifelse(PATFor != 0, (OCTDom + OCTFor - PATDom)/PATFor, 999)]
      group_summary[[market]][, PDomCForRatio := ifelse(PATFor != 0, OCTDom/PATFor, 999)]
      
      test_base_opr <- BASE_OUTPUT_PURCHASE_RATIO * 0.99999 # to avoid flagging approx equality
      group_summary[[market]][, MarketCapShortfall := ifelse(CDomRatio < test_base_opr | CForRatio < test_base_opr | PDomCForRatio < test_base_opr, TRUE, FALSE)]
      
      # tag with Market name
      group_summary[[market]][, Market := market]
      
      # rm the tables
      rm(prodc, consc)
    }
  }
  
  group_summary <- rbindlist(group_summary)
  
  # add group summary fields to naics_set_groups
  group_summary[, Market_Group := paste(Market, group, sep = "_")]
  
  naics_set_groups <- data.table(NAICS = rep(naics_set$NAICS, naics_set$groups),
                                 Commodity_SCTG = rep(naics_set$Commodity_SCTG, naics_set$groups),
                                 Market = rep(naics_set$Market, naics_set$groups),
                                 Group = unlist(lapply(naics_set$groups, seq, from=1)))
  naics_set_groups[, Market_Group := paste(Market, Group, sep = "_")]
  
  naics_set_groups[group_summary, names(group_summary)[2:13] := 
                     .(i.MarketProdN, i.MarketProdOCT, i.MarketConsN, i.MarketConsPAT, i.OCTDom, i.OCTFor, i.PATDom,
                       i.PATFor, i.CDomRatio, i.CForRatio, i.PDomCForRatio, i.MarketCapShortfall), on = "Market_Group"]
  
  # save the groups summary - csv form this step and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_markets.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
}

### Buy Sell --------------------------------------------------------------------------

sc_sim_buy_sell_check <- function(naics_set){
  
  # parse the buy_sell script
  log.buysell <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_buy_sell.txt"))))
  close(con)
  log.buysell.com <- log.buysell[grep("Finished Create_Buy_Sell_market", LogLine)]
  
  # Add market, groups, and elapsed time fields
  split_buysell_line <- function(finished_text){
    splits <- c("market-", "-groups-", " ")
    splittextvec <- gsub(",", "", finished_text)
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,11,12)]) 
  }
  
  log.buysell.com[, c("Market", "Groups", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_buysell_line)))]
  log.buysell.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  
  # cycle through the completed markets and grabs some stats
  # for all of the groups in that have completed, cycle through and check
  naics_set[log.buysell.com, BuySellRT := i.RuntimeSec, on = "Market"]
  naics_set[, BuySell := ifelse(is.na(BuySellRT), FALSE, TRUE)]
  
  # after buy sell:
  # there is a workspace called Market.Rdata
  # load the market
  for(market in naics_set$Market){
    
    print(market)
    market_completed_buysell <- naics_set[Market == market]$BuySell
    #should contains: consc, prodc
    if(market_completed_buysell){
      load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
      # check the results in the market file
      naics_set[Market == market, c("BuySellProdN", "BuySellProdOCT") := .(prodc[, .N], sum(prodc$OutputCapacityTons))]
      naics_set[Market == market, c("BuySellConsN", "BuySellConsPAT") := .(consc[,.N], sum(consc$PurchaseAmountTons))]
      # rm the tables
      rm(prodc, consc)
    }
  }
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # check whether all of the buy and sell .csv files were actually produced, seperately from the check
  # on the overall workspace
  buyfiles <- list.files(path = SCENARIO_OUTPUT_PATH, pattern = ".buy.csv")
  sellfiles <- list.files(path = SCENARIO_OUTPUT_PATH, pattern = ".sell.csv")
  
  # parse out the names and add a field to naics_set_groups
  # that indicates whether the buy and sell files exist
  buysplitlist <- strsplit(buyfiles, "[[:punct:]]")
  buyfilesdt <- data.table(NAICS = unlist(lapply(buysplitlist, "[", 1)),
                           Commodity_SCTG = as.integer(unlist(lapply(buysplitlist, "[", 2))),
                           Group = as.integer(sub("g", "", unlist(lapply(buysplitlist, "[", 3)))))
  
  sellsplitlist <- strsplit(sellfiles, "[[:punct:]]")
  sellfilesdt <- data.table(NAICS = unlist(lapply(sellsplitlist, "[", 1)),
                            Commodity_SCTG = as.integer(unlist(lapply(sellsplitlist, "[", 2))),
                            Group = as.integer(sub("g", "", unlist(lapply(sellsplitlist, "[", 3)))))
  
  naics_set_groups[buyfilesdt, BuyFileExists := TRUE, on = c("NAICS", "Commodity_SCTG", "Group")]
  naics_set_groups[sellfilesdt, SellFileExists := TRUE, on = c("NAICS", "Commodity_SCTG", "Group")]
  
  naics_set_groups[is.na(BuyFileExists), BuyFileExists := FALSE]
  naics_set_groups[is.na(SellFileExists), SellFileExists := FALSE] 
  
  # save the groups summary - csv form this step and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_buysell.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
}

### Sampling --------------------------------------------------------------------------

sc_sim_sampling_check <- function(naics_set){
  
  # parse the sampling scripts
  log.samp <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_sampling.txt"))))
  close(con)
  log.samp.com <- log.samp[grep("Finished Create_PC_Samples_market", LogLine)]
  
  # Add market, group, and elapsed time fields
  split_sampling_line <- function(finished_text){
    splits <- c("market-", "-group-", "-of", "submitted: ", " ", " ")
    splittextvec <- finished_text
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,10,11)]) 
  }
  
  log.samp.com[, c("Market", "Group", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_sampling_line)))]
  log.samp.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  log.samp.com[, Market_Group := paste(Market, Group, sep = "_")]
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # cycle through the completed markets and grabs some stats
  # for all of the groups in that have completed, cycle through and check
  naics_set_groups[log.samp.com, SamplingRT := i.RuntimeSec, on = "Market_Group"]
  naics_set_groups[, Sampling := ifelse(is.na(SamplingRT), FALSE, TRUE)]
  
  # after sampling:
  # there is a workspace called Market_gGroup.Rdata
  # load the market
  for(market_group in naics_set_groups[Sampling == TRUE]$Market_Group){
    
    print(market_group)
    market <- naics_set_groups[Market_Group == market_group]$Market
    g <- naics_set_groups[Market_Group == market_group]$Group
    
    #should contains: conscg, pc, prodcg
    load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
    
    if(exists("prodcg") & exists("conscg")){
      naics_set_groups[Market_Group == market_group, 
                       c("SampProdN", "SampProdOCT", "SampConsN", "SampConsPAT") := 
                         .(prodcg[,.N],
                           sum(prodcg$OutputCapacityTons),
                           conscg[,.N],
                           sum(conscg$PurchaseAmountTons))]
      
      naics_set_groups[Market_Group == market_group, 
                       c("SampConsProdRatio", "SampOCTPATRatio") := 
                         .(SampConsN/SampProdN, SampProdOCT/SampConsPAT)]
      rm(prodcg, conscg)
    }
    
    if(exists("pc")){
      naics_set_groups[Market_Group == market_group, SampPCtable := TRUE]
      
      naics_set_groups[Market_Group == market_group, 
                       c("SampCombs", "SampUnSeller", "SampUnBuyer") := 
                         .(pc[,.N],
                           pc[,length(unique(SellerID))],
                           pc[,length(unique(BuyerID))])]
      
      naics_set_groups[Market_Group == market_group, 
                       c("SampMissSeller", "SampMissBuyer") := 
                         .(SampProdN - SampUnSeller, SampConsN - SampUnBuyer)]
      naics_set_groups[Market_Group == market_group, 
                       c("SampMinSeller", "SampMaxSeller") := 
                         pc[, .(SampSize = .N), by = BuyerID][,.(SampMinSeller = min(SampSize), SampMaxSeller = max(SampSize))]]
      
      naics_set_groups[Market_Group == market_group, 
                       SampAvSeller := SampCombs/SampUnBuyer]
      
      naics_set_groups[Market_Group == market_group, 
                       c("SampOCTPATRatioMin", "SampOCTPATRatioMax") := 
                         pc[, .(OCTPATRatio = sum(OutputCapacityTons)/(sum(PurchaseAmountTons)/.N)), by = BuyerID][,.(SampOCTPATRatioMin = min(OCTPATRatio), SampOCTPATRatioMax = max(OCTPATRatio))]]
      
      naics_set_groups[Market_Group == market_group, 
                       c("SampOCTPATRatioLT1") := 
                         ifelse(SampOCTPATRatioMin >= 1, 0, pc[, .(OCTPATRatio = sum(OutputCapacityTons)/(sum(PurchaseAmountTons)/.N)), by = BuyerID][OCTPATRatio < 1,.N])]
      # rm the tables
      rm(pc)
    } else {
      naics_set_groups[Market_Group == market_group, SampPCtable := FALSE]
    }
  }
  
  # move some of the summary stats into the market level table (naics_set)
  naics_set[naics_set_groups[, .(Sampling = ifelse(any(Sampling==FALSE), FALSE, TRUE),
                       SamplingRT = sum(SamplingRT, na.rm = TRUE),           
                       SampCombs = sum(SampCombs, na.rm = TRUE), 
                       SampMissSeller = sum(SampMissSeller, na.rm = TRUE),
                       SampMissBuyer = sum(SampMissBuyer, na.rm = TRUE),
                       SampOCTPATRatioLT1 = sum(SampMissBuyer, na.rm = TRUE)), 
                   by = Market],
            c("Sampling", "SamplingRT", "SampCombs", "SampMissSeller", "SampMissBuyer", "SampOCTPATRatioLT1") := 
              .(i.Sampling, i.SamplingRT, i.SampCombs, i.SampMissSeller, i.SampMissBuyer, i.SampOCTPATRatioLT1),
            on = "Market"]
  
  # save the groups summary - csv and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_sampling.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
  
}

### Dist Channel --------------------------------------------------------------------------

sc_sim_distchannel_check <- function(naics_set){
  
  # parse the distribution channel scripts
  log.dist <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_distchannel.txt"))))
  close(con)
  log.dist.com <- log.dist[grep("Finished Apply_Distchannel_market", LogLine)]
  
  # Add market, group, and elapsed time fields
  split_dist_line <- function(finished_text){
    splits <- c("market-", "-group-", "-of", "submitted: ", " ", " ")
    splittextvec <- finished_text
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,10,11)]) 
  }
  
  log.dist.com[, c("Market", "Group", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_dist_line)))]
  log.dist.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  log.dist.com[, Market_Group := paste(Market, Group, sep = "_")]
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # cycle through the completed markets and grabs some stats
  # for all of the groups in that have completed, cycle through and check
  naics_set_groups[log.dist.com, DistChannelRT := i.RuntimeSec, on = "Market_Group"]
  naics_set_groups[, DistChannel := ifelse(is.na(DistChannelRT), FALSE, TRUE)]
  
  # there is a workspace called Market_gGroup.Rdata
  # load the market
  for(market_group in naics_set_groups[DistChannel == TRUE]$Market_Group){
    
    print(market_group)
    market <- naics_set_groups[Market_Group == market_group]$Market
    g <- naics_set_groups[Market_Group == market_group]$Group
    
    #should contains: conscg, pc, prodcg
    load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
    
    if(exists("pc")){
      
      naics_set_groups[Market_Group == market_group, DistChannelPCtable := TRUE] 
      
      naics_set_groups[Market_Group == market_group, 
                       c("DistChannelCombs", "DistChannelMissing") := 
                         .(pc[,.N], pc[is.na(distchannel),.N])]
      
      naics_set_groups[Market_Group == market_group, 
                       c("DistChannel1", "DistChannel2", "DistChannel3", "DistChannel4") := 
                         .(pc[distchannel == 1,.N], pc[distchannel == 2,.N], pc[distchannel == 3,.N], pc[distchannel == 4,.N])]
      # rm the tables
      rm(pc)
    } else {
      naics_set_groups[Market_Group == market_group, DistChannelPCtable := FALSE]
    }
  }
  
  # move some of the summary stats into the market level table (naics_set)
  naics_set[naics_set_groups[, .(DistChannel = ifelse(any(DistChannel==FALSE), FALSE, TRUE),
                                 DistChannelRT = sum(DistChannelRT, na.rm = TRUE),           
                                 DistChannelCombs = sum(DistChannelCombs, na.rm = TRUE), 
                                 DistChannelMissing = sum(DistChannelMissing, na.rm = TRUE),
                                 DistChannel1 = sum(DistChannel1, na.rm = TRUE),
                                 DistChannelGt1 = sum(DistChannel2, DistChannel3, DistChannel4, na.rm = TRUE)), 
                             by = Market],
            c("DistChannel", "DistChannelRT", "DistChannelCombs", "DistChannelMissing", "DistChannel1", "DistChannelGt1") := 
              .(i.DistChannel, i.DistChannelRT, i.DistChannelCombs, i.DistChannelMissing, i.DistChannel1, i.DistChannelGt1),
            on = "Market"]
  
  # save the groups summary - csv and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_distchannel.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
  
}

### Mode Choice --------------------------------------------------------------------------

sc_sim_modechoice_check <- function(naics_set){
  
  # parse the mode choice scripts
  log.mode <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_modechoice.txt"))))
  close(con)
  log.mode.com <- log.mode[grep("Finished Apply_Mode_Choice_market", LogLine)]
  
  # Add market, group, and elapsed time fields
  split_mode_line <- function(finished_text){
    splits <- c("market-", "-group-", "-of", "submitted: ", " ", " ")
    splittextvec <- finished_text
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,10,11)]) 
  }
  
  log.mode.com[, c("Market", "Group", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_mode_line)))]
  log.mode.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  log.mode.com[, Market_Group := paste(Market, Group, sep = "_")]
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # cycle through the completed markets and grabs some stats
  # for all of the groups in that have completed, cycle through and check
  naics_set_groups[log.mode.com, ModeChoiceRT := i.RuntimeSec, on = "Market_Group"]
  naics_set_groups[, ModeChoice := ifelse(is.na(ModeChoiceRT), FALSE, TRUE)]
  
  # there is a workspace called Market_gGroup.Rdata
  # load the market
  for(market_group in naics_set_groups[ModeChoice == TRUE]$Market_Group){
    
    print(market_group)
    market <- naics_set_groups[Market_Group == market_group]$Market
    g <- naics_set_groups[Market_Group == market_group]$Group
    
    #should contains: conscg, pc, prodcg
    load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
    
    if(exists("pc")){
      
      naics_set_groups[Market_Group == market_group, ModeChoicePCtable := TRUE] 
      
      naics_set_groups[Market_Group == market_group, 
                       c("ModeChoiceCombs", "ModeChoiceMissing") := 
                         .(pc[,.N], pc[is.na(Mode.Domestic),.N])]
      
      naics_set_groups[Market_Group == market_group,
                       c("AverageShipWeight", "Mode.Truck", "Mode.Other") :=
                         .(mean(pc$weight, na.rm = TRUE), pc[Mode.Domestic == "Truck",.N], pc[Mode.Domestic != "Truck",.N])]
      # rm the tables
      rm(pc)
    } else {
      naics_set_groups[Market_Group == market_group, ModeChoicePCtable := FALSE]
    }
  }
  
  # move some of the summary stats into the market level table (naics_set)
  naics_set[naics_set_groups[, .(ModeChoice = ifelse(any(ModeChoice == FALSE), FALSE, TRUE),
                                 ModeChoiceRT = sum(ModeChoiceRT, na.rm = TRUE),           
                                 ModeChoiceCombs = sum(ModeChoiceCombs, na.rm = TRUE), 
                                 ModeChoiceMissing = sum(ModeChoiceMissing, na.rm = TRUE),
                                 AverageShipWeight = mean(AverageShipWeight, na.rm = TRUE),
                                 Mode.Truck = sum(Mode.Truck, na.rm = TRUE),
                                 Mode.Other = sum(Mode.Other, na.rm = TRUE)), 
                             by = Market],
            c("ModeChoice", "ModeChoiceRT", "ModeChoiceCombs", "ModeChoiceMissing", "AverageShipWeight", "Mode.Truck", "Mode.Other") := 
              .(i.ModeChoice, i.ModeChoiceRT, i.ModeChoiceCombs, i.ModeChoiceMissing, i.AverageShipWeight, i.Mode.Truck, i.Mode.Other),
            on = "Market"]
  
  # save the groups summary - csv and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_modechoice.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
  
}

### Costs --------------------------------------------------------------------------

sc_sim_costs_check <- function(naics_set){
  
  # parse the costs script
  log.costs <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_costs.txt"))))
  close(con)
  log.costs.com <- log.costs[grep("Finished Create_Costs_market", LogLine)]
  
  # Add market, groups, and elapsed time fields
  split_costs_line <- function(finished_text){
    splits <- c("market-", "-group-", "-of", "submitted: ", " ")
    splittextvec <- finished_text
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,10,11)]) 
  }
  
  log.costs.com[, c("Market", "Groups", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_costs_line)))]
  log.costs.com[, RuntimeUnit := sub(",","", RuntimeUnit)]
  log.costs.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  
  naics_set[log.costs.com, CostsRT := i.RuntimeSec, on = "Market"]
  naics_set[, Costs := ifelse(is.na(CostsRT), FALSE, TRUE)]
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # check whether all of the cost .csv files were actually produced
  costsfiles <- list.files(path = SCENARIO_OUTPUT_PATH, pattern = ".costs.csv")
  
  # parse out the names and add a field to naics_set_groups
  # that indicates whether the costs files exist
  costssplitlist <- strsplit(costsfiles, "[[:punct:]]")
  costsfilesdt <- data.table(NAICS = unlist(lapply(costssplitlist, "[", 1)),
                             Commodity_SCTG = as.integer(unlist(lapply(costssplitlist, "[", 2))),
                             Group = as.integer(sub("g", "", unlist(lapply(costssplitlist, "[", 3)))))
  
  naics_set_groups[costsfilesdt, CostsFileExists := TRUE, on = c("NAICS", "Commodity_SCTG", "Group")]
  naics_set_groups[is.na(CostsFileExists), CostsFileExists := FALSE]
  
  # save the groups summary - csv form this step and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_costs.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
}

### PMG --------------------------------------------------------------------------

sc_sim_pmg_check <- function(naics_set){
  
  # parse the pmg script
  log.pmg <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_pmg.txt"))))
  close(con)
  log.pmg.com <- log.pmg[grep("Finished RunPMG_market", LogLine)]
  
  # Add market, groups, and elapsed time fields
  split_pmg_line <- function(finished_text){
    splits <- c("market-", "-group-", "-of", "submitted: ", " ")
    splittextvec <- finished_text
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,10,11)]) 
  }
  
  log.pmg.com[, c("Market", "Groups", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_pmg_line)))]
  log.pmg.com[, RuntimeUnit := sub(",","", RuntimeUnit)]
  log.pmg.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  
  naics_set[log.pmg.com, PMGRT := i.RuntimeSec, on = "Market"]
  naics_set[, PMG := ifelse(is.na(PMGRT), FALSE, TRUE)]
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # check whether all of the out .csv files were actually produced
  outfiles <- list.files(path = SCENARIO_OUTPUT_PATH, pattern = ".out.csv")
  
  # parse out the names and add a field to naics_set_groups
  # that indicates whether the costs files exist
  outsplitlist <- strsplit(outfiles, "[[:punct:]]")
  outfilesdt <- data.table(NAICS = unlist(lapply(outsplitlist, "[", 1)),
                           Commodity_SCTG = as.integer(unlist(lapply(outsplitlist, "[", 2))),
                           Group = as.integer(sub("g", "", unlist(lapply(outsplitlist, "[", 3)))))
  
  naics_set_groups[outfilesdt, PMGOutFileExists := TRUE, on = c("NAICS", "Commodity_SCTG", "Group")]
  naics_set_groups[is.na(PMGOutFileExists), PMGOutFileExists := FALSE]
  
  # save the groups summary - csv form this step and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_pmg.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
}

### Shipments --------------------------------------------------------------------------

sc_sim_shipments_check <- function(naics_set){
  
  # parse the shipments scripts
  log.ship <- data.table(LogLine = readLines(con <- file(file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_shipments.txt"))))
  close(con)
  log.ship.com <- log.ship[grep("Finished Process_PMG_Outputs_market", LogLine)]
  
  # Add market, group, and elapsed time fields
  split_ship_line <- function(finished_text){
    splits <- c("market-", "-group-", "-of", "submitted: ", " ", " ")
    splittextvec <- finished_text
    for(splittext in splits) splittextvec <- c(splittextvec, unlist(strsplit(splittextvec[length(splittextvec)], split = splittext)))
    return(splittextvec[c(4,6,10,11)]) 
  }
  
  log.ship.com[, c("Market", "Group", "Runtime", "RuntimeUnit") := data.table(t(sapply(LogLine, split_ship_line)))]
  log.ship.com[, RuntimeSec := c(1,60,3600)[match(RuntimeUnit,c("secs","mins","hours"))] * as.numeric(Runtime)]
  log.ship.com[, Market_Group := paste(Market, Group, sep = "_")]
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # cycle through the completed markets and grabs some stats
  # for all of the groups in that have completed, cycle through and check
  naics_set_groups[log.ship.com, ShipmentsRT := i.RuntimeSec, on = "Market_Group"]
  naics_set_groups[, Shipments := ifelse(is.na(ShipmentsRT), FALSE, TRUE)]
  
  # there is a workspace called Market_gGroup.Rdata
  # load the market
  for(market_group in naics_set_groups[ModeChoice == TRUE]$Market_Group){
    
    print(market_group)
    market <- naics_set_groups[Market_Group == market_group]$Market
    g <- naics_set_groups[Market_Group == market_group]$Group
    
    #should contains: conscg, prodcg, pc, pc_pairs
    load(file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, ".Rdata")))
    
    if(exists("prodcg")){
      
      naics_set_groups[Market_Group == market_group, ShipmentsProdtable := TRUE] 
      
      naics_set_groups[Market_Group == market_group, 
                       c("ShipmentsProdN", "ShipmentsProdOCT") := 
                         .(prodcg[,.N], sum(prodcg$OutputCapacityTons, na.rm = TRUE))]
      
      # rm the tables
      rm(prodcg)
    } else {
      naics_set_groups[Market_Group == market_group, ShipmentsProdtable := FALSE]
    }
    
    if(exists("conscg")){
      
      naics_set_groups[Market_Group == market_group, ShipmentsConstable := TRUE] 
      
      naics_set_groups[Market_Group == market_group, 
                       c("ShipmentsConsN", "ShipmentsConsPAT") := 
                         .(conscg[,.N], sum(conscg$PurchaseAmountTons, na.rm = TRUE))]
      
      # rm the tables
      rm(conscg)
    } else {
      naics_set_groups[Market_Group == market_group, ShipmentsConstable := FALSE]
    }
    
    if(exists("pc")){
      
      naics_set_groups[Market_Group == market_group, ShipmentsPCtable := TRUE] 
      
      naics_set_groups[Market_Group == market_group, 
                       ShipmentsPCCombs := pc[,.N]]
      
      # rm the tables
      rm(pc)
    } else {
      naics_set_groups[Market_Group == market_group, ShipmentsPCtable := FALSE]
    }
    
    if(exists("pc_pairs")){
      
      naics_set_groups[Market_Group == market_group, ShipmentsPCPairstable := TRUE] 
      
      naics_set_groups[Market_Group == market_group, 
                       c("ShipmentsPCPairsCombs", "ShipmentsPCPairsTraded") := 
                         .(pc_pairs[,.N], sum(pc_pairs$Last.Iteration.Quantity, na.rm = TRUE))]
      
      naics_set_groups[Market_Group == market_group,
                       c("ShipmentsProdUnusedCap", "ShipmentsConsShortfall") := 
                         .(ShipmentsProdOCT - ShipmentsPCPairsTraded, ShipmentsConsPAT - ShipmentsPCPairsTraded)]
      
      # rm the tables
      rm(pc_pairs)
    } else {
      naics_set_groups[Market_Group == market_group, ShipmentsPCPairstable := FALSE]
    }
    
  }
  
  # move some of the summary stats into the market level table (naics_set)
  naics_set[naics_set_groups[, .(Shipments = ifelse(any(Shipments == FALSE), FALSE, TRUE),
                                 ShipmentsRT = sum(ShipmentsRT, na.rm = TRUE),           
                                 ShipmentsProdN = sum(ShipmentsProdN, na.rm = TRUE), 
                                 ShipmentsProdOCT = sum(ShipmentsProdOCT, na.rm = TRUE), 
                                 ShipmentsConsN = sum(ShipmentsConsN, na.rm = TRUE), 
                                 ShipmentsConsPAT = sum(ShipmentsConsPAT, na.rm = TRUE), 
                                 ShipmentsPCCombs = sum(ShipmentsPCCombs, na.rm = TRUE),
                                 ShipmentsPCPairsCombs = sum(ShipmentsPCPairsCombs, na.rm = TRUE),
                                 ShipmentsPCPairsTraded = sum(ShipmentsPCPairsTraded, na.rm = TRUE),
                                 ShipmentsConsShortfall = sum(ShipmentsConsShortfall, na.rm = TRUE),
                                 ShipmentsProdUnusedCap = sum(ShipmentsProdUnusedCap, na.rm = TRUE)), 
                             by = Market],
            c("Shipments", "ShipmentsRT", "ShipmentsProdN", "ShipmentsProdOCT", "ShipmentsConsN", "ShipmentsConsPAT",
              "ShipmentsPCCombs", "ShipmentsPCPairsCombs", "ShipmentsPCPairsTraded", "ShipmentsConsShortfall", "ShipmentsProdUnusedCap") := 
              .(i.Shipments, i.ShipmentsRT, i.ShipmentsProdN, i.ShipmentsProdOCT, i.ShipmentsConsN, i.ShipmentsConsPAT, 
                i.ShipmentsPCCombs, i.ShipmentsPCPairsCombs, i.ShipmentsPCPairsTraded, i.ShipmentsConsShortfall, i.ShipmentsProdUnusedCap),
            on = "Market"]
  
  # save the groups summary - csv and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_shipments.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
  
}

### Shipments --------------------------------------------------------------------------

sc_sim_ports_check <- function(naics_set, BuyerSupplierPairs){
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # Check number of records with assigned ports by market and group
  naics_set_groups[BuyerSupplierPairs[!is.na(Port_mesozone),.N, by = .(Market, Group)], PortsAssigned := i.N, on = c("Market", "Group")]
  
  # Add this to naics_set
  naics_set[naics_set_groups[, .(PortsAssigned = sum(PortsAssigned, na.rm = TRUE)), by = Market],
            PortsAssigned := i.PortsAssigned, on = "Market"]
  
  # save the groups summary - csv and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_ports.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
  
}

### Incremental Consumers --------------------------------------------------------------------------

sc_sim_increment_check <- function(naics_set, BuyerSupplierPairs){
  
  # load the naics_set_groups
  naics_set_groups <- readRDS(file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # Check number of records with assigned ports by market and group
  naics_set_groups[BuyerSupplierPairs[PMG_Incr == "PMG",.(ShipmentsPMGN = .N, 
                                                          ShipmentsPMGPAT = sum(Last.Iteration.Quantity, na.rm = TRUE)), 
                                                          by = .(Market, Group)],
                   c("ShipmentsPMGN","ShipmentsPMGPAT") := .(i.ShipmentsPMGN, i.ShipmentsPMGPAT), 
                   on = c("Market", "Group")]
  
  naics_set_groups[BuyerSupplierPairs[PMG_Incr == "Incr",.(ShipmentsIncrN = .N, 
                                                          ShipmentsIncrPAT = sum(Last.Iteration.Quantity, na.rm = TRUE)), 
                                      by = .(Market, Group)],
                   c("ShipmentsIncrN","ShipmentsIncrPAT") := .(i.ShipmentsIncrN, i.ShipmentsIncrPAT), 
                   on = c("Market", "Group")]
  
  # Add this to naics_set
  naics_set[naics_set_groups[, .(ShipmentsPMGN = sum(ShipmentsPMGN, na.rm = TRUE),
                                 ShipmentsPMGPAT  = sum(ShipmentsPMGPAT, na.rm = TRUE),
                                 ShipmentsIncrN  = sum(ShipmentsIncrN, na.rm = TRUE),
                                 ShipmentsIncrPAT  = sum(ShipmentsIncrPAT, na.rm = TRUE)),
                             by = Market],
            c("ShipmentsPMGN", "ShipmentsPMGPAT", "ShipmentsIncrN", "ShipmentsIncrPAT") := 
              .(i.ShipmentsPMGN, i.ShipmentsPMGPAT, i.ShipmentsIncrN, i.ShipmentsIncrPAT), 
            on = "Market"]
  
  # save the groups summary - csv and an rds to add to
  fwrite(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups_increment.csv"))
  saveRDS(naics_set_groups, file.path(SCENARIO_OUTPUT_PATH, "naics_set_groups.rds"))
  
  # return naics_set
  return(naics_set)
  
}
