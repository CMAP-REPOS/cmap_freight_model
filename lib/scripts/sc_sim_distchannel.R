sc_sim_distchannel <- function(naics_set, TAZGCD){
 
  t0 <- Sys.time()
  
  # Expanded set of market-group combinations
  naics_set_expanded <- data.table(NAICS = rep(naics_set$NAICS, naics_set$groups),
                                   SCTG = rep(naics_set$SCTG, naics_set$groups),
                                   Market = rep(naics_set$Market, naics_set$groups),
                                   Group = unlist(lapply(naics_set$groups, seq, from=1)))
  naics_set_expanded[, Market_Group := paste(Market, Group, sep = "_")]
  
  if(USER_COST_CORES > 1){
    require(parallel)
    
    clust <- makeCluster(USER_COST_CORES)
    
    clusterCall(clust, 
                fun = function(packages, lib) lapply(X = as.list(packages), FUN = library, character.only = TRUE, lib.loc = lib),
                packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH)
    
    clusterExport(clust, varlist = getGlobalVars(), envir = .GlobalEnv)
    
    clusterExport(clust, 
                  c("apply_distchannel",
                    "pc_sim_distchannel",
                    "predict_logit",
                    "distchannel_food",
                    "distchannel_mfg",
                    "distchan_calcats",
                    "distchannel_calibration",
                    "famesctg",
                    "mesozone_gcd"), 
                  envir = environment())
    
    naicslist <- parLapplyLB(clust, 
                             1:nrow(naics_set_expanded), 
                             function(x){
                               apply_distchannel(market = as.character(naics_set_expanded$Market[x]),
                                                 g = naics_set_expanded$Group[x],
                                                 TAZGCD = TAZGCD)
                             },
                             chunk.size = 1)
    stopCluster(clust)
    
  } else {
    
    naicslist <- lapply(1:nrow(naics_set_expanded), 
                        function(x){
                          
                          print(paste(x,
                                      as.character(naics_set_expanded$Market[x]),
                                      naics_set_expanded$Group[x]))
                          
                          apply_distchannel(market = as.character(naics_set_expanded$Market[x]),
                                            g = naics_set_expanded$Group[x],
                                            TAZGCD = TAZGCD)
                          
                        })
    
  }
  
  # Check that the complete set of naics groups were processed
  naics_completed <- unlist(naicslist)
  fwrite(data.table(Market_num = 1:length(naics_completed),
                    Market = naics_completed),
         file.path(SCENARIO_OUTPUT_PATH, "log_sc_sim_distchannel.csv"))
  naics_missing <- naics_set_expanded[!Market_Group %in% naics_completed]$Market_Group
  if(length(naics_missing) > 0) cat("Market-Group combinations missing from market simulation in sc_sim_distchannel: ", naics_missing)
  
  t1 <- Sys.time()
  
  cat(
    "\n", "Time taken: ",
    format(round(t1 - t0, 2), units = "mins")
  )
  
  return(naics_set)
  
}

apply_distchannel <- function(market, g, TAZGCD){
  
  # Load the files for this market and group
  conscg <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_consc.fst")),
                     as.data.table = TRUE)
  prodcg <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_prodc.fst")),
                     as.data.table = TRUE)
  
  pc <- read_fst(path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")),
                 as.data.table = TRUE)
  
  # Apply the distribution channel model
  print(paste(Sys.time(), "Applying distribution channel model to", market, "group", g))
  
  # call the distchannel function
  pc[pc_sim_distchannel(pc, prodcg, conscg, TAZGCD, 
                        distchannel_food = distchannel_food, 
                        distchannel_mfg = distchannel_mfg),
     distchannel := i.distchannel,
     on = c("SellerID", "BuyerID")]
  
  # Save pc
  write_fst(pc, path = file.path(SCENARIO_OUTPUT_PATH, paste0(market, "_g", g, "_pc.fst")))
  
  return(paste(market, g, sep = "_"))
}

# Distribution channel model
pc_sim_distchannel <- function(pc, prodcg, conscg, TAZGCD, distchannel_food, distchannel_mfg, calibration = NULL){
  
  ### Create variables used in the distribution channel model
  
  # Add required fields to pc
  pc[prodcg, 
     c("Seller.NAICS", "Seller.Size", "SCTG", "Production_zone") := 
       .(i.NAICS, i.Size, i.Commodity_SCTG, i.Zone),
     on = "SellerID"]
  
  pc[conscg,
     c("Buyer.NAICS", "Buyer.Size", "Consumption_zone") := 
       .(i.NAICS, i.Size , i.Zone),
     on = "BuyerID"]
  
  # Create employment and industry dummy variables
  pc[, c("emple49", "emp50t199", "empge200", "mfgind", "trwind", "whind") := 0L]
  pc[Buyer.Size <= 49, emple49 := 1]
  pc[Buyer.Size >= 50 & Buyer.Size <= 199, emp50t199 := 1]
  pc[Buyer.Size >= 200, empge200 := 1]
  
  pc[,Seller.NAICS2:=substr(Seller.NAICS,1,2)]
  pc[Seller.NAICS2 %in% 31:33, mfgind := 1]
  pc[Seller.NAICS2 %in% 48:49, trwind := 1]
  pc[Seller.NAICS2 %in% c(42, 48, 49), whind := 1]
  
  pc[,Buyer.NAICS2:=substr(Buyer.NAICS,1,2)]
  pc[Buyer.NAICS2 %in% 31:33, mfgind := 1]
  pc[Buyer.NAICS2 %in% 48:49, trwind := 1]
  pc[Buyer.NAICS2 %in% c(42, 48, 49), whind := 1]
  
  # Add the FAME SCTG category for comparison with calibration targets
  pc[, CATEGORY := famesctg[SCTG]]
  
  setkey(pc, Production_zone, Consumption_zone)
  
  # Add zone to zone distances (add average if there are any missing values)
  pc[TAZGCD[, .(Production_zone, Consumption_zone, GCD)], 
          Distance := i.GCD,
          on = c("Production_zone", "Consumption_zone")]
  pc[is.na(Distance), Distance := mean(TAZGCD$GCD, na.rm = TRUE)]
  
  print(paste(Sys.time(), "Applying distribution channel model"))
  
  # Apply choice model of distribution channel and iteratively adjust the ascs
  # The model estimated for mfg products was applied to all other SCTG commodities
  inNumber <- nrow(pc[SCTG %in% c(1:9)])
  
  if (inNumber > 0) {
    
    # Sort on vars so simulated choice is ordered correctly
    model_vars_food <- c("CATEGORY", distchannel_food[TYPE == "Variable", unique(VAR)])
    model_ascs_food <- distchannel_food[TYPE == "Constant", unique(VAR)]
    setkeyv(pc, model_vars_food) #sorted on vars, calibration coefficients, so simulated choice is ordered correctly
    
    pc_food <- pc[SCTG %in% c(1:9),model_vars_food,with=FALSE]
    pc_food_weight <- pc[SCTG %in% 1:9,PurchaseAmountTons]
    
    df <- pc_food[, list(Start = min(.I), Fin = max(.I)), by = model_vars_food] #unique combinations of model coefficients
    
    df[, (model_ascs_food) := 1] #add 1s for constants to each group in df
    
    print(paste(Sys.time(), nrow(df), "unique combinations"))
    
    if(!is.null(calibration)){
      
      pc[SCTG %in% c(1:9), distchannel := predict_logit(df, distchannel_food, cal = distchan_cal, calcats = distchan_calcats, weight = pc_food_weight, path = file.path(model$inputdir,paste0(naics,"_g",g,"_model_distchannel_food_cal.csv")), iter = 4)]
      
    } else {
      
      pc[SCTG %in% 1:9, distchannel := predict_logit(df, distchannel_food)]
      
    }
    
  }
  
  print(paste(Sys.time(), "Finished ", inNumber, " for SCTG %in% c(1:9)"))
  
  ### Apply choice model of distribution channel for other industries
  
  # The model estimated for mfg products is applied to all other SCTG commoditie
  outNumber <- nrow(pc[!SCTG %in% c(1:9)])
  
  if (outNumber > 0) {
    
    # Sort on vars so simulated choice is ordered correctly
    model_vars_mfg <- c("CATEGORY", distchannel_mfg[TYPE == "Variable", unique(VAR)])
    model_ascs_mfg <- distchannel_mfg[TYPE == "Constant", unique(VAR)]
    
    setkeyv(pc, model_vars_mfg) #sorted on vars so simulated choice is ordered correctly
    
    pc_mfg <- pc[!SCTG %in% c(1:9),model_vars_mfg,with=FALSE]
    pc_mfg_weight <- pc[!SCTG %in% c(1:9),PurchaseAmountTons]
    
    df <- pc_mfg[, list(Start = min(.I), Fin = max(.I)), by = model_vars_mfg] #unique combinations of model coefficients
    
    df[, (model_ascs_mfg) := 1] #add 1s for constants to each group in df
    
    print(paste(Sys.time(), nrow(df), "unique combinations"))
    
    # Simulate choice -- with calibration if calibration targets provided
    if(!is.null(calibration)){
      
      pc[!SCTG %in% c(1:9), distchannel := predict_logit(df, distchannel_mfg, cal = distchannel_calibration, calcats = distchan_calcats, weight = pc_mfg_weight, path = file.path(model$inputdir,paste0(naics,"_g",g,"_model_distchannel_mfg_cal.csv")), iter=4)]
      
    } else {
      
      pc[!SCTG %in% 1:9, distchannel := predict_logit(df, distchannel_mfg)]
      
    }
    
  }
  
  print(paste(Sys.time(), "Finished ", outNumber, " for !SCTG %in% c(1:9)"))
  
  return(pc[,.(BuyerID, SellerID, distchannel)])
  
}


# Override the predict_logit function from rFreight package as there is problem of loading the reshape package in the future processors
predict_logit <- function (df, mod, cal = NULL, calcats = NULL, iter = 1) {
  alts <- max(mod$CHID)
  ut <- diag(alts)
  ut[upper.tri(ut)] <- 1
  if (is.numeric(df$CATEGORY)) 
    df[, `:=`(CATEGORY, paste0("x", CATEGORY))]
  cats <- unique(df$CATEGORY)
  mod <- data.table(reshape::expand.grid.df(mod, data.frame(CATEGORY = cats)))
  if (is.numeric(cal$CATEGORY)) 
    cal[, `:=`(CATEGORY, paste0("x", CATEGORY))]
  for (iters in 1:iter) {
    if (iters > 1 & !is.null(cal) & !is.null(calcats)) {
      sim <- sapply(cats, function(x) tabulate(simchoice[min(df$Start[df$CATEGORY == 
                                                                        x]):max(df$Fin[df$CATEGORY == x])], nbins = alts))
      sim <- sim/colSums(sim)
      if (length(unique(calcats$CHOICE)) < length(unique(calcats$CHID))) {
        sim <- cbind(calcats, sim)
        sim <- melt.data.table(sim, id.vars = c("CHOICE", "CHID"), 
                    variable.name = "CATEGORY")
        sim <- sim[, list(MODEL = sum(value)), by = list(CHOICE, 
                                                         CATEGORY)]
        sim <- merge(sim, cal, c("CATEGORY", "CHOICE"))
        sim[, `:=`(ascadj, log(TARGET/MODEL))]
        adj <- merge(sim, calcats, "CHOICE", allow.cartesian = TRUE)[, 
                                                                     list(CATEGORY, CHID, ascadj)]
      }
      if (length(unique(calcats$CHOICE)) > length(unique(calcats$CHID))) {
        caldat <- merge(cal[CATEGORY %in% cats], calcats, 
                        "CHOICE")
        caldat <- caldat[, list(TARGET = sum(TARGET)), 
                         by = list(CATEGORY, CHID)]
        sim <- data.table(CHID = 1:nrow(sim), sim)
        sim <- melt.data.table(sim, id.vars = c("CHID"), variable.name = "CATEGORY")
        sim <- merge(sim, caldat, c("CATEGORY", "CHID"))
        sim[, `:=`(ascadj, log(TARGET/value))]
        adj <- sim[, list(CATEGORY, CHID, ascadj)]
      }
      if (length(unique(calcats$CHOICE)) == length(unique(calcats$CHID))) {
        stop("Need to implment calibration for same calcats as choice alts")
      }
      mod <- merge(mod, adj, c("CATEGORY", "CHID"))
      mod[TYPE == "Constant", `:=`(COEFF, COEFF + ascadj)]
      mod[, `:=`(ascadj, NULL)]
    }
    utils <- lapply(cats, function(y) sapply(1:alts, function(x) exp(rowSums(sweep(df[CATEGORY == 
                                                                                        y, mod[CHID == x & CATEGORY == y, VAR], with = F], 
                                                                                   2, mod[CHID == x & CATEGORY == y, COEFF], "*")))))
    if(nrow(df)>1){
      utils <- lapply(1:length(cats), function(x) (utils[[x]]/rowSums(utils[[x]])) %*% ut)
    } else {
      utils <- lapply(1:length(cats), function(x) (utils[[x]]/sum(utils[[x]])) %*% ut)
    }
    utils <- do.call("rbind", utils)
    temprand <- runif(max(df$Fin))
    simchoice <- unlist(lapply(1:nrow(df), function(x) 1L + 
                                 findInterval(temprand[df$Start[x]:df$Fin[x]], utils[x,])))
  }
  return(simchoice)
}
