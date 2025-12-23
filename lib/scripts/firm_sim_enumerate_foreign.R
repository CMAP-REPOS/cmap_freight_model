
#Enumerate foreign firms and merge with correspondences
firm_synthesis_enumerate_foreign <- function(for_prod, for_cons, c_n6_n6io_sctg){

  # Enumerate foreign producers and consumers
  # Create one agent per country per commodity
  # Remove any records where the country and FAF zone are not known
  Firms <- rbind(for_prod[!is.na(FAFZONE),.(Industry_NAICS6_CBP = as.integer(Commodity_NAICS6),
                             ProVal = USImpVal/1000000,
                             CBPZONE,
                             FAFZONE,
                             FirmType = "ForeignProducer")],
                 for_cons[!is.na(FAFZONE),.(Industry_NAICS6_CBP = as.integer(Commodity_NAICS6),
                             ProVal = USExpVal/1000000,
                             CBPZONE,
                             FAFZONE,
                             FirmType = "ForeignConsumer")])

  # Merge in the I/O NAICS codes and SCTG codes
  Firms[c_n6_n6io_sctg,
        c("Industry_NAICS6_Make" , "Commodity_SCTG") := .(i.Industry_NAICS6_Make, i.Commodity_SCTG),
        on  = "Industry_NAICS6_CBP"]
  
  # This function identifies producers who make 2+ commodities and
  # simulates a specific commodity for them based on probability thresholds for multiple commodities
  mult_n6cbp <- unique(c_n6_n6io_sctg[Commodity_SCTG > 0 & Proportion < 1, 
                                      .(Industry_NAICS6_CBP, Commodity_SCTG, Proportion)])
  
  assign_mult_sctg <- function(n6cbp){
    sample(mult_n6cbp[Industry_NAICS6_CBP == n6cbp]$Commodity_SCTG,
           n6cbp_samp[Industry_NAICS6_CBP == n6cbp]$N,
           replace = TRUE,
           prob = mult_n6cbp[Industry_NAICS6_CBP == n6cbp]$Proportion)
  }
  
  # Do this for both foreign producers and foreign consumers
  # Foreign consumers are simply representative endpoints for a NAICS commodity that is exported
  # and therefore need to be treated in the same way to match correctly with a single SCTG code
  
  n6cbp_samp <- Firms[Industry_NAICS6_CBP %in% unique(mult_n6cbp$Industry_NAICS6_CBP)][, .N, by = Industry_NAICS6_CBP]
  
  set.seed(BASE_SEED_VALUE)
  
  for (n6cbp in n6cbp_samp$Industry_NAICS6_CBP){
    Firms[Industry_NAICS6_CBP == n6cbp, 
          Commodity_SCTG := assign_mult_sctg(n6cbp)]
  }

  # Summarize and remove any industries with unknown NAICS IO codes
  # Also remove any flows that are coded as SCTG = 0, as these should 
  # all be transportable commodities (i.e., actual goods being imported or exported)
  Firms <- Firms[!is.na(Industry_NAICS6_Make) & Commodity_SCTG > 0,
                 .(ProVal = sum(ProVal)),
                 by = .(Industry_NAICS6_Make, CBPZONE, FAFZONE, Commodity_SCTG, FirmType)]

  # Return the foreign firms table
  return(Firms)

}

