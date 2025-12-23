# Allocating specific commodities to each establishment
firm_synthesis_commodities <- function(Firms, c_n6_n6io_sctg, c_n4_sctg_whl){

  # Merge in the I/O NAICS codes and SCTG codes
  Firms[c_n6_n6io_sctg[,.(NAICS6 = Industry_NAICS6_CBP, Industry_NAICS6_Make, Commodity_SCTG)],
                c("Industry_NAICS6_Make" , "Commodity_SCTG") := .(i.Industry_NAICS6_Make, i.Commodity_SCTG),
                on  = "NAICS6"]
  
  # This function identifies producers who make 2+ commodities (especially wholesalers) and
  # simulates a specific commodity for them based on probability thresholds for multiple commodities
  mult_n6cbp <- unique(c_n6_n6io_sctg[Commodity_SCTG > 0 & Proportion < 1, 
                                      .(Industry_NAICS6_CBP, Commodity_SCTG, Proportion)])
  n6cbp_samp <- Firms[NAICS6 %in% unique(mult_n6cbp$Industry_NAICS6_CBP)][, .N, by = NAICS6]
  
  assign_mult_sctg <- function(n6cbp){
    sample(mult_n6cbp[Industry_NAICS6_CBP == n6cbp]$Commodity_SCTG,
           n6cbp_samp[NAICS6 == n6cbp]$N,
           replace = TRUE,
           prob = mult_n6cbp[Industry_NAICS6_CBP == n6cbp]$Proportion)
  }
  
  set.seed(BASE_SEED_VALUE)
  
  for (n6cbp in n6cbp_samp$NAICS6){
    Firms[NAICS6 == n6cbp, Commodity_SCTG := assign_mult_sctg(n6cbp)]
  }
  
  # Assign a commodity that is handled by each of the wholesale establishments (EmpCatName = 42)
  # Most 4 digit NAICS codes can be associated with multiple SCTG, simulate one for each establishment
  Firms[, n4 := floor(NAICS6/100)]
  n4whl_samp <- Firms[EmpCatName == 42][, .N, keyby = n4]
  
  assign_whl_sctg <- function(n4whl){
    if(nrow(c_n4_sctg_whl[Industry_NAICS4 == n4whl])==1){
      rep(x = c_n4_sctg_whl[Industry_NAICS4 == n4whl]$Commodity_SCTG, 
          times = n4whl_samp[n4 == n4whl]$N)
    } else {
      sample(c_n4_sctg_whl[Industry_NAICS4 == n4whl]$Commodity_SCTG,
           n4whl_samp[n4 == n4whl]$N,
           replace = TRUE,
           prob = c_n4_sctg_whl[Industry_NAICS4 == n4whl]$Proportion)
    }
  }
  
  set.seed(BASE_SEED_VALUE)
  
  for (n4whl in n4whl_samp$n4){
    Firms[n4 == n4whl, Commodity_SCTG := assign_whl_sctg(n4whl)]
  }
  
  Firms[, n4 := NULL]

  # Key on BusID
  setkey(Firms, BusID)

  # Return the processed Firms table
  return(Firms)

}
