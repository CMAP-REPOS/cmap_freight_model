# Define sample sizes for procurement markets to be run in the next step
firm_synthesis_sample_groups <- function(firms_sum){


  # Get number of (none NA) matches by NAICS, and check for imbalance in producers and consumers
  naics_set <- firms_sum$matches_naics_sctg[!is.na(firms_sum$matches_naics_sctg[,"Possible_Matches"]),
                                            c("NAICS","Commodity_SCTG","Producers","Consumers","Possible_Matches")]

  naics_set$ConsProd_Ratio <- naics_set$Consumers/naics_set$Producers

  naics_set$Split_Prod <- ifelse(naics_set$ConsProd_Ratio < BASE_CONS_PROD_RATIO_LIMIT, TRUE, FALSE)

  # calculate group sizes for each commodity so that all groups are less than threshold
  # Either cut both consumers and producers or just consumers (with all producers in each group)
  calcSampleGroups <- function(ncons,nprod,cthresh,sprod,cprl){
    ngroups <- 1L
    nconst <- as.numeric(ncons)
    nprodt <- as.numeric(nprod)
    nSuppliersPerBuyer <- BASE_SUPPLIERS_PER_BUYER
    if(sprod){
      while (nconst * nSuppliersPerBuyer > cthresh) {
        ngroups <- ngroups + 1L
        nconst <- as.numeric(ceiling(as.numeric(ncons) / ngroups))
        nprodt <- as.numeric(ceiling(as.numeric(nprod) / ngroups))
      }
    } else {
      while ((nconst * nSuppliersPerBuyer > cthresh) | (nconst/nprodt > cprl)) {
        ngroups <- ngroups + 1L
        nconst <- as.numeric(ceiling(as.numeric(ncons) / ngroups))
      }
    }
    return(c(nprodt,nconst,nconst * nprodt, nconst/nprodt, ngroups))
  }

  naics_set[,c("nProducers","nConsumers","nMatches","rev_CPRatio","groups")] <- do.call(rbind,
                                                                                        lapply(1:nrow(naics_set),
                                                                                               function(x) calcSampleGroups(naics_set$Consumers[x],
                                                                                                                            naics_set$Producers[x],
                                                                                                                            BASE_COMBINATION_THRESHOLD,
                                                                                                                            naics_set$Split_Prod[x],
                                                                                                                            BASE_CONS_PROD_RATIO_LIMIT)
                                                                                        )
  )

  # Add group identifier
  naics_set$Market <- paste(naics_set$NAICS, naics_set$Commodity_SCTG, sep = "_")

  # Return the naics_set table
  return(naics_set)

}
