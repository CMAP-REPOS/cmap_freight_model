# Write out the individual NAICS_SCTG market producer and consumer files
firm_synthesis_write_groups <- function(producers, consumers, naics_set){

  #key the tables for faster subsetting on naics_sctg codes
  consumers[, Market := paste(InputCommodity, Commodity_SCTG, sep = "_")]
  setkey(consumers, Market)

  producers[, Market := paste(OutputCommodity, Commodity_SCTG, sep = "_")]
  setkey(producers, Market)

  for (market in naics_set$Market) {
    # Construct data.tables for just the current commodity
    consc <- consumers[market,]
    prodc <- producers[market,]
    #write the tables to an R data file
    save(consc, prodc, file = file.path(SCENARIO_OUTPUT_PATH, paste0(market, ".Rdata")))
  }

  # Return the updated producers and consumers tables in a list
  return(producers_consumers_list = list(consumers = consumers,
                                         producers = producers))

}
