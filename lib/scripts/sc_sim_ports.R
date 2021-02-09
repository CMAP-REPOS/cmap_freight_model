sc_sim_ports <- function(BuyerSupplierPairs, ShipmentRoutesCosts, skims_airports, skims_ports, ModeChoiceParameters, sctg){
  
  # Process shipments to/from ports/airports
  
  # Select international shipment trades (water or air)
  BuyerSupplierPairs[path %in% c(51:54) | 
          (path == 47 & ((Production_zone > 150 & Consumption_zone > 273) | 
                           (Production_zone > 273 & Consumption_zone > 150))), 
        flag := 1]
  
  # no additional processing on this portion of the shipments
  pair1 <- BuyerSupplierPairs[is.na(flag)][,flag:=NULL]
  
  # process international shipment trades
  pair2 <- BuyerSupplierPairs[flag==1][,flag:=NULL]
  
  # add skim data (airports/ports)
  pair2[skims_airports, 
        c("FrAir_mesozone", "FrAirport_name") := .(i.FrAir_mesozone, i.FrAirport_name),
        on = c("Production_zone", "Consumption_zone")]
  
  pair2[skims_ports, 
        c("Port_mesozoneNB", "Port_NameNB", "Port_mesozoneB", "Port_NameB") := .(i.Port_mesozoneNB, i.Port_NameNB, i.Port_mesozoneB, i.Port_NameB),
        on = c("Production_zone", "Consumption_zone")]
  
  pair2[, IntlPath := path]
  pair2[, IntlDone := 0]
  
  # Adjust port for bulk/nonbulk goods
  bulksctgs <- sctg$Commodity_SCTG[grep("Bulk", sctg$Category)]
  
  # international shipping - water
  pair2[path %in% c(51:54), 
        Port_mesozone:= as.integer(ifelse(Commodity_SCTG %in% bulksctgs, Port_mesozoneB, Port_mesozoneNB))]					
  
  pair2[path %in% c(51:54), 
        Port_name:= as.character(ifelse(Commodity_SCTG %in% bulksctgs, Port_NameB, Port_NameNB))]
  
  # international shipping - air
  pair2[path == 47, Port_mesozone := as.integer(FrAir_mesozone)]																					
  pair2[path == 47, Port_name := as.character(FrAirport_name)]																		
  
  # clean up table
  pair2[, c("Port_mesozoneB","Port_mesozoneNB","Port_NameB","Port_NameNB","FrAir_mesozone","FrAirport_name") := NULL]
  
  # Imports
  pair2[Production_zone > 273 & Consumption_zone <= 273 & IntlDone == 0, 
        c("ShipZone", "Production_zone", "Intl_zone", "IntlDone") := .(Production_zone, Port_mesozone, "P", 1L)]
  
  # Exports
  pair2[Production_zone <= 273 & Consumption_zone > 273 & IntlDone == 0,
        c("ShipZone", "Consumption_zone", "Intl_zone", "IntlDone") := .(Consumption_zone, Port_mesozone, "C", 1L)]
  
  # Alaska/Hawaii to Continental US
  pair2[Production_zone %in% c(154, 179, 180) & Consumption_zone <= 273 & IntlDone == 0, 
        c("ShipZone", "Production_zone", "Intl_zone", "IntlDone") := .(Production_zone, Port_mesozone, "P", 1L)]
  
  # Continental US to Alaska/Hawaii
  pair2[Production_zone <= 273 & Consumption_zone %in% c(154, 179, 180) & IntlDone == 0, 
        c("ShipZone", "Consumption_zone", "Intl_zone", "IntlDone") := .(Consumption_zone, Port_mesozone, "C", 1L)]
  
  # Calculate Domestic Linehaul-to-Port Mode
  pair2[, c("Attribute2_ShipTime", "path", "minc", "Mode.Domestic", "time", "cost", "avail", "MinCost", "Constant") := NULL]
  setkey(pair2, Production_zone, Consumption_zone)
  
  modeChoiceConstants <- NULL ###TODO update for calibration
  
  pair2.list <- lapply(1:length(unique(pair2$Commodity_SCTG)),
                        function(x) minLogisticsCostPort(pair2[Commodity_SCTG == unique(pair2$Commodity_SCTG)[x]], 
                                                         ShipmentRoutesCosts, 
                                                         sctg, 
                                                         modeChoiceConstants))
  pair2 <- rbindlist(pair2.list)
  
  pair2[, Attribute2_ShipTime := time / 24] 
  
  BuyerSupplierPairs <- rbindlist(list(pair1, pair2), fill = TRUE)
  
  return(BuyerSupplierPairs)
   
}

# Function to apply the mode choice model for shipments between domestic zone and domestic port
minLogisticsCostPort <- function(DomesticShipmentPairs, ShipmentRoutesCosts, sctg, modeChoiceConstants=NULL){
  
  iSCTG <- unique(DomesticShipmentPairs$Commodity_SCTG)
  
  DomesticShipmentPairs <- minLogisticsCostSctgPaths(DomesticShipmentPairs,
                                                       iSCTG,
                                                       c(1:2,4:12,14:30,32:45,55:57),
                                                       sctg,
                                                       ShipmentRoutesCosts, 
                                                       modeChoiceConstants)		
  
  return(DomesticShipmentPairs)
}
