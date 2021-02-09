
#Enumerate firms and merge with correspondenses
firm_synthesis_enumerate <- function(cbp, c_n6_n6io_sctg, EmpBounds, cbp_ag = NULL){

  # Clean the input data, combine with Ag data if needed
  # NOTE: this section should be simplified, reviewed and probably removed when the
  # input employment data is updated
  # At that point, carefully check the year of the NAICS coding system and make sure
  # that the c_n6_n6io_sctg is updated to walk from that NAICS system to the BEA IO system
  if(!is.null(cbp_ag)){
    # Combine Domestic Non Ag Firm Records with the Ag Firm Records
    # Remove any ag records in cbp and replacing with cbp_ag
    cbp <- rbind(cbp[Industry_NAICS6_CBP >= 113110],
                 cbp_ag)
  }

  # Remove old construction records
  cbp <- cbp[!Industry_NAICS6_CBP %in% c(230101, 230102, 230103, 230201, 230202, 230301, 230302)]

  # Recode any out of date 2002 NAICs codes in old employment data to 2007 codes used in correspondence
  # Laboratory Apparatus and Furniture Manufacturing to Analytical laboratory instrument manufacturing
  cbp[Industry_NAICS6_CBP == 339111, Industry_NAICS6_CBP := 334516]
  # Internet Publishing and Broadcasting to Internet publishing and broadcasting and web search portal
  cbp[Industry_NAICS6_CBP == 516110, Industry_NAICS6_CBP := 519130]
  # Paging to Wireless telecommunications carriers (except satellite)
  cbp[Industry_NAICS6_CBP == 517211, Industry_NAICS6_CBP := 517210]
  # Cellular and Other Wireless Telecommunications to Wireless telecommunications carriers (except satellite)
  cbp[Industry_NAICS6_CBP == 517212, Industry_NAICS6_CBP := 517210]
  # Telecommunications Resellers to Telecommunications resellers
  cbp[Industry_NAICS6_CBP == 517310, Industry_NAICS6_CBP := 517911]
  # Cable and Other Program Distribution to All other telecommunications
  cbp[Industry_NAICS6_CBP == 517510, Industry_NAICS6_CBP := 517919]
  # Other Telecommunications to All other telecommunications
  cbp[Industry_NAICS6_CBP == 517910, Industry_NAICS6_CBP := 517919]
  # Internet Service Providers to Internet publishing and broadcasting and web search portals
  cbp[Industry_NAICS6_CBP == 518111, Industry_NAICS6_CBP := 519130]
  # Web Search Portals to Internet publishing and broadcasting and web search portals
  cbp[Industry_NAICS6_CBP == 518112, Industry_NAICS6_CBP := 519130]
  # Real Estate Investment Trusts to Other financial vehicles
  cbp[Industry_NAICS6_CBP == 525930, Industry_NAICS6_CBP := 525990]
  # Research and Development in the Physical, Engineering, and Life Sciences to Research and development in the physical, engineering, and life sciences
  cbp[Industry_NAICS6_CBP == 541710, Industry_NAICS6_CBP := 541712]
  # Employment Placement Agencies to Employment placement agencies
  cbp[Industry_NAICS6_CBP == 561310, Industry_NAICS6_CBP := 561311]

  # Aggregate the employment data by zones, NAICS, and firm size category
  # 1='1-19',2='20-99',3='100-249',4='250-499',5='500-999',6='1,000-2,499',7='2,500-4,999',8='Over 5,000'
  # Remove records with missing zones and NAICS codes
  FirmsDomestic <- cbp[!is.na(CBPZONE) & !is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP),
                       .(e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                         e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
                       by = .(Industry_NAICS6_CBP, CBPZONE, FAFZONE)]

  # Merge in the I/O NAICS codes and SCTG codes
  FirmsDomestic[c_n6_n6io_sctg,
                c("Industry_NAICS6_Make" , "Commodity_SCTG") := .(i.Industry_NAICS6_Make, i.Commodity_SCTG),
                on  = "Industry_NAICS6_CBP"]

  # Create and save a diagnostics file -- firms that did not get a match in the IO NAICS/SCTG correspondence
  # Then filter the firms with missing NAICS IO codes out (ideally there will be no firms removed)
  FirmsDomestic[is.na(Industry_NAICS6_Make), .N, by = Industry_NAICS6_CBP]
  fwrite(FirmsDomestic[is.na(Industry_NAICS6_Make)], file = file.path(SCENARIO_OUTPUT_PATH, "FirmsDomestic_No_Match_NAICSIO.csv"))
  fwrite(FirmsDomestic[is.na(Industry_NAICS6_Make), .(Firms = sum(e1+e2+e3+e4+e5+e6+e7+e8)), by = Industry_NAICS6_CBP], file = file.path(SCENARIO_OUTPUT_PATH, "FirmsDomestic_No_Match_NAICSIO_Summary.csv"))

  FirmsDomestic <- FirmsDomestic[!is.na(Industry_NAICS6_Make)]

  # Add 2 and 4 digit NAICS
  FirmsDomestic[, c("n2","n4") := .(substr(Industry_NAICS6_CBP, 1, 2), substr(Industry_NAICS6_CBP, 1, 4))]

  # Melt to create separate rows for each firm size category
  FirmsDomestic <- melt(FirmsDomestic,
                        measure.vars = paste0("e",1:8),
                        variable.name ="esizecat",
                        value.name = "est")

  # Convert esizecat to an integer (1:8)
  FirmsDomestic[, esizecat := as.integer(esizecat)]

  # Enumerates the agent businesses using the est variable.
  FirmsDomestic <- FirmsDomestic[rep(seq_len(FirmsDomestic[, .N]), est),]

  # Estimate the number of employees
  # Draw from the employment range using a draw from the uniform distribution
  set.seed(151)
  FirmsDomestic[, Emp := round(runif(n = .N,
                                     min = EmpBounds[esizecat],
                                     max = EmpBounds[esizecat + 1] - 1))]

  # Add an ID and firm type
  FirmsDomestic[, BusID := .I]

  FirmsDomestic[, FirmType := "Domestic"]

  # Remove uncessary fields
  FirmsDomestic[, est := NULL]

  # Return the enumerated firms table
  return(FirmsDomestic)

}
