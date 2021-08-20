# Compare the new and old FAF zones
library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(openxlsx)

# Define color using RSG palette
rsgcolordf <- data.frame(red=c(246,0,99,186,117,255,82),
                         green=c(139,111,175,18,190,194,77),
                         blue=c(31,161,94,34,233,14,133),
                         colornames=c("orange","marine","leaf","cherry","sky","sunshine","violet"))

# county -- FAF corresp
faf5 <- fread("County_to_FAFZone.csv")
length(unique(faf5[StateFIPS <= 56]$FAFZone_5.0))
faf3 <- fread("corresp_countyfips_faf3.csv")
length(unique(faf4$FAFZONE))

# CFS FAF 5 names
cfs <- data.table(read.xlsx("CFS-area-code-FAF5-zone-id.xlsx"))
cfs[, FAFZone_5 := as.integer(FAF)]

# Compare FAF zone set
faf3zones <- unique(faf3[,.(StateFIPS, FAF3 = FAFZONE)])[order(StateFIPS, FAF3)]
faf5zones <- unique(faf5[StateFIPS <= 56,.(StateFIPS, FAF5 = FAFZone_5.0)])[order(StateFIPS, FAF5)]

fafstate <- merge(faf3zones[,.(StateFIPS, FAF = FAF3, FAF3 = "Yes")], 
                  faf5zones[,.(StateFIPS, FAF = FAF5, FAF5 = "Yes")], 
                  by = c("StateFIPS", "FAF"),
                  all = TRUE)
fafstate[is.na(FAF3)]
fafstate[is.na(FAF5)]
# states with changes
# 6 California -- extra metro area
# 10 Delaware -- split into metro area and remainder
# 18 Indiana -- extra metro area
# 20 Kansas -- extra metro area
# 21 Kentucky -- extra metro area
# 31 Nebraska -- split into metro area and remainder
# 33 New Hampshire -- split into metro area and remainder
# 42 Pennsylvania -- extra metro area
# 44 Rhode Island -- renumbered zone
# 47 Tennessee -- extra metro area
# 53 Washington -- extra metro area

# compare counties
fafcomp <- merge(faf3[,.(StateFIPS, CountyFIPS, County, FAFZone_3 = FAFZONE, FAFName_3 = FAFNAME, FIPS)],
                 faf5[StateFIPS <= 56, .(StateFIPS, CountyFIPS, FIPS, State, FAFZone_5 = FAFZone_5.0)],
                 by = c("StateFIPS", "CountyFIPS", "FIPS"),
                 all = TRUE)
fafcomp[cfs,
        FAFName_5 := i.CFSAREANAM, 
        on = "FAFZone_5"]

# FIPS code changes

# StateFIPS CountyFIPS  FIPS County FAFZONE FAFNAME        State FAFZone_5.0
# 1:         2        158  2158   <NA>      NA    <NA>       Alaska          20 # Kusilvah, 
# 2:        46        102 46102   <NA>      NA    <NA> South Dakota         460 # Oglala Lakota
# StateFIPS CountyFIPS  FIPS       County FAFZONE FAFNAME State FAFZone_5.0
# 1:         2        270  2270 Wade Hampton      20      AK  <NA>          NA # Now 02158 Kusilvah
# 2:        46        113 46113      Shannon     460      SD  <NA>          NA # Now Oglala Lakota 46102
# 3:        51        515 51515      Bedford     519  VA rem  <NA>          NA # Combined into Bedford County 51019

fafcomp[, Changed := ifelse(FAFZone_3 != FAFZone_5, "Different Zone (FAF3 to FAF5)", "Same Zone in FAF3 and FAF5")]
fafcomp[, FAF3_Type := ifelse(FAFZone_3 - StateFIPS * 10 == 0, "Whole State", ifelse(FAFZone_3 - StateFIPS * 10 == 9, "Remainder", "Metro"))]
fafcomp[, FAF5_Type := ifelse(FAFZone_5 - StateFIPS * 10 == 0, "Whole State", ifelse(FAFZone_5 - StateFIPS * 10 == 9, "Remainder", "Metro"))]
fafcomp[is.na(FAF3_Type)]
fafcomp[, FAF_Change := ifelse(FAF3_Type == FAF5_Type | is.na(FAF3_Type), FAF5_Type, 
                               ifelse(FAF5_Type == "Metro", "New Metro", "New Remainder"))]
unique(fafcomp$FAF_Change)

fwrite(fafcomp,
       "FAF3_FAF5_FIPS_Comparison.csv")

fafcomp[FAFName_5 == "Chicago-Naperville, IL-IN-WI  CFS Area (IL Part)"]

# create maps to show differences
state <- read_sf("GIS/tl_2015_us_state.shp")
county <- read_sf("GIS/tl_2015_us_county.shp")
county[county$GEOID %in% c("02158", "46102"),] # New FIPSs are in there
county[county$GEOID %in% c("02270", "46113", "51515"),] # Old FIPS are not

# for plotting just maps of lower 48
state$Lower <- ifelse(as.integer(state$STATEFP) <= 56 & !as.integer(state$STATEFP) %in% c(2,15), "Lower48", "AK, HI, US Terr")
county$Lower <- ifelse(as.integer(county$STATEFP) <= 56 & !as.integer(county$STATEFP) %in% c(2,15), "Lower48", "AK, HI, US Terr")
county$FIPS <- as.integer(county$GEOID)
county <- merge(county, fafcomp[, .(FIPS, FAF_Change)], by = "FIPS", all.x = TRUE)

p_faf_state_county <- ggplot(data = county[county$Lower == "Lower48",]) +
  geom_sf(aes(fill = FAF_Change), color = gray(0.5), size = 0.1) +
  scale_fill_manual("FAF Zone Type", values = rgb(rsgcolordf[c(1:5),],maxColorValue = 255)) +
  geom_sf(data = state[state$Lower == "Lower48",], size = 1, fill = NA) +
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(1.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  #coord_sf(xlim = c(-84.3, -82.3), ylim = c(41.6, 43.3), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  theme_bw() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) +
  labs(title = "Zone System Comparison", 
       subtitle = "Comparing County Composition of FAF 3 and FAF 5", 
       caption = "Source: FHWA FAF3, FAF5 Zone Systems") 
ggsave(filename = "p_faf_state_county.png", plot = p_faf_state_county, width = 12, height = 8)
