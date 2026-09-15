# Make top biomass index plots
library(esrindex)

channel <- esrindex::get_connected(schema = "AFSC")

# Aleutian Islands top 7 fish species by biomass
plot_top_biomass(
  channel = channel, 
  region = "ai", 
  max_year = 2026, 
  n_species = 7, 
  legend_position = 
    "top-left"
)

# EBS select taxa (pollock, NRS, YFS, FHS, cod, ATF, Alaska skate)
plot_top_biomass(
  channel = channel, 
  region = "ebs", 
  max_year = 2026, 
  species_codes = c(21740, 10261, 10210, 10130, 21720, 10110, 471),
  legend_position = "top-right"
)


# GOA select species (ATF, FHS, cod, Pacific halibut, POP, sablefish, pollock)
plot_top_biomass(
  channel = channel, 
  region = "goa", 
  max_year = 2025, 
  species_codes = c(10110, 10130, 21720, 10120, 30060, 20510, 21740), 
  legend_position = "top-right"
)
