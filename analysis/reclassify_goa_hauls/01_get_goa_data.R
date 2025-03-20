## Import Libraries
library(gapindex)
library(esrindex)
library(dplyr)
library(ggplot2)

## Connect to Oracle
channel <- gapindex::get_connected()

# Function to convert esrindex::species_groups list to data.frame
list_to_df <- function(lst) {
  df <- 
    do.call(rbind, lapply(names(lst), function(name) {
    data.frame(GROUP_CODE = name, SPECIES_CODE = lst[[name]], stringsAsFactors = FALSE)
  }))
  return(df)
}

goa_esr_groups <- esrindex::species_groups |>
  list_to_df() |>
  dplyr::filter(
    GROUP_CODE %in% unname(unlist(esrindex::chapter_settings$GOA))
      )

## Pull data from gapindex for 1990-2025 surveys, excluding partial survey in 2000
gp_data <- gapindex::get_data(
  year_set = c(seq(1990, 1999, 3), seq(2003, 2025, 2)), 
                              survey_set = "GOA",
                              spp_codes = goa_esr_groups[goa_esr_groups$GROUP_CODE == "Sea stars", ], 
                              channel = channel, 
                              pull_lengths = FALSE
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Post-stratify hauls based on the NMFS Areas
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Calculate midpoint of the observed start and end longitudes for each haul
gp_data$haul$MID_LONGITUDE <- 
  (gp_data$haul$START_LONGITUDE + gp_data$haul$END_LONGITUDE) / 2  

## Following ... reclassify hauls
gp_data$haul$STRATUM[gp_data$haul$STRATUM == 40 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 50
gp_data$haul$STRATUM[gp_data$haul$STRATUM == 142 & 
                       gp_data$haul$MID_LONGITUDE < -140] <- 141

# Stratum 152 is new and wasn't part of the 1984 design
gp_data$haul$STRATUM[gp_data$haul$STRATUM == 142 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 152

gp_data$haul$STRATUM[gp_data$haul$STRATUM == 143] <- 152

gp_data$haul$STRATUM[gp_data$haul$STRATUM == 240 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 250
gp_data$haul$STRATUM[gp_data$haul$STRATUM == 241 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 250

gp_data$haul$STRATUM[gp_data$haul$STRATUM == 340 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 350
gp_data$haul$STRATUM[gp_data$haul$STRATUM == 341 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 351

gp_data$haul$STRATUM[gp_data$haul$STRATUM == 440 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 450
gp_data$haul$STRATUM[gp_data$haul$STRATUM == 540 & 
                       gp_data$haul$MID_LONGITUDE >= -140] <- 550

## This is a hard-copied version of GOA.SPLIT_something
updated_stratum_area <- 
  data.frame(
    SURVEY_DEFINITION_ID = 47, SURVEY = "GOA", DESIGN_YEAR = 1984,
    STRATUM = c(40,	41,	50,	140,	141,	152,	150,	151, 240,	241, 250,	251,	
                340,	341,	350,	351, 440,	450,	540,	550),
    AREA_NAME = NA, DESCRIPTION = NA,
    AREA_KM2 = c(4980.005501,	6714.745,	11514.5325,	7346.035, 9993.915778,	
                 12043.32322,	4196.599,	6888.172, 2286.139849,	1503.635678,	
                 1882.079151, 4551.089322,	751.2781795,	1296.716466, 
                 2700.424821,	996.6395344,	1252.954196, 1249.897804, 
                 1609.551032,	1484.372968) )

## Update the stratum information in the gp_data list
gp_data$strata <- 
  rbind(subset(x = gp_data$strata,
               subset = !(STRATUM %in% updated_stratum_area$STRATUM)),
        updated_stratum_area)


goa_layers_2025 <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")

goa_layers_1984 <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338", design.year = 1984)


sf::st_write(goa_layers_1984$survey.strata,
             dsn = here::here("analysis", "reclassify_goa_hauls", "goa_strata_1984.gpkg"))



reclassify_subareas_sf <- 
  gp_data$stratum_groups |>
  dplyr::select(AREA_ID, STRATUM) |>
  dplyr::filter(AREA_ID %in% c(919, 929, 939, 949, 959)) |>
  dplyr::inner_join(gp_data$haul, by = "STRATUM") |>
  sf::st_as_sf(coords = c("MID_LONGITUDE", "START_LATITUDE"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

sf::st_write(reclassify_subareas_sf,
             dsn = here::here("analysis", "reclassify_goa_hauls", "goa_hauls_reclassified.gpkg"),
             delete_dsn = TRUE)

nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338") |>
  dplyr::filter(REP_AREA %in% c(610, 620, 630, 640, 650)) 

sf::st_write(nmfs_areas,
             dsn = here::here("analysis", "reclassify_goa_hauls", "nmfs_goa_outside_waters.gpkg"),
             delete_dsn = TRUE)

inpfc_areas <- akgfmaps::get_inpfc_strata(select.region = "GOA", set.crs = "EPSG:3338")

sf::st_write(inpfc_areas,
             dsn = here::here("analysis", "reclassify_goa_hauls", "inpfc_goa_areas.gpkg"),
             delete_dsn = TRUE)

ggplot() +
  geom_sf(data = reclassify_subareas_sf,
          mapping = aes(color = factor(AREA_ID)),
          size = rel(0.1)) +
  geom_sf(data = nmfs_areas, fill = NA) +
  geom_sf_text(
    data = sf::st_centroid(nmfs_areas), 
               mapping = aes(label = REP_AREA)) +
  facet_wrap(~AREA_ID) +
  theme_minimal()



## Update the stratum groupings in the gp_data list to include the new stratum
## 152. Note I'm only modifying the stratum grouping for AREA_ID 959 because
## it is the only AREA_ID that is affected. 
gp_data$stratum_groups <- rbind(
  gp_data$stratum_groups, 
  data.frame(SURVEY_DEFINITION_ID = 47,
             SURVEY = "GOA",
             AREA_ID = c(959),
             DESIGN_YEAR = 1984,
             STRATUM = 152))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Calculate CPUE, stratum biomass, then aggregate biomass across subareas
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gp_cpue <- 
  gapindex::calc_cpue(gapdata = gp_data)

gp_stratum_biomass <- 
  gapindex::calc_biomass_stratum(gapdata = gp_data, 
                                 cpue = gp_cpue)
gp_subarea_biomass <- 
  gapindex::calc_biomass_subarea(gapdata = gp_data, 
                                 biomass_stratum = gp_stratum_biomass)

gp_subarea_biomass$AREA_ID

gp_data$subarea

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Reformat gp_subarea_biomass to something that looks like
##   GOA.GOA_SPLIT_FRACTION
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_split_fractions <- 
  gp_subarea_biomass[gp_subarea_biomass$AREA_ID %in% c(949, 959), 
                     c("YEAR", "SPECIES_CODE", "AREA_ID", "BIOMASS_MT")]
goa_split_fractions <- 
  reshape(data = goa_split_fractions, 
          idvar = c("SPECIES_CODE", "YEAR"),
          timevar = "AREA_ID",
          direction = "wide")
names(x = goa_split_fractions) <- c("YEAR", "MANAGEMENT_GROUP",
                                    "WEST_BIOMASS", "EAST_BIOMASS")
goa_split_fractions$WEST_BIOMASS <- 
  round(x = goa_split_fractions$WEST_BIOMASS, digits = 1)
goa_split_fractions$EAST_BIOMASS <- 
  round(x = goa_split_fractions$EAST_BIOMASS, digits = 1)