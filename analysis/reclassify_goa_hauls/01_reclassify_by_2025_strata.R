# Assign hauls to new strata and calculate biomass
# Sean Rohan

library(esrindex)
library(akgfmaps)
library(navmaps)
library(ggthemes)

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

gp_data_reclassified <- 
  gp_data |>
  esrindex:::restratify_goa_hauls()

# # Look-up table for GOA 2025 strata, by NMFS area
# goa_2025_strata <- 
#   data.frame(
#     SURVEY_DEFINITION_ID = 47,
#     SURVEY = "GOA",
#     DESIGN_YEAR = 2025,
#     AREA_ID = 
#       c(
#         rep(610, 5),
#         rep(620, 6),
#         rep(630, 6),
#         rep(640, 6),
#         rep(650, 5)
#       ),
#     STRATUM = 
#       c(
#         c(14, 15, 113, 211, 511),
#         c(23, 24, 123, 222, 321, 521),
#         c(36, 37, 38, 135, 136, 531),
#         c(42, 43, 144, 145, 242, 541),
#         c(51, 152, 252, 352, 551)
#       )
#   )
# 
# # Convert haul to sf and add start/end coordinates to multipoint features
# hauls <- 
#   dat$haul |>
#   dplyr::select(HAULJOIN, LATITUDE = START_LATITUDE, LONGITUDE = START_LONGITUDE) |>
#   dplyr::bind_rows(
#     dat$haul |>
#       dplyr::select(HAULJOIN, LATITUDE = END_LATITUDE, LONGITUDE = END_LONGITUDE)
#   ) |>
#   sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
#                crs = "WGS84") |>
#   dplyr::group_by(HAULJOIN) |>
#   dplyr::summarise()
# 
# # Convert MULTIPOINT features to LINESTRINGS when hauls have both start and end coordinates
# haul_lines <- 
#   hauls |>
#   dplyr::filter(sf::st_geometry_type(geometry) == "MULTIPOINT") |>
#   sf::st_cast(to = "LINESTRING")
# 
# # Find midpoint of each haul path and add hauls that only had one point
# haul_midpoint <- 
#   haul_lines |>
#   navmaps::st_line_midpoints() |>
#   dplyr::bind_rows(
#     dplyr::filter(hauls, sf::st_geometry_type(geometry) == "POINT")
#   ) |>
#   sf::st_transform(crs = "EPSG:3338")
# 
# # Intersect points with survey stratum polygon to retrieve new stratum
# haul_strata <- 
#   haul_midpoint |>
#   sf::st_intersection(goa_strata_2025) |>
#   sf::st_drop_geometry() |>
#   dplyr::select(HAULJOIN, STRATUM)
# 
# # Replaced old strata with new strata
# dat$haul <- dat$haul |>
#   dplyr::select(-STRATUM) |>
#   dplyr::left_join(haul_strata)
# 
# # Use 2025 design for all survey years
# dat$survey$DESIGN_YEAR <- 2025
# dat$cruise$DESIGN_YEAR <- 2025
# 
# dat$strata <- dplyr::filter(dat$strata, DESIGN_YEAR == 2025)
# dat$stratum_groups <- dplyr::filter(dat$stratum_groups, DESIGN_YEAR == 2025)
# dat$subarea <- dplyr::filter(dat$subarea, DESIGN_YEAR == 2025)
# 
# # Add NMFS areas to stratum grouping table
# dat$stratum_groups <- dplyr::bind_rows(dat$stratum_groups , goa_2025_strata) |>
#   unique()

# Calculate index
cpue <- gapindex::calc_cpue(gapdata = gp_data_reclassified)

biomass_stratum <- 
  gapindex::calc_biomass_stratum(
    gapdata = gp_data_reclassified, 
    cpue = cpue)

subarea_biomass <- gapindex::calc_biomass_subarea(
  gapdata = gp_data_reclassified,
  biomass_stratum = biomass_stratum
)

ggplot() +
  geom_area(data = dplyr::filter(subarea_biomass, AREA_ID %in% 610:650),
             mapping = aes(x = YEAR, y = BIOMASS_MT, fill = factor(AREA_ID)),
            position = "stack") +
  geom_point(data = dplyr::filter(subarea_biomass, AREA_ID == 99903),
             mapping = aes(x = YEAR, y = BIOMASS_MT,
                           color = "Total biomass"), size = rel(2)) +
  scale_fill_tableau(name = "NMFS Area", direction = -1) +
  scale_color_manual(name = "AREA_ID 99903", values = "blue") +
  theme_bw()
