# Evalute option to classify hauls by new strata and generate NMFS area biomass indices
# Sean Rohan
# January 31, 2025

library(esrindex)
library(akgfmaps)
library(navmaps)
library(ggthemes)

# Look-up table for GOA 2025 strata, by NMFS area
goa_2025_strata <- 
  data.frame(
    SURVEY_DEFINITION_ID = 47,
    SURVEY = "GOA",
    DESIGN_YEAR = 2025,
    AREA_ID = 
      c(
        rep(610, 5),
        rep(620, 6),
        rep(630, 6),
        rep(640, 6),
        rep(650, 5)
      ),
    STRATUM = 
      c(
        c(14, 15, 113, 211, 511),
        c(23, 24, 123, 222, 321, 521),
        c(36, 37, 38, 135, 136, 531),
        c(42, 43, 144, 145, 242, 541),
        c(51, 152, 252, 352, 551)
      ))

# Retrieve features from akgfmaps and  and data
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")

dat <- gapindex::get_data(
  year_set = 1991:2025,
  survey_set = "GOA",
  spp_codes = 21720
)

# Convert haul to sf and add start/end coordinates to multipoint features
hauls <- dat$haul |>
  dplyr::select(HAULJOIN, LATITUDE = START_LATITUDE, LONGITUDE = START_LONGITUDE) |>
  dplyr::bind_rows(
    dat$haul |>
      dplyr::select(HAULJOIN, LATITUDE = END_LATITUDE, LONGITUDE = END_LONGITUDE)
  ) |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               crs = "WGS84") |>
  dplyr::group_by(HAULJOIN) |>
  dplyr::summarise()

# Convert MULTIPOINT features to LINESTRINGS when hauls have both start and end coordinates
haul_lines <- hauls |>
  dplyr::filter(sf::st_geometry_type(geometry) == "MULTIPOINT") |>
  sf::st_cast(to = "LINESTRING")

# Find midpoint of each haul path and add hauls that only had one point
haul_midpoint <- haul_lines |>
  navmaps::st_line_midpoints() |>
  dplyr::bind_rows(
    dplyr::filter(hauls, sf::st_geometry_type(geometry) == "POINT")
  ) |>
  sf::st_transform(crs = "EPSG:3338")

# Intersect points with survey stratum polygon to retrieve new stratum
haul_strata <- sf::st_intersection(haul_midpoint, goa_layers$survey.strata) |>
  sf::st_drop_geometry() |>
  dplyr::select(HAULJOIN, STRATUM)

# Replaced old strata with new strata
dat$haul <- dat$haul |>
  dplyr::select(-STRATUM) |>
  dplyr::left_join(haul_strata)

# Use 2025 design for all survey years
dat$survey$DESIGN_YEAR <- 2025
dat$cruise$DESIGN_YEAR <- 2025

dat$strata <- dplyr::filter(dat$strata, DESIGN_YEAR == 2025)
dat$stratum_groups <- dplyr::filter(dat$stratum_groups, DESIGN_YEAR == 2025)
dat$subarea <- dplyr::filter(dat$subarea, DESIGN_YEAR == 2025)

# Add NMFS areas to stratum grouping table
dat$stratum_groups <- dplyr::bind_rows(dat$stratum_groups , goa_2025_strata) |>
  unique()

# Calculate index
cpue <- gapindex::calc_cpue(gapdata = dat)

biomass_stratum <- 
  gapindex::calc_biomass_stratum(
    gapdata = dat, 
    cpue = cpue)

subarea_biomass <- gapindex::calc_biomass_subarea(
  gapdata = dat,
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
