#' Post-stratify GOA hauls based on 2025 design
#'
#' Assign 1990-2023 GOA hauls to 2025 strata that align with NMFS Statistical Area boundaries.
#'
#' @param gapdata gapdata output from `gapindex::get_data()`
#' @param method "2025_strata" (default)
#' @import akgfmaps sf dplyr
#' @noRd

poststratify_goa_hauls <- function(gapdata, method = "use_2025_strata") {
  
  # Original hauls to help with error checking at the end
  input_haul <- gapdata$haul
  
  if(method == "use_2025_strata") {
    
    # Load GOA 2025 stratum polygons
    goa_2025_strata <-
      system.file(
        "extdata", "afsc_bottom_trawl_surveys.gpkg", 
        package = "akgfmaps", 
        mustWork = TRUE) |>
      sf::st_read(
        layer = "survey_strata",
        quiet = TRUE
      ) |>
      dplyr::filter(
        SURVEY_DEFINITION_ID == 47,
        DESIGN_YEAR == 2025
      ) |>
      dplyr::select(
        SURVEY_DEFINITION_ID, 
        DESIGN_YEAR, 
        STRATUM = AREA_ID, 
        AREA_M2) |>
      sf::st_transform(
        crs = "EPSG:3338"
      )
    
    # Look-up table for GOA 2025 strata, by NMFS area
    goa_2025_stratum_lut <- 
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
          )
      )
    
    # Convert haul to sf and add start/end coordinates to multipoint features
    hauls <- 
      gapdata$haul |>
      dplyr::select(HAULJOIN, LATITUDE = START_LATITUDE, LONGITUDE = START_LONGITUDE) |>
      dplyr::bind_rows(
        gapdata$haul |>
          dplyr::select(HAULJOIN, LATITUDE = END_LATITUDE, LONGITUDE = END_LONGITUDE)
      ) |>
      sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                   crs = "WGS84") |>
      dplyr::group_by(HAULJOIN) |>
      dplyr::summarise()
    
    # Convert MULTIPOINT features to LINESTRINGS when hauls have both start and end coordinates
    haul_lines <- 
      hauls |>
      dplyr::filter(sf::st_geometry_type(geometry) == "MULTIPOINT") |>
      sf::st_cast(to = "LINESTRING")
    
    # Find midpoint of each haul path and add hauls that only had one point
    haul_midpoint <- 
      haul_lines |>
      st_line_midpoints() |>
      dplyr::bind_rows(
        dplyr::filter(hauls, sf::st_geometry_type(geometry) == "POINT")
      ) |>
      sf::st_transform(crs = "EPSG:3338")
    
    # Intersect points with survey stratum polygon to retrieve new stratum
    haul_strata <- 
      haul_midpoint |>
      sf::st_intersection(goa_2025_strata, tolerance = 0.1) |>
      sf::st_drop_geometry() |>
      dplyr::select(HAULJOIN, STRATUM)
    
    # Replaced old strata with new strata
    gapdata$haul <- gapdata$haul |>
      dplyr::select(-STRATUM) |>
      dplyr::left_join(haul_strata)
    
    # Use 2025 design for all survey years
    gapdata$survey$DESIGN_YEAR <- 2025
    gapdata$cruise$DESIGN_YEAR <- 2025
    
    gapdata$strata <- dplyr::filter(gapdata$strata, DESIGN_YEAR == 2025)
    gapdata$stratum_groups <- dplyr::filter(gapdata$stratum_groups, DESIGN_YEAR == 2025)
    gapdata$subarea <- dplyr::filter(gapdata$subarea, DESIGN_YEAR == 2025)
    
    # Add NMFS areas to stratum grouping table
    gapdata$stratum_groups <- 
      dplyr::bind_rows(gapdata$stratum_groups, 
                       goa_2025_stratum_lut
                       ) |>
      unique()
    
  }
  
  return(gapdata)
  
}