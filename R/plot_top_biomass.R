#' Biomass index trends for AFSC bottom trawl survey regions
#'
#' Queries GAP_PRODUCTS AKFIN biomass tables and generates a  `ggplot2` time-series plot with mean 
#' design-based biomass index and 95% CI. Exports PNG image to the /plots/<region>/ directory.
#'
#' @param channel An active ODBC connection channel object generated via `RODBC`.
#' @param region Character string specifying the survey region. Options are:
#'   * `"ebs"`: Eastern Bering Sea (area_id = 99901, survey_definition_id = 98)
#'   * `"goa"`: Gulf of Alaska (area_id = 99903, survey_definition_id = 47)
#'   * `"ai"`: Aleutian Islands (area_id = 99904, survey_definition_id = 52)
#'  * `"nbs"`: Northern Bering Sea (area_id = 99905, survey_definition_id = 143) 
#' @param max_year Numeric or integer specifying the final survey year to query and base species biomass ranking upon.
#' @param n_species Integer. Number of top biomass species to select. Default is `NULL`. Ignored if `species_codes` is provided.
#' @param species_codes Vector of numeric RACE species codes to use. Overrides `n_species`. Default is `NULL`.
#' @param legend_position Character string setting the relative position of the plot legend inside the panel. Options are `"top-left"` (default) or `"top-right"`.
#'
#' @details 
#' The function writes two 6x6 inch PNG files to /plots/<region>/:
#' * `<region>_db_top_species.png` (with main title)
#' * `<region>_db_top_species_no_title.png` (without title)
#'
#' @return A named `list` containing:
#' \describe{
#'   \item{biomass_plot}{A `ggplot` object with design-based biomass index plots.}
#'   \item{biomass_data}{A `data.frame` containing the raw query records from GAP_PRODUCTS.AKFIN_BIOMASS.}
#' }
#' 
#' @import ggplot2 here
#' @importFrom RODBC sqlQuery
#' @export

plot_top_biomass <- function(channel, region, max_year, n_species = NULL, species_codes = NULL, legend_position = "top-left") {
  
  area_id <- switch(region,
                    ebs = 99901, 
                    goa = 99903,
                    nbs = 99905,
                    ai = 99904)
  
  sid <- switch(region,
                ebs = 98, 
                goa = 47,
                ai = 52,
                nbs = 143)
  
  species_ranks <- RODBC::sqlQuery(
    channel = channel,
    query = paste0("select b.*, tc.common_name from 
  gap_products.akfin_biomass b, gap_products.akfin_taxonomic_classification tc  
  where b.area_id = ", area_id,
                   " and b.year = ", max_year,  
                   " and b.survey_definition_id = ", sid, 
                   " and b.species_code < 40000
  and tc.species_code = b.species_code")
  )
  
  species_ranks <- species_ranks[order(-species_ranks$BIOMASS_MT), ]
  
  if(is.null(species_codes)) {
    
    species_codes <- species_ranks$SPECIES_CODE[1:n_species]
    
  }
  
  common_names <- species_ranks$COMMON_NAME[species_ranks$SPECIES_CODE %in% species_codes]
  
  biomass_data <- RODBC::sqlQuery(
    channel = channel,
    query = paste0("select b.*, tc.common_name 
  from gap_products.akfin_biomass b, gap_products.akfin_taxonomic_classification tc
  where b.survey_definition_id = ", sid,  
                   " and b.species_code in (", paste(species_codes, collapse = ","),
                   ") and b.area_id =  ", area_id,
                   " and tc.species_code = b.species_code")
  )
  
  biomass_data <- biomass_data[order(biomass_data$YEAR), ]
  
  legend_coords <- 
    switch(legend_position,
           `top-left` = c(0.15, 0.82),
           `top-right` = c(0.85, 0.82))
  
  pal <- c(
    "#3D348A", 
    "#B3EDEF", 
    "#A2D269", 
    "#001743",
    "#FF8400", 
    "#54ADDB", 
    "#D0D0D0"
  )
  
  names(pal) <- common_names
  
  biomass_data$COMMON_NAME <- factor(biomass_data$COMMON_NAME, levels = common_names)
  
  biomass_plot <-
    ggplot(data = biomass_data,
           mapping = aes(
             x = YEAR, 
             y = BIOMASS_MT/1000, color = COMMON_NAME,
             ymin = (BIOMASS_MT - sqrt(BIOMASS_VAR))/1000, 
             ymax = (BIOMASS_MT + sqrt(BIOMASS_VAR))/1000,
             fill = COMMON_NAME)) +
    geom_ribbon(
      alpha = 0.35,
      linewidth = rel(1.02),
      color = NA
    ) +
    geom_path(linewidth = rel(1.01)) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Biomass (thousand mt)") + 
    scale_fill_manual(name = "Species", values = pal) +
    scale_color_manual(name = "Species", values = pal) +
    theme_bw() +
    theme(legend.position = "inside",
          legend.position.inside = legend_coords,
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          plot.title = element_text(color = "grey30"),
          legend.background = element_blank(),
          legend.title = element_blank())
  
  main_title <- paste0("NOAA bottom trawl survey-estimated biomass\n(thousand mt) for select ", toupper(region), " species through ", max_year)
  
  png(here::here("plots", region, paste0(toupper(region), "_db_top_groundfish.png")), width = 6, height = 6, res = 300, units = "in")
  print(biomass_plot + ggtitle(main_title))
  dev.off()
  
  png(here::here("plots", region, paste0(toupper(region), "_db_top_groundfish_no_title.png")), width = 6, height = 6, res = 300, units = "in")
  print(biomass_plot)
  dev.off()
  
  return(
    list(biomass_plot = biomass_plot, biomass_data = biomass_data)
  )
  
}