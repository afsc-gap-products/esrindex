#' Create indicator plots for a region
#'
#' This function generates a region-specific biomass plot based on indicator data and saves the plot as a png.
#'
#' @param indicator_data A list containing indicator data, e.g. esrindex::EBS_INDICATOR, esrindex::GOA_INDICATOR.
#' @param indicator_name A character string specifying the indicator name included in esrindex::chapter_settings (e.g. "misc_species", "jellyfish").
#' @param error_bars A logical value indicating whether to include error bars (default is TRUE).
#' @param bar_color Color for error bars"#0085CA").
#'
#' @return A ggplot plot.
#'
#' @examples
#' \dontrun{
#' make_region_plot(indicator_data = AI_INDICATOR,
#'                  indicator_name = "misc_species",
#'                  bar_color = "#0085CA",
#'                  error_bars = TRUE)
#' }
#' @import ggplot2 stats
#' @importFrom grDevices png
#'
#' @export

make_region_plot <- function(indicator_data, indicator_name, error_bars = TRUE, bar_color = "#0085CA") {

  region <- indicator_data$timeseries$SURVEY[1]

  area_id <- region_settings[[region]][['esr_area_id']]

  group_name <- chapter_settings[[region]][[indicator_name]][['group_name']]

  timeseries <- indicator_data$timeseries[indicator_data$timeseries$AREA_ID == area_id &
                                            indicator_data$timeseries$SPECIES_CODE %in% group_name, ]

  mean_sd <- indicator_data$mean_sd[indicator_data$mean_sd$AREA_ID == area_id &
                                      indicator_data$mean_sd$SPECIES_CODE %in% group_name, ]

  if(error_bars) {
    p1 <- ggplot() +
      geom_point(data = timeseries,
                 mapping = aes(x = YEAR, y = BIOMASS_MT)) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_BIOMASS)) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_PLUS1), linetype = 2) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_PLUS2), linetype = 3) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_MINUS1), linetype = 2) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_MINUS2), linetype = 3) +
      geom_linerange(data = timeseries,
                     mapping = aes(x = YEAR,
                                   ymax = BIOMASS_PLUS2_SD,
                                   ymin = BIOMASS_MINUS2_SD),
                     color = "black") +
      geom_rect(data = timeseries,
                mapping = aes(xmin = YEAR+0.4,
                              xmax = YEAR-0.4,
                              ymax = BIOMASS_PLUS1_SD,
                              ymin = BIOMASS_MINUS1_SD),
                fill = bar_color) +
      geom_segment(data = timeseries,
                   aes(x = YEAR+0.4,
                       xend = YEAR-0.4,
                       y = BIOMASS_MT,
                       yend = BIOMASS_MT,
                       group = YEAR),
                   color = "black") +
      scale_y_continuous(name = "Biomass (mt)", expand = c(0,0)) +
      scale_x_continuous(name = "Year") +
      facet_wrap(~SPECIES_CODE, scales = "free", nrow = length(group_name)) +
      theme_blue_strip()
  } else {
    p1 <- ggplot() +
      geom_point(data = timeseries,
                 mapping = aes(x = YEAR, y = BIOMASS_MT)) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_BIOMASS)) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_PLUS1), linetype = 2) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_PLUS2), linetype = 3) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_MINUS1), linetype = 2) +
      geom_hline(data = mean_sd,
                 mapping = aes(yintercept = MEAN_MINUS2), linetype = 3) +
      geom_point(data = timeseries,
                 aes(x = YEAR,
                     y = BIOMASS_MT),
                 color = "black") +
      scale_y_continuous(name = "Biomass (mt)", expand = c(0,0)) +
      scale_x_continuous(name = "Year") +
      facet_wrap(~SPECIES_CODE, scales = "free", nrow = length(group_name)) +
      theme_blue_strip()
  }

  suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))

  png(filename = paste0("./plots/", region, "/", region, "_", gsub(x = indicator_name, pattern = " ", replacement = "_"), "_full_region.png"),
      width = 169,
      height = 40*length(group_name),
      units = "mm",
      res = 300)
  print(p1)
  dev.off()

  return(p1)

}

