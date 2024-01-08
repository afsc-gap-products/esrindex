#' Create Subarea-specific Biomass Plots
#'
#' This function generates subarea-specific biomass plots based on indicator data.
#'
#' @param indicator_data A list containing indicator data, including timeseries and mean_sd.
#' @param indicator_name A character string specifying the indicator name.
#' @param bar_color A character string representing the color of the error bars (default is "#0085CA").
#' @param error_bars A logical value indicating whether to include error bars (default is TRUE).
#'
#' @return A ggplot object representing the generated subarea-specific biomass plots.
#'
#' @examples
#' \dontrun{
#' # Example Usage:
#' make_subarea_plot(indicator_data = AI_INDICATOR,
#'                   indicator_name = "misc_species",
#'                   bar_color = "#0085CA",
#'                   error_bars = TRUE)
#' }
#'
#' @import ggplot2 grDevices
#' @importFrom grDevices png
#'
#' @export

make_subarea_plot <- function(indicator_data, indicator_name, bar_color = "#0085CA", error_bars = TRUE) {

  region <- indicator_data$timeseries$SURVEY[1]

  area_id <- region_settings[[region]][['esr_subarea_id']]

  group_name <- chapter_settings[[region]][[indicator_name]][['group_name']]

  timeseries <- indicator_data$timeseries[indicator_data$timeseries$AREA_ID %in% area_id &
                                            indicator_data$timeseries$SPECIES_CODE %in% group_name, ]

  mean_sd <- indicator_data$mean_sd[indicator_data$mean_sd$AREA_ID %in% area_id &
                                      indicator_data$mean_sd$SPECIES_CODE %in% group_name, ]


  suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))

  for(ii in 1:length(group_name)) {

    sel_timeseries <- timeseries[timeseries$SPECIES_CODE == group_name[ii], ]
    sel_mean_sd <- mean_sd[mean_sd$SPECIES_CODE == group_name[ii], ]

    fig_cols <- length(area_id)

    if(fig_cols > 4) {
      fig_cols <- 3
    }

    if(error_bars) {

      p1 <- ggplot() +
        geom_point(data = sel_timeseries,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_BIOMASS)) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_PLUS1), linetype = 2) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_PLUS2), linetype = 3) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_MINUS1), linetype = 2) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_MINUS2), linetype = 3) +
        geom_linerange(data = sel_timeseries,
                       mapping = aes(x = YEAR,
                                     ymax = BIOMASS_PLUS2_SE,
                                     ymin = BIOMASS_MINUS2_SE),
                       color = "black") +
        geom_rect(data = sel_timeseries,
                  mapping = aes(xmin = YEAR+0.4,
                                xmax = YEAR-0.4,
                                ymax = BIOMASS_PLUS1_SE,
                                ymin = BIOMASS_MINUS1_SE),
                  fill = bar_color) +
        geom_segment(data = sel_timeseries,
                     aes(x = YEAR+0.4,
                         xend = YEAR-0.4,
                         y = BIOMASS_MT,
                         yend = BIOMASS_MT,
                         group = YEAR),
                     color = "black") +
        scale_y_continuous(name = capitalize_first(paste0(group_name[ii], " biomass (mt)")),
                           expand = c(0,0),
                           labels = function(x) format(x, scientific = TRUE)) +
        scale_x_continuous(name = "Year") +
        facet_wrap(~set_stratum_order(stratum = AREA_NAME, region = region),
                   scales = "free",
                   ncol = fig_cols) +
        theme_blue_strip()

    } else {

      p1 <- ggplot() +
        geom_point(data = sel_timeseries,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_BIOMASS)) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_PLUS1), linetype = 2) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_PLUS2), linetype = 3) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_MINUS1), linetype = 2) +
        geom_hline(data = sel_mean_sd,
                   mapping = aes(yintercept = MEAN_MINUS2), linetype = 3) +
        geom_point(data = sel_timeseries,
                   aes(x = YEAR,
                       y = BIOMASS_MT),
                   color = "black") +
        scale_y_continuous(name = capitalize_first(paste0(group_name[ii], " biomass (mt)")),
                           expand = c(0,0),
                           labels = function(x) format(x, scientific = TRUE)) +
        scale_x_continuous(name = "Year") +
        facet_wrap(~set_stratum_order(stratum = AREA_NAME, region = region),
                   scales = "free",
                   ncol = fig_cols) +
        theme_blue_strip()

    }

    png(filename = paste0("./plots/", region, "/", region, "_",
                          gsub(x = indicator_name, pattern = " ", replacement = "_"),
                          "_",
                          gsub(x = group_name[ii], pattern = " ", replacement = "_"), "_stratum.png"),
        width = 169,
        height = ifelse(length(area_id) > 4, 50 + 40 * ceiling(length(area_id)/3) - 1, 50),
        units = "mm",
        res = 300)
    print(p1 + theme(axis.text = element_text(size = 7),
                     axis.title = element_text(size = 8),
                     strip.text = element_text(size = 8)))
    dev.off()

  }

}
