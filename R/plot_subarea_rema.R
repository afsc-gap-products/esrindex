#' Make ESR subarea/stratum plots with rema output
#'
#' This function writes full region ESR abundance indicators for rema fits and writes them to ./plots/[region]/[indicator]_subarea_bar.png and ./plots/[region]/[indicator]_subarea_point.png
#'
#' @param x Indicator list (e.g. esrindex::AI_INDICATOR)
#' @param error_bar Should annual observations include error bars?
#' @param benchmarks "none", "zscore", or "quantile"
#' @param append_filename Character vector to include in file name.
#' @param set_unit Set units to use for output ("kt" = kiloton, "mt" = metric tons)
#' @param point_color Color for points.
#' @param errorbar_color Color for error bar.
#' @param timeseries_color Color for time series line.
#' @param ribbon_fill Color for time series ribbon.
#' @param hline_color Color for Z-score/quantile lines.
#' @return The function doesn't return any value; it generates and saves plots based on the input data.
#'
#' @examples
#' \dontrun{
#' # Example:
# ' plot_subarea_rema(x = AI_INDICATOR, 
#'                    error_bar = TRUE, 
#'                    benchmarks = "zscore")
#' }
#' 
#' @import ggplot2
#' @importFrom grDevices png
#' @export

plot_subarea_rema <- function(x, 
                              error_bar = TRUE, 
                              point_color = "#0085CA",
                              timeseries_color = "#000000",
                              errorbar_color = "#000000",
                              ribbon_fill = "grey50",
                              hline_color = "grey50",
                              benchmarks = "none", 
                              append_filename = "", 
                              set_unit = "kt") {
  
  region <- x$timeseries$SURVEY[1]
  
  indicator_name <- names(chapter_settings[[region]])
  
  for(ii in 1:length(indicator_name)) {
    
    group_name <- chapter_settings[[region]][[indicator_name[ii]]]$group_name
    
    fit_dat <- data.frame()
    obs_dat <- data.frame()
    
    for(jj in 1:length(group_name)) {
      
      fit_dat <- rbind(fit_dat, 
                       x$rema_fit[[group_name[jj]]]$biomass_by_strata |>
                         dplyr::filter(strata %in% region_settings[[region]]$esr_subarea_id))
      
      obs_dat <- rbind(obs_dat, 
                       dplyr::filter(x$timeseries, 
                                     SPECIES_CODE == group_name[jj],
                                     AREA_ID %in% region_settings[[region]]$esr_subarea_id))
      
    }
    
    bar_dat <- fit_dat |>
      dplyr::filter(year %in% unique(obs_dat$YEAR))
    
    obs_dat$group_name <- obs_dat$SPECIES_CODE
    
    if(set_unit == "kt") {
      
      fit_dat$pred <- fit_dat$pred/1000
      fit_dat$obs <- fit_dat$obs/1000
      fit_dat$obs_lci <- fit_dat$obs_lci/1000
      fit_dat$obs_uci <- fit_dat$obs_uci/1000
      fit_dat$pred_lci <- fit_dat$pred_lci/1000
      fit_dat$pred_uci <- fit_dat$pred_uci/1000
      
      bar_dat$pred <- bar_dat$pred/1000
      
      lab_fun <- scales::label_comma
      lab_trim <- FALSE
      lab_digits <- NULL
      
    }
    
    if(set_unit == "mt") {
      
      lab_fun <- scales::label_comma
      lab_trim <- TRUE
      lab_digits <- 2
      
    }
    
    ts_summary <- fit_dat |>
      dplyr::group_by(group_name, strata) |>
      dplyr::summarise(z_mean = mean(pred, na.rm = TRUE),
                       sd = sd(pred, na.rm = TRUE),
                       q100 = max(pred, na.rm = TRUE),
                       q75 = quantile(pred, probs = 0.75, na.rm = TRUE),
                       q50 = quantile(pred, probs = 0.5, na.rm = TRUE),
                       q25 = quantile(pred, probs = 0.5, na.rm = TRUE),
                       q0 = min(pred, na.rm = TRUE))
    
    ts_summary$plus1 <- ts_summary$z_mean + ts_summary$sd
    ts_summary$plus2 <- ts_summary$z_mean + 2*ts_summary$sd
    ts_summary$minus1 <- ts_summary$z_mean - ts_summary$sd
    ts_summary$minus2 <- ts_summary$z_mean - 2*ts_summary$sd
    
    ts_summary$minus1[ts_summary$minus1 < 0] <- 0
    ts_summary$minus2[ts_summary$minus2 < 0] <- 0
    
    fill_colors <- esrindex_pal()(length(unique(ts_summary$strata)))
    n_facet_row <- length(unique(obs_dat$SPECIES_CODE))
    
    
    grid_levels <- c()
    
    taxa_levels <- chapter_settings[[region]][[indicator_name[ii]]]$group_name
    
    stratum_levels <- levels(esrindex::set_stratum_order(area_id = unique(fit_dat$strata), 
                                                         region = region,
                                                         use_abbreviation = TRUE))
    for(kk in 1:length(taxa_levels)){
      
      grid_levels <- c(grid_levels, paste(stratum_levels, taxa_levels[kk]))
      
    }
    
    fill_label <- "Subarea"
    
    if(region %in% "EBS") {
      fill_label <- "Stratum"
    }
    
    
    # Setup plot order
    obs_dat$taxa_stratum <- factor(
      paste(esrindex::set_stratum_order(area_id = obs_dat$AREA_ID, 
                                        region = region,
                                        use_abbreviation = TRUE), 
            obs_dat$SPECIES_CODE), 
      levels = grid_levels)
    
    bar_dat$taxa_stratum <- factor(
      paste(esrindex::set_stratum_order(area_id = bar_dat$strata, 
                                        region = region,
                                        use_abbreviation = TRUE), 
            bar_dat$group_name), 
      levels = grid_levels)
    
    fit_dat$taxa_stratum <- factor(
      paste(esrindex::set_stratum_order(area_id = fit_dat$strata, 
                                        region = region,
                                        use_abbreviation = TRUE), 
            fit_dat$group_name), 
      levels = grid_levels)
    
    ts_summary$taxa_stratum <- factor(
      paste(esrindex::set_stratum_order(area_id = ts_summary$strata, 
                                        region = region,
                                        use_abbreviation = TRUE), 
            ts_summary$group_name), 
      levels = grid_levels)
    
    if(error_bar & benchmarks == "none") {
      
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3,
                    fill = ribbon_fill) +
        geom_errorbar(data = fit_dat,
                      mapping = aes(x = year,
                                    ymin = obs_lci,
                                    ymax = obs_uci), 
                      width = 0.5,
                      color = errorbar_color) +
        geom_point(data = fit_dat,
                   mapping = aes(x = year, y = obs),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~taxa_stratum, 
                   scales = "free_y",
                   nrow = n_facet_row) +
        theme_blue_strip()
      
    }
    
    if(error_bar & benchmarks == "zscore") {
      
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = z_mean),
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus1), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus2), 
                   linetype = 3,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus1), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus2), 
                   linetype = 3,
                   color = hline_color) +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3,
                    fill = ribbon_fill) +
        geom_errorbar(data = fit_dat,
                      mapping = aes(x = year,
                                    ymin = obs_lci,
                                    ymax = obs_uci), 
                      width = 0.5,
                      color = errorbar_color) +
        geom_point(data = fit_dat,
                   mapping = aes(x = year, y = obs),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~taxa_stratum, 
                   scales = "free_y",
                   nrow = n_facet_row) +
        theme_blue_strip()
      
    }
    
    
    if(!error_bar & benchmarks == "zscore") {
      
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = z_mean),
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus1), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus2), 
                   linetype = 3,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus1), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus2), 
                   linetype = 3,
                   color = hline_color) +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3,
                    fill = ribbon_fill) +
        geom_point(data = fit_dat,
                   mapping = aes(x = year, y = obs),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~taxa_stratum, 
                   scales = "free_y",
                   nrow = n_facet_row) +
        theme_blue_strip()
      
      
    }
    
    
    if(error_bar & benchmarks == "quantile") {
      
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q50),
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q75), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q100), 
                   linetype = 3,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q25), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q0), 
                   linetype = 3,
                   color = hline_color) +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3,
                    fill = ribbon_fill) +
        geom_errorbar(data = fit_dat,
                      mapping = aes(x = year,
                                    ymin = obs_lci,
                                    ymax = obs_uci), 
                      width = 0.5,
                      color = errorbar_color) +
        geom_point(data = fit_dat,
                   mapping = aes(x = year, y = obs),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~taxa_stratum, 
                   scales = "free_y",
                   nrow = n_facet_row) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "quantile") {
      
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q50),
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q75), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q100), 
                   linetype = 3,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q25), 
                   linetype = 2,
                   color = hline_color) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q0), 
                   linetype = 3,
                   color = hline_color) +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3,
                    fill = ribbon_fill) +
        geom_point(data = fit_dat,
                   mapping = aes(x = year, y = obs),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~taxa_stratum, 
                   scales = "free_y",
                   nrow = n_facet_row) +
        theme_blue_strip()
      
    }
    
    p2 <- ggplot() +
      geom_bar(data = bar_dat,
               mapping = aes(x = year,
                             y = pred,
                             fill = esrindex::set_stratum_order(area_id = strata, 
                                                                region = region,
                                                                use_abbreviation = TRUE)),
               position = "stack",
               stat = "identity", width = 1) +
      facet_wrap(~group_name, scales = "free_y", nrow = length(group_name)) +
      scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                         expand = expansion(mult = c(0, 0.05)), 
                         labels = lab_fun(trim = lab_trim)) +
      scale_fill_manual(name = fill_label, values = fill_colors) +
      scale_x_continuous(name = "Year") +
      theme_blue_strip() +
      theme(legend.title = element_text())
    
    # Save plots
    suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))
    
    png(filename = paste0("./plots/", region, "/", region, "_rema_", 
                          gsub(x = indicator_name[ii], pattern = " ", replacement = "_"), 
                          "_subarea_point", append_filename, ".png"),
        width = 169,
        height = min(c(20+40*length(group_name), 225)),
        units = "mm",
        res = 300)
    print(p1 + theme(axis.text = element_text(size = 6.5)))
    dev.off()
    
    
    suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))
    
    png(filename = paste0("./plots/", region, "/", region, "_rema_", 
                          gsub(x = indicator_name[ii], pattern = " ", replacement = "_"), 
                          "_subarea_bar", append_filename, ".png"),
        width = 169,
        height = min(c(40+40*length(group_name), 225)),
        units = "mm",
        res = 300)
    print(p2)
    dev.off()
    
  }
  
}