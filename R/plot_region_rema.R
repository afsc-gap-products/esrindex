#' Make ESR region plots with rema output
#'
#' This function writes full region ESR abundance indicators for rema fits and writes them to ./plots/[region]/[indicator]_full_region.png
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
# ' plot_region_rema(x = AI_INDICATOR, 
#'                   error_bar = TRUE, 
#'                   benchmarks = "zscore")
#' }
#' @import ggplot2 scales
#' @export

plot_region_rema <- function(x, 
                             error_bar = TRUE, 
                             benchmarks = "none", 
                             point_color = "#0085CA",
                             timeseries_color = "#000000",
                             errorbar_color = "#000000",
                             ribbon_fill = "grey50",
                             hline_color = "grey50",
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
                        x$rema_fit[[group_name[jj]]]$total_predicted_biomass)
      
      obs_dat <- rbind(obs_dat, 
                       dplyr::filter(x$timeseries, 
                                     SPECIES_CODE == group_name[jj],
                                     AREA_ID %in% region_settings[[region]]$esr_area_id))
      
    }
    
    obs_dat$group_name <- factor(obs_dat$SPECIES_CODE, 
                                 levels = group_name)
    
    fit_dat$group_name <- factor(fit_dat$group_name, 
                                 levels = group_name)
    
    if(set_unit == "kt") {
      
      fit_dat$pred <- fit_dat$pred/1000
      fit_dat$pred_lci <- fit_dat$pred_lci/1000
      fit_dat$pred_uci <- fit_dat$pred_uci/1000
      
      obs_dat$BIOMASS_MT <- obs_dat$BIOMASS_MT/1000
      obs_dat$BIOMASS_MINUS2_SD <- obs_dat$BIOMASS_MINUS2_SD/1000
      obs_dat$BIOMASS_MINUS1_SD <- obs_dat$BIOMASS_MINUS1_SD/1000
      obs_dat$BIOMASS_PLUS1_SD <- obs_dat$BIOMASS_PLUS1_SD/1000
      obs_dat$BIOMASS_PLUS2_SD <- obs_dat$BIOMASS_PLUS2_SD/1000
      
      lab_fun <- scales::label_comma
      lab_trim <- FALSE
      
    }
    
    if(set_unit == "mt") {
      lab_fun <- scales::label_comma
      lab_trim <- TRUE
    }
    
    ts_summary <- fit_dat |>
      dplyr::group_by(group_name) |>
      dplyr::summarise(z_mean = mean(pred, na.rm = TRUE),
                       sd = stats::sd(pred, na.rm = TRUE),
                       q100 = max(pred, na.rm = TRUE),
                       q75 = stats::quantile(pred, probs = 0.75, na.rm = TRUE),
                       q50 = stats::quantile(pred, probs = 0.5, na.rm = TRUE),
                       q25 = stats::quantile(pred, probs = 0.5, na.rm = TRUE),
                       q0 = min(pred, na.rm = TRUE))
    
    ts_summary$plus1 <- ts_summary$z_mean + ts_summary$sd
    ts_summary$plus2 <- ts_summary$z_mean + 2*ts_summary$sd
    ts_summary$minus1 <- ts_summary$z_mean - ts_summary$sd
    ts_summary$minus2 <- ts_summary$z_mean - 2*ts_summary$sd
    
    ts_summary$minus1[ts_summary$minus1 < 0] <- 0
    ts_summary$minus2[ts_summary$minus2 < 0] <- 0
    
    if(error_bar & benchmarks == "none") {
      
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3,
                    fill = ribbon_fill) +
        geom_errorbar(data = obs_dat,
                      mapping = aes(x = YEAR,
                                    ymin = BIOMASS_PLUS2_SD,
                                    ymax = BIOMASS_MINUS2_SD), 
                      width = 0.5,
                      color = errorbar_color) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
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
        geom_errorbar(data = obs_dat,
                      mapping = aes(x = YEAR,
                                    ymin = BIOMASS_PLUS2_SD,
                                    ymax = BIOMASS_MINUS2_SD), 
                      width = 0.5,
                      color = errorbar_color) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "zscore") {
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = z_mean)) +
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
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
    }
    
    
    if(error_bar & benchmarks == "quantile") {
      
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q50)) +
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
        geom_errorbar(data = obs_dat,
                      mapping = aes(x = YEAR,
                                    ymin = BIOMASS_PLUS2_SD,
                                    ymax = BIOMASS_MINUS2_SD), 
                      width = 0.5,
                      color = errorbar_color) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "quantile") {
      p1 <- ggplot() +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q50)) +
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
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT),
                   color = point_color) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred),
                  color = timeseries_color) +
        scale_y_continuous(name = paste0("Biomass Index (", set_unit, ")"), 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = lab_fun(trim = lab_trim)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
    }
    
    suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))
    
    grDevices::png(filename = paste0("./plots/", region, "/", region, "_rema_", 
                                     gsub(x = indicator_name[ii], pattern = " ", replacement = "_"), 
                                     "_full_region", append_filename, ".png"),
                   width = 169,
                   height = min(c(20+40*length(group_name), 225)),
                   units = "mm",
                   res = 300)
    print(p1)
    grDevices::dev.off()
    
  }
  
}
