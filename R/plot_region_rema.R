#' Make ESR region plots with rema output
#'
#' This function writes full region ESR abundance indicators for rema fits and writes them to ./plots/[region]/[indicator]_full_region.png
#'
#' @param x Indicator list (e.g. esrindex::AI_INDICATOR)
#' @param error_bar Should annual observations include error bars?
#' @param benchmarks "none", "zscore", or "quantile"
#' @param append_filename Character vector to include in file name.
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

plot_region_rema <- function(x, error_bar = TRUE, benchmarks = "none", append_filename = "") {
  
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
    
    obs_dat$group_name <- obs_dat$SPECIES_CODE
    
    ts_summary <- fit_dat |>
      dplyr::group_by(group_name) |>
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
    
    if(error_bar & benchmarks == "none") {
      
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_errorbar(data = obs_dat,
                      mapping = aes(x = YEAR,
                                    ymin = BIOMASS_PLUS2_SD,
                                    ymax = BIOMASS_MINUS2_SD), 
                      width = 0.5) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred)) +
        scale_y_continuous(name = "Biomass Index (mt)", 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = scales::label_scientific(digits = 2, 
                                                             trim = FALSE)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(error_bar & benchmarks == "zscore") {
      
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_errorbar(data = obs_dat,
                      mapping = aes(x = YEAR,
                                    ymin = BIOMASS_PLUS2_SD,
                                    ymax = BIOMASS_MINUS2_SD), 
                      width = 0.5) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = z_mean)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus1), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus2), 
                   linetype = 3) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus1), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus2), 
                   linetype = 3) +
        scale_y_continuous(name = "Biomass Index (mt)", 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = scales::label_scientific(digits = 2, 
                                                             trim = FALSE)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "zscore") {
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = z_mean)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus1), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = plus2), 
                   linetype = 3) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus1), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = minus2), 
                   linetype = 3) +
        scale_y_continuous(name = "Biomass Index (mt)", 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = scales::label_scientific(digits = 2, 
                                                             trim = FALSE)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
    }
    
    
    if(error_bar & benchmarks == "quantile") {
      
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_errorbar(data = obs_dat,
                      mapping = aes(x = YEAR,
                                    ymin = BIOMASS_PLUS2_SD,
                                    ymax = BIOMASS_MINUS2_SD), 
                      width = 0.5) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q50)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q75), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q100), 
                   linetype = 3) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q25), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q0), 
                   linetype = 3) +
        scale_y_continuous(name = "Biomass Index (mt)", 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = scales::label_scientific(digits = 2, 
                                                             trim = FALSE)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "quantile") {
      p1 <- ggplot() +
        geom_ribbon(data = fit_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_point(data = obs_dat,
                   mapping = aes(x = YEAR, y = BIOMASS_MT)) +
        geom_path(data = fit_dat,
                  mapping = aes(x = year, y = pred)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q50)) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q75), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q100), 
                   linetype = 3) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q25), 
                   linetype = 2) +
        geom_hline(data = ts_summary,
                   mapping = aes(yintercept = q0), 
                   linetype = 3) +
        scale_y_continuous(name = "Biomass Index (mt)", 
                           expand = expansion(mult = c(0, 0.05)), 
                           labels = scales::label_scientific(digits = 2, 
                                                             trim = FALSE)) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
    }
    
    suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))
    
    png(filename = paste0("./plots/", region, "/", region, "_rema_", 
                          gsub(x = indicator_name[ii], pattern = " ", replacement = "_"), 
                          "_full_region", append_filename, ".png"),
        width = 169,
        height = min(c(20+40*length(group_name), 225)),
        units = "mm",
        res = 300)
    print(p1)
    dev.off()
    
  }
  
}
