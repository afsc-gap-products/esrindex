#' Make ESR region plots with rema output
#'
#' This function writes full region ESR abundance indicators for rema fits and writes them to ./plots/[region]/[indicator].png
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
# ' plot_rema_region(x = AI_INDICATOR)
#' }
#' 
#' @import ggplot2 grDevices scales dplyr
#' @export


plot_rema_region <- function(x, error_bar = TRUE, benchmarks = "none", append_filename = "") {
  
  region <- x$timeseries$SURVEY[1]
  
  indicator_name <- names(chapter_settings[[region]])
  
  for(ii in 1:length(indicator_name)) {
    
    group_name <- chapter_settings[[region]][[indicator_name[ii]]]$group_name
    
    plot_dat <- data.frame()
    
    for(jj in 1:length(group_name)) {
      
      plot_dat <- rbind(plot_dat, x$rema_fit[[group_name[jj]]]$biomass_by_strata)
      
    }
    
    ts_summary <- plot_dat |>
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
        geom_ribbon(data = plot_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_errorbar(data = plot_dat,
                      mapping = aes(x = year,
                                    ymin = obs_lci,
                                    ymax = obs_uci), width = 0.5) +
        geom_point(data = plot_dat,
                   mapping = aes(x = year, y = obs)) +
        geom_path(data = plot_dat,
                  mapping = aes(x = year, y = pred)) +
        scale_y_continuous(name = "Biomass (mt)", expand = expansion(mult = c(0, 0.05)), labels = scales::scientific) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(error_bar & benchmarks == "zscore") {
      
      p1 <- ggplot() +
        geom_ribbon(data = plot_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_errorbar(data = plot_dat,
                      mapping = aes(x = year,
                                    ymin = obs_lci,
                                    ymax = obs_uci), width = 0.5) +
        geom_point(data = plot_dat,
                   mapping = aes(x = year, y = obs)) +
        geom_path(data = plot_dat,
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
        scale_y_continuous(name = "Biomass (mt)", expand = expansion(mult = c(0, 0.05)), labels = scales::scientific) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "zscore") {
      p1 <- ggplot() +
        geom_ribbon(data = plot_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_point(data = plot_dat,
                   mapping = aes(x = year, y = obs)) +
        geom_path(data = plot_dat,
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
        scale_y_continuous(name = "Biomass (mt)", expand = expansion(mult = c(0, 0.05)), labels = scales::scientific) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
    }
    
    
    if(error_bar & benchmarks == "quantile") {
      
      p1 <- ggplot() +
        geom_ribbon(data = plot_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_errorbar(data = plot_dat,
                      mapping = aes(x = year,
                                    ymin = obs_lci,
                                    ymax = obs_uci), width = 0.5) +
        geom_point(data = plot_dat,
                   mapping = aes(x = year, y = obs)) +
        geom_path(data = plot_dat,
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
        scale_y_continuous(name = "Biomass (mt)", expand = expansion(mult = c(0, 0.05)), labels = scales::scientific) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
      
    }
    
    if(!error_bar & benchmarks == "quantile") {
      p1 <- ggplot() +
        geom_ribbon(data = plot_dat,
                    mapping = aes(x = year,
                                  ymin = pred_lci,
                                  ymax = pred_uci),
                    alpha = 0.3) +
        geom_point(data = plot_dat,
                   mapping = aes(x = year, y = obs)) +
        geom_path(data = plot_dat,
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
        scale_y_continuous(name = "Biomass (mt)", expand = expansion(mult = c(0, 0.05)), labels = scales::scientific) +
        scale_x_continuous(name = "Year") +
        expand_limits(y = 0) +
        facet_wrap(~group_name, scales = "free", nrow = length(group_name)) +
        theme_blue_strip()
    }
    
    suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))
    
    png(filename = paste0("./plots/", region, "/", region, "_rema_", append_filename, gsub(x = indicator_name[ii], pattern = " ", replacement = "_"), "_full_region.png"),
        width = 169,
        height = 40*length(group_name),
        units = "mm",
        res = 300)
    print(p1)
    dev.off()
    
  }
  
}
