#' Make ESR region plots with rema output
#'
#' This function writes full region ESR abundance indicators for rema fits and writes them to ./plots/[region]/[indicator].png
#'
#' @param x Indicator list (e.g. esrindex::AI_INDICATOR)
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


plot_rema_region <- function(x) {
  
  region <- x$timeseries$SURVEY[1]
  
  indicator_name <- names(chapter_settings[[region]])
  
  for(ii in 1:length(indicator_name)) {
    
    group_name <- chapter_settings[[region]][[indicator_name[ii]]]$group_name
    
    plot_dat <- data.frame()
    
    for(jj in 1:length(group_name)) {
      
      plot_dat <- rbind(plot_dat, x$rema_fit[[group_name[jj]]]$biomass_by_strata)
      
    }
    
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
    
    
    suppressWarnings(dir.create(paste0("./plots/", region), recursive = TRUE))
    
    png(filename = paste0("./plots/", region, "/", region, "_rema_", gsub(x = indicator_name[ii], pattern = " ", replacement = "_"), "_full_region.png"),
        width = 169,
        height = 40*length(group_name),
        units = "mm",
        res = 300)
    print(p1)
    dev.off()
    
  }
  
}
