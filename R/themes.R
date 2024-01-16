#' Blue strip theme
#'
#' Custom theme for ggplot2 with a blue strip at the top and specific styling for axes, legend, and strip text.
#'
#' @return A ggplot2 theme object.
#'
#' @details
#' This function creates a custom ggplot2 theme with a blue strip at the top, styled axes, legend at the bottom,
#' and customized strip text appearance. It is designed for aesthetic improvements in data visualization.
#'
#' @import ggplot2
#' @export

theme_blue_strip <- function() {
  theme_bw() %+replace%
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          # panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA))
}


#' Set stratum plotting order
#'
#' @param stratum Stratum as a character vector
#' @param region Character vector of length one indicating whether the region is AI, GOA, or BS
#' @export

set_stratum_order <- function(stratum, region) {
  region <- toupper(region)
  if(!(region %in% c("AI", "GOA", "EBS", "NBS"))) {
    stop("Region must be either: AI, EBS, NBS, or GOA")
  }
  if(region == "GOA") {
    return(factor(stratum,
                  levels = c("Shumagin",
                             "Chirikof",
                             "Kodiak",
                             "Yakutat",
                             "Southeastern")))
  } else if(region == "AI") {
    return(factor(stratum,
                  levels = c("Southern Bering Sea",
                             "Eastern Aleutians",
                             "Central Aleutians",
                             "Western Aleutians")))
  } else if(region == "EBS") {
    return(factor(stratum,
                  levels = c("1.0",
                             "2.0",
                             "3.0",
                             "4.0",
                             "5.0",
                             "6.0"),
                  labels = c("Stratum 10", "Stratum 20", "Stratum 30", "Stratum 40", "Stratum 50", "Stratum 60")))
  } else {
    return(factor(stratum))
  }
}
