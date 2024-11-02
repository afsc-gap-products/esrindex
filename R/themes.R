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
          panel.grid = element_blank(),
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
#' Set stratum plotting order based on AREA_ID or STRATUM
#'
#' @param stratum GAP_PRODUCTS stratum as a character vector.
#' @param area_id GAP_PRODUCTS AREA_ID as a character vector.
#' @param region Character vector of length one indicating whether the region is AI, GOA, or BS
#' @param use_abbreviation Should stratum labels be abbreviated?
#' @export

set_stratum_order <- function(stratum = NULL, area_id = NULL, region, use_abbreviation = FALSE) {
  
  region <- toupper(region)
  
  if(!(region %in% c("AI", "GOA", "EBS", "NBS"))) {
    stop("Region must be either: AI, EBS, NBS, or GOA")
  }

  
  by_stratum <- is.numeric(stratum) | is.character(stratum) | is.factor(stratum)
  by_area_id <- is.numeric(area_id) | is.character(area_id) | is.factor(area_id)
  
  stopifnot("set_stratum_order: Must provide either stratum or area_id" = sum(c(!is.null(stratum), 
                                                                                !is.null(area_id))) == 1)
  
  stopifnot("set_stratum_order: Must pass either stratum or area_id, not both" = sum(c(by_stratum , 
                                                                                       by_area_id)) == 1)
  
  if(by_stratum) {
    
    x <- as.character(stratum)
    
    set_levels <- switch(region,
                         "GOA" = c("Shumagin",
                                   "Chirikof",
                                   "Kodiak",
                                   "Yakutat",
                                   "Southeastern"),
                         "EBS" = c("1.0",
                                   "2.0",
                                   "3.0",
                                   "4.0",
                                   "5.0",
                                   "6.0"),
                         "NBS" = c("Inner Domain",
                                   "Middle Domain"),
                         "AI" = c("Southern Bering Sea",
                                  "Eastern Aleutians",
                                  "Central Aleutians",
                                  "Western Aleutians")
    )
    
  }
  
  if(by_area_id) {
    
    x <- as.character(area_id)
    
    
    set_levels <- switch(region,
                         "GOA" = c("919",
                                   "929",
                                   "939",
                                   "949",
                                   "959"),
                         "EBS" = c("1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6"),
                         "NBS" = c("70", 
                                   "71", 
                                   "81"),
                         "AI" = c("799",
                                  "5699",
                                  "3499",
                                  "299")
    )
    
    
  }
  
  if(!use_abbreviation) {
    
    set_labels <- switch(region,
                         "GOA" = c("Shumagin",
                                   "Chirikof",
                                   "Kodiak",
                                   "Yakutat",
                                   "Southeastern"),
                         "EBS" = c("Stratum 10", 
                                   "Stratum 20", 
                                   "Stratum 30", 
                                   "Stratum 40", 
                                   "Stratum 50", 
                                   "Stratum 60"),
                         "NBS" = c("Stratum 70",
                                   "Stratum 71",
                                   "Stratum 81"),
                         "AI" = c("Southern Bering Sea",
                                  "Eastern Aleutians",
                                  "Central Aleutians",
                                  "Western Aleutians")
    )
    
  }
  
  
  if(use_abbreviation) {
    
    set_labels <- switch(region,
                         "GOA" = c("Shu",
                                   "Chi",
                                   "Kod",
                                   "Yak",
                                   "SE"),
                         "EBS" = c("10", 
                                   "20", 
                                   "30", 
                                   "40", 
                                   "50", 
                                   "60"),
                         "NBS" = c("70", 
                                   "71", 
                                   "81"),
                         "AI" = c("SBS",
                                  "EAI",
                                  "CAI",
                                  "WAI")
    )
    
  }
  
  output <- factor(x, levels = set_levels, labels = set_labels)
  
  return(output)
  
}


#' Color palette for esrindex
#' 
#' This is the 'oceans' color palette from nmfspalette
#' 
#' @param option Color palette option (noaa-blue or brown-green)
#' @param ... Optional arguments passed to colorRampPalette
#' @importFrom grDevices colorRampPalette
#' @export

esrindex_pal <- function (option = "noaa-blue", ...) 
{
  
  pal <- switch(option,
         'noaa-blue' = c("#B3EDEF", "#6DDBE1", "#1ECAD3", "#3EA2D5", "#001743"),
         'brown-green' = c("#8C510A", "#D8B365", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#5AB4AC", "#01665E"))

  colorRampPalette(pal, ...)
}
