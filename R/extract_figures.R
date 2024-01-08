#' Extract figure info from ESR XML template
#'
#' This function extracts information about figures from XML <figureNumber>, <figureBasename>, and <figureCaption> tags.
#'
#' @param x A character vector containing XML data.
#'
#' @return A list containing information about figures, including figure number, path, and caption.
#' @export

extract_figures <- function(x) {

  n_figs <- length(grep(x = x, pattern = "<figureNumber>"))

  if(n_figs < 1) {
    return(NULL)
  }

  fig_list <- vector(mode = "list", length = n_figs)

  for(ii in 1:n_figs) {

    num_range <- grep(x = x, pattern = "<figureNumber>")[ii]:grep(x = x, pattern = "</figureNumber>")[ii]
    path_range <- grep(x = x, pattern = "<figureBasename>")[ii]:grep(x = x, pattern = "</figureBasename>")[ii]
    caption_range <- grep(x = x, pattern = "<figureCaption>")[ii]:grep(x = x, pattern = "</figureCaption>")[ii]

    fig_list[[ii]] <- list(fig_number = extract_tag(x = x[num_range],
                                                    tag = "figureNumber"),
                           fig_path = extract_tag(x = x[path_range],
                                                  tag = "figureBasename"),
                           caption = extract_tag(x = x[caption_range],
                                                 tag = "figureCaption"))
  }

  return(fig_list)

}
