#' Add Figure to QMD or RMD document
#'
#' This function uses outputs from esrindex::read_esr_xml to add figures to an RMD or QMD template document.
#'
#' @param x A list from esrindex::read_esr_xml containing figure number (fig_num), caption (fig_caption), and figure path (fig_path).
#' @export

add_figure <- function(x) {

  fig_path <- list.files(path = "./plots/", pattern = x$fig_path, recursive = TRUE, full.names = TRUE)

  if(nchar(fig_path) < 1) {
    paste0("add_figure: Figure path (", x$fig_path, ")", "from the XML template file not found. Check that the file exists in the /plots/ directory.")

  }

  c("", "", "",
    paste0("```{r fig", x$fig_number, ", include = TRUE, echo = FALSE, " ,
           "fig.cap='\\\\label{fig:figs}Figure ", x$fig_number, ". ", x$caption, "'}"),
    paste0("knitr::include_graphics('.", fig_path, "')"),
    "```", "", "")
}
