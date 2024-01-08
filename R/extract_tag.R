#' Extract content between ESR XML template tags
#'
#' This function extracts the content between XML tags specified as a character vector.
#'
#' @param x A character vector containing XML data.
#' @param tag The XML tag to extract content from (e.g. "indicatorTitle").
#' @param remove_tabs A logical value indicating whether to remove tabs from the extracted content (default is TRUE).
#'
#' @return A character vector containing the extracted content between the specified XML tags.
#' @export

extract_tag <- function(x, tag, remove_tabs = TRUE) {

  tag_range <- grep(x = x, pattern = tag)

  x_sub <- x[min(tag_range):max(tag_range)]

  x_sub <- gsub(x = x_sub, pattern = paste0("<", tag, ">"), replacement = "")
  x_sub <- gsub(x = x_sub, pattern = paste0("</", tag, ">"), replacement = "")

  x_sub <- x_sub[nchar(x_sub) > 0]

  x_sub <- gsub(x = x_sub, pattern = "\t", replacement = "")

  x_sub <- trimws(x_sub, which = "right")

  return(x_sub)

}
