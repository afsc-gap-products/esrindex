#' Replace a line in a character vector with multiple lines
#'
#' This function replaces lines in a character vector based on a specified pattern. Similar to gsub, but replaces multiple lines simultaneously.
#'
#' @param x A character vector.
#' @param pattern The pattern to search for in the lines.
#' @param replacement The replacement text for the lines matching the pattern.
#'
#' @return A modified character vector with replaced lines.
#'
#' @examples
#' x <- c("line1", "line2", "line3")
#' replacement <- c("a", "b", "c")
#' pattern <- "line2"
#' replace_lines(x, pattern, replacement)
#'
#' @export

replace_lines <- function(x, pattern, replacement) {

  if(length(replacement) > 1) {

    ind <- grep(x = x, pattern = pattern)

    index_line <- gsub(x = x[ind], pattern = pattern, replacement = replacement[1])

    leading <- x[1:(ind-1)]

    trailing <- x[(ind+1):length(x)]

    output <- c(leading, index_line, replacement[2:length(replacement)], trailing)

  } else {

    output <- gsub(x = x, pattern = pattern, replacement = replacement)

  }

  return(output)

}
