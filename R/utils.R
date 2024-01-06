#' Get Connected Oracle Channel
#'
#' Establishes a connection to an Oracle database channel using the specified schema, username, and password.
#'
#' @param schema A character string specifying the Oracle schema. If not provided, it prompts the user to enter the schema.
#'
#' @return An RODBC channel object representing the connection to the Oracle database.
#'
#' @details
#' This function connects to an Oracle database using the provided schema, username, and password. If the schema is not provided,
#' it prompts the user to enter the Oracle schema. The function returns an RODBC channel object representing the connection.
#'
#' @seealso
#' \code{\link{RODBC::odbcConnect}}, \code{\link{getPass::getPass}}
#'
#' @importFrom RODBC odbcConnect
#' @importFrom getPass getPass
#'
#' @examples
#' \dontrun{
#' # Example Usage:
#' channel <- get_connected(schema = "AFSC")
#' }
#'
#' @export

get_connected <- function(channel = NULL, schema = NA){
  if(is.null(channel)) {
    (echo = FALSE)
    if(is.na(schema)) {
      schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
    }
    username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
    password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
    channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                   uid = paste(username),
                                   pwd = paste(password),
                                   believeNRows = FALSE)
  }
  return(channel)
}


#' Blue Strip Theme
#'
#' Custom theme for ggplot2 with a blue strip at the top and specific styling for axes, legend, and strip text.
#'
#' @return A ggplot2 theme object.
#'
#' @details
#' This function creates a custom ggplot2 theme with a blue strip at the top, styled axes, legend at the bottom,
#' and customized strip text appearance. It is designed for aesthetic improvements in data visualization.
#'
#' @seealso
#' \code{\link{ggplot2::theme_bw}}, \code{\link{ggplot2::element_text}}, \code{\link{ggplot2::element_line}},
#' \code{\link{ggplot2::element_blank}}, \code{\link{ggplot2::element_rect}}, \code{\link{margin}}
#'
#' @importFrom ggplot2 theme_bw element_text element_line element_blank element_rect margin
#'
#' @examples
#' \dontrun{
#' # Example Usage:
#' ggplot(data, aes(x = variable, y = value, fill = group)) +
#'   geom_bar(stat = "identity") +
#'   theme_blue_strip()
#' }
#'
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
