#' Get Connected Oracle Channel
#'
#' Establishes a connection to an Oracle database channel using the specified schema, username, and password.
#'
#' @param channel An open RODBC channel-- only used within functions to handle open connections.
#' @param schema A character string specifying the Oracle schema. If not provided, it prompts the user to enter the schema.
#'
#' @return An RODBC channel object representing the connection to the Oracle database.
#'
#' @details
#' This function connects to an Oracle database using the provided schema, username, and password. If the schema is not provided,
#' it prompts the user to enter the Oracle schema. The function returns an RODBC channel object representing the connection.
#'
#' @importFrom RODBC odbcConnect
#' @importFrom getPass getPass
#' @export

get_connected <- function(channel = NULL, schema = NA) {
  if (is.null(channel)) {
    (echo <- FALSE)
    if (is.na(schema)) {
      schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
    }
    username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
    password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
    channel <- RODBC::odbcConnect(
      dsn = paste(schema),
      uid = paste(username),
      pwd = paste(password),
      believeNRows = FALSE
    )
  }
  return(channel)
}


#' Capitalize the first letter of a character string
#'
#' This function takes a character string as input and returns a new string where the first letter is capitalized.
#'
#' @param x A character string.
#'
#' @return A character string with the first letter capitalized.
#' @export

capitalize_first <- function(x) {
  x <- paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
  return(x)
}


#' Find the midpoint of an sf LINESTRING
#' 
#' @param sf_lines sf object with LINESTRING geometries
#' @import dplyr sf
#' @noRd

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- sf::st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    get_mids <- function(coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- sf::st_point(get_mids(coords))
  })
  
  geometry <- sf::st_sfc(g_mids, crs = sf::st_crs(sf_lines))
  
  out <- sf::st_sf(geometry) |>
    dplyr::bind_cols(as.data.frame(sf_lines) |>
                       dplyr::select(-geometry))
  
  return(out)
}