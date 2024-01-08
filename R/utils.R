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
