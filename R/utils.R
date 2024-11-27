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



#' Get AFSC species codes for each taxonomic group
#'
#' This function takes a taxonomic grouping in the form of a character string and returns a vector of species codes that are found within that taxonomic group.
#'
#' @param group A character string designating the taxonomic group
#' @param level A character string designating the taxonomic rank of grouping
#' @param tt Taxonomic classification table from Oracle database
#'
#' @return A numeric vector of species codes
#' @export
get_group_codes <- function(group, level, tt) {
  if (level != "Species") {
    col <- paste0(toupper(level), "_TAXON")
    out <- tt$SPECIES_CODE[tt[eval(col)] == eval(group) & tt$SURVEY_SPECIES == 1]
    if (group == "Anthozoa") {
      # fixing paraphyly of coral group
      out <- tt$SPECIES_CODE[tt$SUBPHYLUM_TAXON == "Anthozoa" |
                               tt$FAMILY_TAXON == "Stylasteridae"]
      dd <- tt$SPECIES_CODE[tt$SUPERFAMILY_TAXON == "Pennatuloidea" |
                              tt$ORDER_TAXON == "Actiniaria" |
                              tt$ORDER_TAXON == "Zoantharia" |
                              tt$FAMILY_TAXON == "Corallimorphidae"]
      out <- out[!out %in% dd]
    }
    
    # awkward fixes for differences between taxonomy and what we functionally and commonly include in these taxonomic groupings
    if (group == "Scyphozoa") {
      out <- tt$SPECIES_CODE[tt$CLASS_TAXON == "Scyphozoa" |
                               tt$FAMILY_TAXON == "Laodiceidae" |
                               tt$FAMILY_TAXON == "Aequoreidae" |
                               tt$FAMILY_TAXON == "Corynidae"]
    }
    if (group == "Actiniaria") {
      out <- tt$SPECIES_CODE[tt$ORDER_TAXON == "Actiniaria" |
                               tt$ORDER_TAXON == "Zoantharia" |
                               tt$ORDER_TAXON == "Corallimorpharia"]
    }
    if (group == "Caridea") {
      out <- tt$SPECIES_CODE[tt$INFRAORDER_TAXON == "Caridea" |
                               tt$SUBORDER_TAXON == "Dendrobranchiata"]
    }
  } else {
    out <- tt$SPECIES_CODE[tt$SPECIES_NAME == eval(group) & tt$SURVEY_SPECIES == 1]
  }
  out <- sort(unique(out[!is.na(out)]))
  out
}

