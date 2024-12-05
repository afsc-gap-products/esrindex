#' Get AFSC species codes for each taxonomic group
#'
#' This function takes a taxonomic grouping in the form of a character string and returns a vector of species codes that are found within that taxonomic group.
#'
#' @param group A character string designating the taxonomic group
#' @param rank A character string designating the taxonomic rank of grouping
#' @param tax_table Taxonomic classification table from Oracle database
#'
#' @return A numeric vector of species codes
#' @export

get_group_codes <- function(group, rank, tax_table) {
  tt <- tax_table
  if (rank != "Species") {
    col <- paste0(toupper(rank), "_TAXON")
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
