#' Read tag data from an ESR XML template file
#'
#' This function reads tag data from an ESR (Ecosystem Status Report) XML template file.
#'
#' @param xml_path The path to the ESR XML template file.
#'
#' @return A list containing information extracted from various XML tags.
#' @export

read_esr_xml <- function(xml_path) {

  xml_path <- "AI_misc_species.xml"

  stopifnot("read_esr_xml: File path (xml_path) not found." = file.exists(xml_path))

  lines <- readLines(con = xml_path)

  text_data <- list(
    esr_name = extract_tag(x = lines, tag = "indicatorShortName"),
    title = extract_tag(x = lines, tag = "indicatorTitle"),
    region = extract_tag(x = lines, tag = "indicatorRegion"),
    authors = extract_tag(x = lines, tag = "authors"),
    email = extract_tag(x = lines, tag = "contactEmail"),
    affiliation = extract_tag(x = lines, tag = "authorAffiliations"),
    last_update = extract_tag(x = lines, tag = "lastUpdate"),
    description = extract_tag(x = lines, tag = "indicatorDescription"),
    methodological_changes = extract_tag(x = lines, tag = "methodologicalChanges"),
    status_trends = extract_tag(x = lines, tag = "statusAndTrends"),
    factors = extract_tag(x = lines, tag = "factorsInfluencingTrends"),
    implications = extract_tag(x = lines, tag = "implications"),
    research_priorities = extract_tag(x = lines, tag = "researchPriorities")
    )

  fig_data <- extract_figures(lines)

  return(list(text_data = text_data,
              fig_data = fig_data))

}
