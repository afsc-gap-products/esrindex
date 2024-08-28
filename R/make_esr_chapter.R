#' Create ESR Chapter Document
#'
#' This function generates an R Markdown document for an ESR chapter using an ESR XML template file then renders as a .docx.
#'
#' @param xml_path Path to the ESR template file containing ESR chapter information.
#' @param output_path Path to save the generated R Markdown document. If not specified, a default path will be used.
#' @param bib_path Optional. Path to bibliography (.bib) file.
#' @param csl_path Optional (required if bib_path is provided). Path to Citation Style Language (.csl) file.
#' @return None
#' @examples
#' \dontrun{
#' make_esr_chapter(xml_path = "./chapters/AI_misc_species.xml")
#' }
#'
#' @import rmarkdown knitr
#' @export

make_esr_chapter <- function(xml_path, csl_path = NULL, bib_path = NULL, output_path = NULL) {

  chapter_data <- read_esr_xml(xml_path = xml_path)

  if(is.null(output_path)) {
    output_path <- gsub(x = paste0("./chapters/", basename(xml_path)),
                        pattern = ".xml",
                        replacement = ".Rmd")
  }

  suppressWarnings(dir.create(dirname(output_path)))

  lines <- readLines(system.file("extdata/esr_index_template.Rmd", package = "esrindex"))
  
  if(!is.null(csl_path)) {
    
    stopifnot("make_esr_chapter: .csl file not found at csl_path." = file.exists(csl_path))
    stopifnot("make_esr_chapter: output_path and csl_path must point to the same root directory." = 
                dirname(output_path) == dirname(csl_path))
    
    lines <- gsub(pattern = "\\[CSL_PATH\\]",
                  replacement = paste0("csl: ", basename(csl_path)),
                  x = lines)
  } else {
    
    lines <- lines[!grepl(pattern = "\\[CSL_PATH\\]",
                          x = lines)]
    
  }
  
  if(!is.null(bib_path)) {
    
    stopifnot("make_esr_chapter: .bib file not found at bib_path." = file.exists(csl_path))
    stopifnot("make_esr_chapter: output_path and bib_path must point to the same root directory." = 
                dirname(output_path) == dirname(bib_path))
    
    lines <- gsub(pattern = "\\[BIB_PATH\\]",
                  replacement = paste0("bibliography: ", basename(bib_path)),
                  x = lines)
  } else {
    
    lines <- lines[!grepl(pattern = "\\[BIB_PATH\\]",
                          x = lines)]
  }

  lines <- gsub(pattern = "\\[AUTHOR\\]",
       replacement = chapter_data$text_data$authors,
       x = lines)

  lines <- gsub(pattern = "\\[TITLE\\]",
                replacement = chapter_data$text_data$title,
                x = lines)

  lines <- gsub(pattern = "\\[AFFILIATION\\]",
                replacement = chapter_data$text_data$affiliation,
                x = lines)

  lines <- gsub(pattern = "\\[EMAIL\\]",
                replacement = chapter_data$text_data$email,
                x = lines)

  lines <- gsub(pattern = "\\[LAST_UPDATE\\]",
                replacement = chapter_data$text_data$last_update,
                x = lines)

  lines <- replace_lines(x = lines,
                         pattern = "\\[DESCRIPTION\\]",
                         replacement = chapter_data$text_data$description)

  lines <- replace_lines(x = lines,
                         pattern = "\\[STATUS_AND_TRENDS\\]",
                         replacement = chapter_data$text_data$status_trends)

  lines <- replace_lines(x = lines,
                         pattern = "\\[FACTORS\\]",
                         replacement = chapter_data$text_data$factors)

  lines <- replace_lines(x = lines,
                         pattern = "\\[IMPLICATIONS\\]",
                         replacement = chapter_data$text_data$implications)

  if(all(as.character(chapter_data$text_data$methodological_changes) == "NA")) {
    rm_line <- grep(pattern = "\\[METHODOLOGICAL CHANGES\\]", x = lines)
    lines <- lines[-c(rm_line, rm_line+1)]
  } else {
    lines <- replace_lines(x = lines,
                           pattern = "\\[METHODOLOGICAL CHANGES\\]",
                           replacement = chapter_data$text_data$methodological_changes)
  }

  if(all(as.character(chapter_data$text_data$research_priorities) == "NA")) {
    rm_line <- grep(pattern = "\\[RESEARCH_PRIORITIES\\]", x = lines)
    lines <- lines[-c(rm_line, rm_line+1)]
  } else {
    lines <- replace_lines(x = lines,
                           pattern = "\\[RESEARCH_PRIORITIES\\]",
                           replacement = chapter_data$text_data$research_priorities)
  }

  for(ii in 1:length(chapter_data$fig_data)) {
    lines <- c(lines, add_figure(chapter_data$fig_data[[ii]]))
  }

  lines <- c(lines, "", "## References")

  writeLines(text = lines, con = output_path)

  message("make_esr_chapter: .Rmd file for ",
          chapter_data$text_data$esr_name,
          " in the ",
          chapter_data$text_data$region,
          " written to ",
          output_path)

  rmarkdown::render(input = output_path)

  docx_path <- gsub(x = output_path, pattern = ".Rmd", replacement = ".docx")

  if(file.exists(docx_path)) {
    message("make_esr_chapter: .docx file for ",
            chapter_data$text_data$esr_name,
            " in the ",
            chapter_data$text_data$region,
            " written to ",
            docx_path)
  }

}
