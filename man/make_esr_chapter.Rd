% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_esr_chapter.R
\name{make_esr_chapter}
\alias{make_esr_chapter}
\title{Create ESR Chapter Document}
\usage{
make_esr_chapter(xml_path, output_path = NULL)
}
\arguments{
\item{xml_path}{Path to the ESR template file containing ESR chapter information.}

\item{output_path}{Path to save the generated R Markdown document. If not specified, a default path will be used.}
}
\value{
None
}
\description{
This function generates an R Markdown document for an ESR chapter using an ESR XML template file then renders as a .docx.
}
\examples{
\dontrun{
make_esr_chapter(xml_path = "./chapters/AI_misc_species.xml")
}

}
