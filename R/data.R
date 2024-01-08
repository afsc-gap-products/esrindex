#' Species Groups
#'
#' This dataset contains group names and RACEBASE SPECIES_CODE ranges for species groups used in ESRs.
#'
#' @name species_groups
#' @docType data
#' @format A data frame with 4 variables:
#'   \describe{
#'     \item{group_name}{Character vector, names of taxonomic groups.}
#'     \item{min_code}{Numeric vector, minimum SPECIES_CODE for each group.}
#'     \item{max_code}{Numeric vector, maximum SPECIES_CODE for each group.}
#'   }
#' @examples
#' data(species_groups)
#'
#' @usage data(species_groups)
#'
#' @export


#' ESR region settings
#'
#' This dataset contains subarea and area IDs, and the minimum years to use for biomass.
#'
#' @name region_settings
#' @docType data
#' @format A list with named elements:
#'   \describe{
#'     \item{GOA}{List with elements:
#'       \item{esr_subarea_id}{Numeric vector, identifiers for the Gulf of Alaska subareas.}
#'       \item{esr_area_id}{Numeric, area code for the Gulf of Alaska.}
#'       \item{min_year}{Numeric, minimum year for the Gulf of Alaska.}
#'     }
#'     \item{AI}{List with elements:
#'       \item{esr_subarea_id}{Numeric vector, identifiers for the Aleutian Islands subareas.}
#'       \item{esr_area_id}{Numeric, area code for the Aleutian Islands.}
#'       \item{min_year}{Numeric, minimum year for the Aleutian Islands.}
#'     }
#'     \item{EBS}{List with elements:
#'       \item{esr_subarea_id}{Numeric vector, identifiers for the Eastern Bering Sea subareas.}
#'       \item{esr_area_id}{Numeric, area code for the Eastern Bering Sea.}
#'       \item{min_year}{Numeric, minimum year for the Eastern Bering Sea.}
#'     }
#'     \item{NBS}{List with elements:
#'       \item{esr_subarea_id}{Numeric vector, identifiers for the Northern Bering Sea subareas.}
#'       \item{esr_area_id}{Numeric, area code for the Northern Bering Sea.}
#'       \item{min_year}{Numeric, minimum year for the Northern Bering Sea.}
#'     }
#'   }
#' @examples
#' data(region_settings)
#'
#' @usage data(region_settings)
#'
#' @export


#' Chapter Settings
#'
#' Information about species groups included within different chapters.
#'
#' @name region_settings
#' @docType data
#' @format A nested list containing settings for ESR chapters chapter settings.
#' @details
#' The data is organized hierarchically by region (GOA, AI, EBS) and each chapter has the following settings:
#'
#' \itemize{
#'   \item \code{misc_species}: List of miscellaneous species group names (e.g. Echinoderms, Shrimps, Eelpouts, Poachers).
#'   \item \code{jellyfish}: List of jellyfish group names.
#'   \item \code{structural_epifauna}: List of structural epifauna group names (e.g. Sponges, Sea anemones, Gorgonians, Pennatulaceans, Hydrocorals, Soft corals).
#'   \item \code{forage_fish}: List of forage fish species group names (e.g. eulachon, capelin, pricklebacks, sandlance, Pacific sandfish)
#' }
#' @examples
#' data(region_settings)
#'
#' @usage data(region_settings)
#'
#' @export
