#' Species Groups
#'
#' This dataset contains group names and RACEBASE SPECIES_CODE ranges for species groups used in ESRs.
#'
#' @docType data
#' @format A data frame with 3 variables:
#'   \describe{
#'     \item{group_name}{Character vector, names of taxonomic groups.}
#'     \item{min_code}{Numeric vector, minimum SPECIES_CODE for each group.}
#'     \item{max_code}{Numeric vector, maximum SPECIES_CODE for each group.}
#'   }
'species_groups'

#' Non-sequential species groups
#'
#' This named list contains species codes for species categories that are non-sequential.
#'
#' @docType data
'species_groups_ns'

#' ESR region settings
#'
#' This dataset contains subarea and area IDs, and the minimum years to use for biomass.
#'
#' @docType data
#' @format A list with settings for each region, where each region has 3 variables:
#'   \describe{

#'       \item{esr_subarea_id}{Corresponds with subarea AREA_ID from gapindex.}
#'       \item{esr_area_id}{Corresponds with region-wide AREA_ID from gapindex.}
#'       \item{min_year}{Starting year}
#'     }
'region_settings'

#' Chapter Settings
#'
#' Information about species groups included within different chapters.
#'
#' @docType data
#' @format A nested list containing species groups included in each ESR chapters
#'  \describe{
#'    \item{misc_species}{List of miscellaneous species group names (e.g. Echinoderms, Shrimps, Eelpouts, Poachers).}
#'    \item{jellyfish}{List of jellyfish group names.}
#'    \item{structural_epifauna}{List of structural epifauna group names (e.g. Sponges, Sea anemones, Gorgonians, Pennatulaceans, Hydrocorals, Soft corals).}
#'    \item{forage_fish}{List of forage fish species group names (e.g. eulachon, capelin, pricklebacks, sandlance, Pacific sandfish)}
#'    }
'chapter_settings'


#' Aleutian Islands Indicator
#'
#' ESR Indicator data for the Aleutian Islands
#'
#' @docType data
#'
'AI_INDICATOR'

#' Gulf of Alaska Indicator
#'
#' ESR Indicator data for the Gulf of Alaska
#'
#' @docType data
#'
'GOA_INDICATOR'

#' Eastern Bering Sea Indicator
#'
#' ESR Indicator data for the Eastern Bering Sea
#'
#' @docType data
#'
'EBS_INDICATOR'

#' Northern Bering Sea Indicator
#'
#' ESR Indicator data for the Northern Bering Sea
#'
#'@docType data
#'
'NBS_INDICATOR'
