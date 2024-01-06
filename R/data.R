#' Benthic Groups
#'
#' This dataset contains group names and RACEBASE SPECIES_CODE ranges for benthic fish and invertebrate groups used in ESRs.
#'
#' @name invert_groups
#' @docType data
#' @format A data frame with 4 variables:
#'   \describe{
#'     \item{group_name}{Character vector, names of taxonomic groups.}
#'     \item{min_code}{Numeric vector, minimum SPECIES_CODE for each group.}
#'     \item{max_code}{Numeric vector, maximum SPECIES_CODE for each group.}
#'   }
#' @examples
#' data(invert_groups)
#'
#' @usage data(invert_groups)
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
