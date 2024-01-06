#' Get Invertebrate Index Data
#'
#' Retrieves invertebrate data for a specified region.
#'
#' @param region A character string specifying the region for which data should be retrieved ("GOA", "AI", "EBS", or "NBS")
#' @param channel An optional parameter specifying an RODBC database channel. If not provided, a connection is estalbished.
#'
#' @return A list containing two data frames: biomass_df and summary_df.
#' \itemize{
#'   \item \code{biomass_df}: Data frame with detailed biomass for each subarea and invert group.
#'   \item \code{summary_df}: Summary data frame with mean and standard deviation of biomass for each subarea and invert group.
#' }
#'
#' @details
#' This function retrieves invertebrate data for the specified region using the provided channel.
#' If the channel is not provided, the function uses the default connected channel obtained from \code{esrindex::get_connected()}.
#'
#' @importFrom gapindex get_data calc_cpue calc_biomass_stratum calc_biomass_subarea
#' @importFrom RODBC
#' @importFrom dplyr select summarise inner_join mutate
#'
#' @examples
#' \dontrun{
#' # Example Usage:
#' data <- get_invert_data(region = "GOA")
#' }
#'
#' @export

get_invert_data <- function(region, channel = NULL) {

  region <- toupper(region)

  channel <- get_connected(channel = channel, schema = "AFSC")


  esr_subarea_id <- region_settings[[region]][['esr_subarea_id']]
  esr_area_id <- region_settings[[region]][['esr_area_id']]
  min_year <- region_settings[[region]][['min_year']]

  biomass_df <- data.frame()
  summary_df <- data.frame()

  for(ii in 1:nrow(invert_groups)) {

    # Retrieve valid codes
    valid_species_codes <- RODBC::sqlQuery(channel = channel,
                                           query = paste0("select species_code from racebase.species where species_code in (",
                                                          paste(invert_groups$min_code[ii]:invert_groups$max_code[ii], collapse = ","),
                                                          ")")
    )

    valid_species_codes$GROUP <- invert_groups$group_name[ii]

    dat <- gapindex::get_data(year_set = min_year:as.numeric(format(Sys.Date(), "%Y")),
                              survey_set = region,
                              sql_channel = channel,
                              spp_codes = valid_species_codes)

    subareas <- dplyr::select(dat$subarea, AREA_ID, AREA_NAME, DESCRIPTION)

    cpue <- gapindex::calc_cpue(racebase_tables = dat)

    biomass_strata <- gapindex::calc_biomass_stratum(racebase_tables = dat, cpue = cpue)

    subarea_biomass <- gapindex::calc_biomass_subarea(racebase_tables = dat,
                                                      biomass_strata = biomass_strata)

    subarea_biomass_summary <- dplyr::group_by(subarea_biomass, AREA_ID, SPECIES_CODE) |>
      dplyr::summarise(mean_biomass = mean(BIOMASS_MT),
                       sd_biomass = sd(BIOMASS_MT)) |>
      dplyr::inner_join(subareas)

    subarea_biomass <- dplyr::inner_join(subarea_biomass,
                                         subarea_biomass_summary) |>
      dplyr::inner_join(subareas) |>
      dplyr::mutate(z_score = (BIOMASS_MT-mean_biomass)/sd_biomass)


    biomass_df <- rbind(biomass_df,
                        subarea_biomass)

    summary_df <- rbind(summary_df,
                        subarea_biomass_summary)

  }

  return(list(biomass_df = biomass_df,
              summary_df = summary_df))

}
