#' Get index data for ESR species and groups
#'
#' Calculates group-level biomass index for a specified region.
#'
#' @param region A character string specifying the region for which data should be retrieved ("GOA", "AI", "EBS", or "NBS")
#' @param channel An optional parameter specifying an RODBC database channel. If not provided, a connection is established.
#' @param zero_assumption Assumption for zero biomass observations ("tweedie", "na", "small_constant"). Default = "na"
#' @param rema_by_stratum Logical indicating whether to estimate biomass by survey stratum (TRUE) or estimate biomass from the total biomass index (FALSE).
#'
#' @return A list containing two data frames: timeseries and mean_sd.
#' \itemize{
#'   \item \code{timeseries}: Data frame with biomass time series for each subarea and species group.
#'   \item \code{mean_sd}: Summary data frame with mean and standard deviation of biomass for each subarea and species group.
#' }
#'
#' @details
#' This function retrieves invertebrate data for the specified region using the provided channel.
#' If the channel is not provided, the function uses the default connected channel obtained from \code{esrindex::get_connected()}.
#'
#' @importFrom gapindex get_data calc_cpue calc_biomass_stratum calc_biomass_subarea
#' @import RODBC
#'
#' @examples
#' \dontrun{
#' # Example Usage:
#' data <- get_group_data(region = "GOA")
#' }
#' @export

get_group_data <- function(region, 
                           channel = NULL, 
                           zero_assumption = "na", 
                           rema_by_stratum = TRUE) {

  region <- toupper(region)

  channel <- get_connected(channel = channel, schema = "AFSC")

  esr_subarea_id <- region_settings[[region]][['esr_subarea_id']]
  esr_area_id <- region_settings[[region]][['esr_area_id']]
  min_year <- region_settings[[region]][['min_year']]

  timeseries <- data.frame()
  mean_sd <- data.frame()
  
  region_groups <- dplyr::filter(esrindex::species_groups,
                                 group_name %in% unname(unlist(chapter_settings[[region]]))
                                 )
  
  dir.create(here::here("output", region), recursive = TRUE, showWarnings = FALSE)

  for(ii in 1:nrow(region_groups)) {

    message(region_groups$group_name[ii])

    # Retrieve valid SPECIES_CODES for gapindex
    if(region_groups$min_code[ii] == -999 & region_groups$max_code[ii] == -999) {

      # Case where species codes are not sequential -- SPECIES_CODE -999 is used as a flag in esrindex::region_groups
      species_codes <- species_groups_ns[[region_groups$group_name[ii]]]

      valid_species_codes <- RODBC::sqlQuery(channel = channel,
                                             query = paste0("select species_code from racebase.species where species_code >= ",
                                                            min(species_codes), " and ", "species_code <= ", max(species_codes)))

      valid_species_codes$GROUP <- region_groups$group_name[ii]

      valid_species_codes <- valid_species_codes[valid_species_codes$SPECIES_CODE %in% species_codes, ]

    } else {

      # Case where species codes are sequential
      valid_species_codes <- RODBC::sqlQuery(channel = channel,
                                             query = paste0("select species_code from racebase.species where species_code >= ",
                                                            region_groups$min_code[ii], " and ", "species_code <= ", region_groups$max_code[ii]))
    }

    valid_species_codes$GROUP <- region_groups$group_name[ii]

    dat <- try(gapindex::get_data(year_set = min_year:as.numeric(format(Sys.Date(), "%Y")),
                              survey_set = region,
                              sql_channel = channel,
                              spp_codes = valid_species_codes), silent = TRUE)

    # Warn if there was a gapindex error
    if(methods::is(dat, "try-error")) {

      warning("gapindex::get_data retrieval failed. Skipping ", region_groups$group_name[ii])

      next

    }

    subareas <- dplyr::select(dat$subarea, AREA_ID, AREA_NAME, DESCRIPTION)

    cpue <- gapindex::calc_cpue(racebase_tables = dat)

    if(nrow(cpue) > 0) {
      
      biomass_strata <- gapindex::calc_biomass_stratum(racebase_tables = dat, cpue = cpue)
      
      subarea_biomass <- gapindex::calc_biomass_subarea(racebase_tables = dat,
                                                        biomass_strata = biomass_strata)
      
      subarea_biomass_summary <- suppressMessages(
        dplyr::group_by(subarea_biomass, AREA_ID, SPECIES_CODE) |>
        dplyr::summarize(MEAN_BIOMASS = mean(BIOMASS_MT),
                         SD_BIOMASS = sd(BIOMASS_MT)) |>
        dplyr::inner_join(subareas)
        )
      
      subarea_biomass <- suppressMessages(
        dplyr::inner_join(subarea_biomass,
                                           subarea_biomass_summary) |>
        dplyr::inner_join(subareas) |>
        dplyr::mutate(BIOMASS_MT_ZSCORE = (BIOMASS_MT-MEAN_BIOMASS)/SD_BIOMASS)
      )
      
      subarea_biomass <- dplyr::select(subarea_biomass, 
                                       SURVEY, 
                                       AREA_ID, 
                                       SPECIES_CODE, 
                                       YEAR, 
                                       N_HAUL, 
                                       N_WEIGHT, 
                                       CPUE_KGKM2_MEAN, 
                                       CPUE_KGKM2_VAR, 
                                       BIOMASS_MT, 
                                       BIOMASS_VAR, 
                                       AREA_NAME, 
                                       DESCRIPTION, 
                                       BIOMASS_MT_ZSCORE)
      
      
      timeseries <- rbind(timeseries,
                          subarea_biomass)
      
      mean_sd <- rbind(mean_sd,
                       subarea_biomass_summary)
      
    }
    
    # Save intermediate outputs to .rds
    path_biomass <- here::here("output", 
                               region, 
                               paste0("gapindex_", region, "_", region_groups$group_name[ii], ".rds"))
    
    message("get_group_data: Writing gapindex outputs to ",  
            region_groups$group_name[ii], 
            " ", 
            path_biomass)
    
    saveRDS(object = list(
      cpue = cpue,
      biomass_strata = biomass_strata,
      biomass_subarea = subarea_biomass, 
      racebase_tables = dat
    ), 
    file = path_biomass)

  }

  # Filter to strata and species groups that are used in regional ESRs
  timeseries <- timeseries[timeseries$AREA_ID %in% c(esr_area_id, esr_subarea_id) &
                             timeseries$SPECIES_CODE %in% unname(unlist(chapter_settings[[region]])), ]

  mean_sd <- mean_sd[mean_sd$AREA_ID %in% c(esr_area_id, esr_subarea_id) & 
                       mean_sd$SPECIES_CODE %in% unname(unlist(chapter_settings[[region]])), ]

  # Calculate SDs and CVs; fill zeros
  timeseries$BIOMASS_SD <- sqrt(timeseries$BIOMASS_VAR)
  timeseries$CV <- timeseries$BIOMASS_SD/timeseries$BIOMASS_MT
  timeseries$BIOMASS_PLUS1_SD <- timeseries$BIOMASS_MT + timeseries$BIOMASS_SD
  timeseries$BIOMASS_PLUS2_SD <- timeseries$BIOMASS_MT + 2*timeseries$BIOMASS_SD
  timeseries$BIOMASS_MINUS1_SD <- timeseries$BIOMASS_MT - timeseries$BIOMASS_SD
  timeseries$BIOMASS_MINUS2_SD <- timeseries$BIOMASS_MT - 2*timeseries$BIOMASS_SD

  timeseries$BIOMASS_MINUS1_SD <- ifelse(timeseries$BIOMASS_MINUS1_SD < 0, 
                                         0, 
                                         timeseries$BIOMASS_MINUS1_SD)
  
  timeseries$BIOMASS_MINUS2_SD <- ifelse(timeseries$BIOMASS_MINUS2_SD < 0, 
                                         0, 
                                         timeseries$BIOMASS_MINUS2_SD)

  mean_sd$MEAN_PLUS1 <- mean_sd$MEAN_BIOMASS + mean_sd$SD_BIOMASS
  mean_sd$MEAN_PLUS2 <- mean_sd$MEAN_BIOMASS + 2*mean_sd$SD_BIOMASS
  mean_sd$MEAN_MINUS1 <- mean_sd$MEAN_BIOMASS - mean_sd$SD_BIOMASS
  mean_sd$MEAN_MINUS2 <- mean_sd$MEAN_BIOMASS - 2*mean_sd$SD_BIOMASS

  mean_sd$MEAN_MINUS1 <- ifelse(mean_sd$MEAN_MINUS1 < 0, 0, mean_sd$MEAN_MINUS1)
  mean_sd$MEAN_MINUS2 <- ifelse(mean_sd$MEAN_MINUS2 < 0, 0, mean_sd$MEAN_MINUS2)

  # Fit random effects model to time series
  rema_fit <- fit_rema_region(x = timeseries, 
                              zero_assumption = zero_assumption,
                              rema_by_stratum = rema_by_stratum)

  return(list(timeseries = as.data.frame(timeseries),
              mean_sd = as.data.frame(mean_sd),
              rema_fit = rema_fit,
              last_update = Sys.Date(),
              package_version = paste0("esrindex ", packageVersion(pkg = "esrindex"))))

}
