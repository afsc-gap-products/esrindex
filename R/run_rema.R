#' Wrapper function to fit rema to region level biomass indices
#'
#' @param x A data.frame containing SURVEY, YEAR, BIOMASS_MT, and CV columns
#' @param zero_assumption Assumption for zero biomass observations ("tweedie", "na", "small_constant")
#' @param rema_by_stratum Logical indicating whether to estimate biomass by survey stratum (TRUE) or estimate biomass from the total biomass index (FALSE).
#' @import rema
#' @export


fit_rema_region <- function(x, zero_assumption = "na", rema_by_stratum = TRUE) {
  
  zero_assumption <- tolower(zero_assumption)

  region <- x$SURVEY[1]

  unique_group_name <- unname(unlist(esrindex::chapter_settings[[region]]))

  output <- vector(mode = "list", length = length(unique_group_name))

  names(output) <- unique_group_name

  for(ii in 1:length(unique_group_name)) {
    
    select_strata <- switch(as.character(rema_by_stratum),
                            "TRUE" = esrindex::region_settings[[region]]$esr_subarea_id,
                            "FALSE" = esrindex::region_settings[[region]]$esr_area_id)

    dat <- dplyr::filter(x,
                         SPECIES_CODE == unique_group_name[ii],
                         AREA_ID %in% select_strata)
    
    # Remove strata with 0 biomass or less than 3 years with biomass
    sufficient_data <- (dat |>
      dplyr::group_by(AREA_ID) |>
      dplyr::summarise(n = sum(BIOMASS_MT) > 3,
                       cv = !is.na(mean(CV, na.rm = TRUE))) |>
      dplyr::filter(n, cv))$AREA_ID
    
    dat <- dplyr::filter(dat, AREA_ID %in% sufficient_data)
    
    rema_out <- try(run_rema(x = dat, 
                        region = region, 
                        group_name = unique_group_name[ii], 
                        zero_assumption = zero_assumption),
                   silent = TRUE)

    if(methods::is(rema_out, "try-error")) {
      warning("fit_rema_region: ", unique_group_name[ii], " did not run.")
      next
    }

    output[[ii]] <- rema_out

  }

  return(output)

}



#' Fit random effects model to biomass index time series
#'
#' Fit a random effects model using the rema package.
#'
#' @param x A data.frame containing SURVEY, YEAR, BIOMASS_MT, and CV columns
#' @param region A character vector indicating the region  ("AI", "GOA", "EBS", or "NBS")
#' @param group_name Character vector indicating the name of the group (e.g. "Sponges")
#' @param zero_assumption Assumption for zero biomass observations ("tweedie", "na", "small_constant")
#' @import rema
#' @export

run_rema <- function(x, region, group_name, zero_assumption) {
  
  dat <- x |>
    dplyr::select(strata = AREA_ID,
                  year = YEAR,
                  biomass = BIOMASS_MT,
                  cv = CV) |>
    dplyr::mutate(strata = as.character(strata))
  
  if(zero_assumption == "tweedie") {
    zero_list <- list(assumption = 'tweedie',
                              options_tweedie = list(fix_pars = c(1)))
  }
  
  if(zero_assumption == "na") {
    zero_list <- list(assumption = c("NA"))
  }
  
  if(zero_assumption == "small_constant") {
    zero_list = list(assumption = "small_constant",
                     constant = 0.01)
  }

  input <- prepare_rema_input(model_name = paste0("tmb_rema_", region, "_", paste(group_name, collapse = "_")),
                              biomass_dat = dat,
                              zeros = zero_list
                              )

  m <- fit_rema(input)

  output <- tidy_rema(rema_model = m)

  output$biomass_by_strata$group_name <- group_name
  output$total_predicted_biomass$group_name <- group_name
  output$proportion_biomass_by_strata$group_name <- group_name
  output$parameter_estimates$group_name <- group_name

  return(output)

}

