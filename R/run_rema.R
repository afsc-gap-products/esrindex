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

