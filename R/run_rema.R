#' Wrapper function to fit rema to region level biomass indices
#'
#' @param x A data.frame containing SURVEY, YEAR, BIOMASS_MT, and CV columns
#' @import rema
#' @export


fit_rema_region <- function(x) {

  region <- x$SURVEY[1]

  unique_group_name <- unname(unlist(esrindex::chapter_settings[[region]]))

  output <- vector(mode = "list", length = length(unique_group_name))

  names(output) <- unique_group_name

  for(ii in 1:length(unique_group_name)) {

    dat <- dplyr::filter(x,
                         SPECIES_CODE == unique_group_name[ii],
                         AREA_ID == esrindex::region_settings[[region]]$esr_area_id)

    output[[ii]] <- run_rema(x = dat, region = region, group_name = unique_group_name[ii])

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
#' @import rema
#' @export

run_rema <- function(x, region, group_name) {

  dat <- x |>
    dplyr::select(strata = SURVEY,
                  year = YEAR,
                  biomass = BIOMASS_MT,
                  cv = CV)

  input <- prepare_rema_input(model_name = paste0("tmb_rema_", region, "_", group_name),
                              biomass_dat = dat,
                              # zeros = list(assumption = c("NA"))
                              zeros = list(assumption = 'tweedie', 
                                           options_tweedie = list(fix_pars = c(1)))
                              )

  m <- fit_rema(input)

  output <- tidy_rema(rema_model = m)

  output$biomass_by_strata$group_name <- group_name

  return(output)

}

