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