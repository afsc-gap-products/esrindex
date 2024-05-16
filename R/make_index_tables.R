#' Make tables with ESR indicator time series
#'
#' This function writes abundance indicator results to .csv (./plots/[region]/[indicator]_indicator_table.csv)
#'
#' @param x Indicator list (e.g. esrindex::AI_INDICATOR)
#' @export

make_index_tables <- function(x) {
  
  region <- x$timeseries$SURVEY[1]
  
  area_id <- dplyr::select(x$timeseries, 
                           AREA_ID, 
                           AREA_NAME, 
                           DESCRIPTION) |>
    dplyr::mutate(AREA_ID = as.character(AREA_ID)) |>
    unique()
  
  indicator_name <- names(chapter_settings[[region]])  
  
  output <- data.frame()
  
  for(ii in 1:length(indicator_name)) {
    
    group_name <- chapter_settings[[region]][[indicator_name[ii]]]$group_name
    
    for(jj in 1:length(group_name)) {
      
      x$mean_sd |>
        dplyr::mutate(GROUP_NAME = SPECIES_CODE)
      
      stratum_ts <- x$rema_fit[[group_name[jj]]]$biomass_by_strata |>
        dplyr::mutate(INDICATOR_NAME = indicator_name[ii]) |>
        dplyr::select(INDICATOR_NAME, 
                      TAXA = group_name,
                      YEAR = year,
                      AREA_ID = strata,
                      MEAN_REMA_BIOMASS_MT = pred, 
                      LCI_REMA_BIOMASS_MT= pred_lci,
                      UCI_REMA_BIOMASS_MT = pred_uci,
                      LOG_MEAN_REMA_BIOMASS_MT = log_pred, 
                      SD_LOG_REMA_BIOMASS_MT = sd_log_pred,  
                      OBS_REMA_BIOMASS_MT = obs,
                      CV_OBS_REMA_BIOMASS_MT = obs_cv, 
                      LOG_OBS_REMA_BIOMASS_MT = log_obs, 
                      SD_LOG_OBS_REMA_BIOMASS_MT = sd_log_obs, 
                      LCI_OBS_REMA_BIOMASS_MT = obs_lci, 
                      UCI_OBS_REMA_BIOMASS_MT = obs_uci)
      
      region_ts <- x$rema_fit[[group_name[jj]]]$total_predicted_biomass |>
        dplyr::mutate(AREA_ID = as.character(region_settings[[region]]$esr_area_id),
                      INDICATOR_NAME = indicator_name[ii]) |>
        dplyr::select(INDICATOR_NAME,
                      TAXA = group_name,
                      AREA_ID = AREA_ID,
                      YEAR = year,
                      MEAN_REMA_BIOMASS_MT = pred, 
                      LCI_REMA_BIOMASS_MT = pred_lci,
                      UCI_REMA_BIOMASS_MT = pred_uci)
      
      index_ts <- x$timeseries |>
        dplyr::mutate(AREA_ID = as.character(AREA_ID),
                      DBI_CV = sqrt(BIOMASS_VAR)/BIOMASS_MT) |>
        dplyr::select(AREA_ID,
                      TAXA = SPECIES_CODE,
                      YEAR,
                      DBI_BIOMASS_MT = BIOMASS_MT,
                      DBI_BIOMASS_VAR = BIOMASS_VAR,
                      DBI_BIOMASS_PLUS1_SD = BIOMASS_PLUS1_SD,
                      DBI_BIOMASS_PLUS2_SD = BIOMASS_PLUS2_SD,
                      DBI_BIOMASS_MINUS1_SD = BIOMASS_MINUS1_SD,
                      DBI_BIOMASS_MINUS2_SD = BIOMASS_MINUS2_SD,
                      DBI_CV)
      
      indicator_ts <- dplyr::bind_rows(stratum_ts, region_ts) |>
        dplyr::inner_join(area_id, by = "AREA_ID") |>
        dplyr::inner_join(index_ts, by = c("AREA_ID", "TAXA", "YEAR")) |>
        dplyr::select(INDICATOR_NAME, 
                      TAXA, 
                      YEAR, 
                      AREA_ID, 
                      AREA_NAME, 
                      DESCRIPTION, 
                      MEAN_REMA_BIOMASS_MT, 
                      LCI_REMA_BIOMASS_MT, 
                      UCI_REMA_BIOMASS_MT, 
                      LOG_MEAN_REMA_BIOMASS_MT, 
                      SD_LOG_REMA_BIOMASS_MT, 
                      OBS_REMA_BIOMASS_MT, 
                      CV_OBS_REMA_BIOMASS_MT, 
                      LOG_OBS_REMA_BIOMASS_MT, 
                      SD_LOG_OBS_REMA_BIOMASS_MT, 
                      LCI_OBS_REMA_BIOMASS_MT, 
                      UCI_OBS_REMA_BIOMASS_MT,
                      DBI_BIOMASS_MT,
                      DBI_BIOMASS_VAR,
                      DBI_BIOMASS_PLUS1_SD,
                      DBI_BIOMASS_PLUS2_SD,
                      DBI_BIOMASS_MINUS1_SD,
                      DBI_BIOMASS_MINUS2_SD,
                      DBI_CV) |>
        dplyr::arrange(INDICATOR_NAME, TAXA, YEAR, AREA_ID)
        
        output <- dplyr::bind_rows(output, indicator_ts)
        
    }
    
    
  }
  
  fpath <- here::here("plots", 
                      region, 
                      paste0(region, "_indicator_table.csv"))
  
  cat("make_index_tables: Writing output to ", fpath, "\n")
  
  write.csv(output,
            file = fpath,
            row.names = FALSE)
  
}

