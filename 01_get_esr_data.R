# Run this then build to reinstall the package

library(esrindex)

channel <- esrindex::get_connected(schema = "AFSC")

EBS_INDICATOR <- esrindex::get_group_data(region = "EBS", 
                                          channel = channel,
                                          zero_assumption = "na")

NBS_INDICATOR <- esrindex::get_group_data(region = "NBS", 
                                          channel = channel,
                                          zero_assumption = "small_constant")

GOA_INDICATOR <- esrindex::get_group_data(region = "GOA", 
                                          channel = channel,
                                          zero_assumption = "na")

AI_INDICATOR <- esrindex::get_group_data(region = "AI", 
                                         channel = channel,
                                         zero_assumption = "na")

all_complete <- all(length(unique(EBS_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$EBS))),
                    length(unique(NBS_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$NBS))),
                    length(unique(AI_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$AI))),
                    length(unique(GOA_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$GOA))),
                    length(unique(EBS_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$EBS))),
                    length(unique(NBS_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$NBS))),
                    length(unique(AI_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$AI))),
                    length(unique(GOA_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$GOA)))
                    )

if(all_complete) {

  message("Writing indicators to /data/")
  save(EBS_INDICATOR, file = "./data/EBS_INDICATOR.rda")
  save(NBS_INDICATOR, file = "./data/NBS_INDICATOR.rda")
  save(GOA_INDICATOR, file = "./data/GOA_INDICATOR.rda")
  save(AI_INDICATOR, file = "./data/AI_INDICATOR.rda")

} else {

  stop("Indicator field names don't match between chapter_settings and at least one time series.")

}

