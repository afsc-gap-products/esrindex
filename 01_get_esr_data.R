library(esrindex)
library(devtools)

channel <- esrindex::get_connected(schema = "AFSC")


EBS_INDICATOR <- 
  esrindex::get_group_data(
    region = "EBS", 
    channel = channel,
    zero_assumption = "na",
    rema_by_stratum = TRUE
  )

NBS_INDICATOR <- 
  esrindex::get_group_data(
    region = "NBS", 
    channel = channel,
    zero_assumption = "na",
    rema_by_stratum = TRUE
  )

GOA_INDICATOR <- 
  esrindex::get_group_data(
    region = "GOA", 
    channel = channel,
    zero_assumption = "na",
    rema_by_stratum = TRUE
  )

AI_INDICATOR <- 
  esrindex::get_group_data(
    region = "AI", 
    channel = channel,
    zero_assumption = "na",
    rema_by_stratum = TRUE
  )

(all_complete <- 
    all(
      length(
        unique(EBS_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$EBS))),
      length(unique(NBS_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$NBS))),
      length(unique(AI_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$AI))),
      length(unique(GOA_INDICATOR$timeseries$SPECIES_CODE)) == length(unname(unlist(chapter_settings$GOA))),
      length(unique(EBS_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$EBS))),
      length(unique(NBS_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$NBS))),
      length(unique(AI_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$AI))),
      length(unique(GOA_INDICATOR$mean_sd$SPECIES_CODE)) == length(unname(unlist(chapter_settings$GOA)))
    )
)

if(all_complete) {

  message("Writing indicators files to /data/")
  save(EBS_INDICATOR, file = "./data/EBS_INDICATOR.rda", compress = "xz")
  save(NBS_INDICATOR, file = "./data/NBS_INDICATOR.rda", compress = "xz")
  save(GOA_INDICATOR, file = "./data/GOA_INDICATOR.rda", compress = "xz")
  save(AI_INDICATOR, file = "./data/AI_INDICATOR.rda", compress = "xz")

} else {

  stop("Indicator field names don't match between chapter_settings and at least one time series.")

}

# Reinstall the package
devtools::install()
