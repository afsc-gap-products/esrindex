library(esrindex)

# AI
for(ii in 1:length(chapter_settings$AI)) {
  make_region_plot(indicator_data = AI_INDICATOR,
                   indicator_name = names(chapter_settings$AI)[ii],
                   bar_color = "#0085CA",
                   error_bars = TRUE)
  make_subarea_plot(indicator_data = AI_INDICATOR,
                    indicator_name = names(chapter_settings$AI)[ii],
                    bar_color = "#0085CA",
                    error_bars = TRUE)
}

# GOA
for(ii in 1:length(chapter_settings$GOA)) {
  make_region_plot(indicator_data = GOA_INDICATOR,
                   indicator_name = names(chapter_settings$GOA)[ii],
                   bar_color = "#0085CA",
                   error_bars = TRUE)
  make_subarea_plot(indicator_data = GOA_INDICATOR,
                    indicator_name = names(chapter_settings$GOA)[ii],
                    bar_color = "#0085CA",
                    error_bars = TRUE)
}

# EBS
for(ii in 1:length(chapter_settings$EBS)) {
  make_region_plot(indicator_data = EBS_INDICATOR,
                   indicator_name = names(chapter_settings$EBS)[ii],
                   bar_color = "#0085CA",
                   error_bars = TRUE)
  make_subarea_plot(indicator_data = EBS_INDICATOR,
                    indicator_name = names(chapter_settings$EBS)[ii],
                    bar_color = "#0085CA",
                    error_bars = TRUE)
}

# NBS
for(ii in 1:length(chapter_settings$NBS)) {
  make_region_plot(indicator_data = NBS_INDICATOR,
                   indicator_name = names(chapter_settings$NBS)[ii],
                   bar_color = "#0085CA",
                   error_bars = TRUE)
  # make_subarea_plot(indicator_data = NBS_INDICATOR,
  #                   indicator_name = names(chapter_settings$NBS)[ii],
  #                   bar_color = "#0085CA",
  #                   error_bars = TRUE)
}
