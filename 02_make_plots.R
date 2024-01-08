library(ggplot2)
library(esrindex)

indicator_data = AI_INDICATOR
indicator_name = "misc_species"
bar_color = "#0085CA"
error_bars = TRUE


# AI
make_region_plot(indicator_data = AI_INDICATOR,
                 indicator_name = "misc_species",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = AI_INDICATOR,
                 indicator_name = "jellyfish",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = AI_INDICATOR,
                 indicator_name = "structural_epifauna",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_subarea_plot(indicator_data = AI_INDICATOR,
                  indicator_name = "misc_species",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = AI_INDICATOR,
                  indicator_name = "jellyfish",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = AI_INDICATOR,
                  indicator_name = "structural_epifauna",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

# GOA
make_region_plot(indicator_data = GOA_INDICATOR,
                 indicator_name = "misc_species",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = GOA_INDICATOR,
                 indicator_name = "jellyfish",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = GOA_INDICATOR,
                 indicator_name = "structural_epifauna",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = GOA_INDICATOR,
                 indicator_name = "forage_fish",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_subarea_plot(indicator_data = GOA_INDICATOR,
                  indicator_name = "misc_species",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = GOA_INDICATOR,
                  indicator_name = "jellyfish",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = GOA_INDICATOR,
                  indicator_name = "structural_epifauna",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = GOA_INDICATOR,
                  indicator_name = "forage_fish",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

# EBS
make_region_plot(indicator_data = EBS_INDICATOR,
                 indicator_name = "misc_species",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = EBS_INDICATOR,
                 indicator_name = "jellyfish",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = EBS_INDICATOR,
                 indicator_name = "structural_epifauna",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_subarea_plot(indicator_data = EBS_INDICATOR,
                  indicator_name = "misc_species",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = EBS_INDICATOR,
                  indicator_name = "jellyfish",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = EBS_INDICATOR,
                  indicator_name = "structural_epifauna",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

# NBS
make_region_plot(indicator_data = NBS_INDICATOR,
                 indicator_name = "misc_species",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = NBS_INDICATOR,
                 indicator_name = "jellyfish",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_region_plot(indicator_data = NBS_INDICATOR,
                 indicator_name = "structural_epifauna",
                 bar_color = "#0085CA",
                 error_bars = TRUE)

make_subarea_plot(indicator_data = NBS_INDICATOR,
                  indicator_name = "misc_species",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = NBS_INDICATOR,
                  indicator_name = "jellyfish",
                  bar_color = "#0085CA",
                  error_bars = TRUE)

make_subarea_plot(indicator_data = NBS_INDICATOR,
                  indicator_name = "structural_epifauna",
                  bar_color = "#0085CA",
                  error_bars = TRUE)
