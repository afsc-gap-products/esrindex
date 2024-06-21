library(esrindex)

# Full region plots --------------------------------------------------------------------------------

plot_region_rema(x = AI_INDICATOR, 
                 error_bar = FALSE, 
                 benchmarks = "zscore", 
                 append_filename = "_ze",                             
                 point_color = "#0085CA",
                 timeseries_color = "#000000",
                 #errorbar_color = "#000000",
                 ribbon_fill = "grey50",
                 hline_color = "grey50")

plot_region_rema(x = GOA_INDICATOR, 
                 error_bar = FALSE, 
                 benchmarks = "zscore", 
                 append_filename = "_ze",                             
                 point_color = "#0085CA",
                 timeseries_color = "#000000",
                 #errorbar_color = "#000000",
                 ribbon_fill = "grey50",
                 hline_color = "grey50")

plot_region_rema(x = EBS_INDICATOR, 
                 error_bar = FALSE, 
                 benchmarks = "zscore", 
                 append_filename = "_ze",                             
                 point_color = "#0085CA",
                 timeseries_color = "#000000",
                 #errorbar_color = "#000000",
                 ribbon_fill = "grey50",
                 hline_color = "grey50")

plot_region_rema(x = NBS_INDICATOR, 
                 error_bar = FALSE, 
                 benchmarks = "zscore", 
                 append_filename = "_ze",                             
                 point_color = "#0085CA",
                 timeseries_color = "#000000",
                 #errorbar_color = "#000000",
                 ribbon_fill = "grey50",
                 hline_color = "grey50")

# Subarea/Stratum plots ----------------------------------------------------------------------------

plot_subarea_rema(x = AI_INDICATOR, 
                  error_bar = FALSE, 
                  benchmarks = "zscore",                             
                  point_color = "#0085CA",
                  timeseries_color = "#000000",
                  #errorbar_color = "#000000",
                  ribbon_fill = "grey50",
                  hline_color = "grey50")

plot_subarea_rema(x = GOA_INDICATOR, 
                  error_bar = FALSE, 
                  benchmarks = "zscore",                             
                  point_color = "#0085CA",
                  timeseries_color = "#000000",
                  #errorbar_color = "#000000",
                  ribbon_fill = "grey50",
                  hline_color = "grey50")

plot_subarea_rema(x = EBS_INDICATOR, 
                  error_bar = FALSE, 
                  benchmarks = "zscore",                             
                  point_color = "#0085CA",
                  timeseries_color = "#000000",
                  #errorbar_color = "#000000",
                  ribbon_fill = "grey50",
                  hline_color = "grey50")

# Make .csv files containing rema time series and design-based index observations
make_index_tables(AI_INDICATOR)
make_index_tables(GOA_INDICATOR)
make_index_tables(EBS_INDICATOR)
make_index_tables(NBS_INDICATOR)
