library(esrindex)

# Full region plots --------------------------------------------------------------------------------

plot_region_rema(x = AI_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")
plot_region_rema(x = GOA_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")
plot_region_rema(x = EBS_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")
plot_region_rema(x = NBS_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")

# Subarea/Stratum plots ----------------------------------------------------------------------------

plot_subarea_rema(x = AI_INDICATOR, error_bar = TRUE, benchmarks = "zscore")
plot_subarea_rema(x = GOA_INDICATOR, error_bar = TRUE, benchmarks = "zscore")
plot_subarea_rema(x = EBS_INDICATOR, error_bar = TRUE, benchmarks = "zscore")
