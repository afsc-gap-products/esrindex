library(esrindex)


# Full region plots --------------------------------------------------------------------------------

# plot_rema_region(x = AI_INDICATOR, append_filename = "e_")
# plot_rema_region(x = GOA_INDICATOR, append_filename = "e_")
# plot_rema_region(x = EBS_INDICATOR, append_filename = "e_")
# plot_rema_region(x = NBS_INDICATOR, append_filename = "e_")

# plot_rema_region(x = AI_INDICATOR, error_bar = FALSE, benchmarks = "zscore", append_filename = "z_")
# plot_rema_region(x = GOA_INDICATOR, error_bar = FALSE, benchmarks = "zscore", append_filename = "z_")
# plot_rema_region(x = EBS_INDICATOR, error_bar = FALSE, benchmarks = "zscore", append_filename = "z_")
# plot_rema_region(x = NBS_INDICATOR, error_bar = FALSE, benchmarks = "zscore", append_filename = "z_")
# 
# plot_rema_region(x = AI_INDICATOR, error_bar = FALSE, benchmarks = "quantile", append_filename = "q_")
# plot_rema_region(x = GOA_INDICATOR, error_bar = FALSE, benchmarks = "quantile", append_filename = "q_")
# plot_rema_region(x = EBS_INDICATOR, error_bar = FALSE, benchmarks = "quantile", append_filename = "q_")
# plot_rema_region(x = NBS_INDICATOR, error_bar = FALSE, benchmarks = "quantile", append_filename = "q_")

plot_rema_region(x = AI_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")
plot_rema_region(x = GOA_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")
plot_rema_region(x = EBS_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")
plot_rema_region(x = NBS_INDICATOR, error_bar = TRUE, benchmarks = "zscore", append_filename = "_ze")

# plot_rema_region(x = AI_INDICATOR, error_bar = TRUE, benchmarks = "quantile", append_filename = "qe_")
# plot_rema_region(x = GOA_INDICATOR, error_bar = TRUE, benchmarks = "quantile", append_filename = "qe_")
# plot_rema_region(x = EBS_INDICATOR, error_bar = TRUE, benchmarks = "quantile", append_filename = "qe_")
# plot_rema_region(x = NBS_INDICATOR, error_bar = TRUE, benchmarks = "quantile", append_filename = "qe_")


# Subarea/Stratum plots ----------------------------------------------------------------------------

plot_rema_subarea(x = AI_INDICATOR, error_bar = TRUE, benchmarks = "zscore")
plot_rema_subarea(x = GOA_INDICATOR, error_bar = TRUE, benchmarks = "zscore")
plot_rema_subarea(x = EBS_INDICATOR, error_bar = TRUE, benchmarks = "zscore")
