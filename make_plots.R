library(esrindex)

region <- "EBS"

test <- esrindex::get_invert_data(region = region)

# Need to rescale and set the minimum to zero
ggplot() +
  geom_point(data = dplyr::filter(test$biomass_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]) |>
               dplyr::arrange(YEAR),
             mapping = aes(x = YEAR, y = BIOMASS_MT)) +
  geom_path(data = dplyr::filter(test$biomass_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]) |>
              dplyr::arrange(YEAR),
            mapping = aes(x = YEAR, y = BIOMASS_MT)) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]),
             mapping = aes(yintercept = mean_biomass)) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]),
             mapping = aes(yintercept = mean_biomass+sd_biomass), linetype = 2) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]),
             mapping = aes(yintercept = mean_biomass+2*sd_biomass), linetype = 3) +
  # geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]),
  #            mapping = aes(yintercept = max(c(mean_biomass-sd_biomass, 0))), linetype = 2) +
  # geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_area_id']]),
  #            mapping = aes(yintercept = max(c(mean_biomass-2*sd_biomass, 0))), linetype = 3) +
  scale_y_continuous(name = "Mean Stratum Biomass (mt)") +
  scale_x_continuous(name = "Year") +
  facet_wrap(~SPECIES_CODE, scales = "free") +
  theme_blue_strip()



# Obviously need to re-scale and this would need be a separate script and would need to reorder strata E-W
ggplot(data = dplyr::filter(test$biomass_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_subarea_id']]) |>
         dplyr::arrange(YEAR),
       mapping = aes(x = YEAR, y = BIOMASS_MT)) +
  geom_point() +
  geom_path() +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_subarea_id']]),
             mapping = aes(yintercept = mean_biomass)) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_subarea_id']]),
             mapping = aes(yintercept = mean_biomass+sd_biomass), linetype = 2) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_subarea_id']]),
             mapping = aes(yintercept = mean_biomass+2*sd_biomass), linetype = 3) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_subarea_id']]),
             mapping = aes(yintercept = max(c(mean_biomass-sd_biomass, 0))), linetype = 2) +
  geom_hline(data = dplyr::filter(test$summary_df, AREA_ID %in% esrindex::region_settings[[region]][['esr_subarea_id']]),
             mapping = aes(yintercept = max(c(mean_biomass-2*sd_biomass, 0))), linetype = 3) +
  scale_y_continuous(name = "Mean Stratum Biomass (mt)") +
  scale_x_continuous(name = "Year") +
  facet_grid(SPECIES_CODE ~ AREA_NAME, scales = "free") +
  theme_blue_strip()
