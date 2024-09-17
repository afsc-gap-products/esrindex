library(gapindex)
library(esrindex)
library(ggplot2)

# ESR species CVs ----

dir.create(path = here::here("plots", "cvs"), recursive = TRUE)

cv_df <- dplyr::bind_rows(
    dplyr::filter(esrindex::AI_INDICATOR$timeseries,
                  SPECIES_CODE %in% unname(unlist(chapter_settings$AI))),
    dplyr::filter(esrindex::GOA_INDICATOR$timeseries,
                  SPECIES_CODE %in% unname(unlist(chapter_settings$GOA))),
    dplyr::filter(esrindex::EBS_INDICATOR$timeseries,
                  SPECIES_CODE %in% unname(unlist(chapter_settings$EBS))),
    dplyr::filter(esrindex::NBS_INDICATOR$timeseries,
                  SPECIES_CODE %in% unname(unlist(chapter_settings$NBS)))
) |>
  dplyr::mutate(CV = sqrt(BIOMASS_VAR)/BIOMASS_MT,
                type = "ESR") |>
  dplyr::inner_join(dplyr::select(esrindex::species_groups, SPECIES_CODE = group_name, complex))

esr_area_cv <- dplyr::filter(cv_df, AREA_ID %in% c(esrindex::region_settings$GOA$esr_area_id,
                                                 esrindex::region_settings$AI$esr_area_id,
                                                 esrindex::region_settings$EBS$esr_area_id,
                                                 esrindex::region_settings$NBS$esr_area_id))

esr_ai_subarea_cv <- dplyr::filter(cv_df,
                               AREA_ID %in% esrindex::region_settings$AI$esr_subarea_id)

esr_goa_subarea_cv <- dplyr::filter(cv_df,
                                AREA_ID %in% esrindex::region_settings$GOA$esr_subarea_id)

esr_ebs_subarea_cv <- dplyr::bind_rows(dplyr::filter(cv_df,
                                                 AREA_ID %in% esrindex::region_settings$EBS$esr_subarea_id),
                                   dplyr::filter(cv_df,
                                                 AREA_ID %in% esrindex::region_settings$NBS$esr_subarea_id)
                                   ) |>
  dplyr::mutate(AREA_NAME = as.numeric(AREA_NAME)*10)

plot_esr_cv_region <- ggplot() +
  geom_boxplot(data = esr_area_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~SURVEY, scales = "free_x", nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


plot_esr_cv_ebs <- ggplot() +
  geom_boxplot(data = esr_ebs_subarea_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~AREA_NAME, scales = "free_x", ncol = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_esr_cv_goa <- ggplot() +
  geom_boxplot(data = esr_goa_subarea_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~AREA_NAME, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_esr_cv_ai <- ggplot() +
  geom_boxplot(data = esr_ai_subarea_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~AREA_NAME, scales = "free_x", nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


png(filename = here::here("plots", "cvs", "esr_cv_by_region.png"), width = 169, height = 120, res = 300, units = "mm")
print(plot_esr_cv_region)
dev.off()

png(filename = here::here("plots", "cvs", "esr_cv_ebs.png"), width = 250, height = 120, res = 300, units = "mm")
print(plot_esr_cv_ebs)
dev.off()

png(filename = here::here("plots", "cvs", "esr_cv_goa.png"), width = 169, height = 120, res = 300, units = "mm")
print(plot_esr_cv_goa)
dev.off()

png(filename = here::here("plots", "cvs", "esr_cv_ai.png"), width = 169, height = 120, res = 300, units = "mm")
print(plot_esr_cv_ai)
dev.off()

# CVs for species where bottom trawl survey indices used in stock assessment ----
# Includes complexes and component species/groups within complexes for which biomass index point estimates and time series are show independently of the stock.
# Does not include the NBS, sculpin, grenadier, shark, or octopus complexes because of time limitations
# Note that in the GOA, there are subarea-specific splits that are not reflected, also due to time limitations.

channel <- esrindex::get_connected(schema = "AFSC")

index_species <- read.csv(file = here::here("analysis", "esr_safe_cv", "safe_species.csv"))

index_complexes <- read.csv(file = here::here("analysis", "esr_safe_cv", "safe_complexes.csv"))

region <- c("EBS", "BSS", "AI", "GOA")
start_year <- c(1982, 2002, 1991, 1991)

species_biomass <- data.frame()
complex_biomass <- data.frame()

for(ii in 1:length(region)) {

  racebase_tables <- gapindex::get_data(year_set = start_year[ii]:2023,
                                        survey_set = region[ii],
                                        spp_codes = index_species[index_species[[region[ii]]], ],
                                        sql_channel = channel)

  cpue <- gapindex::calc_cpue(racebase_tables = racebase_tables)

  biomass_strata <- gapindex::calc_biomass_stratum(racebase_tables = racebase_tables,
                                                   cpue = cpue)

  subarea_biomass <- gapindex::calc_biomass_subarea(racebase_tables = racebase_tables,
                                                    biomass_strata = biomass_strata) |>
    dplyr::filter(AREA_ID %in% c(esrindex::region_settings[[region[ii]]]$esr_subarea_id,
                                 esrindex::region_settings[[region[ii]]]$esr_area_id,
                                 99905))

  subarea_biomass <- dplyr::inner_join(subarea_biomass,
                                       dplyr::select(racebase_tables$subarea, AREA_ID, AREA_NAME, DESCRIPTION)) |>
    dplyr::inner_join(dplyr::select(racebase_tables$species, SPECIES_CODE, COMMON_NAME = COMMON_NAME.x))

  species_biomass <- rbind(species_biomass, subarea_biomass)

}

species_biomass$CV <- sqrt(species_biomass$BIOMASS_VAR)/species_biomass$BIOMASS_MT
species_biomass$complex <- FALSE
species_biomass$type <- "SAFE"

for(ii in 1:length(region)) {

  sel_complex <- index_complexes[index_complexes[[region[ii]]], ]

  unique_complex <- unique(sel_complex$GROUP)

  for(jj in 1:length(unique_complex)) {

    racebase_tables <- gapindex::get_data(year_set = start_year[ii]:2023,
                                          survey_set = region[ii],
                                          spp_codes = sel_complex[sel_complex$GROUP == unique_complex[jj], ],
                                          sql_channel = channel)

    cpue <- gapindex::calc_cpue(racebase_tables = racebase_tables)

    biomass_strata <- gapindex::calc_biomass_stratum(racebase_tables = racebase_tables,
                                                     cpue = cpue)

    subarea_biomass <- gapindex::calc_biomass_subarea(racebase_tables = racebase_tables,
                                                      biomass_strata = biomass_strata) |>
      dplyr::filter(AREA_ID %in% c(esrindex::region_settings[[region[ii]]]$esr_subarea_id,
                                   esrindex::region_settings[[region[ii]]]$esr_area_id,
                                   99905))

    subarea_biomass <- dplyr::inner_join(subarea_biomass,
                                         dplyr::select(racebase_tables$subarea, AREA_ID, AREA_NAME, DESCRIPTION))

    complex_biomass <- rbind(complex_biomass, subarea_biomass)
  }

}

complex_biomass$CV <- sqrt(complex_biomass$BIOMASS_VAR)/complex_biomass$BIOMASS_MT
complex_biomass$complex <- TRUE
complex_biomass$type <- "SAFE"

# Plot species

unique_species_codes <- unique(species_biomass$SPECIES_CODE)
unique_common_names <- unique(species_biomass$COMMON_NAME)

unique_common_names <- unique(species_biomass$COMMON_NAME)[order(unique_species_codes)]

species_biomass$COMMON_NAME <- factor(species_biomass$COMMON_NAME, levels = unique_common_names)

safe_species_ai_subarea_cv <- dplyr::filter(species_biomass,
                                            AREA_ID %in% esrindex::region_settings$AI$esr_subarea_id)

safe_species_goa_subarea_cv <- dplyr::filter(species_biomass,
                                             AREA_ID %in% esrindex::region_settings$GOA$esr_subarea_id)

safe_species_ebs_subarea_cv <- dplyr::bind_rows(dplyr::filter(species_biomass,
                                                              AREA_ID %in% esrindex::region_settings$EBS$esr_subarea_id)) |>
  dplyr::mutate(AREA_NAME = as.numeric(AREA_NAME)*10)

plot_safe_species_cv_region <- ggplot() +
  geom_boxplot(data = species_biomass,
               mapping = aes(x = factor(COMMON_NAME, levels = unique_common_names),
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~SURVEY, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


plot_safe_species_cv_ebs <- ggplot() +
  geom_boxplot(data = safe_species_ebs_subarea_cv,
               mapping = aes(x = COMMON_NAME,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~AREA_NAME, nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_safe_species_cv_goa <- ggplot() +
  geom_boxplot(data = safe_species_goa_subarea_cv,
               mapping = aes(x = COMMON_NAME,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "GOA"), nrow = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_safe_species_cv_ai <- ggplot() +
  geom_boxplot(data = safe_species_ai_subarea_cv,
               mapping = aes(x = COMMON_NAME,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "AI"),  nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

png(filename = here::here("plots", "cvs", "safe_species_cv_by_region.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_species_cv_region)
dev.off()

png(filename = here::here("plots", "cvs", "safe_species_cv_ebs.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_species_cv_ebs)
dev.off()

png(filename = here::here("plots", "cvs", "safe_species_cv_goa.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_species_cv_goa)
dev.off()

png(filename = here::here("plots", "cvs", "safe_species_cv_ai.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_species_cv_ai)
dev.off()

# Plot complexes

safe_complex_ai_subarea_cv <- dplyr::filter(complex_biomass,
                               AREA_ID %in% esrindex::region_settings$AI$esr_subarea_id)

safe_complex_goa_subarea_cv <- dplyr::filter(complex_biomass,
                                AREA_ID %in% esrindex::region_settings$GOA$esr_subarea_id)

safe_complex_ebs_subarea_cv <- dplyr::filter(complex_biomass,
                                                 AREA_ID %in% esrindex::region_settings$EBS$esr_subarea_id) |>
  dplyr::mutate(AREA_NAME = as.numeric(AREA_NAME)*10)

plot_safe_complex_cv_region <- ggplot() +
  geom_boxplot(data = complex_biomass,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~SURVEY, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


plot_safe_complex_cv_ebs <- ggplot() +
  geom_boxplot(data = safe_complex_ebs_subarea_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~AREA_NAME, nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_safe_complex_cv_goa <- ggplot() +
  geom_boxplot(data = safe_complex_goa_subarea_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "GOA"), nrow = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_safe_complex_cv_ai <- ggplot() +
  geom_boxplot(data = safe_complex_ai_subarea_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "AI"),  nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

png(filename = here::here("plots", "cvs", "safe_complex_cv_by_region.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_complex_cv_region)
dev.off()

png(filename = here::here("plots", "cvs", "safe_complex_cv_ebs.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_complex_cv_ebs)
dev.off()

png(filename = here::here("plots", "cvs", "safe_complex_cv_goa.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_complex_cv_goa)
dev.off()

png(filename = here::here("plots", "cvs", "safe_complex_cv_ai.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_complex_cv_ai)
dev.off()


# ESR and SAFE species CVs by area and subarea

esr_safe_species_cv <- species_biomass |>
  dplyr::mutate(SPECIES_CODE = COMMON_NAME) |>
  dplyr::bind_rows(dplyr::filter(esr_area_cv, !complex)) |>
  dplyr::filter(!(SURVEY %in% c("BSS", "NBS")))

plot_safe_species_cv_region <- ggplot() +
  geom_boxplot(data = esr_safe_species_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100,
                             fill = type)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  facet_wrap(~SURVEY, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ebs_subarea_species_cv <- safe_species_ebs_subarea_cv |>
  dplyr::mutate(SPECIES_CODE = COMMON_NAME) |>
  dplyr::bind_rows(dplyr::filter(esr_ebs_subarea_cv, !complex))

goa_subarea_species_cv <- safe_species_goa_subarea_cv |>
  dplyr::mutate(SPECIES_CODE = COMMON_NAME) |>
  dplyr::bind_rows(dplyr::filter(esr_goa_subarea_cv, !complex))

ai_subarea_species_cv <- safe_species_ai_subarea_cv |>
  dplyr::mutate(SPECIES_CODE = COMMON_NAME) |>
  dplyr::bind_rows(dplyr::filter(esr_ai_subarea_cv, !complex))

plot_safe_esr_species_ebs <- ggplot() +
  geom_boxplot(data = ebs_subarea_species_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100, 
                             fill = type)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  facet_wrap(~AREA_NAME,  nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

plot_safe_esr_species_goa <- ggplot() +
  geom_boxplot(data = goa_subarea_species_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100, 
                             fill = type)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "GOA"),  nrow = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

plot_safe_esr_species_ai <- ggplot() +
  geom_boxplot(data = ai_subarea_species_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100, 
                             fill = type)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "AI"),  nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

png(filename = here::here("plots", "cvs", "esr_safe_species_cv_by_region.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_species_cv_region)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_species_cv_ebs.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_esr_species_ebs)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_species_cv_goa.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_esr_species_goa)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_species_cv_ai.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_esr_species_ai)
dev.off()

# ESR and SAFE complex CVs by region

esr_safe_complex_cv <- complex_biomass |>
  dplyr::bind_rows(esr_area_cv) |>
  dplyr::filter(complex | is.na(complex),
                !(SURVEY %in% c("BSS", "NBS")))

plot_safe_complex_cv_region <- ggplot() +
  geom_boxplot(data = esr_safe_complex_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100,
                             fill = type)) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  facet_wrap(~SURVEY, nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

# ESR and SAFE complex CVs by subarea

ebs_subarea_complex_cv <- dplyr::bind_rows(safe_complex_ebs_subarea_cv,
                                   dplyr::filter(esr_ebs_subarea_cv, complex))

goa_subarea_complex_cv <- dplyr::bind_rows(safe_complex_goa_subarea_cv,
                                  dplyr::filter(esr_goa_subarea_cv, complex))


ai_subarea_complex_cv <- dplyr::bind_rows(safe_complex_ai_subarea_cv,
                                  dplyr::filter(esr_ai_subarea_cv, complex))


plot_safe_esr_complex_ebs <- ggplot() +
  geom_boxplot(data = ebs_subarea_complex_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100, 
                             fill = type)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~AREA_NAME,  nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

plot_safe_esr_complex_goa <- ggplot() +
  geom_boxplot(data = goa_subarea_complex_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100, 
                             fill = type)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "GOA"),  nrow = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

plot_safe_esr_complex_ai <- ggplot() +
  geom_boxplot(data = ai_subarea_complex_cv,
               mapping = aes(x = SPECIES_CODE,
                             y = CV*100, 
                             fill = type)) +
  scale_fill_manual(values = c("ESR" = "#E69F00", "SAFE" = "#56B4E9")) +
  scale_y_continuous(name = "CV (%)", limits = c(0,100)) +
  facet_wrap(~esrindex::set_stratum_order(AREA_NAME, region = "AI"),  nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

png(filename = here::here("plots", "cvs", "esr_safe_complex_cv_by_region.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_complex_cv_region )
dev.off()

png(filename = here::here("plots", "cvs", "safe_complex_cv_by_region.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_complex_cv_region)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_complex_cv_ebs.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_esr_complex_ebs)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_complex_cv_goa.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_esr_complex_goa)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_complex_cv_ai.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_safe_esr_complex_ai)
dev.off()



# Autocorrelation ----

get_autocorrelation <- function(x, lag = 1, min_non_zero = 5) {

  if(sum(x > 0) < min_non_zero) {
    return(NA)
  }

  acf_output <- acf(x, type = "correlation", plot = FALSE)

  return(acf_output$acf[[1+lag]])

}

index_acf <- dplyr::bind_rows(
  dplyr::group_by(cv_df, SURVEY, SPECIES_CODE, AREA_NAME, type, complex) |>
    dplyr::arrange(YEAR) |>
    dplyr::summarise(acf_lag1 = get_autocorrelation(BIOMASS_MT, lag = 1),
                     acf_lag2 = get_autocorrelation(BIOMASS_MT, lag = 2)),
  dplyr::group_by(complex_biomass, SURVEY, SPECIES_CODE, AREA_NAME, type, complex) |>
    dplyr::arrange(YEAR) |>
    dplyr::summarise(acf_lag1 = get_autocorrelation(BIOMASS_MT, lag = 1),
                     acf_lag2 = get_autocorrelation(BIOMASS_MT, lag = 2)),
  dplyr::group_by(species_biomass, SURVEY, COMMON_NAME, AREA_NAME, type, complex) |>
    dplyr::arrange(YEAR) |>
    dplyr::summarise(acf_lag1 = get_autocorrelation(BIOMASS_MT, lag = 1),
                     acf_lag2 = get_autocorrelation(BIOMASS_MT, lag = 2)) |>
    dplyr::rename(SPECIES_CODE = COMMON_NAME)
) |>
  dplyr::ungroup() |>
  dplyr::filter(!(SURVEY %in% c("BSS", "NBS"))) |>
  dplyr::mutate(complex = ifelse(complex, "Complex", "Species"),
                AREA_NAME = ifelse(AREA_NAME == "Standard", "All", AREA_NAME)) |>
  dplyr::mutate(AREA_NAME = ifelse(AREA_NAME %in% c("1.0", "2.0", "3.0", "4.0", "5.0", "6.0"),
                                   as.character(as.numeric(AREA_NAME)*10), AREA_NAME))

plot_autocorrelation_subarea_complex <- ggplot() +
  geom_tile(data = dplyr::filter(index_acf, complex == "Complex"),
            mapping = aes(y = SPECIES_CODE,
                          x = AREA_NAME,
                          fill = cut(acf_lag1, breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8, 1)))) +
  scale_fill_brewer("Autocorrelation\n(lag 1 survey)", 
                    palette = "RdBu",#, 
                    # drop = FALSE,
                    na.translate = FALSE
                    ) +
  facet_wrap(~SURVEY, scales = "free", nrow = 3) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey50"))

plot_autocorrelation_subarea_species <- ggplot() +
  geom_tile(data = dplyr::filter(index_acf, complex == "Species"),
            mapping = aes(y = SPECIES_CODE,
                          x = AREA_NAME,
                          fill = cut(acf_lag1, breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8, 1)))) +
  scale_fill_brewer("Autocorrelation\n(lag 1 survey)", 
                    palette = "RdBu", 
                    # drop = FALSE, 
                    na.translate = FALSE) +
  facet_wrap(~SURVEY, scales = "free", nrow = 3) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey50"))

plot_autocorrelation_region <- ggplot() +
  geom_tile(data = dplyr::filter(index_acf, AREA_NAME == "All"),
            mapping = aes(x = SPECIES_CODE,
                          y = SURVEY,
                          fill = cut(acf_lag1, breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8, 1)))) +
  scale_fill_brewer("Autocorrelation\n(lag 1 survey)", 
                    palette = "RdBu",
                    # drop = FALSE, 
                    na.translate = FALSE) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  facet_wrap(~complex,
             nrow = 2,
             scales = "free_x") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey50"),
        legend.position = "bottom")



png(filename = here::here("plots", "cvs", "esr_safe_autocorrelation_complex.png"), width = 169, height = 169, res = 300, units = "mm")
print(plot_autocorrelation_region)
dev.off()


png(filename = here::here("plots", "cvs", "esr_safe_autocorrelation_subarea_complex.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_autocorrelation_subarea_complex)
dev.off()

png(filename = here::here("plots", "cvs", "esr_safe_autocorrelation_subarea_species.png"), width = 169, height = 200, res = 300, units = "mm")
print(plot_autocorrelation_subarea_species)
dev.off()

