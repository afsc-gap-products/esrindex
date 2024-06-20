library(akgfmaps)
library(esrindex)
library(dplyr)
library(mgcv)


coral_dat <- readRDS(file = here::here("output", "ai", "gapindex_ai_corals.rds"))

coral_cpue <- coral_dat$racebase_tables$haul |>
  dplyr::select(all_of(c("STATIONID", "STRATUM", "HAULJOIN"))) |> 
  dplyr::inner_join(coral_dat$cpue)

multiple_samples <- coral_cpue |>
  dplyr::group_by(STATIONID, STRATUM) |>
  dplyr::summarise(N_YEARS = n()) |>
  dplyr::filter(N_YEARS > 3)

cpue_gt_0 <- coral_cpue |>
  dplyr::group_by(STATIONID, STRATUM) |>
  dplyr::summarise(MEAN_CPUE = mean(CPUE_KGKM2)) |>
  dplyr::filter(MEAN_CPUE > 0)

first_year <- coral_cpue |>
  dplyr::group_by(STATIONID, STRATUM) |>
  dplyr::summarise(FIRST_YEAR = min(YEAR))

coral_cpue <- coral_cpue |>
  dplyr::inner_join(multiple_samples) |>
  dplyr::inner_join(cpue_gt_0) |>
  dplyr::inner_join(first_year)

coral_cpue$PRIOR_HAULS <- NA
coral_cpue$YEARS_SINCE_LAST <- NA
coral_cpue$STN_STRATUM <- factor(paste0(coral_cpue$STATIONID, "-", coral_cpue$STRATUM))
coral_cpue$ELAPSED <- coral_cpue$YEAR - coral_cpue$FIRST_YEAR

fill_years_since_last <- diff(range(coral_cpue$YEAR))

for(ii in 1:nrow(coral_cpue)) {

  stn_year <- coral_cpue[ii, ]

  stn_all_years <- dplyr::filter(coral_cpue,
                                 STATIONID == coral_cpue$STATION[ii],
                                 STRATUM == coral_cpue$STRATUM[ii],
                                 YEAR < stn_year$YEAR)
  
  coral_cpue$PRIOR_HAULS[ii] <- nrow(stn_all_years)

  year_diff <- stn_year$YEAR - stn_all_years$YEAR
  year_diff <- year_diff[year_diff > 0]
  
  if(length(year_diff) > 0) {
    coral_cpue$YEARS_SINCE_LAST[ii] <- min(year_diff)
  } else {
    coral_cpue$YEARS_SINCE_LAST[ii] <- fill_years_since_last 
  }

}

coral_cpue$FIRST_HAUL <- factor(coral_cpue$ELAPSED > 0)

# Fit models
coral_mod_0 <- mgcv::gam(formula = CPUE_KGKM2 ~ s(YEAR) + s(STN_STRATUM, bs = "re"), 
                       data = coral_cpue,
                       control = list(nthreads = 4))

coral_mod_1 <- mgcv::gam(formula = CPUE_KGKM2 ~ s(PRIOR_HAULS) + s(STN_STRATUM, bs = "re"), 
                         data = coral_cpue,
                         control = list(nthreads = 4))

coral_mod_2 <- mgcv::gam(formula = CPUE_KGKM2 ~ s(ELAPSED) + s(STN_STRATUM, bs = "re"), 
                         data = coral_cpue,
                         control = list(nthreads = 4))

coral_mod_3 <- mgcv::gam(formula = CPUE_KGKM2 ~ s(YEARS_SINCE_LAST) + s(STN_STRATUM, bs = "re"), 
                         data = dplyr::filter(coral_cpue, ELAPSED > 0),
                         control = list(nthreads = 4))

coral_mod_4 <- mgcv::gam(formula = CPUE_KGKM2 ~ s(YEAR, by = FIRST_HAUL) + s(STN_STRATUM, bs = "re"), 
                         data = coral_cpue,
                         control = list(nthreads = 4))

plot(coral_mod_0)
plot(coral_mod_1)
plot(coral_mod_2)
plot(coral_mod_3)
plot(coral_mod_4)

