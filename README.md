  <!-- badges: start -->
  [![R-CMD-check](https://github.com/sean-rohan-NOAA/esrindex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sean-rohan-NOAA/esrindex/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# esrindex

The esrindex package is used to estimate bottom trawl survey abundance indices for Structural Epifauna, Jellyfish, Miscellaneous Benthic Fauna, and Forage Fish contributions in [Alaska Ecosystem Status Reports (ESRs)](https://apps-afsc.fisheries.noaa.gov/refm/reem/ecoweb/index.php). Time series for each taxonomic group and region are included in the package as built-in data sets. Indicator time series figures are available in [/plots/](./plots/). Indicator time series are updated in September following the finalization of catch and effort data from summer bottom trawl surveys.

# Methodology

The esrindex package produces estimates of regional and subarea/stratum indices of abundance (biomass in kilotons) and confidence intervals for each ESR taxa by fitting a multivariate random effects model (REM) to design-based abundance index time series from individual survey strata (EBS groundfish strata) or subareas (AI or GOA INPFC subareas). Abundance indices are calculated from AFSC summer bottom trawl survey catch and effort data using the [gapindex R package](https://afsc-gap-products.github.io/gapindex/) (Oyafuso, 2024). Random effects models are fitted to abundance index time series using the [rema R package](https://afsc-assessments.github.io/rema/) (Sullivan and Balstad, 2022).


# Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
