library(esrindex)
library(akgfmaps)

ebs_files <- list.files(path = here::here("output", "EBS"),
                        full.names = TRUE,
                        pattern = "gapindex")


for(jj in 1:length(ebs_files)) {
  group_cpue <- readRDS(file = ebs_files[jj])$cpue
  
  plot_cpue_maps(
    gapindex_cpue = group_cpue,
    crs = "EPSG:3338",
    fig_res = 300)
}



ai_files <- list.files(path = here::here("output", "AI"),
                       full.names = TRUE,
                       pattern = "gapindex")


for(jj in 1:length(ai_files)) {
  group_cpue <- readRDS(file = ai_files[jj])$cpue
  
  plot_cpue_maps(
    gapindex_cpue = group_cpue,
    crs = "EPSG:3338",
    fig_res = 300)
}


goa_files <- list.files(path = here::here("output", "GOA"),
                        full.names = TRUE,
                        pattern = "gapindex")

for(jj in 1:length(goa_files)) {
  group_cpue <- readRDS(file = goa_files[jj])$cpue
  
  plot_cpue_maps(
    gapindex_cpue = group_cpue,
    crs = "EPSG:3338",
    fig_res = 300)
}
