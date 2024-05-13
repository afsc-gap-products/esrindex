# Build internal data sets for generating ESR chapters
#
# Update instructions:
#   - (1) Make any necessary changes to species codes and settings and save the new files to .rda.
#   - (2) Reinstall the package by building from locally-cloned repo.
#   - (3) After updating, check that group_name values in chapter_settings are all represented in group_name values in species_groups.
#   - (4) Update version number in DESCRIPTION file then push to remote.
#   - (5) Generate figures.
#   - (6) Generate knitted chapters with the current year's results and previous year's text.
#   - (7) Update results text to reflect current year's results.
#   - (8) Regenerate knitted chapters with the new text.
#   - (9) Send to group leads for review, incorporate changes, then send to ESR leads.
#   - (10) Push final version with remote.
#   - (11) Update Release on Github.

library(devtools)

# Region settings for years to include and area_ids for ESR areas/subareas
region_settings <- list(GOA = list(esr_subarea_id = c(919, 929, 939, 949, 959),
                                   esr_area_id = 99903,
                                   min_year = 1990),
                        AI = list(esr_subarea_id = c(299, 799, 3499, 5699),
                                  esr_area_id = 99904,
                                  min_year = 1991),
                        EBS = list(esr_subarea_id = c(1:6),
                                   esr_area_id = 99901,
                                   min_year = 1982),
                        NBS = list(esr_subarea_id = NULL,
                                   esr_area_id = 99902,
                                   min_year = 2010))

save(region_settings, file = "./data/region_settings.rda")

# Species group code ranges
species_groups <- data.frame(group_name = c("Sponges", "Sea whips", "Jellyfish", "Sea anemones", "Eelpouts", "Poachers", "Echinoderms", "Shrimps", "pricklebacks", "sandlance", "capelin", "eulachon", "Pacific herring", "Pacific sandfish", "Sea stars", "Gorgonians", "Pennatulaceans", "Hydrocorals", "Soft corals"),
                             min_code = c(91000, 42000, 40500, 43000, 24100, 20000, 80000, 66000, 23800, 20202, 23041, 23010, 21110, 21592, -999, -999, 42000, 44000, 41100),
                             max_code = c(91999, 42013, 40599, 43999, 24499, 20099, 85999, 66999, 23866, 20210, 23041, 23010, 21110, 21592, -999, -999, 42013, 44123, 41106),
                             complex = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))

save(species_groups, file = "./data/species_groups.rda")

# Species codes for non-sequential groups
species_groups_ns <- list(`Sea stars` = c(80000:82499, 83000:84999),
                          `Gorgonians` = c(41099, 41500))

save(species_groups_ns, file = "./data/species_groups_ns.rda")


# Chapter settings by region and contribution; group_name for chapters should match group_name from species_groups data
chapter_settings <- list(
  GOA = list(
    misc_species = list(group_name = c("Echinoderms", "Shrimps", "Eelpouts", "Poachers")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones", "Gorgonians", "Pennatulaceans", "Hydrocorals", "Soft corals")),
    forage_fish = list(group_name = c("eulachon", "capelin", "pricklebacks", "sandlance", "Pacific sandfish"))
  ),
  AI = list(
    misc_species = list(group_name = c("Echinoderms", "Shrimps", "Eelpouts", "Poachers")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones", "Gorgonians", "Hydrocorals", "Soft corals", "Pennatulaceans"))
  ),
  EBS = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sea whips", "Sea anemones", "Sponges"))
  ),
  NBS = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sea anemones", "Sponges"))
  )
)

save(chapter_settings, file = "./data/chapter_settings.rda")

# Reinstall the package
devtools::install()


# Make spreadsheets showing species groups
write.csv(species_groups, 
          here::here("assets", "species_groups.csv"), 
          row.names = FALSE)

write.csv(dplyr::bind_rows(data.frame(group_name = "Sea stars", 
                                      SPECIES_CODE = as.numeric(species_groups_ns[["Sea stars"]])),
                           data.frame(group_name = "Gorgonians", 
                                      SPECIES_CODE = as.numeric(species_groups_ns[["Gorgonians"]]))),
          here::here("assets", "species_groups_ns.csv"),
          row.names = FALSE)
