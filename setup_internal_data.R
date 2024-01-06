# Build internal data sets

# Region settings
region_settings <- list(GOA = list(esr_subarea_id = c(919, 929, 939, 959),
                                   esr_area_id = 99903,
                                   min_year = 1990),
                        AI = list(esr_subarea_id = c(299, 799, 820, 3499, 5699),
                                  esr_area_id = 99904,
                                  min_year = 1990),
                        EBS = list(esr_subarea_id = c(1:6),
                                   esr_area_id = 99901,
                                   min_year = 1982),
                        NBS = 1)

save(region_settings, file = "./data/region_settings.rda")

# Invert groups
benthic_groups <- data.frame(group_name = c("Porifera", "Sea Whips and Pens", "Jellyfish", "Sea Anemones", "Eelpouts", "Poachers", "Sea stars"),
                            min_code = c(91000, 42000, 40000, 41000, 24100, 20000, 80000),
                            max_code = c(91999, 42999, 40999, 41999, 24499, 20099, 84999))

save(invert_groups, file = "./data/invert_groups.rda")

# Forage species

forage_species <- data.frame(common_name = c("capelin", "sandlance", "Pacific herring", "eulachon"),
                             species_code,
                             EBS,
                             NBS,
                             GOA,
                             AI)

