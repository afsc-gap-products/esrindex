# Build internal data sets for generating ESR chapters
#
# Update instructions:
#   - (1) Make any necessary changes to settings and save the new files to .rda.
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
region_settings <- list(
  GOA = list(
    esr_subarea_id = c(610, 620, 630, 640, 650),
    esr_area_id = 99903,
    min_year = 1990,
    min_rema_year = 1990,
    exclude_years = 2001
  ),
  AI = list(
    esr_subarea_id = c(299, 799, 3499, 5699),
    esr_area_id = 99904,
    min_year = 1991,
    min_rema_year = 1991,
    exclude_years = NULL
  ),
  EBS = list(
    esr_subarea_id = c(1:6),
    esr_area_id = 99901,
    min_year = 1982,
    min_rema_year = 1982,
    exclude_years = NULL
  ),
  NBS = list(
    esr_subarea_id = c(70, 71, 81),
    esr_area_id = 99902,
    min_year = 2010,
    min_rema_year = 2017,
    exclude_years = NULL
  )
)

save(region_settings, file = "./data/region_settings.rda")

# pull up to date species information from Oracle
channel <- esrindex::get_connected(schema = "AFSC")
tax_tables <- c("TAXONOMIC_CLASSIFICATION", "TAXONOMIC_CHANGES")
for (i in 1:length(tax_tables)) {
  a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM GAP_PRODUCTS.", tax_tables[i]))
  readr::write_csv(
    x = a,
    file = paste0("assets/", tolower(tax_tables[i]), ".csv")
  )
  b <- readr::read_csv(file = paste0("assets/", tolower(tax_tables[i]), ".csv"))
  assign(x = tolower(tax_tables[i]), value = b)
  remove(a, b)
}


# including taxonomic changes table so species codes aren't dropped through time due to taxonomic reshuffling
code_changes <- merge(
  x = taxonomic_changes[grepl("change taxon code", taxonomic_changes$ACTION), c("OLD_SPECIES_CODE", "NEW_SPECIES_CODE")],
  y = taxonomic_classification,
  by.x = "NEW_SPECIES_CODE",
  by.y = "SPECIES_CODE",
  all.x = TRUE
)
names(code_changes)[names(code_changes) == "OLD_SPECIES_CODE"] <- "SPECIES_CODE"
code_changes <- code_changes[, !names(code_changes) %in% "NEW_SPECIES_CODE"]
classy <- unique(rbind(taxonomic_classification, code_changes))



# Setting up group level data frame
groups <- data.frame(
  group_name = c(
    "Sponges", "Jellyfish", "Sea anemones", "Eelpouts", "Poachers", "Echinoderms",
    "Shrimps", "Pricklebacks", "Sand lances", "Capelin", "Eulachon", "Pacific herring",
    "Pacific sandfish", "Sea stars", "Myctophids", "Corals", "Sea pens"
  ),
  sci_name = c(
    "Porifera", "Scyphozoa", "Actiniaria", "Zoarcidae", "Agonidae", "Echinodermata",
    "Caridea", "Stichaeidae", "Ammodytidae", "Mallotus villosus", "Thaleichthys pacificus",
    "Clupea pallasii", "Trichodon trichodon", "Asteroidea", "Myctophidae",
    "Anthozoa", "Pennatuloidea"
  ),
  group_level = c(
    "Phylum", "Class", "Order", "Family", "Family", "Phylum", "Infraorder",
    "Family", "Family", "Species", "Species", "Species", "Species", "Class",
    "Family", "Subphylum", "Superfamily"
  )
)


# getting all species codes for each group
species_groups <- list()
for (i in 1:nrow(groups)) {
  species_groups[[i]] <- 
    esrindex:::get_group_codes(
    group = groups$sci_name[i], 
    rank = groups$group_level[i], 
    tax_table = classy)
}
names(species_groups) <- groups$group_name


save(species_groups, file = "./data/species_groups.rda")

# Chapter settings by region and contribution; group_name for chapters should match group_name from species_groups data
chapter_settings <- list(
  GOA = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Shrimps", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones", "Corals", "Sea pens")),
    forage_fish = list(group_name = c("Pacific herring", "Capelin", "Eulachon", "Sand lances", "Myctophids", "Pacific sandfish", "Pricklebacks"))
  ),
  AI = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Shrimps", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones", "Corals", "Sea pens")),
    forage_fish = list(group_name = c("Myctophids"))
  ),
  EBS = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones", "Sea pens"))
  ),
  NBS = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones"))
  )
)

save(chapter_settings, file = "./data/chapter_settings.rda")

# Write GOA 2025 stratum geopackage to inst/extdata
# Used to reassign GOA hauls to 2025 strata
library(akgfmaps)
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")

if(all(goa_layers$survey.strata$DESIGN_YEAR == 2025)) {
  sf::st_write(obj = goa_layers$survey.strata,
               dsn = "./inst/extdata/goa_strata_2025.gpkg",
               delete_dsn = TRUE)
}

# Reinstall the package
devtools::install()
