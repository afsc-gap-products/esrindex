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
    "Shrimps", "Pricklebacks", "Sandlances", "Capelin", "Eulachon", "Pacific herring",
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


# # Species group code ranges
# species_groups <- data.frame(group_name = c("Sponges", "Jellyfish", "Sea anemones", "Eelpouts", "Poachers", "Echinoderms", "Shrimps", "Pricklebacks", "Sandlances", "Capelin", "Eulachon", "Pacific herring", "Pacific sandfish", "Sea stars", "Myctophids", "Corals", "Sea pens"),
#                              min_code = c(-999, 40500, 43000, 24100, 20000, 80000, 66000, 23800, 20202, 23041, 23010, 21110, 21592, -999, 22600, -999, 42000),
#                              max_code = c(-999, 40599, 43999, 24499, 20099, 85999, 66999, 23866, 20210, 23041, 23010, 21110, 21592, -999, 22656, -999, 42021),
#                              complex = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))

# save(species_groups, file = "./data/species_groups.rda")


# # Species codes for non-sequential groups
# species_groups_ns <- list(`Sea stars` = c(80000, 80005, 80010, 80012, 80015, 80018, 80020, 80030, 80105, 80106, 80110, 80111, 80112, 80113, 80115, 80116, 80117, 80118, 80119, 80120, 80159, 80160, 80165, 80170, 80171, 80180, 80182, 80190, 80200, 80201, 80220, 80230, 80250, 80310, 80311, 80312, 80520, 80525, 80535, 80536, 80537, 80539, 80540, 80541, 80542, 80543, 80544, 80545, 80546, 80547, 80548, 80549, 80550, 80551, 80552, 80553, 80554, 80555, 80556, 80557, 80560, 80561, 80590, 80591, 80594, 80595, 80596, 80597, 80598, 80601, 80602, 80603, 80604, 80610, 80620, 80624, 80625, 80628, 80629, 80630, 80631, 80632, 80633, 80634, 80635, 80636, 80637, 80640, 80645, 80646, 80647, 80650, 80660, 80670, 80690, 80691, 80710, 80728, 80729, 80730, 80731, 80732, 80733, 80735, 80800, 80810, 80811, 80910, 80915, 81060, 81061, 81062, 81063, 81064, 81065, 81066, 81067, 81068, 81069, 81070, 81071, 81072, 81073, 81074, 81080, 81090, 81092, 81093, 81094, 81095, 81100, 81101, 81120, 81121, 81122, 81125, 81130, 81135, 81295, 81310, 81311, 81312, 81313, 81314, 81315, 81316, 81317, 81318, 81319, 81320, 81321, 81322, 81323, 81340, 81345, 81350, 81355, 81356, 81360, 81361, 81365, 81370, 81741, 81742, 81743, 81779, 81780, 81820, 81829, 81830, 81831, 81835, 81840, 81841, 81850, 81860, 81870, 81875, 81880, 81890, 81908, 81909, 81910, 81912, 81919, 81920, 81921, 81930, 82000, 82001, 82002, 82003, 82030, 82035, 82060, 82090, 82091, 82100, 82101),
#                           `Corals` = c(41100, 41102, 41104, 41105, 41106, 41201, 41220, 41221, 41222, 41300, 41331, 41332, 41333, 41500, 41510, 41519, 41520, 41521, 41522, 41523, 41525, 41526, 41530, 41531, 41540, 41541, 41550, 41551, 41552, 41553, 41570, 41571, 41572, 41573, 41576, 41580, 41581, 41582, 41583, 41584, 41585, 41586, 41587, 41590, 41591, 41595, 41600, 41601, 41605, 41700, 41701, 41750, 41751, 41752, 44000, 44004, 44005, 44009, 44010, 44011, 44012, 44017, 44019, 44020, 44021, 44022, 44023, 44028, 44029, 44030, 44031, 44032, 44033, 44034, 44035, 44036, 44037, 44038, 44039, 44040, 44041, 44042, 44045, 44046, 44047, 44048, 44049, 44050, 44051, 44052, 44053, 44060, 44061, 44065, 44070, 44071, 44072, 44073, 44074, 44075, 44076, 44077, 44078, 44079, 44080, 44081, 44082, 44083, 44084, 44085, 44086, 44087, 44088, 44089, 44090, 44091, 44092, 44093, 44094, 44095, 44096, 44097, 44098, 44099, 44100, 44101, 44102, 44103, 44104, 44105, 44106, 44107, 44108, 44109, 44110, 44111, 44114, 44115, 44120, 44121, 44122, 44123),
#                           `Sponges` = c(91000, 91002, 91005, 91010, 91014, 91015, 91016, 91017, 91018, 91019, 91020, 91030, 91035, 91036, 91037, 91038, 91039, 91040, 91041, 91042, 91044, 91046, 91047, 91048, 91049, 91050, 91051, 91052, 91053, 91054, 91055, 91056, 91057, 91059, 91060, 91061, 91062, 91063, 91064, 91065, 91066, 91067, 91068, 91069, 91070, 91071, 91072, 91073, 91074, 91075, 91076, 91077, 91078, 91079, 91080, 91081, 91082, 91083, 91084, 91085, 91086, 91087, 91088, 91089, 91090, 91091, 91092, 91093, 91094, 91096, 91097, 91098, 91099, 91100, 91101, 91102, 91103, 91104, 91105, 91106, 91109, 91110, 91111, 91112, 91113, 91114, 91115, 91120, 91121, 91125, 91127, 91128, 91129, 91130, 91131, 91201, 91210, 91211, 91212, 91213, 91215, 91217, 91220, 91221, 91224, 91225, 91226, 91227, 91228, 91230, 91231, 91233, 91234, 91237, 91238, 91239, 91241, 91249, 91250, 91251, 91252, 91255, 91256, 91257, 91260, 91261, 91262, 91263, 91264, 91265, 91266, 91267, 91268, 91269, 91270, 91271, 91272, 91273, 91274, 91700, 91701, 91704, 91705, 91710, 91720, 91721, 91722, 91725, 91995, 91996, 91997, 91998, 95011, 99981, 99982, 99983, 99984, 99985, 99987, 99988))
#
# save(species_groups_ns, file = "./data/species_groups_ns.rda")


# Chapter settings by region and contribution; group_name for chapters should match group_name from species_groups data
chapter_settings <- list(
  GOA = list(
    misc_species = list(group_name = c("Eelpouts", "Poachers", "Shrimps", "Sea stars")),
    jellyfish = list(group_name = c("Jellyfish")),
    structural_epifauna = list(group_name = c("Sponges", "Sea anemones", "Corals", "Sea pens")),
    forage_fish = list(group_name = c("Pacific herring", "Capelin", "Eulachon", "Sandlances", "Myctophids", "Pacific sandfish", "Pricklebacks"))
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

# # Make spreadsheets showing species groups
# write.csv(species_groups,
#           here::here("assets", "species_groups.csv"),
#           row.names = FALSE)
#
# write.csv(dplyr::bind_rows(data.frame(group_name = "Sea stars",
#                                       SPECIES_CODE = as.numeric(species_groups_ns[["Sea stars"]])),
#                            data.frame(group_name = "Corals",
#                                       SPECIES_CODE = as.numeric(species_groups_ns[["Corals"]])),
#                            data.frame(group_name = "Sponges",
#                                       SPECIES_CODE = as.numeric(species_groups_ns[["Sponges"]]))),
#           here::here("assets", "species_groups_ns.csv"),
#           row.names = FALSE)

# Reinstall the package
devtools::install()
