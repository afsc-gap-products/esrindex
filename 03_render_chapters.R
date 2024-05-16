# Knit chapters
library(esrindex)

# Render chapters
make_esr_chapter(xml_path = "./chapters/AI_jellyfish.xml")
make_esr_chapter(xml_path = "./chapters/AI_misc_species.xml")
make_esr_chapter(xml_path = "./chapters/AI_structural_epifauna.xml")
make_esr_chapter(xml_path = "./chapters/AI_forage_fish.xml")

make_esr_chapter(xml_path = "./chapters/GOA_jellyfish.xml")
make_esr_chapter(xml_path = "./chapters/GOA_misc_species.xml")
make_esr_chapter(xml_path = "./chapters/GOA_structural_epifauna.xml")
make_esr_chapter(xml_path = "./chapters/GOA_forage_fish.xml")

make_esr_chapter(xml_path = "./chapters/EBS_jellyfish.xml")
make_esr_chapter(xml_path = "./chapters/EBS_misc_species.xml")
make_esr_chapter(xml_path = "./chapters/EBS_structural_epifauna.xml")


# Make .csv files containing rema time series and design-based index observations
make_index_tables(esrindex::AI_INDICATOR)
make_index_tables(esrindex::GOA_INDICATOR)
make_index_tables(esrindex::EBS_INDICATOR)
make_index_tables(esrindex::NBS_INDICATOR)