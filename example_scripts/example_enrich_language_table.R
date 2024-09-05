# This script contains example of manipulating CLDF LanguageTables in regards to information about Isolate and add a Family_name column, as well as combine Glottolog's ValueTable and LanguageTable. 


#install.packages("remotes", version = "2.4.2.1", repos = "http://cran.us.r-project.org")
library(remotes)

#remotes::install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#devtools::install_github("HedvigS/rgrambank", ref = "v1.0")
library(rgrambank)

GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

LanguageTable <- GB_rcldf_obj_v1$tables$LanguageTable %>% 
  dplyr::rename(Family_ID = Family_level_ID) %>% 
  dplyr::select(-Family_name)

Glottolog_LanguageTable <- glottolog_rcldf_obj$tables$LanguageTable
Glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable

Glottolog_ValueTable_LanguageTable <- rgrambank::combine_ValueTable_LanguageTable(LanguageTable = Glottolog_LanguageTable, 
                                                                       ValueTable = Glottolog_ValueTable, 
                                                                       Is_Glottolog = TRUE)

LanguageTable_Isolates_enriched <- rgrambank::add_isolate_info(LanguageTable = LanguageTable, 
                         Glottolog_ValueTable_LanguageTable = Glottolog_ValueTable_LanguageTable,
                         mark_isolate_dialects_as_isolates = TRUE, 
                         set_isolates_Family_ID_as_themselves = TRUE)


LanguageTable_with_Family_name <- rgrambank::add_family_name_column(LanguageTable = LanguageTable_Isolates_enriched, 
                                                         Glottolog_ValueTable_LanguageTable = Glottolog_ValueTable_LanguageTable, 
                               verbose = FALSE)

