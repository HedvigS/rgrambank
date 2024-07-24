#install.packages("remotes", version = "2.4.2.1", repos = "http://cran.us.r-project.org")
library(remotes)

#remotes::install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

LanguageTable <- GB_rcldf_obj_v1$tables$LanguageTable %>% 
  dplyr::rename(Family_ID = Family_level_ID) %>% 
  dplyr::select(-Family_name)

Glottolog_LanguageTable <- glottolog_rcldf_obj$tables$LanguageTable
Glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable

source("../functions/combine_Glottolog_ValueTable_LanguageTable.R")
Glottolog_ValueTable_LanguageTable <- combine_Glottolog_ValueTable_LanguageTable(Glottolog_LanguageTable = Glottolog_LanguageTable, Glottolog_ValueTable = Glottolog_ValueTable)

source("../functions/add_isolate_info.R")

LanguageTable_Isolates_enriched <- add_isolate_info(LanguageTable = LanguageTable, 
                         Glottolog_ValueTable_LanguageTable = Glottolog_ValueTable_LanguageTable,
                         mark_isolate_dialects_as_isolates = TRUE, 
                         set_isolates_Family_ID_as_themselves = TRUE)


#showcasing adding Family_name
source("../functions/add_family_name_column.R")

LanguageTable_with_Family_name <- add_family_name_column(LanguageTable = LanguageTable_Isolates_enriched, 
                                                         Glottolog_ValueTable_LanguageTable = Glottolog_ValueTable_LanguageTable, 
                               verbose = FALSE)

