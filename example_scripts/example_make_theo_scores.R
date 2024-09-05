
Grambank_version <- 1

#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#devtools::install_github("HedvigS/rgrambank", ref = "v1.0")
library(rgrambank)

if(Grambank_version == 2){
# This part of the script is written solely for people with access to Grambank v2, which is not public at the time of writing (2024-07-25).
# Word_Order was missing from Grambank 2.0, this is being fixed:
#https://github.com/glottobank/Grambank/issues/2782
#in the meantime, grambank v2 will need to be complemented with the ParameterTable from v1.

GB_rcldf_obj_v2 <- rcldf::cldf("../../../../Grambank 2.0/cldf/StructureDataset-metadata.json", load_bib = F)
# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)
LanguageTable <- GB_rcldf_obj_v2$tables$LanguageTable
ValueTable <- GB_rcldf_obj_v2$tables$ValueTable
ParameterTable <- GB_rcldf_obj_v1$tables$ParameterTable
}


if(Grambank_version == 1){
  # fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
  GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)
  LanguageTable <- GB_rcldf_obj_v1$tables$LanguageTable
  ValueTable <- GB_rcldf_obj_v1$tables$ValueTable
  ParameterTable <- GB_rcldf_obj_v1$tables$ParameterTable
  }

ValueTable_binary <- rgrambank::make_binary_ValueTable(ValueTable = ValueTable, keep_multistate = FALSE, keep_raw_binary = TRUE)
ParameterTable_binary <- rgrambank::make_binary_ParameterTable(ParameterTable = ParameterTable,
                                                    keep_multi_state_features = FALSE,
                                                    keep_raw_binary = TRUE)

ValueTable_binary_reduced <- rgrambank::reduce_ValueTable_to_unique_glottocodes(ValueTable = ValueTable_binary, LanguageTable = LanguageTable, merge_dialects = TRUE, method = "combine_random") %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::rename(Language_ID = Glottocode)
                                                                      
theo_scores_table <- rgrambank::make_theo_scores(ValueTable = ValueTable_binary_reduced, ParameterTable = ParameterTable_binary)
