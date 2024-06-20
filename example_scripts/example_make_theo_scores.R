
#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#GB_rcldf_obj <- rcldf::cldf("../../../../Grambank 2.0/grambank/cldf/StructureDataset-metadata.json", load_bib = F)
# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

LanguageTable <- GB_rcldf_obj$tables$LanguageTable
ValueTable <- GB_rcldf_obj$tables$ValueTable
ParameterTable <- GB_rcldf_obj$tables$ParameterTable

source("../functions/make_binary_ParameterTable.R")
source("../functions/make_binary_ValueTable.R")
source("../functions/reduce_ValueTable_to_unique_glottocodes.R")
source("../functions/make_theo_scores.R")

ValueTable_binary <- make_binary_ValueTable(ValueTable = ValueTable, keep_multistate = FALSE, keep_raw_binary = TRUE)
ParameterTable_binary <- make_binary_ParameterTable(ParameterTable = ParameterTable, keep_multi_state_features = FALSE)

ValueTable_binary_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = ValueTable_binary, LanguageTable = LanguageTable, merge_dialects = TRUE, method = "combine_random") %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::rename(Language_ID = Glottocode)
                                                                      
theo_scores_table <- make_theo_scores(ValueTable = ValueTable_binary_reduced, ParameterTable = ParameterTable_binary)
