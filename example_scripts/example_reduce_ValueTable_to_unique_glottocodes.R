#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

source("../functions/reduce_ValueTable_to_unique_glottocodes.R")

ValueTable_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                              LanguageTable =  GB_rcldf_obj$tables$LanguageTable, 
                                                              method = "singular_least_missing_data", 
                                                              merge_dialects = T)

ValueTable_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                              LanguageTable =  GB_rcldf_obj$tables$LanguageTable, 
                                                              method = "singular_random",
                                                              merge_dialects = T)

ValueTable_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                              LanguageTable =  GB_rcldf_obj$tables$LanguageTable, 
                                                              method = "combine_random",
                                                              merge_dialects = T)

ValueTable_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                              LanguageTable =  GB_rcldf_obj$tables$LanguageTable, 
                                                              method = "singular_least_missing_data", 
                                                              merge_dialects = F)

ValueTable_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                              LanguageTable =  GB_rcldf_obj$tables$LanguageTable, 
                                                              method = "singular_random",
                                                              merge_dialects = F)

ValueTable_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                              LanguageTable =  GB_rcldf_obj$tables$LanguageTable, 
                                                              method = "combine_random",
                                                              merge_dialects = F)
