#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#devtools::install_github("HedvigS/rgrambank")
library(rgrambank)

  # fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
  GB_rcldf_obj_v1 <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)
  LanguageTable <- GB_rcldf_obj_v1$tables$LanguageTable
  ValueTable <- GB_rcldf_obj_v1$tables$ValueTable
  ParameterTable <- GB_rcldf_obj_v1$tables$ParameterTable

ValueTable_binary <- rgrambank::make_binary_ValueTable(ValueTable = ValueTable, keep_multistate = FALSE, keep_native_binary = TRUE)
ParameterTable_binary <- rgrambank::make_binary_ParameterTable(ParameterTable = ParameterTable,
                                                    keep_multi_state_features = FALSE,
                                                    keep_native_binary = TRUE)

ValueTable_binary_reduced <- rgrambank::reduce_ValueTable_to_unique_glottocodes(ValueTable = ValueTable_binary, LanguageTable = LanguageTable, merge_dialects = TRUE, method = "combine_random") |> 
  dplyr::select(-Language_ID) |> 
  dplyr::rename(Language_ID = Glottocode)
                                                                      
theo_scores_table <- rgrambank::make_theo_scores(ValueTable = ValueTable_binary_reduced, ParameterTable = ParameterTable_binary, Fusion_option = "count_one_and_half")

theo_scores_table_excl_half <- rgrambank::make_theo_scores(ValueTable = ValueTable_binary_reduced, ParameterTable = ParameterTable_binary, Fusion_option = "count_one_only")

theo_scores_table <- theo_scores_table |> 
  dplyr::select(Language_ID, `Fusion (one & half)` = Fusion)

theo_scores_table_excl_half <- theo_scores_table_excl_half  |> 
  dplyr::select(Language_ID, `Fusion (one only)` = Fusion)

theo_scores_table |> 
  dplyr::full_join(theo_scores_table_excl_half, by = "Language_ID") |>
  ggplot(mapping = aes(x =`Fusion (one only)`, y = `Fusion (one & half)`)) +
  geom_point( color = "#e3b839") +
  theme_classic() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x') 
  
  
if(!dir.exists("output")){dir.create("output")}
ggsave("output/Fusion_compare_options.png", width = 3, height = 3,  units = "in")
