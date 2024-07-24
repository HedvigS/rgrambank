# This script makes a worldmap with dots for languages coloured by the data-sets first three principal components. To accomplish this, the data-set needs to be made binary, dialects merged, missing data cropped, missing data imputed, PCA, match to RGB and finally plotting.


#install.packages("remotes", version = "2.4.2.1", repos = "http://cran.us.r-project.org")
library(remotes)

#remotes::install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#remotes::install_version("missForest", version = "1.5", repos = "http://cran.us.r-project.org")
library(missForest)

#remotes::install_version("reshape2", version = "1.4.4", repos = "http://cran.us.r-project.org")
library(reshape2)

library(grDevices) #version = "4.3.1"

#remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

source("../functions/make_binary_ValueTable.R")
source("../functions/reduce_ValueTable_to_unique_glottocodes.R")
source("../functions/crop_missing_data.R")
source("../functions/match_to_rgb.R")
source("../functions/basemap_pacific_center.R")

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

ValueTable <- GB_rcldf_obj$tables$ValueTable
LanguageTable <- GB_rcldf_obj$tables$LanguageTable

#make Grambank ValueTable binary
ValueTable_binary <- make_binary_ValueTable(ValueTable = ValueTable, keep_multistate = F, keep_raw_binary = T)

#remove duplicate glottocodes and merge dialects
ValueTable_dialect_reduced <- reduce_ValueTable_to_unique_glottocodes(ValueTable = ValueTable_binary,
                                                                      LanguageTable = LanguageTable,
                                                                      merge_dialects = T, 
                                                                      method = "combine_random",
                                                                      replace_missing_language_level_ID = T, 
                                                                      treat_question_mark_as_missing = T)

#prep for imputation
#crop such that features with lots of missing data and languages are removed
ValueTable_cropped <- crop_missing_data(ValueTable = ValueTable_dialect_reduced, 
                                        cut_off_parameters  = 0.75, 
                                        cut_off_languages = 0.75,
                                        turn_question_mark_into_NA = T)

#make wide for imputation
ValueTable_wide <- reshape2::dcast(data = ValueTable_cropped, 
                                   formula = Glottocode ~ Parameter_ID, 
                                   value.var = "Value")

#imputation
imputed_data <- ValueTable_wide %>%
  column_to_rownames("Glottocode") %>% 
  as.matrix() %>%
  data.frame() %>%
  mutate_all(as.factor) %>% 
  missForest::missForest() 

cat(paste0("The imputation OOB error is ", round(imputed_data$OOBerror, 2), ".\n"))

# do Pricinpal Components Analysis on imputed dataset
GB_PCA <- imputed_data$ximp %>% 
  mutate_all(as.numeric) %>% 
  as.matrix() %>% 
  stats::prcomp(scale = T) 

###Map first 3 PCA components to RGB
RGB_vec <- GB_PCA$x %>% 
  match_to_RGB(first_three = T)

# there are records in the data now that don't have long/lat details in the LanguageTable, because they were dialects which were emrged. Therefore, we need long/lat data from glottolog

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

#prep data for basemap_pacific_center function

LongLatTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(ID = Glottocode, Longitude, Latitude)

DataTable <-   data.frame(ID = rownames(GB_PCA$x), 
                          RGB = RGB_vec) 

# the function basemap_pacific_center outputs a list of two objects, the basemap itself and a combination of the LongLatTable and DataTable with Longitude appropraitely adjusted to match.
basemap_list  <- basemap_pacific_center(LongLatTable = LongLatTable, DataTable = DataTable) 

#specifically to plot RGB we can't use mapping = aes() because we want to refer to the values themselves, not have ggplot then map them to colors on its own. Therefore we need to pass it the RGB vector outside of aes().
map <- basemap_list$basemap +
  geom_jitter(mapping = aes(x = Longitude, y = Latitude), color =  basemap_list$MapTable$RGB, size = 2)

ggsave(plot = map, filename = "output/plots/PCA_RGB_map.png", width = 20, height = 15)