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

#devtools::install_github("HedvigS/rgrambank", ref = "v1.0")
library(rgrambank)

if(!dir.exists("ouptut")){dir.create("output")}
if(!dir.exists("output/plots")){dir.create("output/plots")}

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

ValueTable <- GB_rcldf_obj$tables$ValueTable
LanguageTable <- GB_rcldf_obj$tables$LanguageTable

#remove duplicate glottocodes and merge dialects
ValueTable_dialect_reduced <- rgrambank::reduce_ValueTable_to_unique_glottocodes(ValueTable = ValueTable,
                                                                      LanguageTable = LanguageTable,
                                                                      merge_dialects = T, 
                                                                      method = "singular_least_missing_data",
                                                                      replace_missing_language_level_ID = T, 
                                                                      treat_question_mark_as_missing = T) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::rename(Language_ID = Glottocode)

#make Grambank ValueTable binary
ValueTable_binary <- rgrambank::make_binary_ValueTable(ValueTable = ValueTable_dialect_reduced, 
                                                       keep_multistate = F, keep_raw_binary = T)

#prep for imputation

source("../R/crop_missing_data.R")
#crop such that features with lots of missing data and languages are removed
ValueTable_cropped <- crop_missing_data(ValueTable = ValueTable_binary, 
                                        cut_off_parameters  = 0.75, 
                                        cut_off_languages = 0.75,
                                        turn_question_mark_into_NA = T) %>% 
  mutate(Value = str_replace_all(Value, "0", "0 - absent")) %>%
  mutate(Value = str_replace_all(Value, "1", "1 - present")) %>% 
  dplyr::select(Language_ID, Parameter_ID, Value) %>%
  spread(key = Parameter_ID, value = Value, drop = FALSE) 
  
set.seed(1421)


#imputation
imputed_data <- ValueTable_cropped %>%
  column_to_rownames("Language_ID") %>% 
  as.matrix() %>%
  data.frame() %>%
  mutate_all(as.factor) %>% 
  missForest::missForest() 

cat(paste0("The imputation OOB error is ", round(imputed_data$OOBerror, 2), ".\n"))


# do Pricinpal Components Analysis on imputed dataset
GB_PCA <- imputed_data$ximp %>% 
  as.data.frame() %>% 
  rownames_to_column("Language_ID") %>% 
  reshape2::melt(id = "Language_ID")  %>% 
  mutate_all(as.character) %>% 
  mutate(value =  str_replace_all(value, "0 - absent", "0")) %>%
  mutate(value =  str_replace_all(value,  "1 - present", "1")) %>%
  mutate(value = as.numeric(value)) %>%  
  dplyr::select(Language_ID, Parameter_ID = variable, value) %>%
  spread(key = Parameter_ID, value = value, drop = FALSE) %>% 
  column_to_rownames("Language_ID") %>% 
  as.matrix() %>% 
  stats::prcomp(scale = T) 

###Map first 3 PCA components to RGB
RGB_vec <- GB_PCA$x %>% 
  as.data.frame() %>% 
  dplyr::select(PC1, PC2, PC3) %>% 
  rgrambank::match_to_rgb(first_three = T)

# there are records in the data now that don't have long/lat details in the LanguageTable, because they were dialects which were emrged. Therefore, we need long/lat data from glottolog

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

#prep data for rgrambank::basemap_pacific_center function

LongLatTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(ID = Glottocode, Longitude, Latitude)

DataTable <-   data.frame(ID = rownames(GB_PCA$x), 
                          RGB = RGB_vec) 

# the function rgrambank::basemap_pacific_center outputs a list of two objects, the basemap itself and a combination of the LongLatTable and DataTable with Longitude appropraitely adjusted to match.
basemap_list  <- rgrambank::basemap_pacific_center(LongLatTable = LongLatTable, DataTable = DataTable) 

#specifically to plot RGB we can't use mapping = aes() because we want to refer to the values themselves, not have ggplot then map them to colors on its own. Therefore we need to pass it the RGB vector outside of aes().
map <- basemap_list$basemap +
  geom_jitter(mapping = aes(x = Longitude, y = Latitude), color =  basemap_list$MapTable$RGB, size = 2)

ggsave(plot = map, filename = "output/plots/PCA_RGB_map.png", width = 10, height = 10)

library(SH.misc)

SH.misc::basemap_EEZ(south = "down", colour_border_land = "white", colour_border_eez = "lightgray", padding = 0) +
  geom_jitter(data = basemap_list$MapTable, mapping = aes(x = Longitude, y = Latitude), color =  basemap_list$MapTable$RGB, size = 2)

ggsave(filename = "output/plots/PCA_RGB_map_eez.png", width = 10, height = 10)
