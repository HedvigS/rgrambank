#remotes::install_github("Hedvigs/rgrambank",   ref = "ipac")
library(rgrambank)
library(tidyverse)
library(reshape2)
library(missForest)
library(patchwork)
library(testthat)
library(cluster)
#remotes::install_github("annagraff/densify") 
library(densify)
library(beepr)

set.seed(1111)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

Grambank_ValueTable <-  rgrambank::reduce_ValueTable_to_unique_glottocodes(
  ValueTable = GB_rcldf_obj$tables$ValueTable,
  LanguageTable = GB_rcldf_obj$tables$LanguageTable,
  merge_dialects = F, 
  method = "singular_least_missing_data",
  replace_missing_language_level_ID = T) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::rename(Language_ID = Glottocode) 

#densify
recode_patterns <- read.csv("https://raw.githubusercontent.com/annagraff/crossling-curated/0e8695e176044f268b7d8c1ac012061b7bf1b343/scripts/GBI/feature-recode-patterns.csv")
all_decisions <- read.csv("https://raw.githubusercontent.com/annagraff/crossling-curated/0e8695e176044f268b7d8c1ac012061b7bf1b343/scripts/GBI/decisions-log.csv")


# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

###densify
#checking that it runs for Grambank_ValueTable
GB_dense <- rgrambank::densify_GB(
  Grambank_ValueTable = Grambank_ValueTable, 
  Glottolog_ValueTable =  glottolog_rcldf_obj$tables$ValueTable,
  limits = list(min_prop_rows = 0.85, min_prop_cols = 0.85)
)

#checking that it runs for binary
Grambank_ValueTable_binary <- rgrambank::make_binary_ValueTable(ValueTable = Grambank_ValueTable, 
                                                                keep_multistate = F, keep_native_binary = T) 

GB_dense_binary <- rgrambank::densify_GB(
  Grambank_ValueTable = Grambank_ValueTable_binary, 
  Glottolog_ValueTable   = glottolog_rcldf_obj$tables$ValueTable,
  limits = list(min_prop_rows = 0.85, min_prop_cols = 0.85)
)

#checking that it runs for GBI
GBI <- rgrambank::make_GBI(ValueTable = Grambank_ValueTable, recode_patterns_full = recode_patterns, all_decisions = all_decisions)

GBI_dense <- rgrambank::densify_GB(GBI = GBI, Glottolog_ValueTable = glottolog_rcldf_obj$tables$ValueTable,
                        limits = list(min_coding_density = 1, min_prop_rows = NA, min_prop_cols = NA))


beepr::beep(3)

#making each long 
#GB_statistical_multistate_non_numeric_feats <- c("GB995F", "GB332EON", "GB900EO")
#GB800EO

GB_dense_long <- GB_dense$Grambank_densified   %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  filter(!is.na(Value))

GB_dense_long_binary <- GB_dense_binary$Grambank_densified   %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  filter(!is.na(Value))

GBI_logical <- GBI$values_logicalGBI %>%
  dplyr::select(Language_ID, Parameter_ID = new.name, Value = value) %>% 
  dplyr::mutate(Value = ifelse(Value == "?", NA, Value)) 

GBI_logical_dense <- GBI_dense$logical_densified_with_question_mark_and_NA %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  dplyr::mutate(Value = ifelse(Value == "?", NA, Value)) 

GBI_statistical <- GBI$values_statisticalGBI %>% 
  dplyr::select(Language_ID, Parameter_ID = new.name, Value = value) %>% 
  dplyr::mutate(Value = ifelse(Value == "?", NA, Value)) 

GBI_statistical_dense <- GBI_dense$statistical_densified_with_question_mark_and_NA %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value)%>% 
  dplyr::mutate(Value = ifelse(Value == "?", NA, Value)) 

LongLatTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(ID = Glottocode, Longitude, Latitude)

#prep data for rgrambank::basemap_pacific_center function

plot_MDS <- function(plot_title = "", ValueTable, ParameterTable,
                     LongLatTable = LongLatTable, 
                     crop = TRUE){
  
#  ValueTable <- GBI_statistical_dense
  
if(crop == T){
  ValueTable <- rgrambank::crop_missing_data(ValueTable = ValueTable, ParameterTable = ParameterTable,
                                                     cut_off_parameters  = 0.7538462, 
                                                     cut_off_languages = 0.7538462) 
}

  #crop such that features with lots of missing data and languages are removed
ValueTable_prepped <- ValueTable %>% 
    mutate(Value = as.character(Value)) %>%
    dplyr::select(Language_ID, Parameter_ID, Value) %>%  
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") 

percent_missing <-   paste0(  
round(100 * (  
  sum(is.na(ValueTable_prepped[,2:ncol(ValueTable_prepped)])) /   
    (  sum(is.na(ValueTable_prepped[,2:ncol(ValueTable_prepped)])) +   sum(!is.na(ValueTable_prepped[,2:ncol(ValueTable_prepped)])) ) 
  ),digits = 2), "%")
    
nlgs <- ValueTable_prepped %>% nrow()
nfeats <- ncol(ValueTable_prepped) -1


  #imputation
  imputed_data <- ValueTable_prepped %>%
    column_to_rownames("Language_ID") %>% 
    as.matrix() %>%
    data.frame() %>%
    mutate_all(as.factor) %>% 
    missForest::missForest() 
  
  cat(paste0("The imputation OOB error is ", round(imputed_data$OOBerror, 2), ".\n"))
  
imputed_df <-     imputed_data$ximp %>% 
  mutate( across(where(is.factor), ~ factor(na_if(as.character(.x), "NA"))), across(where(is.character), ~ na_if(.x, "NA")) )

Not_applicable <- imputed_df %>%  is.na() %>% sum()

All <- ncol(imputed_df) * nrow(imputed_df)

plot_title <- paste0(plot_title, ".\n nlgs = ", nlgs, ", nfeats = ", nfeats, ",\n imputed missing data = ", percent_missing,",\n not applicable left = ",round((Not_applicable / All) * 100, digits = 0))

dists <- cluster::daisy(x = imputed_df, metric = "gower")

mds <- cmdscale(dists , k = 3)

    
  ###Map first 3 PCA components to RGB
  RGB_vec <- mds %>% 
    as.data.frame() %>% 
    dplyr::select(V1, V2, V3) %>% 
    rgrambank::match_to_rgb(first_three = T)
  
  DataTable <-   data.frame(ID = rownames(mds), 
                            RGB = RGB_vec) 
  
  # the function rgrambank::basemap_pacific_center outputs a list of two objects, the basemap itself and a combination of the LongLatTable and DataTable with Longitude appropraitely adjusted to match.
  basemap_list  <- rgrambank::basemap_pacific_center(LongLatTable = LongLatTable, DataTable = DataTable) 
  
  #specifically to plot RGB we can't use mapping = aes() because we want to refer to the values themselves, not have ggplot then map them to colors on its own. Therefore we need to pass it the RGB vector outside of aes().
  map <- basemap_list$basemap +
    geom_jitter(mapping = aes(x = Longitude, y = Latitude), color =  basemap_list$MapTable$RGB, size = 1) +
  ggtitle(plot_title)
map  
}

datasets <- c(Grambank_ValueTable, GB_dense_long, GBI_logical, GBI_logical_dense, GBI_statistical , GBI_statistical_dense)

GB_map <- plot_MDS(plot_title = "Grambank v1 (cropped)", ValueTable = Grambank_ValueTable_binary, LongLatTable = LongLatTable, crop = T, ParameterTable = GB_rcldf_obj$tables$ParameterTable)

GB_dense_map <- plot_MDS(plot_title = "Grambank v1 (dense)", ValueTable = GB_dense_long, LongLatTable = LongLatTable, crop = F, ParameterTable = GB_rcldf_obj$tables$ParameterTable)

GB_logical_map <- plot_MDS(plot_title = "GBI - logical (cropped)", ValueTable = GBI_logical, LongLatTable = LongLatTable, crop = T, ParameterTable = GBI$parameters_logicalGBI)

GB_logical_dense_map <- plot_MDS(plot_title = "GBI - logical (dense)" , ValueTable = GBI_logical_dense, LongLatTable = LongLatTable, crop = F, ParameterTable = GBI$parameters_logicalGBI)

GB_statistical_map <- plot_MDS(plot_title = "GBI - statistical (cropped)", ValueTable = GBI_statistical, LongLatTable = LongLatTable, crop = T, ParameterTable = GBI$parameters_statisticalGBI)

GB_statitical_dense_map <- plot_MDS(plot_title = "GBI - statistical (dense)", ValueTable = GBI_statistical_dense, LongLatTable = LongLatTable, crop = F, ParameterTable = GBI$parameters_statisticalGBI)

library(beepr)
beep()

p <- (GB_map + GB_logical_map + GB_statistical_map) / (GB_dense_map  + GB_logical_dense_map  + GB_statitical_dense_map)

ggsave(plot = p, "output/GB_GBI_compare_maps.png", width = 35, height = 30, units = "cm")


