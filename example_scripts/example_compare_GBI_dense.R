#remotes::install_github("Hedvigs/rgrambank")
library(rgrambank)
library(tidyverse)
library(testthat)
library(data.table)
library(reshape2)
library(SH.misc)
library(missForest)
library(ggarrange)
#install.packages("patchwork")
library(patchwork)
#remotes::install_github("annagraff/densify")
library(densify)
library(beepr)

set.seed(1421)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

Grambank_ValueTable <-  rgrambank::reduce_ValueTable_to_unique_glottocodes(ValueTable = GB_rcldf_obj$tables$ValueTable,
                                                     LanguageTable = GB_rcldf_obj$tables$LanguageTable,
                                                     merge_dialects = T, 
                                                     method = "singular_least_missing_data",
                                                     replace_missing_language_level_ID = T, 
                                                     treat_question_mark_as_missing = T) %>% 
  dplyr::select(-Language_ID) %>% 
  dplyr::rename(Language_ID = Glottocode) 


Grambank_ValueTable_binary <- rgrambank::make_binary_ValueTable(ValueTable = Grambank_ValueTable, 
                                                                keep_multistate = F, keep_raw_binary = T) %>% 
  filter(Value != "?") %>% 
  filter(Value != "NA") %>% 
  filter(!is.na(Value))

GBI <- make_GBI(ValueTable = Grambank_ValueTable)

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

Glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable 

###densify

#checking that it runs for Grambank_ValueTable
GB_dense <- rgrambank::densify_GB(Grambank_ValueTable = Grambank_ValueTable_binary, Glottolog_ValueTable = Glottolog_ValueTable, limits = list(min_prop_rows = 0.85, min_prop_cols = 0.85))

beep()

#checking that it runs for GBI
GBI_dense <- densify_GB(GBI = GBI, Glottolog_ValueTable = Glottolog_ValueTable)

GB_statistical_multistate_non_numeric_feats <- c("GB995F", "GB332EON", "GB900EO")

#SH.misc::basemap_EEZ()

#rgrambank::match_to_rgb()

GB_dense_long <- GB_dense$Grambank_densified_with_question_mark %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  filter(Value != "?") %>% 
  filter(Value != "NA") %>% 
  filter(!is.na(Value))

GBI_logical <- GBI$logicalGBI %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  filter(Value != "?") %>% 
  filter(Value != "NA") %>% 
  filter(!is.na(Value))

GBI_logical_dense <- GBI_dense$logical_densified_with_question_mark %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  filter(Value != "?") %>% 
  filter(Value != "NA") %>% 
  filter(!is.na(Value))

GBI_statistical <- GBI$statisticalGBI %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  dplyr::filter(!(Parameter_ID %in% GB_statistical_multistate_non_numeric_feats)) %>% 
  mutate(Value = ifelse(Parameter_ID == "GB800EO" & Value == "bound", "1", Value)) %>% 
  mutate(Value = ifelse(Parameter_ID == "GB800EO" & Value == "non-bound", "0", Value)) %>% 
  filter(Value != "?") %>% 
  filter(Value != "NA") %>% 
  filter(!is.na(Value))

GBI_statistical_dense <- GBI_dense$statistical_densified_with_question_mark %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(Language_ID, Parameter_ID = variable, Value = value) %>% 
  dplyr::filter(!(Parameter_ID %in% GB_statistical_multistate_non_numeric_feats)) %>% 
  mutate(Value = ifelse(Parameter_ID == "GB800EO" & Value == "bound", "1", Value)) %>% 
  mutate(Value = ifelse(Parameter_ID == "GB800EO" & Value == "non-bound", "0", Value)) %>% 
  filter(Value != "?") %>% 
  filter(Value != "NA") %>% 
  filter(!is.na(Value))


# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

LongLatTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(ID = Glottocode, Longitude, Latitude)

#prep data for rgrambank::basemap_pacific_center function



plot_PCA <- function(plot_title = "", ValueTable, 
                     LongLatTable = LongLatTable, 
                     crop = TRUE){
  
#  ValueTable <- GBI_statistical
  
ValueTable <- ValueTable %>% 
  filter(Value != "?") %>% 
    filter(Value != "NA") %>% 
    filter(!is.na(Value))
  
if(crop == T){
  ValueTable <- rgrambank::crop_missing_data(ValueTable = ValueTable, 
                                                     cut_off_parameters  = 0.7538462, 
                                                     cut_off_languages = 0.7538462,
                                                     turn_question_mark_into_NA = T) 
    
}

  #crop such that features with lots of missing data and languages are removed
ValueTable_prepped <- ValueTable %>% 
    mutate(Value = as.character(Value)) %>%
    dplyr::select(Language_ID, Parameter_ID, Value) %>%  
    dcast(Language_ID ~ Parameter_ID, value.var = "Value") 

percent_missing <-   paste0(  
round(100 * (  
  sum(is.na(ValueTable_prepped[,2:ncol(ValueTable_prepped)])) /   
    (  sum(is.na(ValueTable_prepped[,2:ncol(ValueTable_prepped)])) +   sum(!is.na(ValueTable_prepped[,2:ncol(ValueTable_prepped)])) ) 
  ),digits = 2), "%")
    
nlgs <- ValueTable_prepped %>% nrow()
nfeats <- ncol(ValueTable_prepped) -1

plot_title <- paste0(plot_title, ".\n nlgs = ", nlgs, ", nfeats = ", nfeats, ",\n imputed missing data = ", percent_missing)

  #imputation
  imputed_data <- ValueTable_prepped %>%
    column_to_rownames("Language_ID") %>% 
    as.matrix() %>%
    data.frame() %>%
    mutate_all(as.factor) %>% 
    missForest::missForest() 
  
  cat(paste0("The imputation OOB error is ", round(imputed_data$OOBerror, 2), ".\n"))
  
  # do Pricinpal Components Analysis on imputed dataset
  df_for_PCA <- imputed_data$ximp %>% 
    as.data.frame() %>%
    mutate_all(as.character) %>% 
    rownames_to_column("Language_ID") %>% 
    reshape2::melt(id.vars = "Language_ID")  %>% 
    mutate(value = as.numeric(value)) %>% 
    reshape2::dcast(Language_ID ~ variable, value.var = "value") %>% 
    column_to_rownames("Language_ID") %>% 
    as.matrix()
      
PCA <- df_for_PCA %>% 
     stats::prcomp(scale = T) 
  
  ###Map first 3 PCA components to RGB
  RGB_vec <- PCA$x %>% 
    as.data.frame() %>% 
    dplyr::select(PC1, PC2, PC3) %>% 
    rgrambank::match_to_rgb(first_three = T)
  
  DataTable <-   data.frame(ID = rownames(PCA$x), 
                            RGB = RGB_vec) 
  
  # the function rgrambank::basemap_pacific_center outputs a list of two objects, the basemap itself and a combination of the LongLatTable and DataTable with Longitude appropraitely adjusted to match.
  basemap_list  <- rgrambank::basemap_pacific_center(LongLatTable = LongLatTable, DataTable = DataTable) 
  
  #specifically to plot RGB we can't use mapping = aes() because we want to refer to the values themselves, not have ggplot then map them to colors on its own. Therefore we need to pass it the RGB vector outside of aes().
  map <- basemap_list$basemap +
    geom_jitter(mapping = aes(x = Longitude, y = Latitude), color =  basemap_list$MapTable$RGB, size = 1) +
  ggtitle(plot_title)
map  
}

datasets <- c(Grambank_ValueTable_binary, GB_dense_long, GBI_logical, GBI_logical_dense, GBI_statistical , GBI_statistical_dense)



beep()

GB_map <- plot_PCA(plot_title = "Grambank v1 (cropped)", ValueTable = Grambank_ValueTable_binary, LongLatTable = LongLatTable, crop = T)

GB_dense_map <- plot_PCA(plot_title = "Grambank v1 (dense)", ValueTable = GB_dense_long, LongLatTable = LongLatTable, crop = F)

GB_logical_map <- plot_PCA(plot_title = "GBI - logical (cropped)", ValueTable = GBI_logical, LongLatTable = LongLatTable, crop = T)

GB_logical_dense_map <- plot_PCA(plot_title = "GBI - logical (dense)" , ValueTable = GBI_logical_dense, LongLatTable = LongLatTable, crop = F)

GB_statistical_map <- plot_PCA(plot_title = "GBI - statistical (cropped)", ValueTable = GBI_statistical, LongLatTable = LongLatTable, crop = T)

GB_statitical_dense_map <- plot_PCA(plot_title = "GBI - statistical (dense)", ValueTable = GBI_statistical_dense, LongLatTable = LongLatTable, crop = F)

library(beepr)
beep()

(GB_map + GB_logical_map + GB_statistical_map) / (GB_dense_map  + GB_logical_dense_map  + GB_statitical_dense_map)

ggsave("test.png", width = 35, height = 30, units = "cm")


