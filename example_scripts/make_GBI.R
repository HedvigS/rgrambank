#remotes::install_github("Hedvigs/rgrambank", ref = "4203614472682683a7670c60d994d9ec7de2c1b1")
library(rgrambank)
library(tidyverse)
library(testthat)
library(data.table)
library(reshape2)
#remotes::install_github("annagraff/densify")
library(densify)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

ValueTable <- GB_rcldf_obj$tables$ValueTable

# the function rgrambank::make_GBI makes GBI logical and GBI statistical according to the script written by Anna Graff. 
output <- rgrambank::make_GBI(ValueTable = ValueTable)

###denisfy

# NA conversions (? to NA, "NA" to NA, blank to NA)
na_convert <- function(data, question_mark_to_na = TRUE){
  if(question_mark_to_na == TRUE){
  data[data=="?"]<-NA}
  data[data=="NA"]<-NA
  data[data==""]<-NA
  data[is.na(data)]<-NA
  return(data)
}

# read in logical GBI data
logical <- output$logicalGBI   %>% as.data.frame()

# read in statistical GBI data
statistical <- output$statisticalGBI    %>% as.data.frame()

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable 

glottolog_tree_adj_table_without_isolates <- glottolog_ValueTable %>% 
  dplyr::select(Language_ID, Parameter_ID, Value) %>% 
  dplyr::filter(Parameter_ID == "classification") %>% 
  mutate(parent_id = str_replace(Value, pattern = "^.*\\/", replacement = "")) %>% 
  dplyr::select(Language_ID, parent_id)

#isolates don't have a classification field at all, so we'll need to inferr which are isolates by finding the ones without an entry in glottolog_tree_adj_table now and add them back in
glottolog_tree_adj_table <- glottolog_ValueTable %>% 
  dplyr::distinct(`Language_ID`) %>%
  anti_join(glottolog_tree_adj_table_without_isolates, by = "Language_ID") %>%
  mutate(parent_id = as.character(NA)) %>% 
  full_join(glottolog_tree_adj_table_without_isolates, by = c("Language_ID", "parent_id")) %>% 
  dplyr::select(id = Language_ID, parent_id) 

taxonomy_matrix  <- densify::as_flat_taxonomy_matrix(x = glottolog_tree_adj_table)

# for densification, ensure all blanks, ? and "NA" are coded as NA
logical_for_pruning <- na_convert(logical)
statistical_for_pruning <- na_convert(statistical) 

# function to summarize matrices

# function to summarize matrices
summarize_matrix <- function(matrix){
  
#  matrix = logical_densified
  matrix <- na_convert(matrix)
  nfam <- taxonomy_matrix  %>% 
    dplyr::filter(id %in% matrix$Language_ID) %>% 
    distinct(level1) %>% nrow()
  
  bare_matrix <- matrix %>% select(-Language_ID)
  nlg <- nrow(bare_matrix)
  nvar <- ncol(bare_matrix)
  prop <- sum(!is.na(bare_matrix))/(nlg*nvar)
  return(c(nlg=nlg, nvar=nvar, nfam=nfam, prop=prop))
}

# describe full matrices
summarize_matrix(logical_for_pruning)
summarize_matrix(statistical_for_pruning)

#checked up to here
##########################
##########################
##########################

# specify parameters for densification 
min_variability <- 3 # each variable must have at least 3 languages in its second-largest state
density_mean <- "log_odds"

# comment on weights: GB is quite dense, and part of the "NA"s on the column/variable side in both curations is explicitly wanted
# densification should thus be biased towards the taxonomic diversity criterion, expressed in a higher weight

# run densify, set seed for reproducibility
set.seed(1111)

logical_log <-
  densify::densify(data = logical_for_pruning,
          min_variability = min_variability,
          density_mean = density_mean,
#          cols = colnames(logical_for_pruning)[!colnames(logical_for_pruning) %in% "Language_ID"],
          taxonomy = glottolog_tree_adj_table,
          taxon_id = "Language_ID",
          density_mean_weights = list(coding = 0.999, taxonomy = 1))

statistical_log <-
  densify::densify(data = statistical_for_pruning,
          min_variability = min_variability,
          density_mean = density_mean,
          cols = colnames(statistical_for_pruning)[!colnames(statistical_for_pruning) %in% "Language_ID"],
          taxonomy = glottolog_tree_adj_table,
          taxon_id = "Language_ID",
          density_mean_weights = list(coding = 0.999, taxonomy = 1))

# prune to optima
# we include minimum row coding density, since NAs on language end should largely be random
# we include taxonomic index since densification here explicitly seeks to increase taxonomic diversity
logical_densified <- prune(logical_log, 
                           scoring_function = n_data_points*coding_density*row_coding_density_min*taxonomic_index^3)

statistical_densified <- prune(statistical_log, 
                               scoring_function = n_data_points*coding_density*row_coding_density_min*taxonomic_index^3)

# retrieve corresponding data from input (to re-establish differences between ? and NA)
logical_densified_with_question_mark <- logical %>% 
  dplyr::filter(Language_ID %in% logical_densified$Language_ID) %>% 
  dplyr::select(Language_ID, all_of(colnames(logical_densified)))

statistical_densified_with_question_mark <- statistical %>% 
  dplyr::filter(Language_ID %in% statistical_densified$Language_ID) %>% 
  dplyr::select(Language_ID, all_of(colnames(statistical_densified)))


#Amelia::missmap(na_convert(logical, question_mark_to_na = TRUE))
#Amelia::missmap(na_convert(logical_densified,  question_mark_to_na = TRUE))


# save densified matrices
#write.csv(logical_densified,"curated_data/GBI/logicalGBI/logicalGBI_densified.csv")
#write.csv(statistical_densified,"curated_data/GBI/statisticalGBI/statisticalGBI_densified.csv")

# describe densified matrices and their relation to the full ones
#logical_densified <- na_convert(logical_densified)
#summarize_matrix(logical_densified)
#summarize_matrix(logical_densified)/summarize_matrix(logical_for_pruning)