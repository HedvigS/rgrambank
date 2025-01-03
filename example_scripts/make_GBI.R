

#remotes::install_github("Hedvigs/rgrambank")
#library(rgrambank)
library(tidyverse)
library(testthat)
library(data.table)
library(reshape2)
library(densify)

#ValueTable <- read.delim("../../../../grambank-v2.0rc2 2/cldf/values.csv", sep = ",") 
ValueTable <- read.delim("../../../grambank/grambank/cldf/values.csv", sep = ",") 

load("../R/sysdata.rda")
source("../R/make_GBI.R")

output <- make_GBI(ValueTable = ValueTable)


old <- read_csv("../../../annagraff/crossling-curated/curated_data/GBI/logicalGBI/logicalGBI.csv", show_col_types = F) %>% 
  dplyr::select(-"...1") %>% 
  reshape2::melt(id.vars = "glottocode") %>% 
  dplyr::select(glottocode, Value.old = value, variable)

new <- output$logicalGBI  %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  dplyr::select(glottocode = Language_ID, Value.new = value, variable)

joined <- full_join(old, new) %>% 
  mutate(diff = ifelse(Value.new == Value.old, "same", "diff")) 


###denisfy

# NA conversions (? to NA, "NA" to NA, blank to NA)
na_convert <- function(data){
  data[data=="?"]<-NA
  data[data=="NA"]<-NA
  data[data=="N/A"]<-NA
  data[data==""]<-NA
  data[is.na(data)]<-NA
  return(data)
}

# read in logical GBI data
logical <- output$logicalGBI 

# read in statistical GBI data
statistical <- output$statisticalGBI  

languages_in_datasets <- c(logical$Language_ID, statistical$Language_ID) %>% unique()

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable %>% 
  dplyr::filter(Language_ID %in% languages_in_datasets)

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
summarize_matrix <- function(matrix, flat_taxonomy_matrix){
  nfam <- flat_taxonomy_matrix %>% 
  dplyr::filter(id %in% matrix$Language_ID) %>% 
    distinct(level1) %>% nrow()

  bare_matrix <- matrix %>% 
    dplyr::select(-Language_ID)
  
  nlg <- nrow(bare_matrix)
  nvar <- ncol(bare_matrix)
  prop <- sum(!is.na(bare_matrix))/(nlg*nvar)
  prop <- round(prop, digits = 3)
  return(c(nlg=nlg, nvar=nvar, nfam=nfam, prop=prop))
}


# describe full matrices
summarize_matrix(logical_for_pruning, flat_taxonomy_matrix = taxonomy_matrix)
summarize_matrix(statistical_for_pruning, flat_taxonomy_matrix = taxonomy_matrix)


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
          cols = colnames(logical_for_pruning)[!colnames(logical_for_pruning) %in% "Language_ID"],
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
logical_densified <- logical[which(logical$glottocode%in%logical_densified$glottocode), which(names(logical)%in%names(logical_densified))]
statistical_densified <- statistical[which(statistical$glottocode%in%statistical_densified$glottocode), which(names(statistical)%in%names(statistical_densified))]

# save densified matrices
write.csv(logical_densified,"curated_data/GBI/logicalGBI/logicalGBI_densified.csv")
write.csv(statistical_densified,"curated_data/GBI/statisticalGBI/statisticalGBI_densified.csv")

# describe densified matrices and their relation to the full ones
logical_densified <- na_convert(logical_densified)
summarize_matrix(logical_densified)
summarize_matrix(logical_densified)/summarize_matrix(logical_for_pruning)

statistical_densified <- na_convert(statistical_densified)
summarize_matrix(statistical_densified)
summarize_matrix(statistical_densified)/summarize_matrix(statistical_for_pruning)

