#' Wrapper function for densify::densify and densify::prune tailored to Grambank data specifically, based on annagraff/crossling-curated/blob/main/scripts/GBI/densify-datasets.R
#'
#' @param 
#' @importFrom Amelia missmap
#' @import densify
#' @import dplyr
#' @author Original densify-functions: Anna Graff, Marc Lischka, Taras Zakharko, Reinhard Furrer and Balthasar Bickel. Wrapper function: Anna Graff and Hedvig Skirgård

densify_GB <- function(Grambank_ValueTable = NA,
                       GBI = NA, 
                       Glottolog_ValueTable = NA, 
                       verbose = T,
                       min_variability = 3,  # each variable must have at least 3 languages in its second-largest state
                       density_mean = "log_odds",
                       density_mean_weights = list(coding = 0.999),
                       random_seed = 1111,
                       scoring_function = "n_data_points*coding_density*row_coding_density_min*taxonomic_index^3"
){
  
  if(is.na(GBI) & is.na(Grambank_ValueTable)){
    stop("Either Grambank_ValueTable or GBI have to be specified, neither.")}
  
  if(!is.na(GBI) & !is.na(Grambank_ValueTable)){
    stop("Either Grambank_ValueTable or GBI have to be specified, not both")}
  
  # NA conversions (? to NA, "NA" to NA, blank to NA)
  .na_convert <- function(data, question_mark_to_na = TRUE){
    if(question_mark_to_na == TRUE){
      data[data=="?"]<-NA}
    data[data=="NA"]<-NA
    data[data==""]<-NA
    data[is.na(data)]<-NA
    return(data)
  }
  
  set.seed(random_seed)
  
  # function to summarize matrices
  .summarize_matrix <- function(matrix){
    
    #  matrix = logical_densified
    matrix <- .na_convert(matrix)
    nfam <- taxonomy_matrix  %>% 
      dplyr::filter(id %in% matrix$Language_ID) %>% 
      distinct(level1) %>% nrow()
    
    bare_matrix <- matrix %>% select(-Language_ID)
    nlg <- nrow(bare_matrix)
    nvar <- ncol(bare_matrix)
    data_prop <- sum(!is.na(bare_matrix))/(nlg*nvar)
    data_prop <- paste0(round(data_prop*100, digits = 2), "%")
    return(list(nlg=nlg, nvar=nvar, nfam=nfam, data_prop=data_prop) %>% as.matrix() %>% t())
  }
  
  
  if(!is.na(GBI)){
    
    #
    
    # read in logical GBI data
    logical <- GBI$logicalGBI   
    
    # read in statistical GBI data
    statistical <- GBI$statisticalGBI   
    
    #make taxonomy matrix out of Glottolog ValueTable in the way that densify expects.
    glottolog_tree_adj_table_without_isolates <- Glottolog_ValueTable %>% 
      dplyr::select(Language_ID, Parameter_ID, Value) %>% 
      dplyr::filter(Parameter_ID == "classification") %>% 
      dplyr::mutate(parent_id = str_replace(Value, pattern = "^.*\\/", replacement = "")) %>% 
      dplyr::select(Language_ID, parent_id)
    
    #isolates don't have a classification field at all, so we'll need to inferr which are isolates by finding the ones without an entry in glottolog_tree_adj_table now and add them back in
    glottolog_tree_adj_table <- Glottolog_ValueTable %>% 
      dplyr::distinct(`Language_ID`) %>%
      anti_join(glottolog_tree_adj_table_without_isolates, by = "Language_ID") %>%
      mutate(parent_id = as.character(NA)) %>% 
      full_join(glottolog_tree_adj_table_without_isolates, by = c("Language_ID", "parent_id")) %>% 
      dplyr::select(id = Language_ID, parent_id) 
    
    taxonomy_matrix  <- densify::as_flat_taxonomy_matrix(x = glottolog_tree_adj_table)
    
    # for densification, ensure all blanks, ? and "NA" are coded as NA
    logical_for_pruning <- .na_convert(logical)
    statistical_for_pruning <- .na_convert(statistical) 
    
    
    if(verbose == T){
      
      cat(paste0("Before pruning, GBI_logical looked like this:\n"))
      # describe full matrices
      .summarize_matrix(logical_for_pruning) 
      cat(paste0("Before pruning, GBI_statistical looked like this:\n"))
      .summarize_matrix(statistical_for_pruning)
    }
    
    # comment on weights: GB is quite dense, and part of the "NA"s on the column/variable side in both curations is explicitly wanted
    # densification should thus be biased towards the taxonomic diversity criterion, expressed in a higher weight
    
    # run densify, set seed for reproducibility
    
    logical_log <-
      densify::densify(data = logical_for_pruning,
                       min_variability = min_variability,
                       density_mean = density_mean,
                       cols = colnames(logical_for_pruning)[!colnames(logical_for_pruning) %in% "Language_ID"],
                       taxonomy = glottolog_tree_adj_table,
                       taxon_id = "Language_ID",
                       density_mean_weights = density_mean_weights)
    
    statistical_log <-
      densify::densify(data = statistical_for_pruning,
                       min_variability = min_variability,
                       density_mean = density_mean,
                       cols = colnames(statistical_for_pruning)[!colnames(statistical_for_pruning) %in% "Language_ID"],
                       taxonomy = glottolog_tree_adj_table,
                       taxon_id = "Language_ID",
                       density_mean_weights = density_mean_weights)
    
    # prune to optima
    # we include minimum row coding density, since NAs on language end should largely be random
    # we include taxonomic index since densification here explicitly seeks to increase taxonomic diversity
    
    if(scoring_function == "n_data_points*coding_density*row_coding_density_min*taxonomic_index^3"){
      logical_densified <- densify::prune(logical_log, 
                                          scoring_function = n_data_points*coding_density*row_coding_density_min*taxonomic_index^3)
      
      statistical_densified <- densify::prune(statistical_log, 
                                              scoring_function = n_data_points*coding_density*row_coding_density_min*taxonomic_index^3)
      
    }
    
    if(scoring_function == "n_data_points * coding_density"){
      logical_densified <- densify::prune(logical_log, 
                                          scoring_function = n_data_points * coding_density)
      
      statistical_densified <- densify::prune(statistical_log, 
                                              scoring_function = n_data_points * coding_density)
    }
    
    # retrieve corresponding data from input (to re-establish differences between ? and NA)
    logical_densified_with_question_mark <- logical %>% 
      dplyr::filter(Language_ID %in% logical_densified$Language_ID) %>% 
      dplyr::select(Language_ID, all_of(colnames(logical_densified)))
    
    statistical_densified_with_question_mark <- statistical %>% 
      dplyr::filter(Language_ID %in% statistical_densified$Language_ID) %>% 
      dplyr::select(Language_ID, all_of(colnames(statistical_densified)))
  }
  
  if(verbose == T){
    
    cat(paste0("Finished.\n
  Before densifying, GBI_logical had ",   .summarize_matrix(logical_for_pruning)[[4]], " data coverage (counting ? as missing). After densifying, it has ",   .summarize_matrix(logical_densified)[[4]], " data coverage. ", 
               format( .summarize_matrix(logical_for_pruning)[[1]] - .summarize_matrix(logical_densified)[[1]], big.mark=",") ,
               " languages and ", 
               .summarize_matrix(logical_for_pruning)[[2]] - .summarize_matrix(logical_densified)[[2]], " GBI_logical features were dropped. See plotting window for comparion plots.\n"))
    
    
    Amelia::missmap(.na_convert(logical_for_pruning, question_mark_to_na = TRUE), main = "Data coverage of \nGBI_logical after densifying")
    Amelia::missmap(.na_convert(logical_densified, question_mark_to_na = TRUE), main = "Data coverage of \nGBI_logical after densifying")
    
    
    cat(paste0("Before densifying, GBI_statistical had ",   .summarize_matrix(statistical_for_pruning)[[4]], " data coverage (counting ? as missing). After densifying, it has ",   .summarize_matrix(statistical_densified)[[4]], " data coverage. ", 
               format(    .summarize_matrix(statistical_for_pruning)[[1]] - .summarize_matrix(statistical_densified)[[1]], big.mark=","),
               " languages and ", 
               .summarize_matrix(statistical_for_pruning)[[2]] - .summarize_matrix(statistical_densified)[[2]], " GBI_statistical features were dropped. See plotting window for comparion plots.\n"))
    
    
    Amelia::missmap(.na_convert(statistical_for_pruning, question_mark_to_na = TRUE), main = "Data coverage of \nGBI_statistical after densifying")
    Amelia::missmap(.na_convert(statistical_densified, question_mark_to_na = TRUE), main = "Data coverage of \nGBI_statistical after densifying")
    
    output <- list(statistical_densified_with_question_mark = statistical_densified_with_question_mark,
                   logical_densified_with_question_mark = logical_densified_with_question_mark)
    
    
  }
  return(output)  
}

