#' Wrapper function for densify::densify and densify::prune tailored to Grambank data specifically, based on annagraff/crossling-curated/blob/main/scripts/GBI/densify-datasets.R
#'
#' @param Grambank_ValueTable data-frame of ValueTable from Grambank as CLDF-dataset
#' @param GBI output object from rgrambank::make_GBI()
#' @param Glottolog_ValueTable data-frame of ValueTable from Glottolog as CLDF-dataset
#' @param verbose logical. If TRUE, function will be more talkative
#' @param min_variability parameter for densify::densify(). Defaults to 3.
#' @param density_mean  parameter for densify::densify(). Defaults to log_odds
#' @param density_mean_weights parameter for densify::densify() (defaults to list(coding = 0.999, taxonomy = 1))
#' @param scoring_function character vector, either "n_data_points*coding_density*row_coding_density_min*taxonomic_index^3" or "n_data_points * coding_density". Other scoring_functions are currently not supported by wrapper function due to evaluation issues.
#' @param limits list which defines lower bounds to aim for when pruning. Defaults to list(min_coding_density = 1, min_prop_rows = NA, min_prop_cols = NA).
#' @param random_seed  Integer
#' @note This is a Wrapper function for densify::densify and densify::prune tailored to Grambank data specifically, based on annagrawf/crossling-curated/blob/main/scripts/GBI/densify-datasets.R. The function requires the package densify, which can be installed like this: remotes::install_github("annagraff/densify"). The authors of the original densify package are: Anna Graff, Marc, Lischka, Taras Zakharko, Reinhard Furrer and Balthasar Bickel.
#'@references Graff, A., Chousou-Polydouri, N., Inman, D., Skirgård, H., Lischka, M., Zakharko, T., Barbieri, C., and Bickel, B., (2025). Curating global datasets of structural linguistic features for independence. Scientific Data 12:106 https://doi.org/10.1038/s41597-024-04319-4
#'@references Graff, A., Lischka, M., Zakharko, T., Furrer, R., & Bickel, B. (2024). densify: An R package to reduce empty cells in data frames of typological linguistic data. Journal of Open Source Software, 9(101), 7024.
#' @author Original densify-functions: Anna Graff, Marc Lischka, Taras Zakharko, Reinhard Furrer and Balthasar Bickel. Wrapper function: Anna Graff and Hedvig Skirgård
#' @export
densify_GB <- function(Grambank_ValueTable = NA,
                       GBI = NA, 
                       Glottolog_ValueTable = NA, 
                       verbose = T,
                       min_variability = 3,  # each variable must have at least 3 languages in its second-largest state
                       density_mean = "log_odds",
                       density_mean_weights = list(coding = 0.999, taxonomy = 1),
                       random_seed = 1111,
                       limits = list(min_coding_density = 1, min_prop_rows = NA, min_prop_cols = NA),
                       scoring_function = "n_data_points*coding_density*row_coding_density_min*taxonomic_index^3"
){
  
  #reality checks
  if(isTRUE(all(is.na(GBI))) && isTRUE(all(is.na(Grambank_ValueTable)))) {
    stop("Either Grambank_ValueTable or GBI must be specified.")
  }
  if(!isTRUE(all(is.na(GBI))) && !isTRUE(all(is.na(Grambank_ValueTable)))) {
    stop("Specify only one of Grambank_ValueTable or GBI, not both.")
  }
  
  #setting up aux functions
  
  # NA conversions (? to NA, "NA" to NA, blank to NA)
  .na_convert <- function(data, question_mark_to_na = TRUE){
    if(question_mark_to_na == TRUE){
      data[data=="?"]<-NA}
    data[data=="NA"]<-NA
    data[data==""]<-NA
    data[is.na(data)]<-NA
    return(data)
  }

  # function to summarize matrices
  .summarize_matrix <- function(matrix){
    
    matrix <- .na_convert(matrix)
    nfam <- taxonomy_matrix  |> 
      dplyr::filter(.data[["id"]] %in% matrix$Language_ID) |> 
      dplyr::distinct(dplyr::across(dplyr::all_of(c("level1")))) |> nrow()
  
    bare_matrix <- matrix |> dplyr::select(-"Language_ID")
    nlg <- nrow(bare_matrix)
    nvar <- ncol(bare_matrix)
    data_prop <- sum(!is.na(bare_matrix))/(nlg*nvar)
    data_prop <- paste0(round(data_prop*100, digits = 2), "%")
    return(list(nlg=nlg, nvar=nvar, nfam=nfam, data_prop=data_prop) |> as.matrix() |> t())
  }
  
  set.seed(random_seed)
  
  #make taxonomy matrix out of Glottolog ValueTable in the way that densify expects.
  glottolog_tree_adj_table_without_isolates <- Glottolog_ValueTable |> 
    dplyr::select("Language_ID", "Parameter_ID", "Value") |> 
    dplyr::filter(.data[["Parameter_ID"]] == "classification") |> 
    dplyr::mutate(parent_id = stringr::str_replace(.data[["Value"]], pattern = "^.*\\/", replacement = "")) |> 
    dplyr::select("Language_ID", "parent_id")
  
  #isolates don't have a classification field at all, so we'll need to infer which are isolates by finding the ones without an entry in glottolog_tree_adj_table now and add them back in
  glottolog_tree_adj_table <- Glottolog_ValueTable |> 
    dplyr::distinct(dplyr::across(dplyr::all_of(c("Language_ID")))) |>
    dplyr::anti_join(glottolog_tree_adj_table_without_isolates, by = "Language_ID") |>
    dplyr::mutate(parent_id = as.character(NA)) |> 
    dplyr::full_join(glottolog_tree_adj_table_without_isolates, by = c("Language_ID", "parent_id")) |> 
    dplyr::select("id" = "Language_ID", "parent_id") 
  
  taxonomy_matrix  <- densify::as_flat_taxonomy_matrix(x = glottolog_tree_adj_table)
  
  ### GBI
  if(!all(is.na(GBI))){
    
    # read in logical GBI data
    logical <- GBI$logicalGBI   
    
    # read in statistical GBI data
    statistical <- GBI$statisticalGBI   
    
    # for densification, ensure all blanks, ? and "NA" are coded as NA
    logical_for_pruning <- .na_convert(logical)
    statistical_for_pruning <- .na_convert(statistical) 
  
    # comment on weights: GB is quite dense, and part of the "NA"s on the column/variable side in both curations is explicitly wanted
    # densification should thus be biased towards the taxonomic diversity criterion, expressed in a higher weight
    
    # run densify, set seed for reproducibility
    
    if(verbose == T){
      cat("\n Densifying GBI logical.\n")}
    
    
    logical_log <-
      densify::densify(data = logical_for_pruning,
                       min_variability = min_variability,
                       density_mean = density_mean,
                       cols = colnames(logical_for_pruning)[!colnames(logical_for_pruning) %in% "Language_ID"],
                       taxonomy = glottolog_tree_adj_table,
                       taxon_id = "Language_ID",
                       limits= limits,
                       density_mean_weights = density_mean_weights)
    
    if(verbose == T){
      cat("\n Densifying GBI statistical.\n")}
  
    statistical_log <-
      densify::densify(data = statistical_for_pruning,
                       min_variability = min_variability,
                       density_mean = density_mean,
                       cols = colnames(statistical_for_pruning)[!colnames(statistical_for_pruning) %in% "Language_ID"],
                       taxonomy = glottolog_tree_adj_table,
                       taxon_id = "Language_ID",
                       limits= limits,
                       density_mean_weights = density_mean_weights)
    
    # prune to optima
    # we include minimum row coding density, since NAs on language end should largely be random
    # we include taxonomic index since densification here explicitly seeks to increase taxonomic diversity
      
      logical_densified <- densify::prune(logical_log, 
                                          scoring_function = !!rlang::parse_expr(scoring_function))
      
      statistical_densified <- densify::prune(statistical_log, 
                                              scoring_function =  !!rlang::parse_expr(scoring_function))
    
    
    # retrieve corresponding data from input (to re-establish differences between ? and NA)
    logical_densified_with_question_mark_and_NA <- logical |> 
      dplyr::filter(.data[["Language_ID"]] %in% logical_densified$Language_ID) |> 
      dplyr::select("Language_ID", dplyr::all_of(colnames(logical_densified)))
    
    logical_densified <- logical_densified_with_question_mark_and_NA |> 
      .na_convert()
    
    statistical_densified_with_question_mark_and_NA <- statistical |> 
      dplyr::filter(.data[["Language_ID"]] %in% statistical_densified$Language_ID) |> 
      dplyr::select("Language_ID", dplyr::all_of(colnames(statistical_densified)))

    statistical_densified <- statistical_densified_with_question_mark_and_NA |> 
      .na_convert()
    
  if(verbose == T){
    
    cat(paste0("Finished.\n
  Before densifying, GBI_logical had ",   .summarize_matrix(logical_for_pruning)[[4]], " data coverage (counting ? as missing). After densifying, it has ",   .summarize_matrix(logical_densified)[[4]], " data coverage. ", 
               format( .summarize_matrix(logical_for_pruning)[[1]] - .summarize_matrix(logical_densified)[[1]], big.mark=",") ,
               " languages and ", 
               .summarize_matrix(logical_for_pruning)[[2]] - .summarize_matrix(logical_densified)[[2]], " GBI_logical features were dropped.\n"))
    
    cat(paste0("Before densifying, GBI_statistical had ",   .summarize_matrix(statistical_for_pruning)[[4]], " data coverage (counting ? as missing). After densifying, it has ",   .summarize_matrix(statistical_densified)[[4]], " data coverage. ", 
               format(    .summarize_matrix(statistical_for_pruning)[[1]] - .summarize_matrix(statistical_densified)[[1]], big.mark=","),
               " languages and ", 
               .summarize_matrix(statistical_for_pruning)[[2]] - .summarize_matrix(statistical_densified)[[2]], " GBI_statistical features were dropped.\n"))
    
    output <- list(logical_densified_with_question_mark_and_NA = logical_densified_with_question_mark_and_NA, 
                   logical_densified = logical_densified, 
                   statistical_densified_with_question_mark_and_NA = statistical_densified_with_question_mark_and_NA,
                   statistical_densified = statistical_densified)
  
  }
}

  ############IF USING "REGULAR" GB, not GBI
  if(any(!is.na(Grambank_ValueTable))){
    
    Grambank_wide <- Grambank_ValueTable |> 
      dplyr::mutate(Value = as.character(.data[["Value"]])) |> 
      tidyr::pivot_wider(
        id_cols = "Language_ID",
        names_from ="Parameter_ID",
        values_from = "Value"
      )
    
    Grambank_ValueTable_for_pruning <- .na_convert(Grambank_wide, question_mark_to_na = T)
    
    if(verbose == T){
      cat("\n Densifying Grambank_ValueTable.\n")}
    Grambank_ValueTable_log <-
      densify::densify(data = Grambank_ValueTable_for_pruning,
                       min_variability = min_variability,
                       density_mean = density_mean,
                       cols = colnames(Grambank_ValueTable_for_pruning)[!colnames(Grambank_ValueTable_for_pruning) %in% "Language_ID"],
                       taxonomy = glottolog_tree_adj_table,
                       taxon_id = "Language_ID",
                       density_mean_weights = density_mean_weights)
    
    
    Grambank_densified <- densify::prune(Grambank_ValueTable_log, 
                                           scoring_function =  !!rlang::parse_expr(scoring_function))
      
  
    Grambank_densified_with_question_mark <- Grambank_wide |> 
      dplyr::filter(.data[["Language_ID"]] %in% Grambank_densified$Language_ID) |> 
      dplyr::select("Language_ID", dplyr::all_of(colnames(Grambank_densified)))
  
    Grambank_densified <- Grambank_densified_with_question_mark |> 
      .na_convert()
  
  if(verbose == T){
    
    cat(paste0("Finished.\n
  Before densifying, Grambank had ",   .summarize_matrix(Grambank_ValueTable_for_pruning)[[4]], " data coverage (counting ? as missing). After densifying, it has ",   .summarize_matrix(Grambank_densified)[[4]], " data coverage. ", 
               format( .summarize_matrix(Grambank_ValueTable_for_pruning)[[1]] - .summarize_matrix(Grambank_densified)[[1]], big.mark=",") ,
               " languages and ", 
               .summarize_matrix(Grambank_ValueTable_for_pruning)[[2]] - .summarize_matrix(Grambank_densified)[[2]], " GBI_logical features were dropped.\n"))
    
    
    }
  output <- list(Grambank_densified_with_question_mark = Grambank_densified_with_question_mark, 
                 Grambank_densified = Grambank_densified)
  }

    return(output)  
}

