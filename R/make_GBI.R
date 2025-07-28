#' make_GBI makes the  GBI-logical dataset as per Graff et al (accepted)
#'
#'@note This function is based on Graff, A., Chousou-Polydouri1, N., Inman, D., Skirgård, H., Lischka, M., Zakharko1, T., Barbieri1, C., and Bickel, B., (Accepted). Curating global datasets of structural linguistic features for independence.Scientific Data . Original code can be found here: https://github.com/annagraff/crossling-curated/tree/main/scripts. The function rgrambank::make_GBI has been modified by Hedvig Skirgård to adapt to the rgrambank package and take into account changes between Grambank v1 and v2. The modifications are: turn binarised features (in Grambank v2 and further versions) corresponding multistate, rename variables to avoid loading recode-patterns several times, remove language meta-data, remove lazy loading of variables for dplyr::filter + dplyr::mutate and replace data.table::setDT with base::as.data.frame.
#' @param ValueTable data-frame. Grambank ValueTable.
#' @references Graff, A., Chousou-Polydouri, N., Inman, D., Skirgård, H., Lischka, M., Zakharko, T., Barbieri, C., and Bickel, B., (2025). Curating global datasets of structural linguistic features for independence. Scientific Data 12:106 https://doi.org/10.1038/s41597-024-04319-4
#' @author Original GBI code: Anna Graff, Natalia Chousou-Polydouri, David Inman, Hedvig Skirgård, Marc Lischka, Taras Zakharko, Chiara Barbieri & Balthasar Bickel. Wrapper function: Anna Graff and Hedvig Skirgård.
#' @export
make_GBI <- function(ValueTable = NULL,
                     verbose = TRUE,
                     recode_patterns_full = NULL, 
                     all_decisions = NULL
                            # LanguageTable = NULL
    ){
  
#  ValueTable <- read.delim("../../../../grambank-v2.0rc2 2/cldf/values.csv", sep = ",") 
#  LanguageTable <- read.delim("../../../../grambank-v2.0rc2 2/cldf/languages.csv", sep = ",") 

  
  ########## load and prepare data ########## 
  # read in original grambank data
  original_feature_matrix <- ValueTable %>% 
    tidyr::pivot_wider(
      id_cols = Language_ID,
      names_from = Parameter_ID,
      values_from = Value
    )
  # replace missing data by ? (--> because these data points are unknown, not "not applicable")
  original_feature_matrix[is.na(original_feature_matrix)] <- "?"
    
  # Grambank v2 contains binarised features of the old multistate features from GB v1 (read more here: https://github.com/grambank/grambank/wiki/Binarised-features). The crossling-curated workflow currently calls for the mulistate features only, which is why the binarised will be turned "back" into the multistate.
  
  if("GB024a" %in% colnames(original_feature_matrix)){

  
  #GB024	What is the order of numeral and noun in the NP?
  #GB024a	Is the order of the numeral and noun Num-N?
  #GB024b	Is the order of the numeral and noun N-Num?
 
    original_feature_matrix$GB024 <- ifelse(original_feature_matrix$GB024 == "?" &   
                                            original_feature_matrix$GB024a == "1" & 
                                            original_feature_matrix$GB024b == "0|?", 
                                              "1",
                                          original_feature_matrix$GB024)

    original_feature_matrix$GB024 <- ifelse(original_feature_matrix$GB024 == "?" &   
                                            original_feature_matrix$GB024a == "0|?"  &
                                            original_feature_matrix$GB024b == "1" , 
                                            "2",
                                          original_feature_matrix$GB024)
    
    original_feature_matrix$GB024 <- ifelse(original_feature_matrix$GB024 == "?" &   
                                              original_feature_matrix$GB024a == "1" &
                                              original_feature_matrix$GB024b == "1" , "3",
                                            original_feature_matrix$GB024)
    
    #GB025	What is the order of adnominal demonstrative and noun?
    #GB025a	Is the order of the adnominal demonstrative and noun Dem-N?
    #GB025b	Is the order of the adnominal demonstrative and noun N-Dem?
    
    original_feature_matrix$GB025 <- ifelse(original_feature_matrix$GB025 == "?" &   
                                              original_feature_matrix$GB025a == "1" &
                                              original_feature_matrix$GB025b == "0|?", "1",
                                            original_feature_matrix$GB025)
    
    original_feature_matrix$GB025 <- ifelse(original_feature_matrix$GB025 == "?" &   
                                              original_feature_matrix$GB025a == "0|?" &
                                              original_feature_matrix$GB025b == "1" , "2",
                                            original_feature_matrix$GB025)
    
    original_feature_matrix$GB025 <- ifelse(original_feature_matrix$GB025 == "?" &   
                                              original_feature_matrix$GB025a == "1" &
                                              original_feature_matrix$GB025b == "1" , "3",
                                            original_feature_matrix$GB025)
    
    #GB065	What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?
    #GB065a	Is the pragmatically unmarked order of adnominal possessor noun and possessed noun PSR-PSD?
    #GB065b	Is the pragmatically unmarked order of adnominal possessor noun and possessed noun PSD-PSR?
    
    
    original_feature_matrix$GB065 <- ifelse(original_feature_matrix$GB065 == "?" &   
                                              original_feature_matrix$GB065a == "1" &
                                              original_feature_matrix$GB065b == "0|?" , "1",
                                            original_feature_matrix$GB065)
    
    original_feature_matrix$GB065 <- ifelse(original_feature_matrix$GB065 == "?" &   
                                              original_feature_matrix$GB065a == "0|?" &
                                              original_feature_matrix$GB065b == "1" , "2",
                                            original_feature_matrix$GB065)
    
    original_feature_matrix$GB065 <- ifelse(original_feature_matrix$GB065 == "?" &   
                                              original_feature_matrix$GB065a == "1" &
                                              original_feature_matrix$GB065b == "1" , "3",
                                            original_feature_matrix$GB065)
    
    #GB130	What is the pragmatically unmarked order of S and V in intransitive clauses?
    #GB130a	Is the pragmatically unmarked order of S and V in intransitive clauses S-V?
    #GB130b	Is the pragmatically unmarked order of S and V in intransitive clauses V-S?
    

    original_feature_matrix$GB130 <- ifelse(original_feature_matrix$GB130 == "?" &   
                                              original_feature_matrix$GB130a == "1" &
                                              original_feature_matrix$GB130b == "0|?", "1",
                                            original_feature_matrix$GB130)
    
    original_feature_matrix$GB130 <- ifelse(original_feature_matrix$GB130 == "?" &   
                                              original_feature_matrix$GB130a == "0|?" &
                                              original_feature_matrix$GB130b == "1" , "2",
                                            original_feature_matrix$GB130)
    
    original_feature_matrix$GB130 <- ifelse(original_feature_matrix$GB130 == "?" &   
                                              original_feature_matrix$GB130a == "1" &
                                              original_feature_matrix$GB130b == "1" , "3",
                                            original_feature_matrix$GB130)

    #GB193	What is the order of adnominal property word and noun?
    #GB193a	Is the order of the adnominal property word (ANM) and noun ANM-N?
    #GB193b	Is the order of the adnominal property word (ANM) and noun N-ANM?
    
    
    original_feature_matrix$GB193 <- ifelse(original_feature_matrix$GB193 == "?" &   
                                              original_feature_matrix$GB193a == "0" &
                                              original_feature_matrix$GB193b == "0" , "0",
                                            original_feature_matrix$GB193)
    
    original_feature_matrix$GB193 <- ifelse(original_feature_matrix$GB193 == "?" &   
                                              original_feature_matrix$GB193a == "1" &
                                              original_feature_matrix$GB193b == "0|?" , "1",
                                            original_feature_matrix$GB193)
    
    original_feature_matrix$GB193 <- ifelse(original_feature_matrix$GB193 == "?" &   
                                              original_feature_matrix$GB193a == "0|?" &
                                              original_feature_matrix$GB193b == "1" , "2",
                                            original_feature_matrix$GB193)
    
    original_feature_matrix$GB193 <- ifelse(original_feature_matrix$GB193 == "?" &   
                                              original_feature_matrix$GB193a == "1" &
                                              original_feature_matrix$GB193b == "1" , "3",
                                            original_feature_matrix$GB193)
    
    
    #GB203	What is the order of the adnominal collective universal quantifier ('all') and the noun?
    #GB203a	Is the order of the adnominal collective universal quantifier (UQ) and noun UQ-N?
    #GB203b	Is the order of the adnominal collective universal quantifier (UQ) and noun N-QU?
    
    
    
    original_feature_matrix$GB203 <- ifelse(original_feature_matrix$GB203 == "?" &   
                                              original_feature_matrix$GB203a == "0" &
                                              original_feature_matrix$GB203b == "0" , "0",
                                            original_feature_matrix$GB203)
    
    original_feature_matrix$GB203 <- ifelse(original_feature_matrix$GB203 == "?" &   
                                              original_feature_matrix$GB203a == "1" &
                                              original_feature_matrix$GB203b == "0|?" , "1",
                                            original_feature_matrix$GB203)
    
    original_feature_matrix$GB203 <- ifelse(original_feature_matrix$GB203 == "?" &   
                                              original_feature_matrix$GB203a == "0|?" &
                                              original_feature_matrix$GB203b == "1" , "2",
                                            original_feature_matrix$GB203)
    
    original_feature_matrix$GB203 <- ifelse(original_feature_matrix$GB203 == "?" &   
                                              original_feature_matrix$GB203a == "1" &
                                              original_feature_matrix$GB203b == "1" , "3",
                                            original_feature_matrix$GB203)
  }
  
  
  ########## parse all recodings in the appropriate order ########## 
  ## include without modification ##
  # extract the features that we don't need to recode because we recode.operation.type them as they are
  retained <- dplyr::filter(recode_patterns_full, .data[["recode.operation.type"]]=="include without modification")
  retained_data <- dplyr::select(original_feature_matrix,c("Language_ID", 
                                                           dplyr::filter(recode_patterns_full, 
                                                                         .data[["recode.operation.type"]]=="include without modification")$`original.names`))
  
  # change names to new.names
  for(i in 2:ncol(retained_data)){
    names(retained_data)[i]<-dplyr::filter(retained,
                                           .data[["original.names"]]==names(retained_data)[i])$new.name
  }
  
  recode_patterns <- dplyr::filter(recode_patterns_full, 
                                   .data[["recode.operation.type"]] != "include without modification")

  ## recode group 1 (simple recode) ##
  # subset to features requiring simple recoding only
  first_set <- dplyr::filter(  recode_patterns, 
                               .data[["recode.operation.type"]] == "recode group 1 (simple recode)")
  
  recode_patterns <- dplyr::filter(recode_patterns, 
                                   .data[["recode.operation.type"]] != "recode group 1 (simple recode)")
  
  # recode all features that require simple recoding
  first_set_rec <- dplyr::rowwise(first_set) %>% dplyr::do({
  
         if(verbose == TRUE){ cat("First set, processing ", .$new.name, "\n", sep="")
         }
    
    # check that the original feature is present in the original feature matrix
    testthat::expect_true(.$`original.names` %in% names(original_feature_matrix)[-1])
    original_data <- stats::na.omit(as.character(original_feature_matrix[[.$`original.names`]]))
    
    # extract relevant attributes for recoding
    expected_states <- unlist(strsplit(unlist(.$original.states), ";"))
    recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
    recoded_states <- unlist(strsplit(.$new.states, ";"))
    
    # recode (single feature)
    new_data <- .implement_recode(original_data, expected_states, recoding_groups, recoded_states, 
                                 nvar="single", recode_mode="simple")
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = stats::na.omit(original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% .$`original.names`))])$Language_ID, 
      value = new_data,  
      stringsAsFactors=FALSE)  
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])

  
  
  # save the recoded data from retained features and the simple recodings as recoded_data for further use
  recoded_data <- dplyr::full_join(first_set_rec, retained_data, by=c(Language_ID="Language_ID"))
  
  ## recode group 2 (merge features - recode via logical arguments) ##
  # subset to features that have to be merged via logical arguments
  second_set <- dplyr::filter(recode_patterns, .data[["recode.operation.type"]]=="recode group 2 (merge features - recode via logical arguments)")
  recode_patterns <- dplyr::filter(recode_patterns, .data[["recode.operation.type"]]!="recode group 2 (merge features - recode via logical arguments)")
  
  # merge and recode features via logical arguments
  second_set_rec <- dplyr::rowwise(second_set) %>% dplyr::do({
    if(verbose == TRUE){cat("Second set, processing ", .$new.name, "\n", sep="")}
    
    # check that the original features are present in the original feature matrix
    original_data <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% unlist(strsplit(.$`original.names`,"&"))))]
    
    if (ncol(original_data) != length(unlist(strsplit(.$`original.names`,"&")))+1){
      original_data<-cbind(original_data,recoded_data[,which(names(recoded_data) %in% unlist(strsplit(.$`original.names`,"&")))])
    }
    
    # extract relevant attributes for recoding
    expected_states <- unlist(strsplit(unlist(.$recode.operation.Rcode), ";"))
    recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
    recoded_states <- unlist(strsplit(.$new.states, ";"))
    
    # recode (multiple features)
    new_data <- .implement_recode(original_data, expected_states, recoding_groups, recoded_states, 
                                 nvar="multiple", recode_mode="logical_arguments")
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = original_data$Language_ID, 
      value = new_data,  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(second_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  ## recode group 3 (merge features - recode via logical arguments - simple conditioning) ##
  # subset to features that have to be recoded via logical arguments if a condition applies
  third_set <- dplyr::filter(recode_patterns, 
                             .data[["recode.operation.type"]]=="recode group 3 (merge features - recode via logical arguments - simple conditioning)")
  recode_patterns <- dplyr::filter(recode_patterns, .data[["recode.operation.type"]]!="recode group 3 (merge features - recode via logical arguments - simple conditioning)")
  
  # merge and recode via logical arguments if a condition applies
  third_set_rec <- dplyr::rowwise(third_set) %>% dplyr::do({
    #. <- third_set[1,]
    if(verbose == TRUE){cat("Third set, processing ", .$new.name, "\n", sep="")}
    
    # check that the original features are present in the original feature matrix
    original_data <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% unlist(strsplit(.$`original.names`,"&"))))]
    
    if (ncol(original_data) != length(unlist(strsplit(.$`original.names`,"&")))+1){
      original_data<-cbind(original_data,recoded_data[,which(names(recoded_data) %in% unlist(strsplit(.$`original.names`,"&")))])
    }
    
    # extract relevant attributes for recoding
    expected_states <- unlist(strsplit(unlist(.$recode.operation.Rcode), ";"))
    recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
    recoded_states <- unlist(strsplit(.$new.states, ";"))
    
    # recode (multiple features)
    new_data <- .implement_recode(original_data, expected_states, recoding_groups, recoded_states, nvar="multiple", recode_mode="logical_arguments")
    
    # create table (unconditioned)
    unconditioned <- data.frame(
      Language_ID = original_data$Language_ID,
      value = new_data,
      stringsAsFactors=FALSE)
    
    # select condition statement
    condition_statement <-  .$condition.if.feature.conditioned
    
    # extract condition and equator
    condition_and_equator <- .extract_condition_and_equator(condition_statement)
    
    # apply condition
    conditioned_data <- .implement_conditioning(unconditioned, 
                                               condition=condition_and_equator[[1]], 
                                               equator=condition_and_equator[[2]], 
                                               recoded_data = recoded_data)
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(third_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  ## recode group 4 (merge features - recode via logical arguments - multiple conditioning) ##
  # subset to features that have to be recoded via logical arguments if several conditions apply
  fourth_set <- dplyr::filter(recode_patterns, 
                              .data[["recode.operation.type"]]=="recode group 4 (merge features - recode via logical arguments - multiple conditioning)")
  recode_patterns <- dplyr::filter(recode_patterns, 
                                   .data[["recode.operation.type"]] !="recode group 4 (merge features - recode via logical arguments - multiple conditioning)")
  
  # merge and recode via logical arguments if a condition applies
  fourth_set_rec <- dplyr::rowwise(fourth_set) %>% dplyr::do({
    if(verbose == TRUE){ cat("Fourth, processing ", .$new.name, "\n", sep="")}
    
    # check that the original features are present in the original feature matrix
    original_data <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% unlist(strsplit(.$`original.names`,"&"))))]
    
    if (ncol(original_data) != length(unlist(strsplit(.$`original.names`,"&")))+1){
      original_data<-cbind(original_data,recoded_data[,which(names(recoded_data) %in% unlist(strsplit(.$`original.names`,"&")))])
    }
    
    # extract relevant attributes for recoding
    expected_states <- unlist(strsplit(unlist(.$recode.operation.Rcode), ";"))
    recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
    recoded_states <- unlist(strsplit(.$new.states, ";"))
    
    # recode (multiple features)
    new_data <- .implement_recode(original_data, expected_states, recoding_groups, recoded_states, 
                                 nvar="multiple", recode_mode="logical_arguments")
    
    # create table (unconditioned)
    unconditioned <- data.frame(
      Language_ID = original_data$Language_ID,
      value = new_data,
      stringsAsFactors=FALSE)
    
    # select conditions
    conditions <- unlist(strsplit(.$condition.if.feature.conditioned," & "))
    nr_conditions <- length(conditions)
    
    # determine and extract conditions and equators for condition 1, apply condition 1
    condition_statement_1 <- conditions[1]
    condition_and_equator_1 <- .extract_condition_and_equator(condition_statement_1)
    conditioned_data <- .implement_conditioning(unconditioned, 
                                               condition=condition_and_equator_1[[1]], 
                                               equator=condition_and_equator_1[[2]], 
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 2, apply condition 2
    condition_statement_2 <- conditions[2]
    condition_and_equator_2 <- .extract_condition_and_equator(condition_statement_2)
    conditioned_data <- .implement_conditioning(conditioned_data, 
                                               condition=condition_and_equator_2[[1]], 
                                               equator=condition_and_equator_2[[2]], 
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 3, apply condition 3, if there are more than 2 conditions
    if (nr_conditions>2){
      condition_statement_3 <- conditions[3]
      condition_and_equator_3 <- .extract_condition_and_equator(condition_statement_3)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_3[[1]], 
                                                 equator=condition_and_equator_3[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # determine and extract conditions and equators for condition 4, apply condition 4, if there are more than 3 conditions
    if (nr_conditions>3){
      condition_statement_4 <- conditions[4]
      condition_and_equator_4 <- .extract_condition_and_equator(condition_statement_4)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_4[[1]], 
                                                 equator=condition_and_equator_4[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(fourth_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  ## recode group 5 (simple conditioning) ##
  # subset to features that have to be conditioned on another feature
  fifth_set <- dplyr::filter(recode_patterns, 
                             .data[["recode.operation.type"]]=="recode group 5 (simple conditioning)")
  recode_patterns <- dplyr::filter(recode_patterns, 
                                   .data[["recode.operation.type"]]!="recode group 5 (simple conditioning)")
  
  # condition feature on another feature
  fifth_set_rec <- dplyr::rowwise(fifth_set) %>% dplyr::do({
    if(verbose == TRUE){ cat("Fifth, processing ", .$new.name, "\n", sep="") }
    
    # check that the original feature is present in the original data
    testthat::expect_true(.$`original.names` %in% names(original_feature_matrix)[-1])
    feature_to_be_conditioned <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% .$`original.names`))]
    
    # select condition statement
    condition_statement <-  .$condition.if.feature.conditioned
    
    # extract condition and equator
    condition_and_equator <- .extract_condition_and_equator(condition_statement)
    
    # apply condition
    conditioned_data <- .implement_conditioning(feature_to_be_conditioned, 
                                                condition=condition_and_equator[[1]], 
                                                equator=condition_and_equator[[2]],
                                                recoded_data = recoded_data)
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(fifth_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  
  ## recode group 6 (multiple conditioning) ## 
  # subset to features that have to be conditioned on several features
  sixth_set <- dplyr::filter(recode_patterns, 
                             .data[["recode.operation.type"]] =="recode group 6 (multiple conditioning)")
  recode_patterns <- dplyr::filter(recode_patterns, 
                                   .data[["recode.operation.type"]] !="recode group 6 (multiple conditioning)")
  
  # condition on several features
  sixth_set_rec <- dplyr::rowwise(sixth_set) %>% dplyr::do({
    if(verbose == TRUE){  cat("Sixth, processing ", .$new.name, "\n", sep="") }
    
    # check that the original feature is present in the original data
    testthat::expect_true(.$`original.names` %in% names(original_feature_matrix)[-1])
    feature_to_be_conditioned <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% .$`original.names`))]
    
    # select conditions
    conditions <- unlist(strsplit(.$condition.if.feature.conditioned," & "))
    nr_conditions <- length(conditions)
    
    # determine and extract conditions and equators for condition 1, apply condition 1
    condition_statement_1 <- conditions[1]
    condition_and_equator_1 <- .extract_condition_and_equator(condition_statement_1)
    conditioned_data <- .implement_conditioning(feature_to_be_conditioned, 
                                               condition=condition_and_equator_1[[1]], 
                                               equator=condition_and_equator_1[[2]],
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 2, apply condition 2
    condition_statement_2 <- conditions[2]
    condition_and_equator_2 <- .extract_condition_and_equator(condition_statement_2)
    conditioned_data <- .implement_conditioning(conditioned_data, 
                                               condition=condition_and_equator_2[[1]], 
                                               equator=condition_and_equator_2[[2]],
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 3, apply condition 3, if there are more than 2 conditions
    if (nr_conditions>2){
      condition_statement_3 <- conditions[3]
      condition_and_equator_3 <- .extract_condition_and_equator(condition_statement_3)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_3[[1]], 
                                                 equator=condition_and_equator_3[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # determine and extract conditions and equators for condition 4, apply condition 4, if there are more than 3 conditions
    if (nr_conditions>3){
      condition_statement_4 <- conditions[4]
      condition_and_equator_4 <- .extract_condition_and_equator(condition_statement_4)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_4[[1]], 
                                                 equator=condition_and_equator_4[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(sixth_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  
  ## recode group 7 (simple conditioning [conditioned feature]) ##
  # subset to features that have to be conditioned on another conditioned feature
  seventh_set <- dplyr::filter(recode_patterns, 
                               .data[["recode.operation.type"]] =="recode group 7 (simple conditioning [conditioned feature])")
  recode_patterns <- dplyr::filter(recode_patterns, 
                                   .data[["recode.operation.type"]] !="recode group 7 (simple conditioning [conditioned feature])")
  
  # condition on conditioned feature
  seventh_set_rec <- dplyr::rowwise(seventh_set) %>% dplyr::do({

    if(verbose == TRUE){  cat("Seventh, processing ", .$new.name, "\n", sep="") }
    
    # check that the original feature is present in the original data
    testthat::expect_true(.$`original.names` %in% names(original_feature_matrix)[-1])
    feature_to_be_conditioned <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% .$`original.names`))]
    
    # select condition statement
    condition_statement <-  .$condition.if.feature.conditioned
    
    # extract condition and equator
    condition_and_equator <- .extract_condition_and_equator(condition_statement)
    
    # apply condition
    conditioned_data <- .implement_conditioning(feature_to_be_conditioned, 
                                                condition=condition_and_equator[[1]], 
                                                equator=condition_and_equator[[2]],
                                                recoded_data = recoded_data)
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(seventh_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  ## recode group 8 (multiple conditioning [conditioned feature]) ## 
  # subset to features that have to be conditioned on several features
  eighth_set <- dplyr::filter(recode_patterns, 
                              .data[["recode.operation.type"]] =="recode group 8 (multiple conditioning [conditioned feature])")
  recode_patterns <- dplyr::filter(recode_patterns, 
                                   .data[["recode.operation.type"]] !="recode group 8 (multiple conditioning [conditioned feature])")
  
  # condition on several features
  eighth_set_rec <- dplyr::rowwise(eighth_set) %>% dplyr::do({
    if(verbose == TRUE){  cat("Eight, processing ", .$new.name, "\n", sep="") }
    
    # check that the original feature is present in the original data
    testthat::expect_true(.$`original.names` %in% names(original_feature_matrix)[-1])
    feature_to_be_conditioned <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% .$`original.names`))]
    
    # select conditions
    conditions <- unlist(strsplit(.$condition.if.feature.conditioned," & "))
    nr_conditions <- length(conditions)
    
    # determine and extract conditions and equators for condition 1, apply condition 1
    condition_statement_1 <- conditions[1]
    condition_and_equator_1 <- .extract_condition_and_equator(condition_statement_1)
    conditioned_data <- .implement_conditioning(feature_to_be_conditioned, 
                                               condition=condition_and_equator_1[[1]], 
                                               equator=condition_and_equator_1[[2]],
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 2, apply condition 2
    condition_statement_2 <- conditions[2]
    condition_and_equator_2 <- .extract_condition_and_equator(condition_statement_2)
    conditioned_data <- .implement_conditioning(conditioned_data, 
                                               condition=condition_and_equator_2[[1]], 
                                               equator=condition_and_equator_2[[2]],
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 3, apply condition 3, if there are more than 2 conditions
    if (nr_conditions>2){
      condition_statement_3 <- conditions[3]
      condition_and_equator_3 <- .extract_condition_and_equator(condition_statement_3)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_3[[1]], 
                                                 equator=condition_and_equator_3[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # determine and extract conditions and equators for condition 4, apply condition 4, if there are more than 3 conditions
    if (nr_conditions>3){
      condition_statement_4 <- conditions[4]
      condition_and_equator_4 <- .extract_condition_and_equator(condition_statement_4)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_4[[1]], 
                                                 equator=condition_and_equator_4[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(eighth_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  
  ## recode group 9 (merge features - recode via logical arguments - simple conditioning [conditioned feature]) ##
  # subset to features that have to be recoded via logical arguments if a condition applies (conditioned feature)
  ninth_set <- dplyr::filter(recode_patterns, 
                             .data[["recode.operation.type"]]=="recode group 9 (merge features - recode via logical arguments - simple conditioning [conditioned feature])")
  recode_patterns <- dplyr::filter(recode_patterns, .data[["recode.operation.type"]]!="recode group 9 (merge features - recode via logical arguments - simple conditioning [conditioned feature])")
  
  # merge and recode via logical arguments if a condition applies
  ninth_set_rec <- dplyr::rowwise(ninth_set) %>% dplyr::do({
    if(verbose == TRUE){    cat("Ninth, processing ", .$new.name, "\n", sep="") }
    
    # check that the original features are present in the original feature matrix
    original_data <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% unlist(strsplit(.$`original.names`,"&"))))]
    
    if (ncol(original_data) != length(unlist(strsplit(.$`original.names`,"&")))+1){
      original_data<-cbind(original_data,recoded_data[,which(names(recoded_data) %in% unlist(strsplit(.$`original.names`,"&")))])
    }
    
    # extract relevant attributes for recoding
    expected_states <- unlist(strsplit(unlist(.$recode.operation.Rcode), ";"))
    recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
    recoded_states <- unlist(strsplit(.$new.states, ";"))
    
    # recode (multiple features)
    new_data <- .implement_recode(original_data, expected_states, recoding_groups, recoded_states, nvar="multiple", recode_mode="logical_arguments")
    
    # create table (unconditioned)
    unconditioned <- data.frame(
      Language_ID = original_data$Language_ID,
      value = new_data,
      stringsAsFactors=FALSE)
    
    # select condition statement
    condition_statement <-  .$condition.if.feature.conditioned
    
    # extract condition and equator
    condition_and_equator <- .extract_condition_and_equator(condition_statement)
    
    # apply condition
    conditioned_data <- .implement_conditioning(unconditioned, 
                                               condition=condition_and_equator[[1]], 
                                               equator=condition_and_equator[[2]],
                                               recoded_data = recoded_data)
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
    
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(ninth_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  
  ## recode group 10 (merge features - recode via logical arguments - multiple conditioning [conditioned feature]) ##
  # subset to features that have to be recoded via logical arguments if a condition applies (conditioned feature)
  tenth_set <- dplyr::filter(recode_patterns, .data[["recode.operation.type"]]=="recode group 10 (merge features - recode via logical arguments - multiple conditioning [conditioned feature])")
  recode_patterns <- dplyr::filter(recode_patterns, .data[["recode.operation.type"]]!="recode group 10 (merge features - recode via logical arguments - multiple conditioning [conditioned feature])")
  
  # merge and recode via logical arguments if a condition applies
  tenth_set_rec <- dplyr::rowwise(tenth_set) %>% dplyr::do({
    if(verbose == TRUE){    cat("Tenth, processing ", .$new.name, "\n", sep="") }
    
    # check that the original features are present in the original feature matrix
    original_data <- original_feature_matrix[,c(1,which(names(original_feature_matrix) %in% unlist(strsplit(.$`original.names`,"&"))))]
    
    if (ncol(original_data) != length(unlist(strsplit(.$`original.names`,"&")))+1){
      original_data<-cbind(original_data,recoded_data[,which(names(recoded_data) %in% unlist(strsplit(.$`original.names`,"&")))])
    }
    
    # extract relevant attributes for recoding
    expected_states <- unlist(strsplit(unlist(.$recode.operation.Rcode), ";"))
    recoding_groups <- unlist(strsplit(.$recode.pattern, "-"))
    recoded_states <- unlist(strsplit(.$new.states, ";"))
    
    # recode (multiple features)
    new_data <- .implement_recode(original_data, expected_states, recoding_groups, recoded_states, 
                                 nvar="multiple", recode_mode="logical_arguments")
    
    # create table (unconditioned)
    unconditioned <- data.frame(
      Language_ID = original_data$Language_ID,
      value = new_data,
      stringsAsFactors=FALSE)
    
    # select conditions
    conditions <- unlist(strsplit(.$condition.if.feature.conditioned," & "))
    nr_conditions <- length(conditions)
    
    # determine and extract conditions and equators for condition 1, apply condition 1
    condition_statement_1 <- conditions[1]
    condition_and_equator_1 <- .extract_condition_and_equator(condition_statement_1)
    conditioned_data <- .implement_conditioning(unconditioned, 
                                               condition=condition_and_equator_1[[1]], 
                                               equator=condition_and_equator_1[[2]],
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 2, apply condition 2
    condition_statement_2 <- conditions[2]
    condition_and_equator_2 <- .extract_condition_and_equator(condition_statement_2)
    conditioned_data <- .implement_conditioning(conditioned_data, 
                                               condition=condition_and_equator_2[[1]], 
                                               equator=condition_and_equator_2[[2]],
                                               recoded_data = recoded_data)
    
    # determine and extract conditions and equators for condition 3, apply condition 3, if there are more than 2 conditions
    if (nr_conditions>2){
      condition_statement_3 <- conditions[3]
      condition_and_equator_3 <- .extract_condition_and_equator(condition_statement_3)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_3[[1]], 
                                                 equator=condition_and_equator_3[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # determine and extract conditions and equators for condition 4, apply condition 4, if there are more than 3 conditions
    if (nr_conditions>3){
      condition_statement_4 <- conditions[4]
      condition_and_equator_4 <- .extract_condition_and_equator(condition_statement_4)
      conditioned_data <- .implement_conditioning(conditioned_data, 
                                                 condition=condition_and_equator_4[[1]], 
                                                 equator=condition_and_equator_4[[2]],
                                                 recoded_data = recoded_data)
    }
    
    # prepare output as data frame
    data.frame(
      feature = .$new.name, 
      Language_ID = conditioned_data$Language_ID, 
      value = as.character(conditioned_data[,2]),  
      stringsAsFactors=FALSE)  
  })  %>% 
    tidyr::spread(.data[["feature"]], .data[["value"]])
  
  
  # merge new features with recoded_data for further use
  recoded_data <- dplyr::full_join(tenth_set_rec, recoded_data, by=c(Language_ID="Language_ID"))
  
  # replace all NA as explicit "NA"
  recoded_data[is.na(recoded_data)]<-"NA"
  
  # subset full data into original layer; logical layer and statistical layer
#  recode_patterns <- read.csv("fixed/feature-recode-patterns.csv")
#  all_decisions <- read.csv("fixed/decisions-log.csv")
  logical_decisions <- all_decisions %>% 
    dplyr::filter( .data[["modification.type"]] %in% c("logical","design"))
  statistical_decisions <- all_decisions %>% 
    dplyr::filter( .data[["modification.type"]] == "statistical")
  
  original_layer <- recode_patterns_full %>% dplyr::filter( .data[["original.features"]]=="TRUE")
  logical_layer <- recode_patterns_full %>% dplyr::filter( .data[["design.logical"]]=="TRUE")
  statistical_layer <- recode_patterns_full %>% dplyr::filter( .data[["design.logical.statistical"]]=="TRUE")
  
  original_data <- recoded_data %>% dplyr::select(c("Language_ID",original_layer$new.name))
  logical_data <- recoded_data %>% dplyr::select(c("Language_ID",logical_layer$new.name))
  statistical_data <- recoded_data %>% dplyr::select(c("Language_ID",statistical_layer$new.name))
  
  
  ########## sanity checks ########## 
  ### check all original features that should be in the original_feature filter are in there and vice versa
  original_names_is <- original_layer$new.name
  original_names_should <- names(retained_data)[-1]
  testthat::expect_true(all(original_names_is %in% original_names_should))
  testthat::expect_true(all(original_names_should %in% original_names_is))
  
  ### logical dataset: check all original features that should be in the logical filter are in there and vice versa
  design_add <- c(unique(unlist(stringr::str_split(dplyr::filter(logical_decisions,
                                                                 .data[["is.added.feature.kept"]] == "yes")$resulting.added.features,", "))),NA)
  design_remove <- unique(unlist(stringr::str_split(logical_decisions$resulting.removed.features,", ")))
  # logical dataset is: a) original features, plus b) all design/logical additions, minus c) all design/logical removals
  logical_names_should <- unique(c(original_names_is,design_add))[unique(c(original_names_is,design_add)) %in% design_remove==F]
  logical_names_is <- names(logical_data)[names(logical_data)!="Language_ID"]
  # sanity checks
  testthat::expect_true(all(logical_names_is %in% logical_names_should))
  testthat::expect_true(all(logical_names_should %in% logical_names_is))
  
  ### statistical dataset: check all original features that should be in the statistical filter are in there and vice versa
  statistical_add <- unique(statistical_decisions$resulting.added.features) 
  statistical_remove <- unique(unlist(stringr::str_split(statistical_decisions$resulting.removed.features,", ")))
  # statistical dataset is: a) the features from the final logical dataset, plus b) all statistical additions, minus c) all statistical removals
  statistical_names_should <- unique(c(logical_names_is,statistical_add))[unique(c(logical_names_is,statistical_add)) %in% statistical_remove==F]
  statistical_names_is <- names(statistical_data)[names(statistical_data)!="Language_ID"]
  # sanity checks
  testthat::expect_true(all(statistical_names_is %in% statistical_names_should))
  testthat::expect_true(all(statistical_names_should %in% statistical_names_is))
  
  ### modification ID match --> ensure all modification IDs in the features sheet are in the modification sheet and vice versa
  mod_IDs_is <- stats::na.omit(unique(c(unlist(stringr::str_split(recode_patterns_full$modification.IDs,";")),(unlist(stringr::str_split(recode_patterns_full$associated.modification.IDs.without.resulting.action,";"))))))
  mod_IDs_is <- mod_IDs_is[mod_IDs_is!=""]
  mod_IDs_should <- stats::na.omit(all_decisions$modification.ID)
  testthat::expect_true(all(mod_IDs_is %in% mod_IDs_should))
  testthat::expect_true(all(mod_IDs_should %in% mod_IDs_is))
  
  # specific modification ID match --> ensure that each modification ID in the features sheet is in the modification sheet, associated via the correct columns and features; and vice versa
  ids <- stats::na.omit(all_decisions$modification.ID)
  for (id in ids){
    type <- dplyr::filter(all_decisions, .data[["modification.ID"]] == id)[["modification.type"]]
    if (type == "statistical"){
      # check that each instance of a modification ID in the spreadsheet ("is") is foreseen in the decisions_log ("should") and vice versa
      should_all <- all_decisions %>% dplyr::filter(.data[["modification.ID"]] == id) %>% 
        dplyr::select(c("feature.1.for.test","feature.2.for.test","resulting.added.features","resulting.removed.features")) %>% 
        as.character() %>% 
        unique()
      should_all <- stats::na.omit(unique(unlist(strsplit(should_all[should_all!="NA"],", "))))
      is_all <- recode_patterns_full %>% dplyr::slice(c(which(grepl(id,recode_patterns_full$modification.IDs)),which(grepl(id,recode_patterns_full$associated.modification.IDs.without.resulting.action))))
      is_all <- is_all$new.name
      testthat::expect_true(all(is_all %in% should_all))
      testthat::expect_true(all(should_all %in% is_all))
      
      # check that each instance of a modification ID WITH EFFECT in the spreadsheet ("is") is foreseen in the decisions_log ("should") and vice versa
      should_actedupon <- all_decisions %>% 
        dplyr::filter(.data[["modification.ID"]] == id) %>% 
        dplyr::select(c("resulting.added.features","resulting.removed.features")) %>% 
        as.character() %>% 
        unique()
      should_actedupon <- stats::na.omit(unique(unlist(strsplit(should_actedupon[should_actedupon!="NA"],", "))))
      is_actedupon <- recode_patterns_full %>% dplyr::slice(which(grepl(id,recode_patterns_full$modification.IDs)))
      is_actedupon <- is_actedupon$new.name
      testthat::expect_true(all(is_actedupon %in% should_actedupon))
      testthat::expect_true(all(should_actedupon %in% is_actedupon))
      
      # check that each instance of a modification ID WITHOUT EFFECT in the spreadsheet ("is") is foreseen in the decisions_log ("should") and vice versa
      should_associated <- all_decisions %>% 
        dplyr::filter(.data[["modification.ID"]] == id) %>% 
        dplyr::select(c("feature.1.for.test","feature.2.for.test")) %>%       
        as.character()  %>% 
        unique()
      
      should_associated <- setdiff(stats::na.omit(unique(unlist(strsplit(should_associated[should_associated!="NA"],", ")))),is_actedupon)
      is_associated <- recode_patterns_full %>% 
        dplyr::slice(which(grepl(id,recode_patterns_full$associated.modification.IDs.without.resulting.action)))
      is_associated <- is_associated$new.name
      testthat::expect_true(all(is_associated %in% should_associated))
      testthat::expect_true(all(should_associated %in% is_associated))
    }
    else if (type %in% c("logical","design-automated","design-manual")){ 
      # check that each instance of a modification ID in the spreadsheet ("is") is foreseen in the decisions_log ("should") and vice versa
      should_all <- all_decisions %>% 
        dplyr::filter(.data[["modification.ID"]] == id) %>% 
        dplyr::select(c("relevant.features","resulting.added.features","resulting.removed.features")) %>% 
        as.character() %>% unique()
      should_all <- stats::na.omit(unique(unlist(strsplit(should_all[should_all!="NA"],", "))))
      is_all <- recode_patterns_full %>% 
        dplyr::slice(c(which(grepl(id,recode_patterns_full$modification.IDs)),which(grepl(id,recode_patterns_full$associated.modification.IDs.without.resulting.action))))
      is_all <- is_all$new.name
      testthat::expect_true(all(is_all %in% should_all))
      testthat::expect_true(all(should_all %in% is_all))
      
      # check that each instance of a modification ID WITH EFFECT in the spreadsheet ("is") is foreseen in the decisions_log ("should") and vice versa
      should_actedupon <- all_decisions %>% 
        dplyr::filter(.data[["modification.ID"]] == id) %>% 
        dplyr::select(c("resulting.added.features","resulting.removed.features")) %>% 
        as.character() %>% 
        unique()
      should_actedupon <- stats::na.omit(unique(unlist(strsplit(should_actedupon[should_actedupon!="NA"],", "))))
      is_actedupon <- recode_patterns_full %>% 
        dplyr::slice(which(grepl(id,recode_patterns_full$modification.IDs)))
      is_actedupon <- is_actedupon$new.name
      testthat::expect_true(all(is_actedupon %in% should_actedupon))
      testthat::expect_true(all(should_actedupon %in% is_actedupon))
      
      # check that each instance of a modification ID WITHOUT EFFECT in the spreadsheet ("is") is foreseen in the decisions_log ("should") and vice versa
      should_associated <- all_decisions %>% 
        dplyr::filter( .data[["modification.ID"]] == id) %>% 
        dplyr::select("relevant.features") %>% as.character() %>% unique()
      should_associated <- setdiff(stats::na.omit(unique(unlist(strsplit(should_associated[should_associated!="NA"],", ")))),is_actedupon)
      is_associated <- recode_patterns_full %>% dplyr::slice(which(grepl(id,recode_patterns_full$associated.modification.IDs.without.resulting.action)))
      is_associated <- is_associated$new.name
      testthat::expect_true(all(is_associated %in% should_associated))
      testthat::expect_true(all(should_associated %in% is_associated))
    }
  }
  
  ## known.remaining dependencies match - check remaining dependencies are appropriately logged (one-sided, because there are dependencies not associated with statistical tests!)
  rds <- dplyr::filter(all_decisions, 
                       .data[["resulting.modification"]] == "tendency logged in known.remaining.dependencies")
  for (rd in seq_len(nrow(rds))){
    id <- rds %>% 
      dplyr::slice(rd) %>% 
      dplyr::select("modification.ID")
    should_associated <- rds %>% 
      dplyr::slice(rd) %>% 
      dplyr::select(c(.data[["feature.1.for.test"]],
                      .data[["feature.2.for.test"]])) %>% 
      as.character()
    is_associated <- recode_patterns_full %>% 
      dplyr::slice(which(grepl(id,recode_patterns_full$known.remaining.dependencies.after.statistical.treatment))) %>% 
      dplyr::select("new.name") %>% unlist %>% as.character
    testthat::expect_true(all(is_associated %in% should_associated))
    testthat::expect_true(all(should_associated %in% is_associated))
  }
  testthat::expect_true(all(rds$modification.ID %in% unlist(strsplit(recode_patterns_full$known.remaining.dependencies.after.statistical.treatment,";"))))
  testthat::expect_true(all(stats::na.omit(unique(unlist(strsplit(recode_patterns_full$known.remaining.dependencies.after.statistical.treatment,";")))) %in% rds$modification.ID))
  
  
  ########## make, check and save cldf  ########## 
  # languages.csv
#  lang_metadata <- LanguageTable
  
  taxonomy_logical <- data.frame(Language_ID = logical_data$Language_ID)
 # taxonomy_logical <- left_join(taxonomy_logical,lang_metadata, by = "Language_ID")
  taxonomy_logical[taxonomy_logical==""] <- NA
  
  taxonomy_statistical <- data.frame(Language_ID = statistical_data$Language_ID)
#  taxonomy_statistical <- left_join(taxonomy_statistical,lang_metadata, by = "Language_ID")
  taxonomy_statistical[taxonomy_statistical==""] <- NA
  
  # parameters.csv
  parameters <- recode_patterns_full
  
  parameters_logical <- parameters %>% 
    dplyr::filter( .data[["design.logical"]] ==T)
  parameters_statistical <- parameters %>% 
    dplyr::filter( .data[["design.logical.statistical"]] ==T)
  
  # values.csv
  logical_long <- tidyr::pivot_longer(
    as.data.frame(logical_data),
    cols = -Language_ID,
    names_to = "new.name",
    values_to = "value"
  )
  logical_long$value_ID <- apply(logical_long,1,function(x) paste(x[2],x[1],sep="-"))
  logical_long$code_ID <- apply(logical_long,1,function(x) paste(x[2],x[3],sep="-"))
  logical_long <- logical_long %>% 
    dplyr::select(c("value_ID","Language_ID","new.name","value","code_ID"))
  logical_long$Language_ID <- as.character(logical_long$Language_ID)
  logical_long$new.name <- as.character(logical_long$new.name)
  
  statistical_long <- tidyr::pivot_longer(
    as.data.frame(statistical_data),
    cols = -Language_ID,
    names_to = "new.name",
    values_to = "value"
  )
  statistical_long$value_ID <- apply(statistical_long,1,function(x) paste(x[2],x[1],sep="-"))
  statistical_long$code_ID <- apply(statistical_long,1,function(x) paste(x[2],x[3],sep="-"))
  statistical_long$Language_ID <- as.character(statistical_long$Language_ID)
  statistical_long$new.name <- as.character(statistical_long$new.name)
  statistical_long <- statistical_long %>% 
    dplyr::select(c("value_ID","Language_ID","new.name","value","code_ID"))
  
  # codes.csv
  logical_codes <- logical_long %>% 
    dplyr::select(c("code_ID","new.name","value")) %>% unique()
  
  statistical_codes <- statistical_long %>% 
    dplyr::select(c("code_ID","new.name","value")) %>% unique()
  
  # modifications.csv
  modifications <- all_decisions
  
  # cldf quality checks:
  testthat::expect_true(all(unique(logical_long$Language_ID) %in% taxonomy_logical$Language_ID))
  testthat::expect_true(all(unique(statistical_long$Language_ID) %in% taxonomy_statistical$Language_ID))
  testthat::expect_true(all(taxonomy_logical$Language_ID %in% unique(logical_long$Language_ID)))
  testthat::expect_true(all(taxonomy_statistical$Language_ID %in% unique(statistical_long$Language_ID)))
  
  testthat::expect_true(all(unique(logical_long$new.name) %in% parameters$new.name))
  testthat::expect_true(all(unique(statistical_long$new.name) %in% parameters$new.name))
  testthat::expect_true(all(dplyr::filter(parameters,
                                .data[["design.logical"]] == "TRUE")$new.name %in% unique(logical_long$new.name)))
  testthat::expect_true(all(dplyr::filter(parameters,
                                .data[["design.logical.statistical"]] == "TRUE")$new.name %in% unique(statistical_long$new.name)))
  
  testthat::expect_true(all(unique(logical_long$code_ID) %in% logical_codes$code_ID))
  testthat::expect_true(all(unique(statistical_long$code_ID) %in% statistical_codes$code_ID))
  testthat::expect_true(all(logical_codes$code_ID %in% unique(logical_long$code_ID)))
  testthat::expect_true(all(statistical_codes$code_ID %in% unique(statistical_long$code_ID)))
  
  testthat::expect_true(all(unique(unlist(strsplit(dplyr::filter(parameters, 
                                                       .data[["modification.IDs"]] !="")$modification.IDs,";"))) %in% modifications$modification.ID))
  testthat::expect_true(all(unique(unlist(strsplit(dplyr::filter(parameters, 
  .data[["associated.modification.IDs.without.resulting.action"]] !="")$associated.modification.IDs.without.resulting.action,";"))) %in% modifications$modification.ID))
  testthat::expect_true(all(modifications$modification.ID %in% c(unique(unlist(strsplit(dplyr::filter(parameters,
                                                                                            .data[["modification.IDs"]] !="")$modification.IDs,";"))),
                                                       unique(unlist(strsplit(dplyr::filter(parameters,
                                                                                            .data[["associated.modification.IDs.without.resulting.action"]]!="")$associated.modification.IDs.without.resulting.action,";"))))))
  
  ########################OUTPUT###################
  
  # this full set of all input and recoded features needs to be stored to perform statistical tests
output <- list(data_for_statsGBI = recoded_data  %>% as.data.frame(),
      
  ########## save data as language-feature matrices ########## 
  # save logical and statistical datasets as language-feature matrices (.csv)
  "logicalGBI" = logical_data %>% as.data.frame(), 
  "statisticalGBI" = statistical_data  %>% as.data.frame(),

  # write all cldf components:
  "parameters_logicalGBI" = parameters_logical  %>% as.data.frame(),
  "parameters_statisticalGBI" = parameters_statistical  %>% as.data.frame(),
  "values_logicalGBI" = logical_long  %>% as.data.frame(),
  "values_statisticalGBI" = statistical_long  %>% as.data.frame(),
  "codes_logicalGBI" = logical_codes  %>% as.data.frame(),
  "codes_statisticalGBI" = statistical_codes  %>% as.data.frame(),
  "modificationsGBI" = modifications  %>% as.data.frame())
  output
}

#### helper functions ####

# this function serves to condition a feature on another -- note that the currently implemented function works for up to 5 desired states in the %in% case
.implement_conditioning <- function(feature_to_be_conditioned, condition, equator, recoded_data){
  
  # select conditioned upon feature
  conditioned_upon_feature <- recoded_data[,c(1,which(names(recoded_data) %in% condition[1]))]
  
  # select Language_IDs for which condition applies and turn data into "?" where applicable
  if (equator == " == "){ 
    # if the condition in question is positive (" == "), we want to keep languages that have the desired state of conditioned_upon_feature OR which are "?" to both conditioned_upon_feature and feature_to_be_conditioned to not become NA
    # select languages with desired state or "?" in conditioned_upon_feature
    col_name <- names(conditioned_upon_feature)[2]
    
    condition_applies_strict <- conditioned_upon_feature %>%
      dplyr::filter(.data[[col_name]] == condition[2]) %>%
      dplyr::pull(.data[["Language_ID"]])
    
    condition_applies_q <- conditioned_upon_feature %>%
      dplyr::filter(.data[[col_name]] == "?") %>%
      dplyr::pull(.data[["Language_ID"]])
    
    # select languages in feature_to_be_conditioned to which condition applies (strict and q)
    conditioned_data <- dplyr::filter(feature_to_be_conditioned, 
                                      .data[["Language_ID"]] %in% as.character(c(condition_applies_strict,condition_applies_q)))
    
    names(conditioned_data)[2] <- "conditioned_upon_feature"
    
    # the languages, which are "?" to conditioned_upon_feature but specified for feature_to_be_conditioned are recoded into "?"
    conditioned_data$conditioned_upon_feature[conditioned_data$Language_ID %in% condition_applies_q] <- rep("?")
    
  } else if (equator == " != "){ ## this applies if the condition in question is negative (" != ")
    # if the condition in question is negative (" != "), we want to keep all languages that do not have the specified state of conditioned_upon_feature
    # select languages which do not have the specified state in conditioned_upon_feature
    
    col_name <- names(conditioned_upon_feature)[2]
    condition_applies_strict <- conditioned_upon_feature %>%
      dplyr::filter(.data[[col_name]] == condition[2]) %>%
      dplyr::pull(.data[["Language_ID"]])
    
    condition_applies <- setdiff(conditioned_upon_feature$Language_ID, condition_applies_strict)
    
    # select languages in feature_to_be_conditioned to which condition applies
    conditioned_data <- dplyr::filter(feature_to_be_conditioned, 
                                      .data[["Language_ID"]] %in% as.character(condition_applies))
    
  } else if (equator == " %in% "){ ## this applies if the condition in the question is multiple --> conservative (" %in% ")
    
    # if the condition in question is multiple (" %in% "), we want to keep languages that have any of the desired state of conditioned_upon_feature OR which are "?" to both conditioned_upon_feature and feature_to_be_conditioned to not become NA
    desired_states <- unlist(strsplit(condition[2],", "))
    nr_desired_states <- length(desired_states)
    
    # select languages with desired states or "?" in conditioned_upon_feature
    
    col_name <- names(conditioned_upon_feature)[2]
    condition_applies_q <- conditioned_upon_feature %>%
      dplyr::filter(.data[[col_name]] == "?") %>%
      dplyr::pull(.data[["Language_ID"]])
    
    condition_applies_desired_states <- conditioned_upon_feature %>%
      dplyr::filter(.data[[col_name]] ==desired_states[1]|
                      .data[[col_name]] ==desired_states[2]  ) %>%
      dplyr::pull(.data[["Language_ID"]])
    
    # if there are more than 2 desired states, add the third
    if(nr_desired_states>2){
      
      col_name <- names(conditioned_upon_feature)[2]
      condition_applies_desired_states_2 <- conditioned_upon_feature %>%
        dplyr::filter(.data[[col_name]] ==desired_states[3] ) %>%
        dplyr::pull(.data[["Language_ID"]])
      
      condition_applies_desired_states <- c(condition_applies_desired_states,condition_applies_desired_states_2)
    }
    # if there are more than 3 desired states, add the fourth
    if(nr_desired_states>3){
      col_name <- names(conditioned_upon_feature)[2]
      condition_applies_desired_states_3 <- conditioned_upon_feature %>%
        dplyr::filter(.data[[col_name]] ==desired_states[4] ) %>%
        dplyr::pull(.data[["Language_ID"]])
      
      condition_applies_desired_states <- c(condition_applies_desired_states,condition_applies_desired_states_3)
    }
    # if there are more than 4 desired states, add the fifth
    if(nr_desired_states>4){
      
      col_name <- names(conditioned_upon_feature)[2]
      condition_applies_desired_states_4 <- conditioned_upon_feature %>%
        dplyr::filter(.data[[col_name]] ==desired_states[5] ) %>%
        dplyr::pull(.data[["Language_ID"]])
      
      condition_applies_desired_states <- c(condition_applies_desired_states,condition_applies_desired_states_4)
    }
    
    # select languages in feature_to_be_conditioned to which condition applies (strict and q)
    conditioned_data <- dplyr::filter(feature_to_be_conditioned, 
                                      .data[["Language_ID"]] %in% as.character(c(condition_applies_q, condition_applies_desired_states)))
    names(conditioned_data)[2] <- "conditioned_upon_feature"
    
    # the languages, which are "?" to conditioned_upon_feature but specified for feature_to_be_conditioned are recoded into "?"
    conditioned_data$conditioned_upon_feature[conditioned_data$Language_ID %in% condition_applies_q] <- rep("?")
    
  } else if (equator == " ! %in%  "){ ## this applies if the condition in the question is multiple (but negative) --> liberal ("! %in% ")
    # if the condition in question is negative multiple (" ! %in%  "), we want to keep all languages that do not have the specified states of conditioned_upon_feature
    # select languages which do not have the specified state in conditioned_upon_feature
    undesired_states <- unlist(strsplit(condition[2],", "))
    nr_undesired_states <- length(undesired_states)
    
    
    col_name <- names(conditioned_upon_feature)[2]
    
    condition_applies_undesired_states <-  conditioned_upon_feature %>%
      dplyr::filter(.data[[col_name]] == undesired_states[1]|
                      .data[[col_name]] == undesired_states[2] ) %>%
      dplyr::pull(.data[["Language_ID"]])
    
    # if there are more than 2 undesired states, add the third
    if(nr_undesired_states>2){
      
      col_name <- names(conditioned_upon_feature)[2]
      
      condition_applies_undesired_states_2 <-  conditioned_upon_feature %>%
        dplyr::filter(.data[[col_name]] == undesired_states[3]) %>%
        dplyr::pull(.data[["Language_ID"]])
      
      condition_applies_undesired_states <- c(condition_applies_undesired_states,      condition_applies_undesired_states_2)
    }
    # if there are more than 3 undesired states, add the fourth
    if(nr_undesired_states>3){
      
      col_name <- names(conditioned_upon_feature)[2]
      
      condition_applies_undesired_states_3 <-  conditioned_upon_feature %>%
        dplyr::filter(.data[[col_name]] == undesired_states[4]) %>%
        dplyr::pull(.data[["Language_ID"]])
      
      condition_applies_undesired_states <- c(condition_applies_undesired_states,      condition_applies_undesired_states_3)
    } 
    
    # if there are more than 4 undesired states, add the fifth
    if(nr_undesired_states>4){
      
      col_name <- names(conditioned_upon_feature)[2]
      
      condition_applies_undesired_states_4 <-  conditioned_upon_feature %>%
        dplyr::filter(.data[[col_name]] == undesired_states[5]) %>%
        dplyr::pull(.data[["Language_ID"]])
      
      condition_applies_undesired_states <- c(condition_applies_undesired_states, condition_applies_undesired_states_4)
    }
    
    condition_applies <- setdiff(conditioned_upon_feature$Language_ID,condition_applies_undesired_states)
    # select languages in feature_to_be_conditioned to which condition applies
    conditioned_data <- dplyr::filter(feature_to_be_conditioned, 
                                      .data[["Language_ID"]] %in% as.character(condition_applies))
  }
  
  return(conditioned_data)
}

# this function serves to extract "condition" and "equator" from a condition statement for further use
.extract_condition_and_equator <- function(condition_statement){
  condition <- unlist(strsplit(condition_statement," == "))
  equator <- " == "
  # if condition has not been split, it is not positive; check whether it is negative
  if (length(condition)==1){ 
    condition <- unlist(strsplit(condition_statement," != "))
    equator <- " != "
  }
  # if condition has not been split, it is not positive or negative, check whether it is positive multiple
  if (length(condition)==1){
    condition <- unlist(strsplit(condition_statement," %in% "))
    equator <- " %in% "
  }
  # if condition has not been split, it is not positive or negative or positive mulitple, so it is negative multiple
  if (length(condition)==1){
    condition <- unlist(strsplit(condition_statement," ! %in%  "))
    equator <- " ! %in%  "
  } 
  
  return(list(condition,equator))
}

# this function serves to recode states from one or several original features into other states, as specified in modifications.csv
.implement_recode <- function(original_data, expected_levels, recoding_groups, recoded_levels, nvar, recode_mode){
  
  # make a table of original values
  expected_levels <- data.frame(i = as.integer(gsub("^([0-9])*([0-9])+.+$", "\\1\\2", expected_levels)),
                                level = gsub("^[0-9]+\\.? +", "", expected_levels),
                                stringsAsFactors=FALSE)
  
  # sanity checks
  testthat::expect_true(all(!is.na(expected_levels$i)))
  testthat::expect_true(all(!is.na(expected_levels$level)))
  
  # make sure that the expected values match the original values found (applies only to simple recode)
  if (recode_mode=="simple"){
    if(nvar=="single"){
      testthat::expect_true(setequal(expected_levels$level, stats::na.omit(original_data)), info=
                              paste0("Expected:\n", paste0("  ", (expected_levels$level), collapse="\n"), "\n",
                                     "Got:\n",  paste0("  ", (unique(original_data)), collapse="\n")))
    }
    if(nvar=="multiple"){
      testthat::expect_true(all(unique(stats::na.omit(original_data$merged)) %in% expected_levels$level), info=
                              paste0("Expected:\n", paste0("  ", (expected_levels$level), collapse="\n"), "\n",
                                     "Got:\n",  paste0("  ", (unique(original_data)), collapse="\n")))
    }
  }
  
  # parse the recoding pattern
  recoding_groups <- strsplit(recoding_groups, "/") %>% lapply(as.integer)
  
  # sanity checks
  testthat::expect_true(length(recoding_groups)>1) # must have at least 2 recoding groups
  testthat::expect_true(all(!is.na(unlist(recoding_groups)))) # can't have NAs
  testthat::expect_true(all(unlist(recoding_groups) %in% expected_levels$i)) # must correspond to original values
  testthat::expect_false(any(duplicated(unlist(recoding_groups)))) # can't have any duplicates
  
  # build the recoding table
  testthat::expect_true(length(recoding_groups)==length(recoded_levels)) # must have at least 2 recoding groups
  recoded_levels <- dplyr::bind_rows(mapply(recoded_levels, recoding_groups, FUN=function(value, ii) {
    data.frame(i = ii, new_level=as.character(value), stringsAsFactors=FALSE)
  }, SIMPLIFY=FALSE))
  
  level_table <- dplyr::full_join(expected_levels, recoded_levels, by="i")
  
  # sanity checks
  testthat::expect_true(all(!is.na(level_table$level)))
  
  # recode the data
  if (recode_mode == "simple"){
    if (nvar=="single"){
      new_data <- level_table$new_level[match(original_data, level_table$level)]
    }
    if (nvar=="multiple"){
      new_data <- level_table$new_level[match(original_data$merged, level_table$level)]
    }
  }
  
  if (recode_mode == "logical_arguments"){
    # recode the data according to prioritised feature
    for (i in seq_len(nrow(level_table))){
      original_data[original_data$Language_ID %in% dplyr::filter(original_data,
                                                                 eval(parse(text=level_table$level[i])))$Language_ID,"merged"]<-level_table$new_level[i]
    }
    # make "other" state become "?" if applicable
    original_data[is.na(original_data)]<-"?"
    new_data <- original_data$merged
  }
  return(new_data)
}
