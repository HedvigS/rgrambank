#' Computes scores based on theoretical linguistics on grambank data.
#'
#' @param ValueTable data-frame, long format, of Grambank values. If not already binarised, make_binary_ValueTable() will be applied.
#' @param ParameterTable data-frame of Grambank ParameterTable. . If not already binarised, make_binary_ParameterTable will be applied.
#' @param missing_cut_off numeric value between 0 and 1 representing cut-off for how much coverage each language should have, for each feature set. For each set of features for the theoretical scores, if a language falls under the threshold, it is not considered for the theoretical score (but may be considered for other sets). 0.75 means that languages with 75% of feature values non-missing for that set of features are included, less than 75% coverage are dropped.
#' @param Fusion_option Character vector: "count_zero_half_and_one", "count_one_only" or "count_one_and_half". The features in the ParameterTable are assigned Fusion weights of 0 (pertains to free-marking), 1 (pertains to bound marking) and half (could be bound, affixal or other). Users can choose approach in how these contribute to the fusion score. If you choose "count_zero_half_and_one" then features assigned as 0 will be reversed, i.e. free-marking with contribute negatively to the fusion-score. Default is "count_one_and_half".
#' @importFrom dplyr select mutate group_by summarise filter
#' @author Hedvig Skirgård and Hannah Haynie and Olena Shcherbakova
#' @return A data-frame with theoretical scores per language.
#' @export

make_theo_scores <- function(ValueTable,
                             ParameterTable,
                             missing_cut_off = 0.75, 
                             Fusion_option = "count_one_and_half"){

  #we need the parameter and value table to be binarised, so we check if it is by looking for the existence of a known binarised feature
if(!"GB203b" %in% ValueTable$Parameter_ID){
        ValueTable <- ValueTable %>% make_binary_ValueTable
}

if(!"GB203b" %in% ParameterTable$ID){
    ParameterTable <- ParameterTable  %>% make_binary_ParameterTable()
} 
  
  if(  !(Fusion_option %in% c("count_zero_half_and_one", "count_one_only", "count_one_and_half"))){
    stop("Fusion_option has to be one of count_zero_half_and_one, count_one_only or count_one_and_half.")
    
  }
    

    #read in sheet with scores for whether a feature denotes fusion
    ParameterTable <- ParameterTable %>%
        dplyr::select(Parameter_ID = ID, Fusion = Boundness, Informativity, Locus_of_Marking, Word_Order, Gender_or_Noun_Class, Flexivity) %>%
        dplyr::mutate(Fusion = as.numeric(.data[["Fusion"]])) %>%
        dplyr::mutate(Gender_or_Noun_Class = as.numeric(.data[["Gender_or_Noun_Class"]])) %>%
        dplyr::mutate(Flexivity = as.numeric(.data[["Flexivity"]])) %>%
        dplyr::mutate(Locus_of_Marking = as.numeric(.data[["Locus_of_Marking"]])) %>%
        dplyr::mutate(Word_Order = as.numeric(.data[["Word_Order"]]))

    if(Fusion_option == "count_one_and_half") {
      n_fusion_feats <- sum(ParameterTable$Fusion == 1, na.rm = T) +  
        sum(ParameterTable$Fusion == 0.5, na.rm = T) 
          }

    if(Fusion_option == "count_one_only" ) {
      n_fusion_feats <- sum(ParameterTable$Fusion == 1, na.rm = T) 
    }

    if(Fusion_option == "count_zero_half_and_one" ) {
      n_fusion_feats <- sum(ParameterTable$Fusion == 1, na.rm = T) +
        sum(ParameterTable$Fusion == 0, na.rm = T) +
        sum(ParameterTable$Fusion == 0.5, na.rm = T) 
    }
    
    n_informativity_feats <- length(ParameterTable$Informativity %>% na.omit())
    n_gender_NC_feats <- length(ParameterTable$Gender_or_Noun_Class %>% na.omit())
    n_flexivity_feats <- length(ParameterTable$Flexivity %>% na.omit())
    n_locus_marking_feats <- length(ParameterTable$Locus_of_Marking %>% na.omit())
    n_word_order_feats <- length(ParameterTable$Word_Order %>% na.omit())
    
    if(any(n_fusion_feats == 0, 
        n_informativity_feats == 0, 
        n_gender_NC_feats == 0, 
        n_flexivity_feats == 0, 
        n_locus_marking_feats == 0, 
        n_word_order_feats == 0
    ) ){
      stop("There is something wrong with the ParameterTable, some of the columns for the metric weights are empty. ")
          }
    
    ValueTable <- ValueTable %>%
        dplyr::inner_join(ParameterTable , by = "Parameter_ID", relationship = "many-to-many") %>%
        dplyr::filter(!is.na(.data[["Value"]])) %>%
        dplyr::filter(Value != "?") %>%
        dplyr::mutate(Value = as.numeric(.data[["Value"]]))  #makes it possible to sum, mean etc

    #fusion counts
    
    if(Fusion_option == "count_one_and_half") {
    Fusion_df <- ValueTable %>%
        dplyr::filter(!is.na(.data[["Fusion"]])) %>%
        dplyr::filter(.data[["Fusion"]] != 0) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_fusion_feats * missing_cut_off) %>%
        dplyr::mutate(Value_weighted = ifelse(.data[["Fusion"]] == 0.5 & 
                                                      .data[["Value"]] == 1, 
                                                    yes = 0.5, 
                                                    no = .data[["Value"]] )) # replacing all instances of 1 for a feature that is weighted to 0.5 bound morph points to 0.5 
      }
      
      if(Fusion_option == "count_one_only") {
        Fusion_df <- ValueTable %>%
          dplyr::filter(!is.na(.data[["Fusion"]])) %>%
          dplyr::filter(.data[["Fusion"]] == 1) %>%
          dplyr::group_by(.data[["Language_ID"]]) %>%
          dplyr::mutate(n = dplyr::n()) %>%
          dplyr::filter(.data[["n"]] >= n_fusion_feats * missing_cut_off) %>% 
          dplyr::rename(Value_weighted = Value)
        }
          
    if(Fusion_option == "count_zero_half_and_one") {
      Fusion_df <- ValueTable %>%
        dplyr::filter(!is.na(Fusion)) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_fusion_feats * missing_cut_off) %>% 
        dplyr::mutate(Value_weighted = ifelse(.data[["Fusion"]] == 0.5 & .data[["Value"]] == 1, 
                                              yes = 0.5, no = .data[["Value"]])) %>%  # replacing all instances of 1 for a feature that is weighted to 0.5 bound morph points to 0.5 
        dplyr::mutate(value_weighted = if_else(.data[["Fusion"]] == 0, 
                                               yes = abs(.data[["value"]]-1), 
                                               no = .data[["value_weighted"]])) # reversing the values of the features that refer to free-standing markers 
    }
    
      lg_df_for_fusion_count <- Fusion_df %>% 
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::summarise(Fusion = mean(.data[["Value_weighted"]]))

    ##Flexivity scores
    lg_df_for_flex_count <- ValueTable  %>%
        dplyr::filter(!is.na(Flexivity)) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_flexivity_feats * missing_cut_off) %>%
        # reversing the Values of the features that have a score of 0
        dplyr::mutate(Value_weighted = ifelse(.data[["Flexivity"]] == 0, 
                                              yes = abs(.data[["Value"]]-1), 
                                              no = .data[["Value"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::summarise(Flexivity = mean(.data[["Value_weighted"]]), .groups = "drop")

    ##`locus of marking`s
    lg_df_for_HM_DM_count <- ValueTable %>%
        dplyr::filter(!is.na(.data[["Locus_of_Marking"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_locus_marking_feats * missing_cut_off) %>%
        # reversing the Values of the features that have a score of 0
        dplyr::mutate(Value_weighted = ifelse(.data[["Locus_of_Marking"]] == 0, 
                                              yes = abs(.data[["Value"]]-1), 
                                              no = .data[["Value"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::summarise(Locus_of_Marking = mean(.data[["Value_weighted"]]), 
                         .groups = "drop_last")

    ##Gender_or_Noun_Class scores
    lg_df_for_gender_nc_count <- ValueTable  %>%
        dplyr::filter(!is.na(.data[["Gender_or_Noun_Class"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_gender_NC_feats * missing_cut_off) %>%
        # reversing the Values of the features that have a score of 0
        dplyr::mutate(Value_weighted = ifelse(.data[["Gender_or_Noun_Class"]] == 0, 
                                              yes = abs(.data[["Value"]]-1), 
                                              no = .data[["Value"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::summarise(Gender_or_Noun_Class = mean(.data[["Value_weighted"]]), 
                         .groups = "drop_last")

     ##OV_VO scores
     lg_df_for_OV_VO_count <- ValueTable  %>%
        dplyr::filter(!is.na(.data[["Word_Order"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_word_order_feats * missing_cut_off) %>%
        dplyr::mutate(Value_weighted = ifelse(.data[["Word_Order"]] == 0, 
                                              yes = abs(.data[["Value"]]-1), 
                                              no = .data[["Value"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::summarise(Word_Order = mean(.data[["Value_weighted"]]), 
                         .groups = "drop_last")

    ##informativity score
    lg_df_informativity_score <-  ValueTable  %>%
        dplyr::filter(!is.na(.data[["Informativity"]])) %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data[["n"]] >= n_informativity_feats * missing_cut_off) %>%
        # reversing GB140 because 0 is the informative state
        dplyr::mutate(Value = ifelse(.data[["Parameter_ID"]] == "GB140", 
                                     yes = abs(.data[["Value"]]-1), 
                                     no = .data[["Value"]])) %>%
        #grouping per language and per informativity category
        dplyr::group_by(.data[["Language_ID"]], .data[["Informativity"]]) %>%
        # for each informativity category in each language,
        # .. how many are answered 1 ("yes")
        # how many of the Values per informativity category are missing
        dplyr::summarise(
            sum_informativity = sum(.data[["Value"]]), 
            sum_na = sum(is.na(.data[["Value"]])), 
            .groups = "drop_last" ) %>%
        # if there is at least one NA and the sum of Values for the entire category is 0, the
        # informativity score should be NA because there could be a 1 hiding under the NA Value.
        dplyr::mutate(sum_informativity = ifelse(.data[["sum_na"]] >= 1 & .data[["sum_informativity"]] == 0, 
                                                 yes = NA, no = .data[["sum_informativity"]])) %>%
        dplyr::mutate(informativity_score = ifelse(.data[["sum_informativity"]] >= 1, 
                                                   yes = 1, 
                                                   no = .data[["sum_informativity"]])) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data[["Language_ID"]]) %>%
        dplyr::summarise(Informativity = mean(.data[["informativity_score"]]), .groups = "drop_last")

    lg_df_for_OV_VO_count %>%
        dplyr::full_join(lg_df_for_flex_count, by = "Language_ID") %>%
        dplyr::full_join(lg_df_for_gender_nc_count, by = "Language_ID") %>%
        dplyr::full_join(lg_df_for_HM_DM_count, by = "Language_ID") %>%
        dplyr::full_join(lg_df_for_fusion_count, by = "Language_ID") %>%
        dplyr::full_join(lg_df_informativity_score, by = "Language_ID")
}