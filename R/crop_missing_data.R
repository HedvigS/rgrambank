#' Crops ValueTables for features and languages with a large amount of missing data.
#'
#' @param ValueTable Data-frame of the CLDF-type VaueTable, i.e. a long data-table with at least columns "Parameter_ID", "Value" and "Language_ID". This ValueTable can be the product of make_binary_ValueTable() and/or reduce_ValueTable_to_unique_glottocodes.
#' @param cut_off_parameters Integer between 0 and 1 representing the cut-off for missing data for features. 0.75 means that features that are filled out for less than 75% of the possible languages are dropped from the result.
#' @param cut_off_languages Integer between 0 and 1 representing the cut-off for missing data for languages. 0.75 means that features that are filled out for less than 75% of the possible features are dropped from the result.
#' @param turn_question_mark_into_NA Logical. If TRUE Value cells consisting of "?" are treated the same as missing data.
#' @param verbose  Logical. If TRUE, the function reports on the number of languages, features and percentage of missing data before and after cropping.
#' @return A data-frame of the long ValueTable type where features and languages that don't meet the cut-off for missing data are removed.
#' @note
#' The cut-offs are defined by the missing data in the full dataset and can be set to any value between 0 and 1. The pruning is not stepwise, i.e. it is not the case that parameters are pruned first and then languages based on the missingness after the first pruning. This can be a practical step before imputation as it reduces missing data to be imputed, but should be used thoughtfully. For more advanced approaches, please see [annagraff/densify](https://github.com/annagraff/densify).
#' @author Hedvig Skirgård
#' @export

crop_missing_data <- function(ValueTable,
                              cut_off_parameters = 0.75,
                              cut_off_languages = 0.75,
                              turn_question_mark_into_NA = TRUE,
                              verbose = TRUE){

if(turn_question_mark_into_NA == TRUE){
    ValueTable <- ValueTable %>%
        filter(Value != "?")
}
if(verbose == TRUE){
    n_lgs <- length(unique(ValueTable$Language_ID))
    n_feats <- length(unique(ValueTable$Parameter_ID))
    theoretical_max_data_points <- n_lgs * n_feats
    n_data_points <- ValueTable %>% nrow()

    coverage_before_cropping <- n_data_points / theoretical_max_data_points

    cat(paste0("Before cropping, the data-set has ",
               round(1 - coverage_before_cropping, digits = 2)*100,
               "% missing data, ",
               format(n_lgs, big.mark = ","),
               " languages and ",
               n_feats,
               " features.\n"))
}

ValueTable_cropped <- ValueTable %>%
    filter(!is.na(Value)) %>%
    group_by(Language_ID) %>%
    mutate(Parameters_filled_for_language = n()) %>%
    group_by(Parameter_ID) %>%
    mutate(Languages_filled_for_parameter = n()) %>%
    filter(Languages_filled_for_parameter > n_lgs*cut_off_parameters) %>%
    filter(Parameters_filled_for_language > n_feats*cut_off_languages)

if(verbose == TRUE){

    n_lgs_cropped <- length(unique(ValueTable_cropped$Language_ID))
    n_feats_cropped <- length(unique(ValueTable_cropped$Parameter_ID))
    theoretical_max_data_points_cropped <- n_lgs_cropped * n_feats_cropped
    n_data_points_cropped <- ValueTable_cropped %>% nrow()

    coverage_after_cropping <- n_data_points_cropped / theoretical_max_data_points_cropped

    cat(paste0("After cropping, the data-set has ",
               round(1 - coverage_after_cropping ,
                     digits = 2)*100,
               "% missing data, ", format(n_lgs_cropped, big.mark = ","), " languages and ", n_feats_cropped, " features.\n"))
}

ValueTable_cropped
}



