#' Reduce duplicates which have the same glottcode in a CLDF-datset ValueTable to one. Simplifies combination of datasets. Can also be applied to word-lists FormTable, with some adjustments (Value = Cognacy).
#'
#' @param ValueTable data-frame, long format. ValueTable from cldf.
#' @param LanguageTable data-frame of a cldf LanguageTable from the same cldf-dataset as ValueTable. Needs to minimally have the columns "ID" (for matching to ValueTable) and "Glottocode" (for identification of duplicates).
#' @param merge_dialects logical. In the case of multiple dialects of the same language, if TRUE they are replaced by the glottocode of their language-parent and all but one is dropped according to the merge method specified, as with other duplicate glottocodes.
#' @param GlottologLanguageTable data-frame. If merge_dialects is TRUE and LanguageTable does not have the column  "Language_level_ID", then the function will need an additional LanguageTable with the necessary columns and it should be supplied here. Needs to minimally have the columns "Glottocode" and "Language_level_ID". Glottolog-cldf LanguageTable recommended (requires renaming Language_ID -> Language_level_ID). The output of the function "combine_Glottolog_ValueTable_LanguageTable" is ideal.
#' @param method character vector, choice between "singular_least_missing_data", "combine_random", "singular_random". combine_random = combine all datapoints for all the dialects/duplicates and if there is more than one datapoint for a given feature/word/variable choose uniformly between the values across all entries, singular_random = choose one entry randomly between the dialects/duplicates, singular_least_missing_data = choose the dialect/duplicate which has the least missing values.
#' @param treat_question_mark_as_missing logical. If TRUE, values which are ? are treated as missing.
#' @param replace_missing_language_level_ID logical. If TRUE and there is a missing value in the column Language_level ID, the Glottocode value is filled in. If FALSE, it remains missing (highly discouraged). Only relevant if merge_dialects is TRUE.
#' @author Hedvig Skirgård
#' @description
#' This function takes a CLDF ValueTable and reduces it down to only entries with unique Glottocodes. If there are dialects of the same language, merge_dialects can be set to TRUE and then they are also treated as duplicates and reduced in the same manner as method specifies.
#' @note
#'  treat_question_mark_as_missing is set to TRUE by default, that means that '?' values are turned into NA.
#' @return data-frame of ValueTable without duplicates
#' @export
#'

# ValueTable <- readr::read_csv("https://github.com/cldf-datasets/apics/raw/master/cldf/values.csv")
# ValueTable <- readr::read_csv("https://github.com/cldf-datasets/wals/raw/master/cldf/values.csv")
# LanguageTable <- readr::read_csv("https://github.com/cldf-datasets/apics/raw/master/cldf/languages.csv")
# GlottologLanguageTable <-readr::read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv")

# cldf <- rcldf::cldf("tests/testthat/fixtures/testdata/StructureDataset-metadata.json")
# ValueTable = cldf$tables$ValueTable
# LanguageTable = cldf$tables$LanguageTable

# method = "singular_least_missing_data"
# merge_dialects = FALSE

reduce_ValueTable_to_unique_glottocodes <- function(
                              ValueTable = NULL,
                              LanguageTable = NULL,
                              merge_dialects = TRUE,
                              GlottologLanguageTable = NULL,
                              method = c("singular_least_missing_data", "combine_random", "singular_random"),
                              replace_missing_language_level_ID = TRUE,
                              treat_question_mark_as_missing = TRUE
                              ) {

    if (!(method %in% c("singular_least_missing_data", "combine_random", "singular_random"))) {
        stop("Method of merging is not defined Has to be singular_least_missing_data, combine_random or singular_random.")
    }

  
  ## Check necessary arguments
  
  if(is.null(LanguageTable)){
    stop("LanguageTable not supplied.")
    
      }
  
  if(is.null(ValueTable)){
    stop("ValueTable not supplied.")
    
  }
  
  
## Check necessary columns in ValueTable

    if (!all(c('Language_ID', 'Parameter_ID', 'Value') %in% colnames(ValueTable))) {
        stop("Invalid table format - ValueTable needs to have columns Language_ID, Parameter_ID & Value.")
    }

  if (!all(c('ID', 'Glottocode') %in% colnames(LanguageTable))) {
    stop("Invalid table format - LanguageTable needs to have columns ID and Glottocode.")
  }

multiple_values_per_parameter <- ValueTable %>%
        dplyr::distinct() %>%
        dplyr::group_by(Language_ID, Parameter_ID) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n > 1) %>%
    nrow()

#in cases like with APiCS there could be more than one value for the same language and parameter to represent distributions of values.
if(multiple_values_per_parameter > 1){

    if(!all(c("Value", "Frequency", "ID", "Code_ID", "Confidence", "Example_ID") %in% colnames(ValueTable))){
        stop("There is more than one Value per Language_ID and Parameter_ID in ValueTable, but the columns Code_ID, ID, Value, Confidence, Frequency and Example_ID are not all there.")
    }

    message("Found more than one Value per Language_ID and Parameter_ID. Collapsing, will unnest at the end. May take a little big longer.")
    ValueTable  <-   ValueTable %>%
        dplyr::group_by(Language_ID, Parameter_ID) %>%
        dplyr::mutate(Value = stringr::str_split(paste0(Value, collapse = ";"), pattern = ";"),
                      Frequency = stringr::str_split(paste0(Frequency, collapse = ";"), pattern = ";"),
                      ID = stringr::str_split(paste0(ID, collapse = ";"), pattern = ";"),
                      Code_ID = stringr::str_split(paste0(Code_ID, collapse = ";"), pattern = ";"),
                      Confidence = stringr::str_split(paste0(Confidence, collapse = ";"), pattern = ";"),
                      Example_ID = stringr::str_split(paste0(Example_ID, collapse = ";"), pattern = ";")) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
    }

if(treat_question_mark_as_missing == TRUE){
  ValueTable <- ValueTable %>% 
    mutate(Value = ifelse(Value == "?", NA, Value))
}

## Check if LanguageTables are able to be used for merging dialects (if merge_dialects == TRUE) and set-up LanguageTable for use later.
if(merge_dialects == TRUE){

  if(!"Language_level_ID" %in% colnames(LanguageTable)){
    GlottologLanguageTable <- GlottologLanguageTable %>%
      dplyr::distinct(Glottocode, Language_level_ID)
      
    LanguageTable <- LanguageTable %>% 
      full_join(GlottologLanguageTable, by = "Glottocode")
  }

if(replace_missing_language_level_ID == TRUE){
    # if there is a missing language level ID, which it can be in some datasets where only
    # dialects get language level IDs and languages and families don't, then replace those
    # with the content in the Glottocode column.
    LanguageTable   <- LanguageTable %>%
        dplyr::mutate(Language_level_ID = ifelse(
            is.na(Language_level_ID) | Language_level_ID == "", Glottocode, Language_level_ID)
        )
}

# Still in the merge_dialect == TRUE if loop
    # Replacing the col glottocode with Language_level_ID merges dialects for the rest of the duplicate pruning
        LanguageTable <- LanguageTable %>%
        dplyr::select(-Glottocode) %>% 
        dplyr::select(Language_ID = ID, Glottocode = Language_level_ID)

}

if(merge_dialects == FALSE){
    LanguageTable <- LanguageTable %>%
        dplyr::select(Language_ID = ID, Glottocode)

    }

    if (method == "singular_least_missing_data") {
      
      ## PICK THE ONE ENTRY WHEN DUPLICATE GLOTTOCODES THAT HAS THE LEAST MISSING DATA
      
        lgs <- ValueTable %>%
            dplyr::filter(!is.na(Value)) %>%
            dplyr::left_join(LanguageTable, by = "Language_ID") %>%
            dplyr::group_by(Language_ID) %>%
            dplyr::mutate(n = dplyr::n()) %>%
            dplyr::arrange(desc(n)) %>%
            dplyr::ungroup() %>%
            dplyr::distinct(Glottocode, .keep_all = T) %>%
            distinct(Language_ID)

        levelled_ValueTable <- ValueTable %>% 
          inner_join(lgs, by = "Language_ID") %>% 
          dplyr::left_join(LanguageTable, by = "Language_ID") 
          

    } 

    if (method == "combine_random") {
      # MERGE BY MAKING A FRANKENSTEIN COMBINATION OF ALL DUPLICATE GLOTTOCODES
        ValueTable_grouped <- ValueTable %>%
            dplyr::filter(!is.na(Value)) %>%
            dplyr::left_join(LanguageTable, by = "Language_ID",
                      relationship = "many-to-many") %>%
            dplyr::group_by(Glottocode, Parameter_ID) %>%
            dplyr::mutate(n = dplyr::n()) %>%
            dplyr::ungroup() 

        # it's faster if we do slice_sample (choose randomly) only on those that have more than 1
        # value per language rather than on all duplicate rows.
        ValueTable_long_n_greater_than_1 <- ValueTable_grouped %>%
            dplyr::filter(n > 1) %>%
            dplyr::group_by(Glottocode, Parameter_ID) %>% 
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup()

        levelled_ValueTable <- ValueTable_grouped %>% 
            dplyr::filter(n == 1) %>%
          suppressMessages( dplyr::full_join(ValueTable_long_n_greater_than_1)) %>%
            dplyr::select(-n) 

    # MERGE BY PICKING DIALECTS WHOLLY AT RANDOM
    } 

    if (method == "singular_random") {
      lgs  <- LanguageTable %>%
            dplyr::group_by(Glottocode) %>%
            dplyr::slice_sample(n = 1) %>%
        ungroup() %>% 
        distinct(Language_ID, .keep_all = T) 
      
    levelled_ValueTable <- ValueTable %>% 
      inner_join(lgs, by = "Language_ID") 

    } 


if(multiple_values_per_parameter > 1){
    levelled_ValueTable <-     levelled_ValueTable %>%
    unnest(cols = c("Value", "Frequency", "ID", "Code_ID", "Confidence", "Example_ID"))
}

levelled_ValueTable
}

