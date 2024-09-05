#' Adds info about isolates to tables of languages with glottocodes.
#'
#' @param LanguageTable data-frame. If any of "Is_Isolate", "Level", "Family_ID", Glottolog_ValueTable_LanguageTable needs to be supplied.
#' @param Glottolog_ValueTable_LanguageTable data-frame with extra information in case LanguageTable lacks any of the following columns:"Is_Isolate", "Level", "Family_ID".
#' @param mark_isolate_dialects_as_isolates logical. If TRUE, dialects of Isolates are also tagged as TRUE in the column Is_Isolate
#' @param set_isolates_Family_ID_as_themselves logical. If TRUE, the Family_ID of isolates are populated by their Glottocode.
#' @return Data-frame with desired modifications.
#' @note If Glottolog_ValueTable_LanguageTable is used, then the following columns are overwritten (if they exists) in LanguageTable: level (changed to "Level"), Family_ID and "Is_Isolate". If the LanguageTable is generated using information from a different version of Glottolog than Glottolog_ValueTable_LanguageTable, mismatches can happen in at least the following columns: lineage, subclassification and classification. For example, in Grambank v1 fuyu1242 appears as a member of the family goli1242, but in Glottolog v5 this language is instead treated as an isolate.
#' @author Hedvig Skirgård
#' @export

add_isolate_info <- function(LanguageTable = NULL,
                             Glottolog_ValueTable_LanguageTable = NULL,
                             mark_isolate_dialects_as_isolates = TRUE, 
                             set_isolates_Family_ID_as_themselves = TRUE
        ){
  
  if(!all(c("Is_Isolate", "Level", "Family_ID")   %in% colnames(LanguageTable)) &
     is.null(Glottolog_ValueTable_LanguageTable)){
    stop("LanguageTable lacks necessary columns and Glottolog_ValueTable_LanguageTable is not defined.")
        }
  
  lgs_in_input <- LanguageTable$ID

   if(!is.null(Glottolog_ValueTable_LanguageTable)){
    Glottolog_ValueTable_LanguageTable <- Glottolog_ValueTable_LanguageTable %>% 
      dplyr::select(Family_ID, Level, Glottocode, Language_level_ID, Is_Isolate)
    
    LanguageTable <- LanguageTable %>% 
      dplyr::select(-any_of(c("Family_ID", "level", "Level", "Language_level_ID", "Language_ID"))) %>% 
      full_join(Glottolog_ValueTable_LanguageTable, by = "Glottocode")
    }
  
    if(mark_isolate_dialects_as_isolates == TRUE){
        LanguageTable <- LanguageTable %>% 
            dplyr::mutate(Is_Isolate = ifelse(Family_ID == Language_level_ID & Level == "dialect",
                                           TRUE, Is_Isolate)) 
        
    }

    if(set_isolates_Family_ID_as_themselves == TRUE){
          LanguageTable <- LanguageTable %>%
            dplyr::mutate(Family_ID = ifelse(is.na(Family_ID)|
                                                 Family_ID == "" & Level == "language",
                                             yes = Glottocode, no = Family_ID)) 
    }

LanguageTable %>% 
  filter(ID %in% lgs_in_input) 
}






