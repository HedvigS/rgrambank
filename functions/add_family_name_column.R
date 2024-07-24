#' Adds a column with the name of the language family.
#'
#' @param LanguageTable data-frame of CLDF table with minimally the columns  "Family_ID", "Name" and "Glottocode".
#' @param Glottolog_ValueTable_LanguageTable data-frame of CLDF-table with additional information on Families in case LanguageTable is lacking information on the names of some/all families.
#' @param verbose logical. If TRUE, function will report on languages not matched to a Family_name.
#' @return data-frame with Family_name column.
#' @note It is necessary that for every unique glottocode in Family_ID there is a row with a Glottocode and Name to match that. If there isn't, languages will have missing values for their Family_name even though they are not isolates.
#'  If The current LanguageTable lacks the required columns, consider using a combination of the LanguageTable and ValueTable of glottolog-cldf.
#' @author Hedvig Skirgård
#' @export

add_family_name_column <- function(LanguageTable = NULL, 
                                   Glottolog_ValueTable_LanguageTable = NULL, 
                                   verbose = TRUE){
  
    if(!all(c("Family_ID", "Name", "Glottocode") %in% colnames(LanguageTable))){
        stop("LanguageTable needs to have all of these columns: Name, Glottocode and Family_ID.")
    }

    if(!is.null(Glottolog_ValueTable_LanguageTable) &
       !all(c("Family_ID", "Name", "Glottocode") %in% colnames(LanguageTable))){
        stop("Glottolog_ValueTable_LanguageTable needs to have all of these columns: Name, Glottocode and Family_ID.")
    }

  lgs_in_input <- LanguageTable$ID
  
    if(!is.null(Glottolog_ValueTable_LanguageTable)){
        Glottolog_ValueTable_LanguageTable <- Glottolog_ValueTable_LanguageTable %>%
            dplyr::select(Name, Glottocode)
        
        LanguageTable_large <- full_join( LanguageTable,  Glottolog_ValueTable_LanguageTable, 
                                    by = c("Name", "Glottocode"))
    }else{
      
      LanguageTable_large <- LanguageTable
      }
  
Family_df <- LanguageTable %>% 
  dplyr::filter(!is.na(Family_ID)) %>% 
  dplyr::distinct(Family_ID) %>% 
  dplyr::rename(Glottocode = Family_ID) %>% 
  dplyr::left_join(dplyr::select(LanguageTable_large, Glottocode, Name), 
                   by = "Glottocode") %>% 
  dplyr::rename(Family_name = Name, Family_ID = Glottocode) 

LanguageTable <- LanguageTable %>% 
  left_join(Family_df,
            by = join_by(Family_ID), relationship = "many-to-many") %>% 
  filter(ID %in% lgs_in_input)
  

    if(NA %in% LanguageTable$Family_name & verbose == TRUE)(

        warning("There was no Family_name found for the following entries. It could be because they are isolates and Family_ID was empty.\n",
                LanguageTable %>%
                    dplyr::filter(is.na(Family_name)) %>%
                    dplyr::select(Name)
                ))

    LanguageTable
}