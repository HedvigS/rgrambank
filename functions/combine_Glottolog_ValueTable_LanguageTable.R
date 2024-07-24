#' Combines Glottolog-cldf ValueTable and LanguageTable, in an opinionated manner.
#'
#' @param Glottolog_LanguageTable data.frame
#' @param Glottolog_ValueTable data.frame
#' @return Data-frame with information from Glottolog's LanguageTable and ValueTabled joined
#' @note The information in "Comment" and "Source" from the ValueTable is not retained in the output data-frame. The column called "Language_ID" in LanguageTable is renamed to "Language_level_ID" to reduce confusion with foreign keys in other tables. The column ID in LanguageTable is renamed "Language_ID".
#' @return A data-frame that combines information from glottolog-cldf ValueTable and LanguageTable. See note for details on modifications.
#' @author Hedvig Skirgård
#' @export

combine_Glottolog_ValueTable_LanguageTable <- function(
    Glottolog_LanguageTable = NULL,      
    Glottolog_ValueTable = NULL){
  
  ValueTable_wide <- Glottolog_ValueTable %>% 
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value")
  
joined <- Glottolog_LanguageTable %>% 
    dplyr::rename(Language_level_ID = Language_ID, Language_ID = ID) %>% 
    full_join(ValueTable_wide, by = "Language_ID") 
    
  
joined
  }