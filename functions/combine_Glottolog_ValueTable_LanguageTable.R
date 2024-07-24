#' Adds info about isolates to tables of languages with glottocodes.
#'
#' @param Glottolog_LanguageTable data.frame
#' @param Glottolog_ValueTable data.frame
#' @return Data-frame with information from Glottolog's LanguageTable and ValueTabled joined
#' @note The information in "Comment" and "Source" from the ValueTable is not retained.
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