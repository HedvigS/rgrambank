#' Combines ValueTable and LanguageTable
#'
#' @param LanguageTable data.frame
#' @param ValueTable data.frame
#' @param Is_Glottolog logical If TRUE, then Language_ID in the LanguageTable is renamed to "Language_level_ID" to reduce confusion.
#' @return Data-frame with information from Glottolog's LanguageTable and ValueTabled joined
#' @note All information in the ValueTable besides Parameter_ID, Language_ID and Value are disregarded. For example, for glottolog-cldf, "Comment" and "Source" are dropped. If Is_Glottolog is TRUE, then the column called "Language_ID" in LanguageTable is renamed to "Language_level_ID" to reduce confusion with foreign keys in other tables. In the resulting data-frame, the column "ID" in LanguageTable is represented in "Language_ID".
#' @return A data-frame that combines information from glottolog-cldf ValueTable and LanguageTable. See note for details on modifications.
#' @author Hedvig Skirgård
#' @export

combine_ValueTable_LanguageTable <- function(
    LanguageTable = NULL,      
    ValueTable = NULL,
    Is_Glottolog = FALSE){
  
if( Is_Glottolog == TRUE){
  LanguageTable <- LanguageTable %>% 
    dplyr::rename(Language_level_ID = Language_ID)
  }
  
  ValueTable_wide <- ValueTable %>% 
    reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value")
  
joined <- LanguageTable %>% 
    dplyr::rename(Language_ID = ID) %>% 
    full_join(ValueTable_wide, by = "Language_ID") 
    
  
joined
  }