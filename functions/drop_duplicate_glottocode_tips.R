#' If a language tree has tips with are matched to duplicate glottocodes, drop all but one at random. If there are tips which are dialects of the same language, you can choose to drop all but one.
#'
#' @param tree 	an object of class "phylo". Tip-labels need to be unique within the tree, but can represent duplicates of Glottocodes.
#' @param merge_dialects a logical specifying whether to replace dialect tip labels with the glottocode of the language that is their parent, and then drop all but one
#' @param LanguageTable data-frame of tip-labels matched to Glottocodes. Needs the columns "taxon" and "Glottocode". If merge_dialects == TRUE then the column "Language_level_ID" is also necessary in either LangugeTable or LanguageTable2.
#' @param LanguageTable2 data-frame of Glottocodes matched to Language_level_ID. If merge-dialects is TRUE and LanguageTable lacks the column 'Language_level_ID', LanguageTable2 needs to be specified. requires the columns "Glottocode" and "Language_level_ID".
#' @param rename_tips_to_glottocodes logical. If TRUE, the tip-labels of the output tree are renamed to the corresponding Glottocodes. If FALSE, the original tip-labels are retained.
#' @return tree without tips with duplicate Glottocodes, optionally all but one dialect is dropped as well.
#' @author Hedvig Skirgård
#' @export

drop_duplicate_glottocode_tips <- function(tree = NULL,
                                      merge_dialects = TRUE,
                                      LanguageTable = NULL,
                                      LanguageTable2 = NULL, 
                                      rename_tips_to_glottocodes = TRUE){

    if(!"taxon" %in% colnames(LanguageTable)){
    stop("LanguageTable lacks 'taxon' column.")
  }
  
  #check that all tip-labels also occur in LanguageTable
if(any(!tree$tip.label %in% LanguageTable$taxon)){
  stop("There are tips in the tree that cannot be matched to an entry in LanguageTable.")
  }

if(tree$tip.label %>% unique() %>% length() != ape::Ntip(tree)){
  stop("Tip-labels are not unique. Tips can be matched to duplicate Glottocodes, but the tip-labels need to be unique within the tree.")
  }

  if(!"Glottocode" %in% colnames(LanguageTable)){
    stop("LanguageTable lacks the column 'Glottocode'.\n")
  }
  

if(merge_dialects == TRUE){

if(all(!"Language_level_ID" %in% colnames(LanguageTable),
       is.null(LanguageTable2)) ){
      stop("LanguageTable lacks the column 'Language_level_ID' and LanguageTable2 not specified, which is necessary for merging dialects.\n")
}

if((!"Language_level_ID" %in% colnames(LanguageTable2)) ){
    stop("LanguageTable2 lacks the column 'Language_level_ID', which is necessary for merging dialects.\n")
  }

  #if it is necessary to use LanguageTable 2  
  if(!is.null(LanguageTable2)){
    if(any(!LanguageTable$Glottocode %in% LanguageTable2$Glottocode)){
      stop("There are Glottocodes in LanguageTable that don't occur in LanguageTable2.")
      
    }
    
      LanguageTable2 <- LanguageTable2 %>%
      dplyr::distinct(Glottocode, Language_level_ID)
    
    LanguageTable <- LanguageTable %>% 
      full_join(LanguageTable2, by = "Glottocode")
  }
  
  #some LanguageTables only contain values for Language_level_ID if the languoid is a dialect. Here we insert the language level glottocode if the level is language or family as well.  
  LanguageTable <-    LanguageTable %>%
        dplyr::mutate(Language_level_ID = ifelse(is.na(Language_level_ID) |
                                                     Language_level_ID == "", Glottocode, Language_level_ID))

  # Still in the merge_dialect == TRUE if loop
  # Replacing the col glottocode with Language_level_ID merges dialects for the rest of the duplicate pruning
  LanguageTable <- LanguageTable %>%
    dplyr::select(-Glottocode) %>% 
    dplyr::select(taxon, Glottocode = Language_level_ID)
  }


#keeping just one tip per unique glottocode tip label in the entire tree. Anytime where there are duplicate tip labels, only one tip is kept. Selection is random.
to_keep <- tree$tip.label %>%
              as.data.frame() %>%
    dplyr::rename(taxon = ".") %>%
    left_join(LanguageTable, by = join_by(taxon)) %>% 
    dplyr::group_by(Glottocode) %>%
    dplyr::mutate(n = n()) %>% 
    dplyr::slice_sample(n = 1)

tree <- ape::keep.tip(tree, tip = to_keep$taxon)

if(rename_tips_to_glottocodes == TRUE){

tip_df <- tree$tip.label %>%
  as.data.frame() %>%
  dplyr::rename(taxon = ".") %>%
  left_join(LanguageTable, by = join_by(taxon)) 

tree$tip.label <- tip_df$Glottocode
}

tree
}
