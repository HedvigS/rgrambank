#' If a language tree has tips with duplicate tip labels, drop all but one at random. If there are tips which are dialects of the same language, you can choose to drop all but one.
#'
#' @param tree 	an object of class "phylo". Tip-labels need to be unique within the tree, but can represent duplicates of Glottocodes.
#' @param merge_dialects a logical specifying whether to replace dialect tip labels with the glottocode of the language that is their parent, and then drop all but one
#' @param LanguageTable data-frame of tip-labels matched to Glottocodes.
#' @param LanguageTable2 data-frame of Glottocodes matched to Language_level_ID. If merge-dialects is TRUE and LanguageTable lacks the column 'Language_level_ID', LanguageTable2 needs to be specified.
#' @param trim_tip_label_to_first_eight a logical specifying whether we should first trim the tip labels to the first 8 characters in the output tree. If the tip labels contain more than just glottocodes, it is necessary to trim away the rest. If set to TRUE, only the first 8 characters are retained. This is convenient for the global tree from Bouckaert et al (2023) where the tip labels are of the style "cent2004_CentralBai_Sino-Tibetan". If the tip labels already only consist of glottocodes, this parameter can be set to TRUE or FALSE without a difference.
#' @return tree without tips with duplicate Glottocodes, optionally also merged dialects.
#' @author Hedvig Skirgård
#' @export

reduce_tree_to_unique_glottocodes <- function(tree = NULL,
                                      merge_dialects = TRUE,
                                      LanguageTable = NULL,
                                      LanguageTable2 = NULL, 
                                      trim_tip_label_to_first_eight = TRUE){

    #tree <- ape::read.tree("tests/testthat/fixtures/example_tree.tree")
    #LanguageTable <- read.delim("tests/testthat/fixtures/taxa.csv", sep = ",")

  #check that all tip-labels also occur in LanguageTable
if(!ape::Ntip(tree) == tree$tip.label %in% LanguageTable$taxon %>% sum() ){
  stop("There are tips in the tree that cannot be matched to an entry in LanguageTable.")
  }

if(tree$tip.label %>% unique() %>% length() != ape::Ntip(tree)){
  stop("Tip-labels are not unique.")
  }

  if(!"Glottocode" %in% colnames(LanguageTable)){
    stop("LanguageTable lacks the column 'Glottocode'.\n")
  }
  

if(merge_dialects == TRUE){

if(all(!"Language_level_ID" %in% colnames(LanguageTable),
   !"Language_level_ID" %in% colnames(LanguageTable2)) ){

      stop("Neither LanguageTable or LanguageTable2 has the column 'Language_level_ID' which is necessary for merging dialects.\n")
}

if(!("Glottocode" %in% colnames(LanguageTable)  &  "Language_level_ID" %in% colnames(LanguageTable))){
stop("LanguageTable does not contain all the necessary columns: ´Glottocode' and 'Language_level_ID'.")
    }

  if(!"Language_level_ID" %in% colnames(LanguageTable)){
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
    dplyr::select(Language_ID = ID, Glottocode = Language_level_ID)
  
}


#keeping just one tip per unique glottocode tip label in the entire tree. Anytime where there are duplicate tip labels, only one tip is kept. Selection is random.
to_keep <- tree$tip.label %>%
              as.data.frame() %>%
    dplyr::rename(tip.label = ".") %>%
    dplyr::group_by(tip.label) %>%
    dplyr::mutate(n = n()) %>% 
    dplyr::slice_sample(n = 1)

tree <- ape::keep.tip(tree, tip = to_keep$tip.label)

#if the tip labels has glottocodes as the first 8 characters and then something else, like a name, then prune that off. This is for example true for the EDGE-trees.
if(trim_tip_label_to_first_eight == TRUE){
  tree$tip.label <- tree$tip.label %>% substr(1, 8)
}


tree
}
