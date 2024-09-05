#install.packages("remotes")
#library(remotes)

#remotes::install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#remotes::install_version("ape", version = "5.8", repos = "http://cran.us.r-project.org")
library(ape)

#remotes::install_version("R.utils", version = "2.12.3", repos = "http://cran.us.r-project.org")
library(R.utils)

#remotes::install.packages("devtools")
library(devtools)

#remotes::install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#devtools::install_github("HedvigS/rgrambank", ref = "v1.0")
library(rgrambank)

#fecthing the global world tree from:
#Bouckaert, R., Redding, D., Sheehan, O., Kyritsis, T., Gray, R., Jones, K. E., & Atkinson, Q. (2022, July 20). Global language diversification is linked to socio-ecology and threat status. https://doi.org/10.31235/osf.io/f8tr6

if(!file.exists("fixed/global-language-tree-MCC-labelled.tree")){
utils::download.file("https://github.com/rbouckaert/global-language-tree-pipeline/releases/download/v1.0.0/global-language-tree-MCC-labelled.tree.gz", destfile = "fixed/global-language-tree-MCC-labelled.tree.gz")

R.utils::gunzip(filename = "fixed/global-language-tree-MCC-labelled.tree.gz")
}

#reading in tree
tree <- ape::read.nexus("fixed/global-language-tree-MCC-labelled.tree")

cat(paste("The original tree has ", ape::Ntip(tree), " tips.\n"))

TaxonTable <- data.frame(taxon= tree$tip.label,
                            Glottocode = tree$tip.label %>% substr(1, 8)) 

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

GlottologLanguageTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(Glottocode, Language_level_ID = Language_ID)

#pruning
#for the global tree from Bouckaert has no duplicate tip labels

pruned_tree <- rgrambank::drop_duplicate_glottocode_tips(tree = tree, 
                                              merge_dialects = FALSE, 
                                                 TaxonTable = TaxonTable, 
                                                 GlottologLanguageTable = GlottologLanguageTable, 
                                                 rename_tips_to_glottocodes = TRUE)

cat(paste("The pruned tree has ", Ntip(pruned_tree), " tips.\n"))

pruned_tree <- rgrambank::drop_duplicate_glottocode_tips(tree = tree, merge_dialects = TRUE, 
                                                 TaxonTable = TaxonTable, 
                                                 GlottologLanguageTable = GlottologLanguageTable, 
                                                 rename_tips_to_glottocodes = FALSE)

cat(paste("The pruned tree has ", Ntip(pruned_tree), " tips.\n"))
