#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_version("ape", version = "5.8", repos = "http://cran.us.r-project.org")
library(ape)

library(R.utils)

#install.packages("devtools")
library(devtools)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)


source("../functions/drop_duplicate_tips_random.R")

#fecthing the global world tree from:
#Bouckaert, R., Redding, D., Sheehan, O., Kyritsis, T., Gray, R., Jones, K. E., & Atkinson, Q. (2022, July 20). Global language diversification is linked to socio-ecology and threat status. https://doi.org/10.31235/osf.io/f8tr6

if(!file.exists("fixed/global-language-tree-MCC-labelled.tree")){
utils::download.file("https://github.com/rbouckaert/global-language-tree-pipeline/releases/download/v1.0.0/global-language-tree-MCC-labelled.tree.gz", destfile = "fixed/global-language-tree-MCC-labelled.tree.gz")

R.utils::gunzip(filename = "fixed/global-language-tree-MCC-labelled.tree.gz")
}

#readin in tree
tree <- ape::read.nexus("fixed/global-language-tree-MCC-labelled.tree")

cat(paste("The original tree has ", Ntip(tree), " tips.\n"))

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

LanguageTable <- glottolog_rcldf_obj$tables$LanguageTable %>% 
  dplyr::select(Glottocode, Language_level_ID = Language_ID)

#pruning
#for the global tree from Bouckaert has no duplicate tip labels
pruned_tree <- drop_duplicate_tips_random(tree = tree, merge_dialects = TRUE, trim_tip_label_to_first_eight = TRUE, LanguageTable = LanguageTable)

cat(paste("The pruned tree has ", Ntip(pruned_tree), " tips.\n"))
