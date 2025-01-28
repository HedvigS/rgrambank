#remotes::install_github("Hedvigs/rgrambank")
library(rgrambank)
library(tidyverse)
library(testthat)
library(data.table)
library(reshape2)
#remotes::install_github("annagraff/densify")
library(densify)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

Grambank_ValueTable <- GB_rcldf_obj$tables$ValueTable

GBI <- rgrambank::make_GBI(ValueTable = Grambank_ValueTable)

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

Glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable 

###densify

#checking that it doesn't run when both GBI and Grambank_ValueTable are defined
densify_GB(GBI = GBI, Grambank_ValueTable = Grambank_ValueTable, Glottolog_ValueTable = Glottolog_ValueTable)

#checking that it doesn't run when neither is defined
densify_GB(Glottolog_ValueTable = Glottolog_ValueTable)

#checking that it runs for Grambank_ValueTable
GB_dense <- densify_GB(Grambank_ValueTable = Grambank_ValueTable, Glottolog_ValueTable = Glottolog_ValueTable)

#checking that it runs for GBI
GBI_dense <- densify_GB(GBI = GBI, Glottolog_ValueTable = Glottolog_ValueTable)


