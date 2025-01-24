#remotes::install_github("Hedvigs/rgrambank", ref = "4203614472682683a7670c60d994d9ec7de2c1b1")
library(rgrambank)
library(tidyverse)
library(testthat)
library(data.table)
library(reshape2)
#remotes::install_github("annagraff/densify")
library(densify)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

ValueTable <- GB_rcldf_obj$tables$ValueTable

GBI <- rgrambank::make_GBI(ValueTable = ValueTable)

# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

Glottolog_ValueTable <- glottolog_rcldf_obj$tables$ValueTable 


###densify