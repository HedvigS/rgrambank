#This is a script that showcases the function get_zenodo. 

#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_version("fs", version = "1.6.3", repos = "http://cran.us.r-project.org")
library(fs)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

source("../functions/get_zenodo.R")

json <- "output/glottolog-cldf_v5/cldf/cldf-metadata.json"

if(!file.exists(json)){
get_zenodo_dir(url = "https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", exdir = "output/glottolog-cldf_v5/")}

Glottolog_rcldf_obj <- rcldf::cldf(json, load_bib = F)

json <- "output/APiCS/cldf/StructureDataset-metadata.json"
if(!file.exists(json)){
get_zenodo_dir(url = "https://zenodo.org/records/7139937/files/apics-v2013.zip", exdir = "output/APiCS/")}

APiCS_rcldf_obj <- rcldf::cldf(json, load_bib = F)

#APICS

apics_languages <- APiCS_rcldf_obj$tables$LanguageTable %>% 
  distinct(Glottocode) %>% 
  filter(!is.na(Glottocode)) 

#filtering
contact_langs_df <- Glottolog_rcldf_obj$tables$LanguageTable %>% 
  filter(Family_ID == "pidg1258"|
           Family_ID == "mixe1287"|
           str_detect(Name, "Creol")|
           str_detect(Name, "Kriol")) %>%  
  full_join(apics_languages, by = "Glottocode") %>% 
  distinct(Glottocode)

dir <- "output/tables/"
if(!dir.exists(dir)){dir.create(dir)}

contact_langs_df %>% 
  write_tsv("output/tables/contact_languages.tsv")