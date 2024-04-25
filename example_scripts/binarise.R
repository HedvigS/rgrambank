# This script showcases how to use functions in this repos to turn multistate features in Grambank into binary versions in the correct way (e.g. "GB024 What is the order of numeral and noun in the NP?" -> "GB024a Is the order of the numeral and noun Num-N?" and "GB024b Is the order of the numeral and noun N-Num?").
# Future releases of Grambank may contain a mix of multistate features and binarised features already in the release. In the functions here, if the dataset already contains binarised features, that is referred to as "raw binary". Users can choose to disregard these, or only use these. If the users selects keep_raw_binary = TRUE AND trim_to_only_raw_binary = FALSE this will result in a mix of raw binarised features and multistate features which have been binarised. For more information, see: https://github.com/grambank/grambank/wiki/Binarised-features.
# The scripts which store the functions contain further information on the arguments, please see the scripts "functions/make_binary_ValueTable.R" and "functions/make_binary_ParameterTable.R" for more details on function behaviour.
# This example uses rcldf to fetch and parse Grambank v1.0.3 from Zenodo's online webplatform. If you already have Grambank downloaded locally, feel free to change the line which calls rcldf::cldf to point to the JSON-file instead. An example of this is provided below in a line that is commented out.
# This script specifies particular versions of the packages tidyverse and rcldf to increase replicability.

# written by Hedvig Skirgård 2024-04-25.

#install.packages("devtools")
library(devtools)

#install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

# fetching Grambank locally 
#GB_rcldf_obj <- rcldf::cldf("../../grambank/grambank/cldf/StructureDataset-metadata.json", load_bib = F)

# load_bib is set to FALSE because the bib-file is not necessary for these actions, and the package that rcldf dependes on for bibTeX parsin (bib2df) has some not-harmful but outdated code which generates warnings.

#loading the necessary functions for making Grambank data binary
source("../functions/make_binary_ValueTable.R")
source("../functions/make_binary_ParameterTable.R")

#turning multistate features and their values in Grambank ValueTable to their binary versions
GB_ValueTable_binary <- make_binary_ValueTable(ValueTable = GB_rcldf_obj$tables$ValueTable, 
                                               keep_multistate = FALSE, 
                                               keep_raw_binary = TRUE,
                                               trim_to_only_raw_binary = FALSE
                                              )
#Turning multistate features in the Grambank ParameterTable to their binary versions.
#This is useful for example in order to have Feature names to use in plotting.
GB_ParameterTable_binary <- make_binary_ParameterTable(ParameterTable = GB_rcldf_obj$tables$ParameterTable,
                                                       keep_multi_state_features = FALSE
                                                      )
dir <- "output"
if(!dir.exists(dir)){dir.create(dir)}

write_tsv(GB_ValueTable_binary, file = "output/Grambank_ValueTable_binary.tsv", quote = "all", na = "")

write_tsv(GB_ParameterTable_binary, file = "output/Grambank_ParameterTable_binary.tsv", quote = "all", na = "")