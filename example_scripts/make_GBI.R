ValueTable <- read.delim("../../../../grambank-v2.0rc2 2/cldf/values.csv", sep = ",") 


remotes::install_github("Hedvigs/rgrambank")
library(rgrambank)
rgrambank::make_