

#remotes::install_github("Hedvigs/rgrambank")
#library(rgrambank)
library(tidyverse)
library(testthat)
library(data.table)
library(reshape2)

ValueTable <- read.delim("../../../../grambank-v2.0rc2 2/cldf/values.csv", sep = ",") 
ValueTable <- read.delim("../../../grambank/grambank/cldf/values.csv", sep = ",") 

load("../R/sysdata.rda")
source("../R/make_GBI.R")

output <- make_GBI(ValueTable = ValueTable)
