library(dplyr) 
library(rcldf)
library(rgrambank)

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

set.seed(32)

PopTable <- GB_rcldf_obj$tables$LanguageTable |> 
  dplyr::group_by("ID") |> 
  dplyr::sample_n(size = 100, replace = FALSE) |> 
  dplyr::ungroup() |> 
  dplyr::select("ID", "Pop_ID" = "Macroarea")

ValueTable_long <- GB_rcldf_obj$tables$ValueTable |> 
  dplyr::filter(Language_ID %in% PopTable$ID) |> 
  dplyr::select(ID = Language_ID, Value, Parameter_ID) 

cfx_object <- CultureFst(ValueTable_long = ValueTable_long, PopTable = PopTable, type = 0, bootstrap = T, loci = NULL) 

cfx_matrix <- cfx_object$mean.fst %>% as.matrix()
cfx_matrix[upper.tri(x = cfx_matrix, diag = T)] <- NA
