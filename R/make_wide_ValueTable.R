make_wide_ValueTable <- function(ValueTable = NULL, verbose = TRUE
){
  
  .check_dups_ValueTable(ValueTable = ValueTable, 
                         verbose = verbose) 
  
  ValueTable_wide <- ValueTable |> 
    dplyr::select(Language_ID, Parameter_ID, Value) |>
    tidyr::pivot_wider(
      names_from = Parameter_ID,
      values_from = Value
    )
  
  return(ValueTable_wide)
}