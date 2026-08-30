#' Makes a version of the Grambank ParameterTable with information on binarised features
#' @param ParameterTable data-frame, long format. ParameterTable from cldf.
#' @param keep_multi_state_features logical. If TRUE, rows with the multistate version of the features remain, if FALSE only binary or binarised features remain in the ParameterTable.
#' @param keep_native_binary logical vector. If ParameterTable already contains binary features, should these be kept? If so, the function just feeds back the same ParameterTable that it received. This is mainly useful for backwards compatability (applying scripts meant for Grambank version 1 to version 2).
#' @author Hedvig Skirgård
#' @return data-frame of ParameterTable with added rows for binarised version of multi-state features
#' @export

make_binary_ParameterTable<- function(ParameterTable,
                                      keep_multi_state_features = TRUE,
                                      keep_native_binary = FALSE){
  
  multistate_features <- c("GB024", "GB025", "GB065", "GB130", "GB193", "GB203")

    binarised_feats <-  c(
    "GB024a", "GB024b",
    "GB025a", "GB025b",
    "GB065a", "GB065b",
    "GB130a","GB130b",
    "GB193a","GB193b",
    "GB203a", "GB203b")
  
  if(keep_native_binary == TRUE && all(  binarised_feats %in% ParameterTable[["ID"]])){

    ParameterTable_new <- ParameterTable
    
  }else{
  
.Parameter_binary <- data.frame(
    ID = c(
        "GB024", "GB024",
        "GB025", "GB025",
        "GB065", "GB065",
        "GB130","GB130",
        "GB193","GB193",
        "GB203", "GB203"
    ),

        ID_binary = c(
        "GB024a", "GB024b",
        "GB025a", "GB025b",
        "GB065a", "GB065b",
        "GB130a","GB130b",
        "GB193a","GB193b",
        "GB203a", "GB203b"
    ),
    Grambank_ID_desc_binary = c(
        "GB024a NUMOrder_Num-N",
        "GB024b NUMOrder_N-Num",
        "GB025a DEMOrder_Dem-N",
        "GB025b DEMOrder_N-Dem",
        "GB065a POSSOrder_PSR-PSD",
        "GB065b POSSOrder_PSD-PSR",
        "GB130a IntransOrder_SV",
        "GB130b IntransOrder_VS",
        "GB193a ANMOrder_ANM-N",
        "GB193b ANMOrder_N-ANM",
        "GB203a UQOrder_UQ-N",
        "GB203b UQOrder_N-UQ"
    ),
    Name_binary = c("Is the order of the numeral and noun Num-N?",
             "Is the order of the numeral and noun N-Num?",
             "Is the order of the adnominal demonstrative and noun Dem-N?",
             "Is the order of the adnominal demonstrative and noun N-Dem?",
             "Is the pragmatically unmarked order of adnominal possessor noun and possessed noun PSR-PSD?",
             "Is the pragmatically unmarked order of adnominal possessor noun and possessed noun PSD-PSR?",
             "Is the pragmatically unmarked order of S and V in intransitive clauses S-V?",
             "Is the pragmatically unmarked order of S and V in intransitive clauses V-S?",
             "Is the order of the adnominal property word (ANM) and noun ANM-N?",
             "Is the order of the adnominal property word (ANM) and noun N-ANM?",
             "Is the order of the adnominal collective universal quantifier (UQ) and noun UQ-N?",
             "Is the order of the adnominal collective universal quantifier (UQ) and noun N-QU?" ),

    "Word_Order_binary"= c(
        0,
        1,
        0,
        1,
        0,
        1,
        NA,
        NA,
        0,
        1,
        0,
        1
    ),
    Binary_Multistate = c("Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised"))

if(keep_native_binary == FALSE){
  
ParameterTable <- ParameterTable |> 
  dplyr::filter(!.data[["ID"]] %in% binarised_feats)
  }

ParameterTable_new <- ParameterTable |>
    dplyr::full_join(.Parameter_binary, by = "ID") |>
    dplyr::mutate(ID = ifelse(!is.na(.data[["ID_binary"]]), yes = .data[["ID_binary"]], no = .data[["ID"]])) |>
    dplyr::mutate(Name = ifelse(!is.na(.data[["Name_binary"]]), yes = .data[["Name_binary"]], no = .data[["Name"]])) |>
    dplyr::mutate(Grambank_ID_desc = ifelse(!is.na(.data[["Grambank_ID_desc_binary"]]), 
                                            yes = .data[["Grambank_ID_desc_binary"]],
                                            no = .data[["Grambank_ID_desc"]])) |>
    dplyr::mutate(Word_Order = ifelse(!is.na(.data[["Word_Order_binary"]]), 
                                      yes = .data[["Word_Order_binary"]], 
                                      no = .data[["Word_Order"]])) |>
    dplyr::select(-c("ID_binary", "Name_binary", "Grambank_ID_desc_binary", "Word_Order_binary")) |>
    dplyr::mutate(Binary_Multistate= ifelse(.data[["ID"]] %in% multistate_features, 
                                                  yes = "Multi", 
                                                  no= .data[["Binary_Multistate"]])) |>
    dplyr::mutate(Binary_Multistate = ifelse(is.na(.data[["Binary_Multistate"]]), 
                                             yes = "Binary", 
                                             no =.data[["Binary_Multistate"]]))
  }

if(keep_multi_state_features == FALSE){
ParameterTable_new <-     ParameterTable_new |>
    dplyr::filter(!(.data[["ID"]] %in% multistate_features))
}
  
# there can be two binary rows for the same feature, e.g. GB024a. This removes that issue
    if(any(duplicated(ParameterTable_new[["ID"]]))
     ){
    ParameterTable_new <- ParameterTable_new |> 
      dplyr::group_by(.data[["ID"]]) |>
      dplyr::filter(!(dplyr::n() > 1 & .data[["Binary_Multistate"]] == "Binarised")) |>
      dplyr::ungroup()
    
  }
  
ParameterTable_new
}
