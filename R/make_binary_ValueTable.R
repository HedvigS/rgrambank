#' Makes multi-state Grambank-features binary in the appropriate manner.
#'
#' @param ValueTable data frame of the ValueTable from grambank-cldf (long).
#' @param keep_multistate logical vector. If FALSE,the multistate parent features of the binarised features are dropped, only binary and/or binarised features remain. If TRUE, they are kept alongside their binarised versions.
#' @param keep_native_binary logical vector. If TRUE and if the ValueTable already contains some binarised features, they are kept. If false, they are overriden and replaced by values derived from the multi-state features. Note that native binary coding and binarised coding principally differs in terms of ? and 0 coding. See note.
#' @param  trim_to_only_native_binary logical vector. If TRUE, multi-state features are dropped and not binarised.
#' @param verbose logical. If TRUE (default), reports any clashes between
#'   multistate and native binary feature codings via warning. If FALSE,
#'   clash detection runs silently.
#' @note The Grambank questionnaire contains multi-state features, all related to word-order. They ask: "Is the order 1) X~Y, 2) Y~X or 3) both?". This function turns them into sets of two binary features: "Is the order X~Y?" and "Is the order Y~X?". If the multi-state feature is coded as "1", the binarised features are "1" and "0" respectively. Please note that absence is inferred, we recode to "1" and "0", not to "1" and "?". Since summer 2023, Grambank coders can also code the binary features from scratch, i.e. code the binary features directly and skip the multi-state. We call this "native binary". If they find clear evidence for presence of one order but not as clear absence of the other, they may code "1" and "?". This means that released version after 1.0 has native binary coding as well as multi-state coding which can be binarised, for the same phenomena for different languages. If you prefer to only have the recoded binarised feature values, set keep_native_binary to FALSE. If you prefer to ONLY have the native binary features, set trim_to_only_native_binary to TRUE. If you prefer a mix, set keep_native_binary to TRUE and trim_to_only_native_binary to FALSE. The last option is the default. There are much fewer native binary feature coding than there are multi-state-coding.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @author Hedvig Skirgård and Simon Greenhill
#' @return Data-frame (long ValueTable)
#' @export
make_binary_ValueTable <- function(ValueTable = NULL,
                     keep_multistate = FALSE,
                     keep_native_binary = TRUE,
                     trim_to_only_native_binary = FALSE,
                     verbose = TRUE){
  
  
    if (!inherits(ValueTable, "data.frame")){ 
      stop("'ValueTable' must be a dataframe.")
      }
  
  if(!all(c("ID", "Language_ID", "Parameter_ID", "Value", "Code_ID") %in% colnames(ValueTable))){
    stop("'ValueTable' must have the columns: 'ID', 'Language_ID', 'Parameter_ID', 'Value' and 'Code_ID'.")
  }
  
  .check_dups_ValueTable(ValueTable = ValueTable, verbose = verbose)

  .binary_parameters <- c(
    "GB024a", "GB024b",
    "GB025a", "GB025b",
    "GB065a", "GB065b",
    "GB130a","GB130b",
    "GB193a","GB193b",
    "GB203a", "GB203b")
  
  .multistate_parameters <- c(
    "GB024",
    "GB025",
    "GB065",
    "GB130",
    "GB193",
    "GB203")
  
  # Check for clashes between multistate and native binary codings.
  # Native binary will take priority (handled downstream by anti_join),
  # but clashes are flagged here for transparency.
  if (keep_native_binary == TRUE && trim_to_only_native_binary == FALSE && verbose == TRUE) {
    .warn_multistate_binary_clashes(ValueTable, verbose = verbose)
  }
  
  if (trim_to_only_native_binary == TRUE) {
      
      if(!(any(ValueTable$Parameter_ID %in% .binary_parameters))){
        stop("There is no native binary coding at all.")
      }
        
    ValueTable <- ValueTable |>
            dplyr::filter(!(.data[["Parameter_ID"]] %in% .multistate_parameters))

    } else {

    if (keep_native_binary == FALSE) {
        ValueTable <- ValueTable |>
            dplyr::filter(!(.data[["Parameter_ID"]] %in% .binary_parameters))
    } else {
      # ValueTable_native_binary is assigned here only when keep_native_binary = TRUE.
      # It is only referenced below inside if (keep_native_binary == TRUE), so this
      # is safe.
        ValueTable_native_binary <- ValueTable |>
            dplyr::filter(.data[["Parameter_ID"]] %in% .binary_parameters)
    }

    # BINARISING MULTISTATE FEATURES
    ValueTable <- .gb_recode(ValueTable, 'GB024', 'GB024a', .binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB024', 'GB024b', .binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB025', 'GB025a', .binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB025', 'GB025b', .binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB065', 'GB065a', .binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB065', 'GB065b', .binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB130', 'GB130a', .binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB130', 'GB130b', .binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB193', 'GB193a', .binarise_GBXXX_to_GBXXXa_with_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB193', 'GB193b', .binarise_GBXXX_to_GBXXXb_with_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB203', 'GB203a', .binarise_GBXXX_to_GBXXXa_with_zero)
    ValueTable <- .gb_recode(ValueTable, 'GB203', 'GB203b', .binarise_GBXXX_to_GBXXXb_with_zero)

    if (keep_native_binary == TRUE) {
      
      cols_to_join_for <- colnames(ValueTable)
        ValueTable <- ValueTable |>
            dplyr::anti_join(
                dplyr::select(ValueTable_native_binary, "Language_ID", "Parameter_ID"),
                     by = c("Language_ID", "Parameter_ID")) |>
            dplyr::full_join(
                ValueTable_native_binary,
                by = cols_to_join_for)

    }
    if (keep_multistate == FALSE) {
        ValueTable <- ValueTable |>
            dplyr::filter(!(.data[["Parameter_ID"]] %in% .multistate_parameters))
        }
    }
return(ValueTable)
}


# helper functions for turning 4 of the multistate features into binarised version. These features don't have the 0 value in the multistate feature
#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
#GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
#GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
#GB130 multistate 1: SV; 2: VS; 3: both

.binarise_GBXXX_to_GBXXXa_without_zero <- function(values) {
  if ("0" %in% values) {
    stop("Feature contains zero-values which are not permitted.")
  }
  # input:  "1" -> "1",  "2" -> "0",  "3" -> "1",  "?" -> "?"
  lookup_input  <- c("1", "2", "3", "?")
  lookup_output <- c("1", "0", "1", "?")
  output <- lookup_output[match(values, lookup_input)]
  return(output)
  }

.binarise_GBXXX_to_GBXXXb_without_zero <- function(values) {
  if ("0" %in% values) {
    stop("Feature contains zero-values which are not permitted.")
  }
  # input:  "1" -> "0",  "2" -> "1",  "3" -> "1",  "?" -> "?"
  lookup_input  <- c("1", "2", "3", "?")
  lookup_output <- c("0", "1", "1", "?")
  output <- lookup_output[match(values, lookup_input)]
  return(output)
}


# functions for turning 2 of the multistate features into binarised version. These features have the 0 option.
# we can just use this function for all multistate, since the other ones shouldn't legally have 0's in them at all. However, to be conservative I (Hedvig) separated them out so that if anything weird happens and somehow GB065 has a 0 value, the code breaks rather than does the wrong thing.

#GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
#GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.

.binarise_GBXXX_to_GBXXXa_with_zero <- function(values) {
  # input:  "0" -> "0",  "1" -> "1",  "2" -> "0",  "3" -> "1",  "?" -> "?"
  lookup_input  <- c("0", "1", "2", "3", "?")
  lookup_output <- c("0", "1", "0", "1", "?")
  output <- lookup_output[match(values, lookup_input)]
  return(output)
}

.binarise_GBXXX_to_GBXXXb_with_zero <- function(values) {
  # input:  "0" -> "0",  "1" -> "0",  "2" -> "1",  "3" -> "1",  "?" -> "?"
  lookup_input  <- c("0", "1", "2", "3", "?")
  lookup_output <- c("0", "0", "1", "1", "?")
  output <- lookup_output[match(values, lookup_input)]
  return(output)
}

.gb_recode <- function(ValueTable, oldvariable, newvariable, func) {
  new_rows <- ValueTable |>
    dplyr::filter(.data[["Parameter_ID"]] == oldvariable) |>
    dplyr::mutate(
      ID = paste0(newvariable, "-", .data[["Language_ID"]]),
      Parameter_ID=newvariable,
      Value=func(.data[["Value"]])
    ) |>
    dplyr::mutate(Code_ID = paste0(.data[["Parameter_ID"]], "-", .data[["Value"]])) 
  
  output <- dplyr::bind_rows(ValueTable, new_rows)
  return(output)
}
