#' Makes multi-state Grambank-features binary in the appropriate manner.
#'
#' @param ValueTable data frame of the ValueTable from grambank-cldf (long).
#' @param keep_multistate logical vector. If FALSE,the multistate parent features of the binarised features are dropped, only binary and/or binarised features remain. If TRUE, they are kept alongside their binarised versions.
#' @param keep_native_binary logical vector. If TRUE and if the ValueTable already contains some binarised features, they are kept. If false, they are overriden and replaced by values derived from the multi-state features. Note that native binary coding and binarised coding principally differs in terms of ? and 0 coding. See note.
#' @param  trim_to_only_native_binary logical vector. If TRUE, multi-state features are dropped and not binarised.
#' @note The Grambank questionnaire contains multi-state features, all related to word-order. They ask: "Is the order 1) X~Y, 2) Y~X or 3) both?". This function turns them into sets of two binary features: "Is the order X~Y?" and "Is the order Y~X?". If the multi-state feature is coded as "1", the binarised features are "1" and "0" respectively. Please note that absence is inferred, we recode to "1" and "0", not to "1" and "?". Since summer 2023, Grambank coders can also code the binary features from scratch, i.e. code the binary features directly and skip the multi-state. We call this "native binary". If they find clear evidence for presence of one order but not as clear absence of the other, they may code "1" and "?". This means that released version after 1.0 has native binary coding as well as multi-state coding which can be binarised, for the same phenomena for different languages. If you prefer to only have the recoded binarised feature values, set keep_native_binary to FALSE. If you prefer to ONLY have the native binary features, set trim_to_only_native_binary to TRUE. If you prefer a mix, set keep_native_binary to TRUE and trim_to_only_native_binary to FALSE. The last option is the default. There are much fewer native binary feature coding than there are multi-state-coding.
#' @importFrom dplyr case_match
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @author Hedvig Skirgård and Simon Greenhill
#' @return Data-frame (long ValueTable)
#' @export
make_binary_ValueTable <- function(ValueTable = NULL,
                     keep_multistate = FALSE,
                     keep_native_binary = TRUE,
                     trim_to_only_native_binary = FALSE
                     verbose = TRUE){
  
  
    if (!inherits(ValueTable, "data.frame")){ 
      stop("'ValueTable' must be a dataframe.")
      }
  
  if(any(c("ID", "Language_ID", "Parameter_ID", "Value", "Code_ID") %in% colnames(ValueTable)) == FALSE){
    stop("'ValueTable' must have the columns:'ID', 'Language_ID', 'Parameter_ID', 'Value' and 'Code_ID'.")
  }
  

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
    .warn_multistate_binary_clashes(ValueTable)
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
ValueTable
}


# functions for turning 4 of the multistate features into binarised version. These features don't have the 0 option.
#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
#GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
#GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
#GB130 multistate 1: SV; 2: VS; 3: both
.binarise_GBXXX_to_GBXXXa_without_zero <- function(values) {
  if ("0" %in% values) {
    stop("Feature contains zero-values which are not permitted.")
  }
  dplyr::case_match(values, "1" ~ "1", "2" ~ "0", "3" ~ "1", "?" ~ "?",  NA ~ NA)
}


.binarise_GBXXX_to_GBXXXb_without_zero <- function(values) {
  if ("0" %in% values) {
    stop("Feature contains zero-values which are not permitted.")
  }
  dplyr::case_match(values, "1" ~ "0", "2" ~ "1", "3" ~ "1",  "?" ~ "?", NA ~ NA)
}

#### helper functions ####

# functions for turning 2 of the multistate features into binarised version. These features have the 0 option.
# we can just use this function for all multistate, since the other ones shouldn't legally have 0's in them at all. However, to be conservative I (Hedvig) separated them out so that if anything weird happens and somehow GB065 has a 0 value, the code breaks rather than does the wrong thing.

#GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
#GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.
.binarise_GBXXX_to_GBXXXa_with_zero <- function(values) {
  dplyr::case_match(values, "0"~"0", "1" ~ "1", "2" ~ "0", "3" ~ "1", "?" ~ "?",  NA ~ NA)
}

.binarise_GBXXX_to_GBXXXb_with_zero <- function(values) {
  dplyr::case_match(values, "0"~"0", "1" ~ "0", "2" ~ "1", "3" ~ "1",  "?" ~ "?", NA ~ NA)
}




.gb_recode <- function(ValueTable, oldvariable, newvariable, func) {
  ValueTable |> dplyr::filter(.data[["Parameter_ID"]] == oldvariable) |>
    dplyr::mutate(
      ID = paste0(newvariable, "-", .data[["Language_ID"]]),
      Parameter_ID=newvariable,
      Value=func(.data[["Value"]])
    ) |>
    dplyr::mutate(Code_ID = paste0(.data[["Parameter_ID"]], "-", .data[["Value"]])) |>
    rbind(ValueTable)
}

.warn_multistate_binary_clashes <- function(ValueTable) {
  
  # Expected compatible native binary values (col_a, col_b) for each multistate value.
  # The implied-absent side accepts "0" or "?" — a coder may have found clear evidence
  # for one order but left the other uncertain, which is not a clash.
  expected <- data.frame(
    base             = c("GB024","GB024","GB024",
                         "GB025","GB025","GB025",
                         "GB065","GB065","GB065",
                         "GB130","GB130","GB130",
                         "GB193","GB193","GB193","GB193",
                         "GB203","GB203","GB203","GB203"),
    multistate_value = c("1","2","3",
                         "1","2","3",
                         "1","2","3",
                         "1","2","3",
                         "0","1","2","3",
                         "0","1","2","3"),
    # acceptable values for col_a and col_b are stored as comma-separated strings
    # and split later — this avoids list columns in base R data frames
    expected_a       = c("1","0,?","1",
                         "1","0,?","1",
                         "1","0,?","1",
                         "1","0,?","1",
                         "0","1","0,?","1",
                         "0","1","0,?","1"),
    expected_b       = c("0,?","1","1",
                         "0,?","1","1",
                         "0,?","1","1",
                         "0,?","1","1",
                         "0","0,?","1","1",
                         "0","0,?","1","1"),
    stringsAsFactors = FALSE
  )
  
  # Check each multistate/binary feature pair in turn
  clashes <- NULL
  
  for (base in unique(expected$base)) {
    
    col_a <- paste0(base, "a")
    col_b <- paste0(base, "b")
    
    # Pull rows for each of the three features, ignoring "?" in multistate
    multistate <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] == base,
                    .data[["Value"]]        != "?") |>
      dplyr::select("Language_ID", multistate_value = "Value")
    
    native_a <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] == col_a) |>
      dplyr::select("Language_ID", value_a = "Value")
    
    native_b <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] == col_b) |>
      dplyr::select("Language_ID", value_b = "Value")
    
    # Only check languages that have all three coded
    combined <- multistate |>
      dplyr::inner_join(native_a, by = "Language_ID") |>
      dplyr::inner_join(native_b, by = "Language_ID")
    
    if (nrow(combined) == 0) next
    
    # Join expected values onto combined and check for incompatibilities
    combined <- combined |>
      dplyr::left_join(
        dplyr::filter(expected, base == !!base),
        by = "multistate_value"
      )
    
    # Split the comma-separated acceptable values and check each row
    clash_rows <- combined |>
      dplyr::filter(
        !mapply(
          function(val_a, val_b, exp_a, exp_b) {
            a_ok <- val_a %in% strsplit(exp_a, ",")[[1]]
            b_ok <- val_b %in% strsplit(exp_b, ",")[[1]]
            a_ok && b_ok
          },
          value_a, value_b, expected_a, expected_b
        )
      ) |>
      dplyr::mutate(feature = base) |>
      dplyr::select("Language_ID", "feature", "multistate_value",
                    "value_a", "value_b", "expected_a", "expected_b")
    
    if (nrow(clash_rows) > 0) {
      clashes <- rbind(clashes, clash_rows)
    }
  }
  
  if (is.null(clashes) || nrow(clashes) == 0) return(invisible(NULL))
  
  clash_lines <- paste(
    apply(clashes, 1, function(r) {
      paste0(
        "  ", r["Language_ID"],
        " | ", r["feature"], " = ", r["multistate_value"],
        " (expects ", r["feature"], "a = ", r["expected_a"],
        " and ",      r["feature"], "b = ", r["expected_b"], ")",
        " but found ", r["feature"], "a = ", r["value_a"],
        " and ",       r["feature"], "b = ", r["value_b"]
      )
    }),
    collapse = "\n"
  )
  
  warning(
    nrow(clashes), " clash(es) found between multistate and native binary feature codings. ",
    "Native binary values will take priority:\n",
    clash_lines,
    call. = FALSE
  )
  
  invisible(NULL)
}


