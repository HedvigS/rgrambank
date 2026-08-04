
.warn_multistate_binary_clashes <- function(ValueTable, verbose = FALSE, ignore_question_mark_mismatch = TRUE) {
  
  # Expected compatible native binary values (col_a, col_b) for each multistate value.
  # The implied-absent side accepts "0" or "?" — a coder may have found clear evidence
  # for one order but left the other uncertain, which is not a clash.
  
  # A value of 1 in the multistate feature value could match to 1 and 0 in the binarised version (GBXXXa), or 1 and ?. The user can set ignore_question_mark_mismatch to TRUE if they don't want a warning raised for 1 and ? values
  
  # acceptable values for col_a and col_b are stored as comma-separated strings
  # and split later — this avoids list columns in base R data frames
  
  if(ignore_question_mark_mismatch == TRUE){
    multistate_value = c("1","2","3", "?",
                         "1","2","3", "?",
                         "1","2","3", "?",
                         "1","2","3", "?",
                         "0","1","2","3", "?",
                         "0","1","2","3","?")
    
     expected_a       = c("1","0,?","1", "?",
                         "1","0,?","1", "?",
                         "1","0,?","1", "?",
                         "1","0,?","1", "?",
                         "0","1","0,?","1", "?",
                         "0","1","0,?","1", "?")
  
  expected_b       = c("0,?","1","1", "?",
                       "0,?","1","1", "?",
                       "0,?","1","1", "?",
                       "0,?","1","1", "?",
                       "0","0,?","1","1", "?",
                       "0","0,?","1","1", "?")
  }  

  if(ignore_question_mark_mismatch == FALSE){
    
    multistate_value = c("1","2","3", "?",
                         "1","2","3", "?",
                         "1","2","3", "?",
                         "1","2","3", "?",
                         "0","1","2","3", "?",
                         "0","1","2","3", "?")
    
    expected_a       = c("1","0","1", "?",
                         "1","0","1", "?",
                         "1","0","1", "?",
                         "1","0","1", "?",
                         "0","1","0","1", "?",
                         "0","1","0","1" ,"?")
    
    expected_b       = c("0","1","1", "?",
                         "0","1","1", "?",
                         "0","1","1", "?",
                         "0","1","1", "?",
                         "0","0","1","1", "?",
                         "0","0","1","1" ,"?")
  }  
  
  expected <- data.frame(
    base             = c("GB024","GB024","GB024","GB024",
                         "GB025","GB025","GB025","GB025",
                         "GB065","GB065","GB065","GB065",
                         "GB130","GB130","GB130", "GB130",
                         "GB193","GB193","GB193","GB193","GB193",
                         "GB203","GB203","GB203","GB203","GB203"),
    multistate_value =  multistate_value ,
      expected_a       =  expected_a ,
    expected_b       =  expected_b ,
    stringsAsFactors = FALSE
  )
  
  # Check each multistate/binary feature pair in turn
  clashes <- NULL
  
  for (base in unique(expected$base)) {
    
    col_a <- paste0(base, "a")
    col_b <- paste0(base, "b")
    
    # Pull rows for each of the three features,
    multistate <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] == base) |>
      dplyr::select("Language_ID", "multistate_value" = "Value")
    
    native_a <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] == col_a) |>
      dplyr::select("Language_ID", "value_a" = "Value")
    
    native_b <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] == col_b) |>
      dplyr::select("Language_ID", "value_b" = "Value")
    
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
          combined[["value_a"]], combined[["value_b"]],
          combined[["expected_a"]], combined[["expected_b"]]
        )
      ) |>
      dplyr::mutate(feature = base) |>
      dplyr::select("Language_ID", "feature", "multistate_value",
                    "value_a", "value_b", "expected_a", "expected_b")
    
    if (nrow(clash_rows) > 0) {
      clashes <- rbind(clashes, clash_rows)
    }
  }
  
  if (is.null(clashes) || nrow(clashes) == 0){ 
    
    if(verbose == TRUE){
          message("ValueTable does not have clashes between multistate and binarised feature values")
    
      }
    return(invisible(NULL))
  }


  
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


.check_dups_ValueTable <- function(ValueTable = NULL, verbose = FALSE){
  
  nrow_ValueTable <- ValueTable |> nrow()
  nrow_ValueTable_distinct <-  dplyr::distinct(dplyr::select(ValueTable, "Parameter_ID", "Language_ID")) |> nrow()
  
  if(nrow_ValueTable != nrow_ValueTable_distinct){
    stop("ValueTable has duplicate rows for Language_ID ~ Parameter_ID.")
  }else{
    message("ValueTable does not have duplicate rows for Language_ID ~ Parameter_ID.")
    }

  invisible(NULL)
}


.check_binarised_feature_pairs <- function(ValueTable = NULL, verbose = FALSE) {
  
  pairs <- list(
    c("GB024a", "GB024b"),
    c("GB025a", "GB025b"),
    c("GB065a", "GB065b"),
    c("GB130a", "GB130b"),
    c("GB193a", "GB193b"),
    c("GB203a", "GB203b")
  )
  
  all_binary_features <- unlist(pairs)
  
  # Check if none of the binary features are present at all
  if (verbose && !any(all_binary_features %in% ValueTable$Parameter_ID)) {
    message("No binarised features found in the data - skipping pair checks.")
    return(invisible(TRUE))
  }
  
  issues <- list()
  
  for (pair in pairs) {
    feat1 <- pair[1]
    feat2 <- pair[2]
    
    feat1_exists <- feat1 %in% ValueTable$Parameter_ID
    feat2_exists <- feat2 %in% ValueTable$Parameter_ID
    
    # XOR: one exists but not the other
    if (feat1_exists != feat2_exists) {
      missing <- ifelse(!feat1_exists, feat1, feat2)
      present <- ifelse(feat1_exists, feat1, feat2)
      issues[[length(issues) + 1]] <- sprintf(
        "Pair mismatch: '%s' is present but '%s' is missing", 
        present, missing
      )
    }
  }
  
  if (length(issues) == 0) {
    if (verbose) {
      message("All feature pairs OK")
    }
    return(invisible(TRUE))
  } else {
    for (issue in issues) {
      message(issue)
    }
    stop("At least one pair is incomplete.")
    return(invisible(FALSE))
  }
}

