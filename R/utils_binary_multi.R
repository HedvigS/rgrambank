
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


