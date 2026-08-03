#' Reconstructs multistate Grambank features from binarised feature pairs. This 
#' can be useful for some code with legacy behaviour from Grambank v1 (e.g. 
#' rgrambank::make_GBI)
#'
#' @description
#' Grambank v2+ contains binarised versions of six multistate features from
#' Grambank v1, all related to word order (read more here:
#' https://github.com/grambank/grambank/wiki/Binarised-features).
#' This function reconstructs the multistate features (e.g. GB065 with values
#' 0, 1, 2, 3) from their binarised pairs (e.g. GB065a and GB065b with values
#' 0, 1 and ?).
#'
#' If both multistate and native binary codings exist for the same language,
#' native binary takes priority. Any such clashes are reported as warnings via
#' \code{.warn_multistate_binary_clashes()}.
#'
#' @param ValueTable data frame of the ValueTable from grambank-cldf (long
#'   format). Must contain columns: ID, Language_ID, Parameter_ID, Value,
#'   Code_ID.
#' @param keep_binarised logical. If FALSE (default), the binarised feature
#'   columns (e.g. GB065a, GB065b) are dropped from the output after
#'   reconstruction. If TRUE, they are retained alongside the reconstructed
#'   multistate features.
#' @param verbose logical. If TRUE (default), reports any clashes between
#'   multistate and native binary feature codings via warning. If FALSE,
#'   clash detection runs silently.
#'   
#' @return A data frame in the same long format as the input ValueTable, with
#'   binarised feature pairs replaced by (or supplemented with, if
#'   keep_binarised = TRUE) their reconstructed multistate parents.
#'
#' @note
#' The six multistate features and their binarised pairs are:
#' \itemize{
#'   \item GB024 (1: Num-N, 2: N-Num, 3: both) from GB024a and GB024b
#'   \item GB025 (1: Dem-N, 2: N-Dem, 3: both) from GB025a and GB025b
#'   \item GB065 (1: PSR-PSD, 2: PSD-PSR, 3: both) from GB065a and GB065b
#'   \item GB130 (1: SV, 2: VS, 3: both) from GB130a and GB130b
#'   \item GB193 (0: not attributive, 1: ANM-N, 2: N-ANM, 3: both) from GB193a and GB193b
#'   \item GB203 (0: no UQ, 1: UQ-N, 2: N-UQ, 3: both) from GB203a and GB203b
#' }
#'
#' GB193 and GB203 are the only features with a "0" value. For all other 
#' features, col_a == "0" and col_b == "0" simultaneously is not a valid 
#' state and will result in NA.
#'
#' Native binary codings allow "?" on the implied-absent side (e.g. GB024a = 1
#' and GB024b = ? is a valid coding for multistate value "1"). This is handled
#' correctly by the reconstruction logic.
#'
#' @seealso \code{\link{make_binary_ValueTable}} for the reverse operation.
#'
#' @importFrom rlang :=
#' @author Hedvig Skirgård
#' @export
make_multistate_ValueTable <- function(ValueTable = NULL,
                                       keep_binarised = FALSE, 
                                       verbose = TRUE) {
  
  # Input validation
  if (!inherits(ValueTable, "data.frame")) {
    stop("'ValueTable' must be a dataframe.")
  }
  
  .check_dups_ValueTable(ValueTable = ValueTable, verbose = verbose)
  
  if (!all(c("ID", "Language_ID", "Parameter_ID", "Value", "Code_ID") %in% colnames(ValueTable))) {
    stop("'ValueTable' must have the columns: 'ID', 'Language_ID', 'Parameter_ID', 'Value' and 'Code_ID'.")
  }
  
  .binarised_parameters <- c(
    "GB024a", "GB024b",
    "GB025a", "GB025b",
    "GB065a", "GB065b",
    "GB130a", "GB130b",
    "GB193a", "GB193b",
    "GB203a", "GB203b"
  )

  # Check that there is something to reconstruct
  if (!any(.binarised_parameters %in% ValueTable$Parameter_ID)) {
    message("No binarised features found in ValueTable - returning input unchanged.")
    return(ValueTable)
  }
  
  # Warn about any clashes between multistate and native binary codings.
  # Native binary will take priority in reconstruction.
  if(verbose == TRUE){
  .warn_multistate_binary_clashes(ValueTable, verbose = verbose)
  }  
  # For each multistate feature, reconstruct from binarised pairs.
  # Native binary priority is achieved by:
  #   1. Removing any existing multistate rows for languages that have
  #      native binary codings (so reconstruction starts from "?"/NA)
  #   2. Reconstructing the multistate value from the native binary values
  #   3. Removing the binarised rows (if keep_binarised = FALSE)
  
  # Define the feature pairs with their reconstruction properties
  feature_pairs <- list(
    # base, col_a, col_b, has_zero
    list(base = "GB024", col_a = "GB024a", col_b = "GB024b", has_zero = FALSE),
    list(base = "GB025", col_a = "GB025a", col_b = "GB025b", has_zero = FALSE),
    list(base = "GB065", col_a = "GB065a", col_b = "GB065b", has_zero = FALSE),
    list(base = "GB130", col_a = "GB130a", col_b = "GB130b", has_zero = FALSE),
    list(base = "GB193", col_a = "GB193a", col_b = "GB193b", has_zero = TRUE),
    list(base = "GB203", col_a = "GB203a", col_b = "GB203b", has_zero = TRUE)
  )
  
  for (pair in feature_pairs) {
    
    base   <- pair$base
    col_a  <- pair$col_a
    col_b  <- pair$col_b
    has_zero <- pair$has_zero
    
    # Skip if neither binarised column is present
    if (!any(c(col_a, col_b) %in% ValueTable$Parameter_ID)) next
    
    # Warn if only one of the pair is present
    if (!all(c(col_a, col_b) %in% ValueTable$Parameter_ID)) {
      warning(
        "Only one of ", col_a, " / ", col_b, " found in ValueTable. ",
        "Both are required for reconstruction of ", base, ". Skipping."
      )
      next
    }
    
    # Identify languages that have native binary codings
    langs_with_native_binary <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] %in% c(col_a, col_b)) |>
      dplyr::pull("Language_ID") |>
      unique()
    
    # For those languages, remove any existing multistate rows so that native 
    # binary values take priority
    ValueTable <- ValueTable |>
      dplyr::filter(!(
        .data[["Parameter_ID"]] == base &
          .data[["Language_ID"]] %in% langs_with_native_binary
      ))
    
    # Pull the binarised values into a wide helper frame for reconstruction
    wide <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] %in% c(base, col_a, col_b)) |>
      dplyr::select("Language_ID", "Parameter_ID", "Value") |>
      tidyr::pivot_wider(names_from  = "Parameter_ID",
                         values_from = "Value")
    
    # Ensure all three columns exist in the wide frame
    if (!base  %in% colnames(wide)) wide[[base]]  <- NA_character_
    if (!col_a %in% colnames(wide)) wide[[col_a]] <- NA_character_
    if (!col_b %in% colnames(wide)) wide[[col_b]] <- NA_character_
    
    # Reconstruct the multistate value
    wide <- wide |>
      dplyr::mutate(
        !!base := dplyr::case_when(
          
          # Already known - keep as-is (only applies to languages without
          # native binary, since we removed multistate for those above)
          !is.na(.data[[base]]) & .data[[base]] != "?"    ~ .data[[base]],
          
          # Value "0": feature entirely absent (GB193 and GB203 only)
          has_zero &
            !is.na(.data[[col_a]]) & .data[[col_a]] == "0" &
            !is.na(.data[[col_b]]) & .data[[col_b]] == "0"     ~ "0",
          
          # Value "3": both orders present
          .data[[col_a]] == "1" &
            .data[[col_b]] == "1"                               ~ "3",
          
          # Value "1": only order A present (B is absent or uncertain)
          .data[[col_a]] == "1" &
            .data[[col_b]] %in% c("0", "?")                    ~ "1",
          
          # Value "2": only order B present (A is absent or uncertain)
          .data[[col_a]] %in% c("0", "?") &
            .data[[col_b]] == "1"                               ~ "2",
          
          # Still unknown
          .default = NA_character_
        )
      )
    
    # Convert reconstructed values back to long format
    reconstructed_long <- wide |>
      dplyr::select("Language_ID", !!base) |>
      dplyr::rename(Value = !!base) |>
      dplyr::filter(!is.na(.data[["Value"]])) |>
      dplyr::mutate(
        Parameter_ID = base,
        ID           = paste0(base, "-", .data[["Language_ID"]]),
        Code_ID      = paste0(base, "-", .data[["Value"]])
      ) |>
      dplyr::select(dplyr::all_of(c("ID", "Language_ID", "Parameter_ID", "Value", "Code_ID")))
    
    # Remove old multistate rows for all languages (we replace them wholesale)
    # and remove binarised rows, then add the reconstructed rows
    ValueTable <- ValueTable |>
      dplyr::filter(.data[["Parameter_ID"]] != base)
    
    ValueTable <- dplyr::bind_rows(ValueTable, reconstructed_long)
  }
  
  # Drop binarised columns from output if requested
  if (keep_binarised == FALSE) {
    ValueTable <- ValueTable |>
      dplyr::filter(!(.data[["Parameter_ID"]] %in% .binarised_parameters))
  }
  
  ValueTable
}
