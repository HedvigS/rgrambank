library(rgrambank)

set.seed(1111)

# Check working directory.
# We need to check if it terminates in "rgrambank/tests"
if(!grepl("rgrambank/tests$", getwd())) {
  # Inform the user and quit.
  message("Please run this script from the 'rgrambank/tests' directory.")
  message("Current directory is ", getwd())
  quit(save = "no")
}

return() # debug

# fetching Grambank v1.0.3 from Zenodo using rcldf (requires internet)
GB_rcldf_obj <- rcldf::cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip", load_bib = F)

Grambank_ValueTable <-  rgrambank::reduce_ValueTable_to_unique_glottocodes(
  ValueTable = GB_rcldf_obj$tables$ValueTable,
  LanguageTable = GB_rcldf_obj$tables$LanguageTable,
  merge_dialects = F, 
  method = "singular_least_missing_data",
  replace_missing_language_level_ID = T) |> 
  dplyr::select(-Language_ID) |> 
  dplyr::rename(Language_ID = Glottocode) 

#subset to just 200 languages to speed up for demonstration
lgs_to_keep <- sample(Grambank_ValueTable$Language_ID, size = 300)

Grambank_ValueTable <- Grambank_ValueTable |> 
  dplyr::filter(Language_ID %in% lgs_to_keep)

#densify
recode_patterns <- read.csv("https://raw.githubusercontent.com/annagraff/crossling-curated/0e8695e176044f268b7d8c1ac012061b7bf1b343/scripts/GBI/feature-recode-patterns.csv")
all_decisions <- read.csv("https://raw.githubusercontent.com/annagraff/crossling-curated/0e8695e176044f268b7d8c1ac012061b7bf1b343/scripts/GBI/decisions-log.csv")


# fetching Glottolog v5.0 from Zenodo using rcldf (requires internet)
glottolog_rcldf_obj <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

#checking that it runs for GBI
GBI <- rgrambank::make_GBI(ValueTable = Grambank_ValueTable, recode_patterns_full = recode_patterns, all_decisions = all_decisions)

# Show the value for language "hooo1248" feature ID "GB091".
# Both are in GBI$data_for_statsGBI. 
# The language IDs are listed in Language_ID.
# The GB091 values are listed in GB091.
# So find hooo1248 in the Language_ID column and look at the corresponding GB091 value.
print("This value should be 1: ")
print(GBI$data_for_statsGBI[GBI$data_for_statsGBI$Language_ID == "hooo1248", "GB091"]) # Should be 1 as per https://grambank.clld.org/languages/hooo1248

# Now get GB123, should be 0.
print("This value should be 0: ")
print(GBI$data_for_statsGBI[GBI$data_for_statsGBI$Language_ID == "hooo1248", "GB123"]) # Should be 0 as per https://grambank.clld.org/languages/hooo1248

check_against_csv_logical  <- function(GBI, csv_path, test_count = 1) {
    # Check that GBI$logicalGBI matches the provided csv.
    # If there is a mismatch, print the first row and column it occurs. Say what the values are in the two tables.
    logicalGBI_csv <- read.csv(
    csv_path,
    row.names = 1,          # discard the unnamed index column
    check.names = FALSE,
    stringsAsFactors = FALSE
    )

    # Use glottocodes as row identifiers
    csv_ids <- logicalGBI_csv$glottocode
    logicalGBI_csv$glottocode <- NULL

    gbi <- GBI$logicalGBI

    # Identify the language-ID column in GBI$logicalGBI
    gbi_ids <- gbi$Language_ID
    gbi$Language_ID <- NULL

    # Compare only rows and columns present in both data frames
    common_ids <- intersect(csv_ids, gbi_ids)
    common_features <- intersect(names(logicalGBI_csv), names(gbi))

    csv_values <- logicalGBI_csv[
    match(common_ids, csv_ids),
    common_features,
    drop = FALSE
    ]

    gbi_values <- gbi[
    match(common_ids, gbi_ids),
    common_features,
    drop = FALSE
    ]

    # Compare without changing either data frame
    csv_matrix <- as.matrix(csv_values)
    gbi_matrix <- as.matrix(gbi_values)

    csv_missing <- is.na(csv_matrix) | csv_matrix == "NA"
    gbi_missing <- is.na(gbi_matrix) | gbi_matrix == "NA"

    # Missing values match only when both tables are missing
    same <- (csv_matrix == gbi_matrix) |
    (csv_missing & gbi_missing)

    # Missing in only one table is considered a mismatch
    same[is.na(same)] <- FALSE

    if (all(same)) {
    cat("Test", test_count, ": No mismatches found.\n")
    } else {
    mismatch <- which(!same, arr.ind = TRUE)[1, ]

    cat(
        "Test", test_count, ": Mismatch found for language", common_ids[mismatch["row"]],
        "and feature", common_features[mismatch["col"]], "\n"
    )
    cat(
        "logicalGBI.csv value:",
        csv_values[mismatch["row"], mismatch["col"]], "\n"
    )
    cat(
        "GBI$logicalGBI value:",
        gbi_values[mismatch["row"], mismatch["col"]], "\n"
    )
    }

}

# Test with the correct CSV.
check_against_csv_logical(GBI, "test_data/logicalGBI.csv", test_count = 1)

# Test the incorrect value for "alya1239" and "GB027".
check_against_csv_logical(GBI, "test_data/logicalGBI_incorrect.csv", test_count = 2)

check_against_csv_statistical  <- function(GBI, csv_path, test_count = 1) {
    # Check that GBI$logicalGBI matches the provided csv.
    # If there is a mismatch, print the first row and column it occurs. Say what the values are in the two tables.
    statisticalGBI_csv <- read.csv(
        csv_path,
        row.names = 1,          # discard the unnamed index column
        check.names = FALSE,
        stringsAsFactors = FALSE
    )

    # Use glottocodes as row identifiers
    csv_ids <- statisticalGBI_csv$glottocode
    statisticalGBI_csv$glottocode <- NULL

    gbi <- GBI$statisticalGBI

    # Identify the language-ID column in GBI$logicalGBI
    gbi_ids <- gbi$Language_ID
    gbi$Language_ID <- NULL

    # Compare only rows and columns present in both data frames
    common_ids <- intersect(csv_ids, gbi_ids)
    common_features <- intersect(names(statisticalGBI_csv), names(gbi))

    csv_values <- statisticalGBI_csv[
    match(common_ids, csv_ids),
    common_features,
    drop = FALSE
    ]

    gbi_values <- gbi[
    match(common_ids, gbi_ids),
    common_features,
    drop = FALSE
    ]

    # Compare without changing either data frame
    csv_matrix <- as.matrix(csv_values)
    gbi_matrix <- as.matrix(gbi_values)

    csv_missing <- is.na(csv_matrix) | csv_matrix == "NA"
    gbi_missing <- is.na(gbi_matrix) | gbi_matrix == "NA"

    # Missing values match only when both tables are missing
    same <- (csv_matrix == gbi_matrix) |
    (csv_missing & gbi_missing)

    # Missing in only one table is considered a mismatch
    same[is.na(same)] <- FALSE

    if (all(same)) {
    cat("Test", test_count, ": No mismatches found.\n")
    } else {
    mismatch <- which(!same, arr.ind = TRUE)[1, ]

    cat(
        "Test", test_count, ": Mismatch found for language", common_ids[mismatch["row"]],
        "and feature", common_features[mismatch["col"]], "\n"
    )
    cat(
        "statisticalGBI.csv value:",
        csv_values[mismatch["row"], mismatch["col"]], "\n"
    )
    cat(
        "GBI$statisticalGBI value:",
        gbi_values[mismatch["row"], mismatch["col"]], "\n"
    )
    }

}

# Run check_against_csv_statistical
check_against_csv_statistical(GBI, "test_data/statisticalGBI.csv", test_count = 3)

# Run against incorrect data, should find a mismatch
check_against_csv_statistical(GBI, "test_data/statisticalGBI_incorrect.csv", test_count = 4)

print("If mismatches were found for alya1239/GB027 and amri1238/GB024e, and there are no other mismatches, everything works as expected.")