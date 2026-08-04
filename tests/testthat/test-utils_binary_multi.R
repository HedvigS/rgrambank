
test_that(".check_dups_ValueTable() stops on duplicate Language_ID ~ Parameter_ID", {
  expect_error(
    .check_dups_ValueTable(df_check_dups, verbose = TRUE),
    "duplicate rows"
  )
})

test_that(".check_dups_ValueTable() passes silently on clean data", {
  expect_invisible(
    .check_dups_ValueTable(df_check_dups_clean, verbose = TRUE)
  )
})

test_that(".check_binarised_feature_pairs fails if one feature in the binarised pair is missing.", {
  expect_error(
    .check_binarised_feature_pairs(ValueTable = df_check_multistate_binary_clashes_one_missing),
    "At least one pair is incomplete", fixed = TRUE
  )
})

test_that(".check_binarised_feature_pairs fails if one feature in the binarised pair is missing.", {
  expect_message(
    .check_binarised_feature_pairs(ValueTable = df_make_binary_solo, verbose = TRUE),
    "No binarised features found in the data - skipping pair checks", fixed = TRUE
  )
})

test_that(".warn_multistate_binary_clashes stops on clashings values", {
  expect_warning(
    .warn_multistate_binary_clashes(ValueTable = df_check_multistate_binary_clashes, verbose = TRUE),
    "clash(es) found ", fixed = TRUE
  )
})

test_that(".warn_multistate_binary_clashespasses silently on clean data", {
  expect_invisible(
    .warn_multistate_binary_clashes(ValueTable = df_check_multistate_binary_clashes_clean, verbose = TRUE)
  )
})


test_that(".warn_multistate_binary_clashespasses silently on clean data", {
  expect_invisible(
    .warn_multistate_binary_clashes(ValueTable = df_check_multistate_binary_clashes_one_missing, verbose = TRUE)
  )
})



