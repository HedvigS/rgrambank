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
