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






