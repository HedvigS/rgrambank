

test_that("make_multistate_ValueTable correctly renders multistate features from binary.", {
  
  result <- make_multistate_ValueTable(ValueTable = df_make_multistate_solo, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_multistate_solo_expected[order(df_make_multistate_solo_expected$Parameter_ID), ]
  
  expect_equal(result, expected, ignore_attr = TRUE)
})



