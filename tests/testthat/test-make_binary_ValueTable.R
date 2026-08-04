

test_that("make_binary_ValueTable() correctly binarises multistate features", {
  
  result <- make_binary_ValueTable(ValueTable = df_make_binary_solo, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_binary_solo_expected[order(df_make_binary_solo_expected$Parameter_ID), ]
  
  expect_equal(result, expected, ignore_attr = TRUE)
})


test_that("make_binary_ValueTable() correctly binarises multistate features when there are both multistate and binary values for the same features", {
  
  result <- make_binary_ValueTable(df_make_binary_multistate_mixed, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_binary_multistate_mixed_expected[order(df_make_binary_multistate_mixed_expected$Parameter_ID), ]
  
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("make_binary_ValueTable() correctly binarises multistate features when there are both multistate and binary values for the same features. This test checks that it doesn't render what we know is incorrect", {
  
  result <- make_binary_ValueTable(df_make_binary_multistate_mixed, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_binary_multistate_mixed_not_expected[order(df_make_binary_multistate_mixed_not_expected$Parameter_ID), ]
  
  expect_false(isTRUE(all.equal(result, expected, check.attributes = FALSE)))
  })

