df_make_binary_solo_expected <- data.frame(
  ID           = c("GB025a-lang1", "GB025b-lang1", 
                   "GB130a-lang1", "GB130b-lang1",
                   "GB065a-lang1", "GB065b-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1", "lang1", "lang1", "lang1"),
  Parameter_ID = c("GB025a", "GB025b", "GB130a", "GB130b", "GB065a", "GB065b"),
  Value        = c("1", "0", "1", "0", "1", "1"),
  Code_ID      = c("GB025a-1", "GB025b-0", "GB130a-1", "GB130b-0", "GB065a-1", "GB065b-1"),
  Comment      = c(NA, NA, NA, NA, NA, NA),
  Source       = c("griff1", "griff1", "griff1", "griff1", "griff1", "griff1"),
  Source_comment= c("griff1", "griff1", "griff1", "griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS", "HS", "HS", "HS"),
  stringsAsFactors = FALSE
)

test_that("make_binary_ValueTable() correctly binarises multistate features", {
  
  result <- make_binary_ValueTable(ValueTable = df_make_binary_solo, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_binary_solo_expected[order(df_make_binary_solo_expected$Parameter_ID), ]
  
  expect_equal(result, expected, ignore_attr = TRUE)
})



df_make_binary_multistate_mixed_expected <- data.frame(
  ID           = c("GB025a-lang1", "GB025b-lang1", "GB130a-lang1","GB130b-lang1", "GB065a-lang1", "GB065b-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1", "lang1", "lang1", "lang1"),
  Parameter_ID = c("GB025a","GB025b", "GB130a","GB130b",  "GB065a", "GB065b"),
  Value        = c("1", "0" , "1", "0" ,"1", "0"),
  Code_ID      = c("GB025a-1","GB025b-0", "GB130a-1","GB130b-0",  "GB065a-1", "GB065b-0"),
  Comment      = c(NA, NA, NA, NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1", "griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1", "griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS", "HS", "HS", "HS"),
  stringsAsFactors = FALSE
)

test_that("make_binary_ValueTable() correctly binarises multistate features when there are both multistate and binary values for the same features", {
  
  result <- make_binary_ValueTable(df_make_binary_multistate_mixed, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_binary_multistate_mixed_expected[order(df_make_binary_multistate_mixed_expected$Parameter_ID), ]
  
  expect_equal(result, expected, ignore_attr = TRUE)
})


df_make_binary_multistate_mixed_not_expected <- data.frame(
  ID           = c("GB025a-lang1", "GB025b-lang1", "GB130a-lang1","GB130b-lang1", "GB065a-lang1", "GB065b-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1", "lang1", "lang1", "lang1"),
  Parameter_ID = c("GB025a","GB025b", "GB130a","GB130b",  "GB065a", "GB065b"),
  Value        = c("1", "0" , "1", "0" ,"1", "1"),
  Code_ID      = c("GB025a-1","GB025b-0", "GB130a-1","GB130b-0",  "GB065a-1", "GB065b-1"),
  Comment      = c(NA, NA, NA, NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1", "griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1", "griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS", "HS", "HS", "HS"),
  stringsAsFactors = FALSE
)

test_that("make_binary_ValueTable() correctly binarises multistate features when there are both multistate and binary values for the same features", {
  
  result <- make_binary_ValueTable(df_make_binary_multistate_mixed, verbose = FALSE)
  
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  result <- result[order(result$Parameter_ID), ]
  expected <- df_make_binary_multistate_mixed_not_expected[order(df_make_binary_multistate_mixed_not_expected$Parameter_ID), ]
  
  expect_equal(result, expected, ignore_attr = TRUE)
})

