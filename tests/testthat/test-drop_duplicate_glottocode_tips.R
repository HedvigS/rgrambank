




test_that("drop_duplocate_glottocode_tips should drop duplicates", {
  
  result_tree <- drop_duplicate_glottocode_tips(tree = test_tree, TaxonTable = TaxonTable, LanguageTable = LanguageTable, merge_dialects = FALSE)
  result <- result_tree$tip.label |> sort() 
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  expected <- c("ainu1240", "kuri1271", "sakh1245", "hokk1243", "tara1248") |> sort()
  
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("drop_duplocate_glottocode_tips should drop duplicates", {
  
  result_tree <- drop_duplicate_glottocode_tips(tree = test_tree, TaxonTable = TaxonTable, LanguageTable = LanguageTable, merge_dialects = TRUE)
  result <- result_tree$tip.label |> sort() 
  # Sort both by Parameter_ID to ensure row order does not affect comparison
  expected <- c("ainu1240", "kuri1271", "sakh1245") |> sort()
  
  expect_equal(result, expected, ignore_attr = TRUE)
})
