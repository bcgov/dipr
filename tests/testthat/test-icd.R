test_that("a missing icd_codes value fails", {
  expect_error(dipr_icd_categories())
})

test_that("dipr_icd_categories returns a vector",{
  expect_equal(dipr_icd_categories("786", 9), "disease of unknown etiology")
  expect_vector(dipr_icd_categories("786", 9))
})
