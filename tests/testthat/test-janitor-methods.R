test_that("clean_names works with Datasets", {
  tf <- withr::local_tempdir()
  arrow::write_dataset(iris, tf)

  ds <- arrow::open_dataset(tf)
  expect_equal(names(clean_names(ds)), janitor::make_clean_names(names(iris)))
})

test_that("clean_names works with arrow_dplyr_query", {
  tf <- withr::local_tempdir()
  arrow::write_dataset(iris, tf)

  ds <- arrow::open_dataset(tf) %>%
    dplyr::filter(Species == "setosa")
  expect_equal(names(clean_names(ds)), janitor::make_clean_names(names(iris)))
})

test_that("clean_names works with Table", {
  ds <- arrow::as_arrow_table(iris)
  expect_equal(names(clean_names(ds)), janitor::make_clean_names(names(iris)))
})
