test_that("add_linked_column works on a data.frame",{
  d_out <- add_linked_status_col(d_in)

  expect_s3_class(d_out, class(d_in))
  expect_identical(names(d_out), c(names(d_in), "linked_status"))
})

test_that("add_linked_column works with a different studyid col name",{
  d_out <- add_linked_status_col(d_in, studyid_col = "alt_studyid")

  expect_s3_class(d_out, class(d_in))
  expect_identical(names(d_out), c(names(d_in), "linked_status"))
})

test_that("add_linked_column works on a data.frame",{
  p_in <- arrow::open_dataset(parquet_path)
  p_out <- add_linked_status_col(p_in)

  expect_s3_class(p_out, "arrow_dplyr_query")
  expect_identical(names(p_out), c(names(p_in), "linked_status"))
})

test_that("filter_linked works", {
  d_filt <- d_in %>% filter_linked()
  expect_equal(unique(d_filt$linked_status), "linked")

  p_filt <- arrow::open_dataset(parquet_path) %>%
    filter_linked() %>%
    dplyr::collect()
  expect_equal(unique(p_filt$linked_status), "linked")
})
