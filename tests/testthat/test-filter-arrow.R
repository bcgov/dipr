make_test_data <- function(format = "Dataset"){
  #make some data
  d <- data.frame(id = 1:10,
                  icd_1 = as.character(1001:1010),
                  icd_2 = as.character(1011:1020))

  if (format == "Dataset"){
    tf <- tempfile()
    arrow::write_dataset(d, tf, format =  "arrow")
    d = arrow::open_dataset(tf, format = "arrow")
  }
  return(d)
}

test_that("test a non-dataset object",{
  expect_error(filter_across("blah", code = "9999", cols = c("icd_1", "icd_2"), partial = FALSE))
})

test_that("test code data types",{
  #create Dataset object
  d <- make_test_data(format = "Dataset")
  expect_error(filter_across(d, code = NULL, cols = c("icd_1", "icd_2"), partial = FALSE))
  expect_warning(filter_across(d, code = 1001:1005, cols = c("icd_1", "icd_2")))
})

test_that("test for partial codes",{
  #create Dataset object
  d <- make_test_data(format = "Dataset")
  expect_s3_class(filter_across(d, code = "01", cols = c("icd_1", "icd_2"), partial = FALSE), "arrow_dplyr_query")
  expect_equal(nrow(filter_across(d, code = "01", cols = c("icd_1", "icd_2"), partial = TRUE)), 10)
  expect_equal(nrow(filter_across(d, code = c("3", "1004"), cols = c("icd_1", "icd_2"), partial = TRUE)), 2)
})

test_that("test for no icd columns",{
  #create Dataset object
  d <- make_test_data(format = "Dataset")
  expect_error(filter_across(d, code = c("1001"), partial = TRUE))
  expect_error(filter_across(d, code = c("1001", "03"), cols = c("dog", "cat"), partial = TRUE))
})

test_that("test for arrow_dplyr_query return type",{
  #create Dataset object
  d <- make_test_data(format = "Dataset" )
  expect_s3_class(filter_across(d, "1009", cols = c("icd_1", "icd_2")), "arrow_dplyr_query")

  #create arrow_dplyr_query object
  d <- d %>% dplyr::select(names(d))
  expect_s3_class(filter_across(d, "1009", cols = c("icd_1")), "arrow_dplyr_query")
})

test_that("test for data.frame return type",{
  #create Dataset object
  d <- make_test_data(format = "data.frame" )
  expect_s3_class(filter_across(d, "1019", cols = c("icd_1")), "data.frame")
})

test_that("test for simple regex pattern matching",{
  #create Dataset object
  d <- make_test_data(format = "data.frame" )
  expect_equal(nrow(filter_across(d, "[0-9]", cols = c("icd_1"))), 0)
  expect_equal(nrow(filter_across(d, "[0-9]", cols = c("icd_1"), partial = TRUE)), 10)
  expect_equal(nrow(filter_across(d, "101[0-9]", cols = c("icd_1"), partial = TRUE)), 1)
})

test_that("flag_across works with arrow_dplyr_query and Dataset return type",{
  #create Dataset object
  d <- make_test_data(format = "Dataset" )
  out <- flag_across(d, "1009", cols = c("icd_1", "icd_2"), flag_name = "new_col")
  expect_s3_class(out, "arrow_dplyr_query")
  expect_equal(names(out), c(names(d), "new_col"))

  #create arrow_dplyr_query object
  d <- d %>% dplyr::select(names(d))
  out <- flag_across(d, "1009", cols = c("icd_1"), flag_name = "new_col")
  expect_s3_class(out, "arrow_dplyr_query")
  expect_equal(names(out), c(names(d), "new_col"))
  out <- dplyr::collect(out)
  expect_type(out$new_col, "logical")
})

test_that("flag_across works with data.frame return type",{
  #create Dataset object
  d <- make_test_data(format = "data.frame" )
  out <- flag_across(d, "1019", cols = c("icd_1"), flag_name = "new_col")
  expect_equal(names(out), c(names(d), "new_col"))
  expect_type(out$new_col, "logical")
  expect_s3_class(out, "data.frame")
})

test_that("flag_across fails correctly", {
  d <- make_test_data(format = "data.frame" )
  expect_error(flag_across(d, "1019", cols = c("icd_1"), flag_name = "id"),
               "already a column")
  expect_error(flag_across(d, "1019", cols = c("icd_1"), flag_name = 9),
               "character")
  expect_error(flag_across(d, "1019", cols = c("icd_1"), flag_name = c("a", "b")),
               "length")
})
