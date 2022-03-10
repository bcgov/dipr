context('read-dat')

test_that("reads in file",{
  raw <- read_dat(data_path = dat_path,
                      data_dict = dict)

  expect_is(raw, "data.frame")

  raw2 <- read_dat(dat_path_csv, data_format = "csv")
  expect_is(raw2, "data.frame")

  expect_equivalent(raw, raw2)
})


test_that("reads in two files via vector", {
  files_vec <- c(dat_path, dat_path2)
  two_files <- read_dat(
    data_path = files_vec,
    data_dict = dict,
    id = "file"
  )
  expect_is(two_files, "tbl_df")
  expect_identical(files_vec, unique(two_files$file))
})

test_that("reads in two csv files via vector", {
  files_vec <- c(dat_path_csv, dat_path_csv2)
  two_files <- read_dat(
    data_path = files_vec,
    data_format = "csv",
    id = "file"
  )
  expect_is(two_files, "tbl_df")
  expect_identical(files_vec, unique(two_files$file))
})


test_that("returns a tibble when as.data.table=FALSE",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  as.data.table = FALSE)
  expect_is(raw, "tbl_df")
})


test_that("returns a message when as.data.table=TRUE",{
  expect_message(read_dat(data_path = dat_path,
                  data_dict = dict,
                  as.data.table = TRUE,
                  use_cache = FALSE),
                 'read_dat now only returns tibbles. To return a data.table object see read_dat_dt')
})

test_that('read_dat_dt returns a data.table',{
  dt <- read_dat_dt(data_path = dat_path,
                    data_dict = dict,
                    use_cache = FALSE)
  expect_is(dt, "data.table")
})

test_that("one can specify which values are NA.",{
  ls <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  use_cache = FALSE,
                  na = c("", "Luke Skywalker"))
  expect_equal(nrow(ls[ls$name == "Luke Skywalker",]), 1)
})





