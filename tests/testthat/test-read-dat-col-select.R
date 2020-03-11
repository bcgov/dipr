context('read_dat col_select mechanisms')

## fst
test_that('does initial read in for only two columns',{
  s_raw <- read_dat(data_path = dat_path,
                      data_dict = dict,
                      col_select = c("height","mass"),
                      use_cache = FALSE)
  expect_equal(ncol(s_raw), 2)
})

test_that('errors when an non-existent column is provided',{
  expect_error(read_dat(data_path = dat_path,
                        data_dict = dict,
                        col_select = c("height","foo"),
                        use_cache = FALSE))
})


test_that('does initial read in for only two columns for fst then cached read on has two cols',{
  fst_raw <- read_dat(data_path = dat_path,
                      data_dict = dict,
                      cache_type = "fst",
                      col_select = c("name", "height"),
                      cache_dir = cache_dir,
                      use_cache = TRUE)
  expect_equal(ncol(fst_raw), 2)

  temp_cache_file <- temp_file(cache_dir, "starwars-fwf.fst")
  expect_true(file.exists(temp_cache_file))

  fst_cached <- read_dat(data_path = dat_path,
                         data_dict = dict,
                         cache_type = "fst",
                         cache_dir = cache_dir,
                         use_cache = TRUE)
  expect_equal(ncol(fst_cached), 2)
  expect_equal(unlink(temp_cache_file), 0L)
})


test_that('does initial read in for only two columns for parquet then cached read on has two cols',{
  parquet_raw <- read_dat(data_path = dat_path,
                         data_dict = dict,
                         cache_type = "parquet",
                         col_select = c("name", "height"),
                         cache_dir = cache_dir,
                         use_cache = TRUE)
  expect_equal(ncol(parquet_raw), 2)

  temp_cache_file <- temp_file(cache_dir, "starwars-fwf.parquet")
  expect_true(file.exists(temp_cache_file))
#
  parquet_cached <- read_dat(data_path = dat_path,
                         data_dict = dict,
                         cache_type = "parquet",
                         cache_dir = cache_dir,
                         use_cache = TRUE)
  expect_equal(ncol(parquet_cached), 2)
})
