context('read-dat')

test_that("reads in file with no cache",{
  raw <- read_dat(data_path = dat_path,
                      data_dict = dict,
                      use_cache = FALSE)

  expect_is(raw, "data.table")
})


test_that("returns a tibble when as.data.table=FALSE",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  as.data.table = FALSE,
                  use_cache = FALSE)
  expect_is(raw, "tbl_df")
})


test_that("writes fst to cache then read fst from cache",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  cache_dir = cache_dir,
                  use_cache = TRUE)

  temp_cache_file <- temp_file(cache_dir, "starwars-fwf.fst")
  expect_true(file.exists(temp_cache_file))


  d <- read_dat(data_path = dat_path,
                data_dict = dict,
                cache_dir = cache_dir,
                use_cache = TRUE)
  expect_is(d, "data.table")
  expect_equal(unlink(temp_cache_file), 0L)
})


test_that("writes readme and gitignore",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  cache_dir = cache_dir,
                  use_cache = TRUE)

  readme_file <- temp_file(cache_dir, "README.md")
  gitignore_file <- temp_file(cache_dir, ".gitignore")
  expect_true(file.exists(readme_file))
  expect_true(file.exists(gitignore_file))

  expect_equal(unlink(readme_file), 0L)
  expect_equal(unlink(gitignore_file), 0L)
})


test_that("writes csv to cache then reads csv from cache",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  cache_type = "csv",
                  cache_dir = cache_dir,
                  use_cache = TRUE)

  temp_cache_file <- temp_file(cache_dir, "starwars-fwf.csv")
  expect_true(file.exists(temp_cache_file))


  d <- read_dat(data_path = dat_path,
                data_dict = dict,
                cache_type = "csv",
                cache_dir = cache_dir,
                use_cache = TRUE)
  expect_is(d, "tbl_df")
})



test_that("writes parquet to cache then read parquet from cache",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  cache_type = "parquet",
                  cache_dir = cache_dir,
                  use_cache = TRUE)

  temp_cache_file <- temp_file(cache_dir, "starwars-fwf.parquet")
  expect_true(file.exists(temp_cache_file))


  d <- read_dat(data_path = dat_path,
                data_dict = dict,
                cache_type = "parquet",
                cache_dir = cache_dir,
                use_cache = TRUE)
  expect_is(d, "tbl_df")
})


test_that("use cache env variable", {
  Sys.setenv(DIPR_CACHE_PATH = cache_dir)


  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  cache_dir = cache_dir,
                  use_cache = TRUE)

  temp_cache_file <- temp_file(cache_dir, "starwars-fwf.fst")
  expect_true(file.exists(temp_cache_file))


  d <- read_dat(data_path = dat_path,
                data_dict = dict,
                cache_dir = cache_dir,
                use_cache = TRUE)
  expect_is(d, "data.table")
  expect_equal(unlink(temp_cache_file), 0L)

  Sys.unsetenv("DIPR_CACHE_PATH")

})

test_that("one can specify which values are NA.",{
  ls <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  use_cache = FALSE,
                  na = c("", "Luke Skywalker"))
  expect_equal(nrow(ls[ls$name == "Luke Skywalker",]), 0)
})





