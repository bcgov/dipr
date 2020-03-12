context('read-dat preserves column types')

test_that("reads in every variable as character vector",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  col_types = c("cccc"),
                  use_cache = FALSE)

  ## Everything is a vector
  expect_is(unique(unlist(lapply(raw, class))), "character")
})



test_that("reads in every variable as character vector",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  col_types = c("cddl"),
                  use_cache = FALSE)

  ## Everything is a vector
  expect_is(unlist(lapply(raw, class)), c("character", "numeric", "numeric", "logical"))
})
