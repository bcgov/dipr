test_that("reads in every variable as character vector",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  col_types = c("cccccc"),
                  use_cache = FALSE)

  ## Everything is a vector
  lapply(raw, expect_type, "character")
})



test_that("reads in variables as specified",{
  raw <- read_dat(data_path = dat_path,
                  data_dict = dict,
                  col_types = c("cddlcD"),
                  use_cache = FALSE,
                  date_format = "%Y%m%d")

  ## Everything is a vector
  types <- c("character", "numeric", "numeric", "logical", "character", "Date")
  for (i in seq_along(raw)) {
    expect_true(inherits(raw[[i]], types[i]))
  }
})
