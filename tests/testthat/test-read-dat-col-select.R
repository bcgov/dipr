context('read_dat col_select mechanisms')

test_that('read_dat works when you dont specify a column', {
  s_no_col <- read_dat(data_path = dat_path,
                    data_dict = dict)
  expect_is(s_no_col, "tbl_df")
})

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

