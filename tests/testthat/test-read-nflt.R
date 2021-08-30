context('test read_nflt')

test_that('read_nflt works with .nflt files',{
  expect_message(d <- read_nflt(dipr_example('starwars-dict.nflt')))

  expect_is(d, "tbl_df")
})
