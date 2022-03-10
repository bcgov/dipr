context('test read_nflt')

test_that('read_nflt works with .nflt files',{
  expect_message(d <- read_nflt(dipr_example('starwars-dict.nflt')))

  expect_is(d, "tbl_df")
})

test_that('read_nflt works with .nflt files with comments',{
  expect_message(d <- read_nflt(dipr_example('starwars-dict.nflt')))
  expect_message(d2 <- read_nflt(dipr_example('starwars-dict-with-comments.nflt')))

  expect_is(d2, "tbl_df")
  expect_equal(d, d2)
})
