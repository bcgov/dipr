context("test add_* functions")


test_that("add_linked_column works",{
  d_in <- data.frame(studyid = c("sxxxx", "uxxxx"))
  d_out <- add_linked_status_col(d_in)

  expect_is(d_out, class(d_in))
  expect_identical(names(d_out), c(names(d_in), "linked_status"))
})
