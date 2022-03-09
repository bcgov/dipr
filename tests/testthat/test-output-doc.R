context('test_output_doc')


test_that("dipr_document_output_groups works", {
  tcsv <- file.path(tempdir(), "temp.csv")
  write.csv(iris, file = tcsv, row.names = FALSE)
  expect_output(
    dipr_document_output_groups(dirname(tcsv)),
    "150 rows by 5 columns")
  on.exit(unlink(tcsv))
})
