test_that("remove_root_from_path works", {
  exp <- "foo/bar"
  expect_equal(
    remove_root_from_path("C:\\a_folder\\path\\foo\\bar", "C:/a_folder/path"),
    exp
  )
  expect_equal(
    remove_root_from_path("C:/a_folder/path/foo/bar", "C:\\a_folder\\path"),
    exp
  )
  expect_equal(
    remove_root_from_path("C:/a-folder/path/foo/bar", "C:/a-folder/path"),
    exp
  )
})
