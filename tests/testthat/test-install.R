test_that("get_installed_version works with package not installed", {
  expect_equal(package_version("0.0"),
               get_installed_version("foobar")
  )
  expect_true(package_version("0.0.0.1") > get_installed_version("foobar"))
})

test_that("get_installed_version works with installed package", {
  expect_true(package_version("0.0") < get_installed_version("stats"))
})
