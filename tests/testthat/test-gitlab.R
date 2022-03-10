context("Test install tools")

test_that("get_gitlab_sre_repos returns a data.frame",{
  skip_if_not(Sys.getenv("USERDOMAIN") == "POPDATA")
  expect_s3_class(get_gitlab_sre_repos(set_gitlab_credentials()$password), "data.frame")
})

test_that("get_repo_id for dipr returns 90", {
  skip_if_not(Sys.getenv("USERDOMAIN") == "POPDATA")
  expect_equal(get_repo_id("dipr", set_gitlab_credentials()$password), 90)
})


test_that("update_needed returns a NULL when ref is 'dev'",{
  skip_if_not(Sys.getenv("USERDOMAIN") == "POPDATA")
  expect_null(update_needed("dipr", ref = "dev", set_gitlab_credentials()$password))
})

test_that("a custom sha is passed all the way to the return value",{
  skip_if_not(Sys.getenv("USERDOMAIN") == "POPDATA")
  expect_equal(
    update_needed("dipr", ref = "custom_sha", token = set_gitlab_credentials()$password),
    "custom_sha"
  )
})

