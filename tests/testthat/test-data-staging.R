test_that("make_dup_id_table", {
  set.seed(42)
  clients <- data.frame(
    c_id = letters,
    studyid = paste0(sample(letters, 26), rpois(26, 5))
  )

  dupids <- data.frame(
    c_id = letters[1:10],
    studyid = paste0(sample(letters, 10), rpois(10, 5))
  )

  d <- make_dup_id_table(clients, dupids, "c_id")
  expect_equal(ncol(d), 2)
  expect_false(all(d$primary_studyid < d$duplicate_studyid))

  d <- make_dup_id_table(clients, dupids, "c_id", sorted = TRUE)
  expect_equal(ncol(d), 2)
  expect_true(all(d$primary_studyid < d$duplicate_studyid))

  d <- make_dup_id_table(clients, dupids, "c_id", drop_clientid = FALSE)
  expect_equal(ncol(d), 3)
  expect_false(all(d$primary_studyid < d$duplicate_studyid))

  d <- make_dup_id_table(clients, dupids, "c_id", drop_clientid = FALSE, sorted = TRUE)
  expect_equal(ncol(d), 3)
  expect_true(all(d$primary_studyid < d$duplicate_studyid))
})

test_that("restage", {
  set.seed(42)
  clients <- data.frame(
    c_id = letters,
    studyid = paste0(sample(letters, 26), rpois(26, 5))
  )

  dupids <- data.frame(
    c_id = letters[1:10],
    studyid = paste0(sample(letters, 10), rpois(10, 5))
  )

  domain <- data.frame(
    studyid = c(
      paste0(sample(letters, 26), rpois(26, 5)),
      paste0(sample(letters, 30, replace = TRUE), rpois(30, 6))
    ),
    some_data = rnorm(56)
  )

  d <- make_dup_id_table(clients, dupids, "c_id", sorted = TRUE)

  expect_message(out <- restage(domain, d), "3")
  expect_silent(restage(domain, d, count_changed_ids = FALSE))
  expect_false(any(d$duplicate_studyid %in% out$studyid))
})

test_that("restage with duckdb", {
  set.seed(42)
  clients <- data.frame(
    c_id = letters,
    studyid = paste0(sample(letters, 26), rpois(26, 5))
  )

  dupids <- data.frame(
    c_id = letters[1:10],
    studyid = paste0(sample(letters, 10), rpois(10, 5))
  )

  domain <- data.frame(
    studyid = c(
      paste0(sample(letters, 26), rpois(26, 5)),
      paste0(sample(letters, 30, replace = TRUE), rpois(30, 6))
    ),
    some_data = rnorm(56)
  )

  d <- make_dup_id_table(clients, dupids, "c_id", sorted = TRUE)

  db <- DBI::dbConnect(duckdb::duckdb())
  dplyr::copy_to(db, domain)
  dplyr::copy_to(db, d)

  domain_tbl <- dplyr::tbl(db, "domain")
  d_tbl <- dplyr::tbl(db, "d")

  expect_message(out <- restage(domain_tbl, d_tbl), "3")
  expect_silent(restage(domain_tbl, d_tbl, count_changed_ids = FALSE))
  expect_false(
    any(dplyr::pull(d_tbl, duplicate_studyid) %in% dplyr::pull(out, studyid))
    )

  ## Write table
  qry <- restage(domain_tbl, d_tbl, count_changed_ids = FALSE)
  dplyr::compute(qry, name = "restaged_domain", temporary = FALSE)
  expect_in("restaged_domain", DBI::dbListTables(db))
})
