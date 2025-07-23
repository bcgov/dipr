test_that("make_sql_schema_string works", {
  db <- DBI::dbConnect(duckdb::duckdb())

  schema <- schema <- c(
    SERVDT = "DATE",
    PHNNUM = "VARCHAR",
    FITM = "BIGINT",
    PRACNUM = "VARCHAR",
    PAYENUM = "VARCHAR",
    FIRSTPAIDDT = "TIMESTAMP",
    LASTPAIDDT = "TIMESTAMP",
    PAIDSERV = "BIGINT",
    PMEPDAMT = "REAL",
    PMENIAMT = "REAL"
  )

  expect_snapshot(make_sql_schema_string(db, schema = schema))

  expect_snapshot(
    make_sql_schema_string(
      db,
      schema = schema,
      dt_override = c(LASTPAIDDT = "%Y-%m")
    )
  )

  schema <- c(
    SERVDT = "DATE",
    PHNNUM = "VARCHAR",
    FITM = "itsabiginteger"
  )

  expect_snapshot(make_sql_schema_string(db, schema = schema), error = TRUE)
})

test_that("make_select_statement_works", {
  schema <- schema <- c(
    SERVDT = "DATE",
    PHNNUM = "VARCHAR",
    FITM = "BIGINT",
    PRACNUM = "VARCHAR",
    PAYENUM = "VARCHAR",
    FIRSTPAIDDT = "TIMESTAMP",
    LASTPAIDDT = "TIMESTAMP",
    PAIDSERV = "BIGINT",
    PMEPDAMT = "REAL",
    PMENIAMT = "REAL"
  )

  expect_snapshot(make_select_statement(schema))

  expect_snapshot(
    make_select_statement(
      schema,
      col_rename = c(PHNNUM = "studyid", LASTPAIDDT = "last_paid_date")
    )
  )

  expect_snapshot(
    make_select_statement(
      schema,
      col_rename = c(PHNNUM = "studyid", LASTPAIDDT = "last_paid_date"),
      dt_override = c(LASTPAIDDT = "%Y-%m")
    )
  )
})

test_that("csv_to_duckdb_query works", {
  db <- DBI::dbConnect(duckdb::duckdb())

  schema <- schema <- c(
    SERVDT = "DATE",
    PHNNUM = "VARCHAR",
    FITM = "BIGINT",
    PRACNUM = "VARCHAR",
    PAYENUM = "VARCHAR",
    FIRSTPAIDDT = "TIMESTAMP",
    LASTPAIDDT = "TIMESTAMP",
    PAIDSERV = "BIGINT",
    PMEPDAMT = "REAL",
    PMENIAMT = "REAL"
  )


  expect_snapshot(
    csv_to_duckdb_query(
      db,
      "foo.csv",
      "tbl",
      schema,
      col_rename = c(PHNNUM = "studyid", LASTPAIDDT = "last_paid_date"),
      date_format = "%Y/%m/%d",
      timestamp_format = "%Y-%m%d $H:%M:%S",
      dt_override = c(LASTPAIDDT = "%Y-%m")
    )
  )

    expect_snapshot(
    csv_to_duckdb_query(
      db,
      "foo.csv",
      "tbl",
      schema,
      col_rename = c(PHNNUM = "studyid", LASTPAIDDT = "last_paid_date"),
      date_format = "%Y/%m/%d",
      timestamp_format = "%Y-%m%d $H:%M:%S",
      dt_override = list(FIRSTPAIDDT = c("%Y-m%", "%Y%m%d %h%M%S"), LASTPAIDDT = "%Y-%m")
    )
  )

  expect_snapshot(
    csv_to_duckdb_query(
      db,
      c("foo.csv", "bar.csv"),
      "tbl",
      schema
    )
  )

  expect_snapshot(
    csv_to_duckdb_query(
      db,
      "foo.csv",
      "tbl",
      schema,
      nullstr = "NULL"
    )
  )

  expect_snapshot(
    csv_to_duckdb_query(
      db,
      "foo.csv",
      "tbl",
      schema,
      nullstr = c("NULL", "NA", "")
    )
  )

    expect_snapshot(
    csv_to_duckdb_query(
      db,
      "foo.csv",
      "tbl",
      schema,
      nullstr = c("NULL", "NA", ""),
      opts = list(
        comment = "#",
        force_not_null = c("a", "b"),
        null_padding = TRUE,
        buffer_size = 5)
    )
  )
})


test_that("parse_opts works", {
  db <- DBI::dbConnect(duckdb::duckdb())

  expect_equal(parse_opts(db, NULL, list()), "")
  expect_snapshot(parse_opts(db, NULL, NULL), error = TRUE)
  expect_snapshot(parse_opts(db, list(), NULL), error = TRUE)

  expect_snapshot(
    parse_opts(db,
      nullstr = c("NA", "NULL"),
      opts = list(
        comment = "#",
        force_not_null = c("a", "b"),
        null_padding = TRUE,
        buffer_size = 5)
    )
  )

  expect_snapshot(
    parse_opts(db,
      opts = list(
        comment = "#",
        buffer_size = 5)
    )
  )

  expect_snapshot(
    parse_opts(
      db,
      opts = list(
        comment = "#",
        blahblah = 5
        )
    ),
    error = TRUE
  )
})
