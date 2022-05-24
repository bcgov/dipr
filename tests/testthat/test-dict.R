test_that("fix_date_format works", {
  expect_equal(fix_dt_format("YYYY-MM-DD HH:MM:SS"), "%Y-%m-%d %H:%M:%S")
  expect_equal(fix_dt_format("YYYY-MM-DD hhmmss"), "%Y-%m-%d %H%M%S")
  expect_equal(fix_dt_format("yyyy/mm/dd HH:MM:SS"), "%Y/%m/%d %H:%M:%S")
  expect_equal(fix_dt_format("ccyymmdd"), "%Y%m%d")
  expect_equal(fix_dt_format("yyyy-mm-dd"), "%Y-%m-%d")
  expect_equal(fix_dt_format("mm-dd-yyyy"), "%m-%d-%Y")
})

test_that("health_dict_to_spec works", {
  dict <- data.frame(
    name = c(letters[1:8], "foo", "linefeed"),
    data_type = c("char", "varchar", "num", "number", "none", "date", "date", "date", "blah", "char"),
    data_format = c(rep(NA_character_, 5), "YYYY-MM-DD", "ccyymmdd hh:mm:ss", "", "", "")
  )
  out <- health_dict_to_spec(dict, special = list(foo = readr::col_integer()))
  expect_equal(
    out,
    list(a = readr::col_character(),
         b = readr::col_character(),
         c = readr::col_double(),
         d = readr::col_double(),
         e = readr::col_character(),
         f = readr::col_date(format = "%Y-%m-%d"),
         g = readr::col_datetime(format = "%Y%m%d %H:%M:%S"),
         h = readr::col_character(),
         foo = readr::col_integer(),
         linefeed = readr::col_skip())
  )
})

test_that("read_health_dict works", {
  dict <- read_health_dict(dipr_example("sample_hlth_dict.csv"))
  expect_s3_class(dict, "data.frame")
  expect_equal(names(dict),
               c("start", "stop", "length", "name", "data_type", "data_format", "col_type"))
  expect_equal(dict$col_type,
               c("c", "c", "d", "d", "c", "c", "d", "c", "c", "c", "c", "c"))
  spec <- health_dict_to_spec(dict, special = list(code1 = readr::col_integer()))
  expect_equal(
    spec,
    list(code = readr::col_character(),
         date = readr::col_date(format = "%Y%m%d"),
         anum = readr::col_double(),
         spec = readr::col_double(),
         expl_cd = readr::col_character(),
         amt1 = readr::col_character(),
         code1 = readr::col_integer(),
         code2 = readr::col_character(),
         type = readr::col_character(),
         date2 = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
         studyid = readr::col_character(),
         linefeed = readr::col_skip())
  )
})

