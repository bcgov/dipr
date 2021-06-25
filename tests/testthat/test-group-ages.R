# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

test_that("group_ages fails with a character vector",{
  c_vec <- as.character(seq(1:20))
  expect_error(group_ages(c_vec))
})

test_that("group_ages returns a character vector",{
  c_vec <- seq(1:10)
  g_vec <- group_ages(c_vec)
  expect_type(g_vec, "character")
})

test_that("group_ages produces a correct age grouping",{
  c_vec <- seq(1:10)
  g_vec <- group_ages(c_vec)
  expect_equal(g_vec, c("0-4", "0-4", "0-4", "0-4", "5-9", "5-9", "5-9", "5-9", "5-9", "10-14"))

  g_vec2 <- group_ages(c_vec, age_group = 2)
  expect_equal(g_vec2, c("0-1", "2-3", "2-3", "4-5", "4-5", "6-7", "6-7", "8-9", "8-9", "10-11"))
})

test_that("group_ages returns the same length as the input",{
  c_vec <- seq(1:10)
  g_vec <- group_ages(c_vec)

  expect_equal(length(c_vec), length(g_vec))
})


test_that("group_ages accepts a custom category as a list",{
  c_vec <- seq(1:10)
  l_vec <- group_ages(c_vec, age_group = list(1:3, 4:5, 6:10))
  expect_equal(length(c_vec), length(l_vec))
})

test_that("group_ages accepts a custom category as a list and returns a factor",{
  c_vec <- seq(1:10)
  l_vec <- group_ages(c_vec, age_group = list(1:3, 4:5, 6:10), return_chr = FALSE)
  expect_s3_class(l_vec, "factor")
})


test_that("overlapping ranges fail",{
  c_vec <- seq(1:10)
  expect_error(group_ages(c_vec, age_group = list(1:6, 6:10)), 'manual age categories have overlapping ranges')
})

test_that('a message is output when a age_group is smaller the range of age_column',{
  c_vec <- seq(1:8)
  expect_message(group_ages(c_vec, age_group = list(4:7,8:10)),
                 'NA were produced likely because the age_group fell outside the range of age_column')
})


test_that('test that age_at_date works',{
  expect_equal(age_at_date(lubridate::ymd("2019-12-25"), 1990, 01), 29)

  expect_warning(
    age_at_date(lubridate::ymd("2019-12-25"), 2020, 01),
    "some birth dates showing up after age_at_date")

})
