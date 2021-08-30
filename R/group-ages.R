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



#' Create age categories from a age vector
#'
#' @param age_column a numeric vector of age to be categorized
#' @param age_group Either a manually specified set of age groups OR
#' an age interval to be divided up into equal parts. To manually specify the age range
#' pass a vector of the right hand size of the age range. See example.
#' @param max_age Maximum age in the age internal. For example if max_age is 100 then
#' the function will calculate intervals all the way to 100+. This is ignored if a manual
#' age range is specified.
#' @param return_chr Should the function return a character or vector?
#'
#' @return A vector of age group the same length as the `age_column`
#'
#' @examples
#'   c_vec <- seq(1:100)
#'   g_vec <- group_ages(c_vec)
#'
#'   g_vec <- group_ages(c_vec, age_group = list(0:24, 25:50, 51:54, 55))
#'
#' @export
group_ages <-
  function(age_column,
             age_group = 5,
             max_age = 100,
             return_chr = TRUE) {
    if (!is.numeric(age_column)) {
      stop("age_column must be numeric")
    }

    if (inherits(age_group, "list")) {
      ## Check to see if age_groups overlap at all.
      if (!is_emptyish(Reduce(intersect, age_group))) {
        stop("manual age categories have overlapping ranges", call. = FALSE)
      }

      min_val <- vapply(age_group, min, FUN.VALUE = double(1))
      max_val <- vapply(age_group, max, FUN.VALUE = double(1))

      ## not pretty but it will work
      labs <-
        gsub("\\+-", "+", paste0(
          ifelse(min_val == max_val, paste0(min_val, "+"), min_val),
          "-",
          ifelse(max_val == min_val, "", max_val)
        ))

      res <-
        cut(
          age_column,
          breaks = c(min_val, Inf),
          labels = labs,
          right = FALSE
        )

      if (any(is.na(res))) {
        message("NA were produced likely because the age_group fell outside the range of age_column")
      }
    }

    if (inherits(age_group, "numeric")) {
      lb_labs <- seq(0, max_age - age_group, by = age_group)
      ub_labs <- seq(0 + age_group - 1, max_age - 1, by = age_group)
      labs <-
        c(
          glue::glue("{lb_labs}-{ub_labs}"),
          glue::glue("{max_age}+")
        )
      res <-
        cut(
          age_column,
          breaks = c(seq(0, max_age, by = age_group), Inf),
          labels = labs,
          right = FALSE
        )
    }

    if (!identical(length(age_column), length(res))) {
      stop("the age group is a different length that age_column")
    }


    if (!return_chr) {
      return(res)
    }

    as.character(res)
  }


#' Calculate the age of an invidividual on a certain date.
#'
#' This function is setup to primarily to deal with the demographics file.
#'
#' @param age_at_date date from which you want to calculate the age
#' @param dobyyyy year of birth.
#' @param dobmm month of birth.
age_at_date <- function(age_at_date, dobyyyy, dobmm) {
  birth_date <- lubridate::ymd(glue::glue("{dobyyyy}-{dobmm}-01"))

  if (any(birth_date > age_at_date)) warning("some birth dates showing up after age_at_date", call. = FALSE)

  life_interval <- lubridate::interval(birth_date, age_at_date) ## NAs fail to pars
  age <- floor(lubridate::time_length(life_interval, unit = "year"))
  ifelse(age == -1, 0, age)

}

