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


#' Add a column defining linked status
#'
#' This function is really meant to be used in a pipeline as a quick shortcut to add the
#' linked status column
#'
#' @param .data a data frame (or tibble) or arrow Dataset with a studyid column
#' @param studyid_col the name of the study id column. Defaults to "studyid"
#'
#' @return the same data frame plus a `linked_status` column
#'
#' @export
add_linked_status_col <- function(.data, studyid_col = "studyid"){

  UseMethod("add_linked_status_col")

}

#' @export
add_linked_status_col.default <- function(.data, studyid_col = "studyid"){
  stop("No add_linked_status_col method for an object of class ", class(.data), call. = FALSE)
}


#' @export
add_linked_status_col.data.frame <- function(.data, studyid_col = "studyid") {
  dplyr::mutate(
    .data, linked_status = ifelse(
      grepl("^s", !!rlang::sym(studyid_col)), "linked", "unlinked"
      )
    )
}

#' @export
add_linked_status_col.Dataset <- add_linked_status_col.data.frame


#' @export
add_linked_status_col.arrow_dplyr_query <- add_linked_status_col.Dataset

#' Wrapper to filter out unlinked studyids from either a data.frame or a Dataset
#'
#' This is a convenience function which automatically filter for only "linked" studyids
#' as defined by `add_linked_status()`.
#'
#' @inheritParams add_linked_status_col
#' @inheritDotParams add_linked_status_col
#'
#' @export
#'
#' @examples
#'
#' filter_linked(data.frame(studyid = c("sxxxx", "uxxxx")))
filter_linked <- function(.data, ...) {
    d <- add_linked_status_col(.data, ...)
    dplyr::filter(d, linked_status == "linked")
}
