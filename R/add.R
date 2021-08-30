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
#' @param .data a data frame (or tibble) with a studyid column
#'
#' @return the same data frame plus a `linked_status` column
#'
#' @export


add_linked_status_col <- function(.data) {
  if (!"studyid" %in% names(.data)) stop("No studyid column", call. = FALSE)

  .data$linked_status <- ifelse(grepl("^s", .data$studyid), "linked", "unlinked")

  .data
}
