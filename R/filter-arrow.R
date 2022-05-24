# Copyright 2021 Province of British Columbia
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




#' Search across columns in a data.frame or an arrow .Dataset for rows matching a given pattern.
#'
#' Searches across columns in a data object for rows matching a given pattern.
#'
#' @param .data An object of either class data.frame or arrow .Dataset
#' @param code A character string representing a search term.
#' @param cols A vector of column names to filter on
#' @param partial A logical determining when partial matching should occur
#'
#' @details
#' -  Function currently returns a "data.frame" if passed a "data.frame".  If passed a .Dataset or arrow_dplyr_query,
#' the result will be an object of class arrow_dplyr_query which can preceed arrow::collect().
#' @export
filter_across <- function(.data, code, cols, partial = FALSE){

  UseMethod("filter_across")

}

#' @export
filter_across.default <- function(.data, code, cols, partial = FALSE){

  stop("No filter_across method for an object of class ", class(.data), call. = FALSE)

}

#' @export
filter_across.Dataset <- function(.data, code = NULL, cols = NULL, partial = FALSE){

  cond_str <- make_condition_string(.data, code = code, cols = cols, partial = partial)

  dplyr::filter(.data, eval(str2lang(cond_str)))
}

#' @export
filter_across.data.frame <- filter_across.Dataset


#' @export
filter_across.arrow_dplyr_query <- filter_across.Dataset


## add to filter_icd() documentation (details)
## - ICD-9 categories were defined according to the BC Ministry of Health's ICD-9 Diagnostic Code Descriptions
##  website: https://www2.gov.bc.ca/gov/content/health/practitioner-professional-resources/msp/physicians/diagnostic-code-descriptions-icd-9



#' Add a logical flag (new column) based on a pattern search across columns
#'
#' @inheritParams filter_across
#' @param cols a vector of column names to search in
#' @param flag_name the name of the new column to create
#'
#' @return An object the same class as the input (methods for `data.frame`, `Dataset`, `arrow_dplyr_query`),
#' with a new logical column
#'
#' @seealso [filter_across()]
#' @export
flag_across <- function(.data, code = NULL, cols = NULL, partial = FALSE, flag_name = NULL) {
  UseMethod("flag_across")
}

#' @export
flag_across.Dataset <- function(.data, code = NULL, cols = NULL, partial = FALSE, flag_name = NULL) {

  if (!is.character(flag_name) || !length(flag_name) == 1L) {
    stop("flag_name must be a length one character vector")
  }

  if (flag_name %in% names(.data)) {
    stop(flag_name, " is already a column in data")
  }

  cond_str <- make_condition_string(.data, code = code, cols = cols, partial = partial)

  dplyr::mutate(.data, {{flag_name}} := eval(str2lang(cond_str)))
}

#' @export
flag_across.data.frame <- flag_across.Dataset

#' @export
flag_across.arrow_dplyr_query <- flag_across.Dataset


make_condition_string <- function(.data, code, cols, partial) {

  ## restrict values of partial
  if (!is.logical(partial)) stop("partial needs to be TRUE or FALSE in filter_across", call. = FALSE)

  #check code is a string
  if(!length(code))
    stop("No code pattern provided to filter_across.", call. = FALSE)

  #check code is a string
  if(!nrow(.data))
    stop("No dataset provided to filter_across.", call. = FALSE)

  #coerce numeric codes to character
  if(is.numeric(code)){
    warning("Integer codes coerced to character type in filter_across.", call. = FALSE)
  }

  cols <- intersect(names(.data), cols)
  if(!length(cols))
    stop("No dataset columns match those provided to filter_across.", call. = FALSE)

  grepl_call <- ifelse(partial, "grepl('(%s)',%s)", "grepl('^(%s)$',%s)")

  paste0(sprintf(grepl_call, paste0(code, collapse = ")|("), cols), collapse = " | ")
}
