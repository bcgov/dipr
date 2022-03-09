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

#' Get the dat path of a particular dataset for a given core table name
#'
#' Return the paths of all data within a version that match a table name
#'
#' @param core_version Core version of tables typically formatted as a YYYYMMDD string
#' @param core_table_name an exact string of the data set name. Should match the folder inside the core-snapshot path. Leave as `NULL` if using `core_table_pattern` to match several folders.
#' @param core_table_pattern a string (can be a regex) to match a table name - can be a subdataset of `core_table_name` or a pattern to match a dataset that may be split over multiple dataset folders in the core-snapshot path.
#' @param pattern Defaults to dat.gz but can be extended for more extended using regex for more
#' flexibility
#' @param base_path Defaults to "R:/DATA/core-snapshot"
#'
#' @export

get_core_dat_path <- function(core_version, core_table_name = NULL, core_table_pattern = NULL,
                              pattern = "\\.dat.gz$", base_path = "R:/DATA/core-snapshot") {


  path <- if (is.null(core_table_name)) {
    file.path(base_path, core_version)
  } else {
    file.path(base_path, core_version, core_table_name, "dat")
  }

  core_table_files <- list.files(
    path = path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
    )

  if (!is.null(core_table_pattern)) {
    core_table_files <- core_table_files[grepl(core_table_pattern, core_table_files)]
  }

  normalizePath(
    core_table_files,
    winslash = "/",
    mustWork = TRUE
  )

}

#' Get paths of data dictionaries
#'
#' @inheritParams get_core_dat_path
#' @param core_dict_name a string of the dataset name.
#' @export
get_core_dict_path <- function(core_version, core_dict_name, base_path = "R:/DATA/core-snapshot") {

  core_version_docs <- normalizePath(
    file.path(base_path, core_version, core_dict_name, "docs"),
    winslash = "/",
    mustWork = TRUE
  )

  core_doc <- list.files(core_version_docs, pattern = "\\.xlsx$|\\.nflt$",
                         full.names = TRUE,
                         recursive = TRUE,
                         ignore.case = TRUE)

  if (length(core_doc) > 1) core_doc <- core_doc[grepl("dictionary|\\.nflt$", core_doc)]

  if(grepl("\\.xlsx$", core_doc) & length(core_doc) == 1) {
    sheets <- readxl::excel_sheets(core_doc)

    if(length(sheets) > 1) {
      message(basename(core_doc), " contains the following sheets:")
      message(paste(sheets, "\n"))
    }
  }

  core_doc
}
