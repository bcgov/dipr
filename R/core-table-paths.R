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
#' @param core_table_name a string of the data set name
#' @param pattern Defaults to dat.gz but can be extended for more extended using regex for more
#' flexibility
#' @param base_path Defaults to "R:/DATA/core-snapshot"
#' @export

get_core_dat_path <- function(core_version, core_table_name, pattern = "\\.dat.gz$", base_path = "R:/DATA/core-snapshot") {

  core_table_dir <- list.files(
    path = file.path(base_path, core_version),
    pattern = core_table_name, full.names = TRUE
    )
  list.files(core_table_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)

}

#' Get paths of data dictionaries
#'
#' @inheritParams get_core_dat_path
#' @param core_dict_name a string of the dictionary name. Often this is different from the data name
#' @export
get_core_dict_path <- function(core_version, core_dict_name, base_path = "R:/DATA/core-snapshot") {

  core_version_docs <- file.path(base_path, core_version, "docs")
  core_doc <- list.files(core_version_docs, pattern = core_dict_name,
                         full.names = TRUE,
                         recursive = TRUE,
                         ignore.case = TRUE)
  core_doc <- core_doc[grepl("\\.xlsx$|\\.nflt$", core_doc)]

  if (length(core_doc) > 1) core_doc <- core_doc[grepl("dictionary", core_doc)]

  if(grepl("\\.xlsx$", core_doc) & length(core_doc) == 1) {
    sheets <- readxl::excel_sheets(core_doc)

    if(length(sheets) > 1) {
      message(basename(core_doc), " contains the following sheets:")
      message(paste(sheets, "\n"))
    }
  }

  core_doc
}
