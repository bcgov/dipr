# Copyright 2019 Province of British Columbia
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

#' A function to extract the data name from a path
#'
#' This function depends on the path being a .dat.gz file
#'
#' @param path file path

extract_path_name <- function(path) {

  extension <- tools::file_ext(path)

  name <- switch(extension,
                 gz = gsub("\\.dat.gz", "", basename(path)),
                 nflt = gsub("\\.nflt", "", basename(path)))

  if (is.null(name)) {
    stop(glue::glue(".{extension} extension not supported"), call. = FALSE)
  }

  name

}


#' Read data dictionaries in nflt format
#'
#' This function will be quite fragile to changes in the data format. It assumes four columns
#'
#' @param path Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' @export
read_nflt <- function(path) {

  if (!tools::file_ext(path) == "nflt") {
    stop("Not an .nflt file")
  }

  d <- readr::read_delim(path,
                         delim = "\t",
                         skip = 1,
                         col_names = c("start", "length", "type", "name"))
  d <- dplyr::mutate(d, start = as.numeric(trimws(start)))
  d <- dplyr::mutate(d, length = as.numeric(trimws(length)))
  d <- dplyr::mutate(d, stop = start + length -1)
  dplyr::mutate(d, name = janitor::make_clean_names(name))
}


#' Read Health Data Dictionaries
#'
#' Read into R pre-formatted popdata data dictionaries. These files are in .xlsx format and therefore
#' require both a path and sheet argument. The rest of the function is a thin wrapper around reaxl::read_excel
#' with some formatting taking place.
#'
#' @inheritParams readxl::read_excel
#' @param ... arguments passed to `readxl::read_excel`
#' @export
read_health_dict <- function(path, sheet, ...) {

  if (!tools::file_ext(path) == "xlsx") {
    stop("Not an .xlsx file")
  }

  readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = 1,
    na = c("", "NA", "N/A"),
    .name_repair = 'minimal'
  ) %>% ## TODO: deconstruct pipe
    janitor::clean_names() %>%
    dplyr::mutate(name = janitor::make_clean_names(.data$name_abbrev)) %>%
    dplyr::select(.data$start:.data$name, .data$data_type, .data$data_format, -.data$name_abbrev) %>%
    dplyr::mutate(data_type = tolower(.data$data_type)) %>% ## makes conditional safer
    dplyr::mutate(
      col_type = dplyr::case_when(
        ## separate out into a function
        .data$data_type %in% c("number", "num") ~ "d",
        .data$data_type == "date" ~ "c",
        .data$data_type == "char" ~ "c",
        TRUE ~ "c"
      )
    )
}


delete_rtmp_dirs <- function(are_you_sure = FALSE){

  if(!are_you_sure) stop("Change the are_you_sure argument to TRUE if you really want to delete these files", call. = FALSE)

  tmpdir <- list.dirs(dirname(tempdir()))
  rtmpdirs <- tmpdir[grepl("Rtmp", tmpdir)]

  files_to_delete <- list.files(rtmpdirs, recursive = TRUE, full.names = TRUE)

  unlink(files_to_delete)
}
