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
#' This function will be quite fragile to changes in the data format. It assumes
#' four columns
#'
#' @param path Either a path to a file, a connection, or literal data (either a
#'   single string or a raw vector).
#' @param comment A string used to identify comments. Any text after the comment
#'   characters will be silently ignored. Default `"/*"`, which is used in some
#'   nflt files.
#' @param ... arguments passed on to [readr::read_delim()]
#'
#' @export
read_nflt <- function(path, comment = "/*", ...) {

  if (!tools::file_ext(path) == "nflt") {
    stop("Not an .nflt file")
  }

  d <- readr::read_delim(path,
                         delim = "\t",
                         skip = 1,
                         col_names = c("start", "length", "type", "name"),
                         comment = comment,
                         trim_ws = TRUE,
                         ...)
  d$start <- as.numeric(trimws(d$start))
  d$length <-  as.numeric(trimws(d$length))
  d$stop <-  d$start + d$length - 1
  d$name <-  janitor::make_clean_names(d$name)
  d
}


#' Read Health Data Dictionaries
#'
#' **`read_health_dict`** reads into R pre-formatted popdata data dictionaries,
#' while **`health_dict_to_spec`** converts a health dict to a readr spec.
#'
#'
#' **`read_health_dict`**: Files are in .xlsx format and therefore require both a path and sheet
#' argument. The rest of the function is a thin wrapper around reaxl::read_excel
#' with some formatting taking place.
#'
#' **`health_dict_to_spec`** converts a
#' health dict created by `read_health_dict` to a readr spec. This will use the
#' dictionary to create specifications even for date and datetime columns, and allows
#' overriding the default column specs by using the `special` parameter.
#'
#' @inheritParams readxl::read_excel
#' @param ... arguments passed to `readxl::read_excel`
#' @examples
#' dict <- read_health_dict(dipr_example("sample_hlth_dict.csv"))
#' dict
#' health_dict_to_spec(dict, special = list(code1 = readr::col_integer()))
#' @return `read_health_dict`: A clean data.frame of health data dictionary
#' @export
read_health_dict <- function(path, sheet, ...) {

  ext <- tools::file_ext(path)

  if (!ext %in% c("xlsx", "xls", "csv")) {
    stop("Not an .xls(x) or csv file")
  }

  if (ext ==  "csv") {
    dict <- readr::read_csv(path, na = c("", "NA", "N/A"))
  } else {
    dict <- readxl::read_excel(
      path = path,
      sheet = sheet,
      skip = 1,
      na = c("", "NA", "N/A"),
      .name_repair = 'minimal'
    )
  }

  janitor::clean_names(dict) %>%
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

#' @param health_dict a data.frame, output of [read_health_dict()]
#' @param special a named list of readr column specifications for columns where
#'   you want to override the format in the dictionary file
#'
#' @return `health_dict_to_spec`: a named list of readr column specifications
#'   that can be passed on to the `col_types` argument of any of the readr
#'   functions, or [dat_to_parquet()] and friends.
#' @rdname read_health_dict
#' @export
health_dict_to_spec <- function(health_dict, special = NULL) {
  coltypes <- list(
    num = readr::col_double(),
    number = readr::col_double(),
    char = readr::col_character(),
    varchar = readr::col_character(),
    none = readr::col_character()
  )

  col_spec <- sapply(health_dict$name, function(n) {
    col_meta <- unlist(health_dict[health_dict$name == n, ])
    type <- col_meta["data_type"]
    fmt <- col_meta["data_format"]

    if (grepl("date", type) && grepl("[Yy]", fmt) && grepl("[Mm]", fmt) && grepl("[Dd]", fmt)) {
      if (grepl("[Hh]", fmt)) {
        readr::col_datetime(format = fix_dt_format(fmt))
      } else {
        readr::col_date(format = fix_dt_format(fmt))
      }
    } else if (n == "linefeed") {
      readr::col_skip()
    } else if (type %in% names(coltypes)) {
      coltypes[[type]]
    } else {
      readr::col_character()
    }
  })

  if (!is.null(special)) {
    for (s in names(special)) {
      stopifnot(s %in% names(col_spec))
      stopifnot(inherits(special[[s]], "collector"))
      col_spec[[s]] <- special[[s]]
    }
  }

  col_spec
}

fix_dt_format <- function(x) {
  x <- gsub("[CcYy]{4}([-/])?[Mm]{1,2}([-/])?[Dd]{1,2}", "%Y\\1%m\\2%d", x)
  x <- gsub("[Mm]{1,2}([-/])?[Dd]{1,2}([-/])?[CcYy]{4}", "%m\\1%d\\2%Y", x)
  x <- gsub("[Dd]{1,2}([-/])?[Mm]{1,2}([-/])?[CcYy]{4}", "%d\\1%m\\2%Y", x)
  x <- gsub("[Hh]{1,2}(:)?[Mm]{1,2}(:)?[Ss]{1,2}", "%H\\1%M\\1%S", x)
  unname(x)
}

dipr_locale <- function(tz = "UTC", date_format, time_format) {
  readr::locale(encoding = "latin1", tz = tz,
                date_format = date_format, time_format = time_format)
}

delete_rtmp_dirs <- function(are_you_sure = FALSE){

  if(!are_you_sure) stop("Change the are_you_sure argument to TRUE if you really want to delete these files", call. = FALSE)

  tmpdir <- list.dirs(dirname(tempdir()))
  rtmpdirs <- tmpdir[grepl("Rtmp", tmpdir)]

  files_to_delete <- list.files(rtmpdirs, recursive = TRUE, full.names = TRUE)

  unlink(files_to_delete)
}
