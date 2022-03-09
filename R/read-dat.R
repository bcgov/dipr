## Copyright 2019 Province of British Columbia
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
## http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

#' Read compressed files in R
#'
#' Unzip and read into memory. The `read_dat_dt` function will return a data.table
#' object, replacing the previous `as.data.table` argument which is now deprecated. You can
#' use the same arguments as the `read_dat` function. If cache = TRUE, the first time you call
#' this function, the data is cached in a folder called \code{.dipr} in a format specified by
#' the cache_type argument. This function expects a data dictionary with the following columns:
#'  - start
#'  - stop
#'  - name
#'
#'
#'
#' @param data_path A path or a vector of paths to a `.dat.gz` file. If supplying a vector of paths,
#' they must share a common data dictionary.
#' @param data_dict A data.frame with `start`, `stop` and `name` columns
#' @param as.data.table Deprecated. See `read_dat_dt`
#' @param col_select A vector of column names
#' @param use_cache deprecated,
#' @param data_format the format of the input data. Default is `"fwf"`, other choices
#' are `"csv"`, `"csv2"`, `"tsv"`
#' @inheritParams readr::read_fwf
#' @inheritDotParams readr::read_fwf
#'
#' @export
#'
#' @examples
#' data_dict_path <- dipr_example("starwars-dict.txt")
#' dict <- read.table(data_dict_path)
#' dat_path <- dipr_example("starwars-fwf.dat.gz")
#' read_dat(data_path = dat_path,
#'          data_dict = dict)


read_dat <- function(data_path,
                     data_dict,
                     as.data.table = FALSE,
                     use_cache = FALSE,
                     col_select = NULL,
                     col_types = NULL,
                     data_format = c("fwf", "csv", "tsv", "csv2"),
                     ...) {

  data_format = match.arg(data_format)

  if (data_format == "fwf") {
    ## Check if data dictionary is in a valid format
    is_valid_data_dict(data_dict)
  }

  data_name <- gsub(".dat.gz", "", basename(data_path))

  if(use_cache) stop("Caching directly in dipr is deprecated", call. = FALSE)

  ## Columns
  if(!is.null(col_select))  col_select <- col_selector(data_dict, col_select)

  cli::cli_alert_success("Reading {data_name}")
  d <- dipr_reader(data_path, data_dict, col_types, col_select, data_format, ...)

  if (as.data.table) return(message('read_dat now only returns tibbles. To return a data.table object see read_dat_dt'))

  gc()

  d
}


#' @export
#' @describeIn read_dat

#'
read_dat_dt <- function(...) {
  d <- read_dat(...)

  data.table::setDT(d)
}

#' Delete all content from any temp directory
#'
#' This function helps clean up after readr and other functions that use the temp directory
#' to store transient files. Sometimes this can balloon in size and it is useful run this command to
#' clean things up.
#'
#' @param are_you_sure logical Defaults to FALSE so that a user does not do this by accident.
#' @export
delete_rtmp_dirs <- function(are_you_sure = FALSE){

  if(!are_you_sure) stop("Change the are_you_sure argument to TRUE if you really want to delete these files", call. = FALSE)

  tmpdir <- list.dirs(dirname(tempdir()))
  rtmpdirs <- tmpdir[grepl("Rtmp", tmpdir)]

  files_to_delete <- list.files(rtmpdirs, recursive = TRUE, full.names = TRUE)

  unlink(files_to_delete)
}
