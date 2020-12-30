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
#' Unzip and read into memory (using vroom). The first time you call this function, the data
#' is cached in a folder called \code{.dipr} in a format specified by the cache_type argument.
#' This function expects a data dictionary with the following columns:
#'  - start
#'  - stop
#'  - name
#'
#'
#' @param data_path A path to a `.dat.gz` file
#' @param data_dict A data.frame with `start`, `stop` and `name` columns
#' @param use_cache A logical value if you want to use a cache. This create a `.dipr` directory
#' in your project and stores data there. Defaults to FALSE.
#' @param cache_type A string of the file type you want to store as your cache. Possible values
#' are "fst", "csv", or "parquet"
#' @param cache_dir Directory where you want you to store your cache. Defaults to `NULL` which results in
#' a `.dipr` directory in your project. This can also be set by the environment variable `DIPR_CACHE_PATH`
#' @param as.data.table A logical value if you want a data.table object returned.
#' @param col_select A vector of column names
#' @inheritParams vroom::vroom_fwf
#' @param ... arguments passed to `vroom::vroom_fwf`
#'
#' @export


read_dat <- function(data_path,
                     data_dict,
                     use_cache = FALSE,
                     cache_type = "fst",
                     cache_dir = NULL,
                     as.data.table = TRUE,
                     col_select = NULL,
                     col_types = NULL, ...) {


  ## Check if data dictionary is in a valid format
  is_valid_data_dict(data_dict)

  ## Columns
  if(!is.null(col_select))  col_select <- col_selector(data_dict, col_select)

  ## Caching
  data_name <- gsub(".dat.gz", "", basename(data_path))

  cache_path <- NULL
  if (is.null(cache_dir) && nzchar(Sys.getenv('DIPR_CACHE_PATH')) && use_cache) {

    message(glue::glue("Using cache at {Sys.getenv('DIPR_CACHE_PATH')} set by the DIPR_CACHE_PATH env variable"))
    cache_path <- glue::glue("{Sys.getenv('DIPR_CACHE_PATH')}/{data_name}.{cache_type}")
  }

  if (is.null(cache_dir) && !nzchar(Sys.getenv('DIPR_CACHE_PATH')) && use_cache) {
    cache_path <- glue::glue(".dipr/{data_name}.{cache_type}")
  }

  if (!is.null(cache_dir)) {
    cache_path <- glue::glue("{cache_dir}/{data_name}.{cache_type}")
  }


  if (dip.file.exists(cache_path) && use_cache) {
    return(read_cache(cache_path, cache_type, col_select, as.data.table))
  } else {
    message(glue::glue('Reading {data_name}'))
    d <- vroom_reader(data_path, data_dict, col_types, col_select, ...)
  }

  if (use_cache) {
    message(glue::glue('Creating a {cache_type} cache_type for the first time in this project.
    Go get a coffee. This could take awhile'))
    create_cache_dir(cache_path)
    write_cache(d, cache_path, cache_type)
  }

  if (as.data.table) data.table::setDT(d)

  gc()

  d
}

#' Delete all content from any temp directory
#'
#' This function helps clean up after vroom and other functions that use the temp directory
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
