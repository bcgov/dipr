## Copyright 2020 Province of British Columbia
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

#' Convert .dat.gz files into arrow formats
#'
#' Function to convert into formats provided by Apache Arrow.
#' These functions will overwrite files of the same name by default.
#' Convenience wrappers are provided for each arrow format.
#'
#' @inheritParams read_dat
#' @inheritParams base::file.copy
#' @param output_dir path to where you want to save the output file
#' @param arrow_format must be one of parquet, arrow or feather
#' @param ... passed to one or arrows writing functions depending on the value of `arrow_format`
#'
#' @export
#'

dat_to_arrow_formats <- function(data_path,
                           data_dict,
                           output_dir,
                           arrow_format,
                           col_types = NULL,
                           col_select = NULL,
                           overwrite = TRUE,
                           ...) {

  #browser()
  ## Check if data dictionary is in a valid format
  is_valid_data_dict(data_dict)

  if(!dir.exists(output_dir)) dir.create(output_dir)

  data_name <- gsub(".dat.gz", "", basename(data_path))

  ## Columns
  if(!is.null(col_select))  col_select <- col_selector(data_dict, col_select)

  d <- vroom_reader(data_path, data_dict, col_types, col_select)

  tf <- file.path(tempdir(), paste0(data_name, ".", arrow_format))


  if (arrow_format == "parquet") {
    arrow::write_parquet(d, sink = tf, ...)
  }

  if (arrow_format == "arrow") {
    arrow::write_arrow(d, sink = tf, ...)
  }

  if (arrow_format == "feather") {
    arrow::write_feather(d, sink = tf)
  }

  ## copy over
  if(!file.copy(tf, output_dir, overwrite = overwrite)) {
    stop(glue::glue("File copy from {tf} to {output_dir} was unsuccessful"), call. = FALSE)
  } else {
    ## delete temp file
    unlink(tf)
  }

  invisible(file.path(output_dir, paste0(data_name, ".", arrow_format)))
}

#' @describeIn dat_to_arrow_formats Convert to parquet
#' @export
dat_to_parquet <- function(...) {

  dat_to_arrow_formats(..., arrow_format = 'parquet')

}


#' @describeIn dat_to_arrow_formats Convert to arrow
#' @export
dat_to_arrow <- function(...) {

  dat_to_arrow_formats(..., arrow_format = 'arrow')
}

#' @describeIn dat_to_arrow_formats Convert to feather
#' @export
dat_to_feather <- function(...) {

  dat_to_arrow_formats(..., arrow_format = 'feather')

}


#' Write a parquet file in the SRE
#'
#' Wraps `arrow::write_parquet` but first writes to disk then moves to remote drive.
#'
#' @inheritParams arrow::write_parquet
#' @inheritParams base::file.copy
#' @param ... passed to `arrow::write_parquet`
#'
#' @export
dipr_write_parquet <- function (x, sink, overwrite = TRUE, ...) {

  if (!dir.exists(dirname(sink))) dir.create(dirname(sink))
  data_name <- basename(sink)

  tf <- file.path(tempdir(), data_name)

  arrow_ret <- arrow::write_parquet(x = x, sink = tf, ...)

  if (!file.copy(tf, sink, overwrite = overwrite)) {
    stop(glue::glue("File copy from {tf} to {sink} was unsuccessful"),
         call. = FALSE)
  }
  else {
    unlink(tf)
  }

  invisible(arrow_ret)
}

