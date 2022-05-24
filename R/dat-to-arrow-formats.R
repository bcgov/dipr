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
                           data_format = c("fwf", "csv", "tsv", "csv2"),
                           tz = "UTC",
                           date_format = "%AD",
                           time_format = "%AT",
                           ...) {

  data_format = match.arg(data_format)

  #browser()

  data_name <- gsub(".dat.gz", "", basename(data_path))

  ## Columns
  if (!is.null(col_select))  col_select <- col_selector(data_dict, col_select)

  d <- dipr_reader(data_path, data_dict, col_types, col_select, data_format, tz = tz, date_format = date_format, time_format = time_format, ...)

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

  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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

#' Convert dat.gz file to Apache Arrow Datasets using partitioned folder structures
#'
#' Some large files in the SRE are too large to fit into memory. `dat_to_datasets`
#' reads files into memory in smaller chunks (controlled by the `chunk_size` argument)
#' and converts them into Arrow Datasets. All `...` argument are passed to `dipr::read_dat`
#' which is where column types can be specified.
#'
#' @param chunk_size The number of rows to include in each chunk. The value of this
#' parameter you choose will depend on both the number of rows in the data you are
#' trying to process _and_ the RAM available. You can check the RAM available using
#' `memory.size(max = TRUE)`. The default for this values is currently 10 million.
#'
#' @inheritParams read_dat
#' @inheritParams arrow::write_dataset
#' @inheritDotParams readr::read_fwf
#' @export
#'
#' @examples
#' \dontrun{
#' data_dict_path <- dipr_example("starwars-dict.txt")
#' dict <- read.table(data_dict_path)
#' dat_path <- dipr_example("starwars-fwf.dat.gz")
#'
#' ## Create a partitioned datasets in the "bar" folder
#' dat_to_datasets(
#'     data_path = dat_path,
#'     data_dict = dict,
#'     path = "starwars_arrow",
#'     partitioning = "species",
#'     chunk_size = 2)
#'     }
#'
dat_to_datasets <- function(data_path, data_dict, chunk_size = 1000000, path, partitioning,
                            tz = "UTC", date_format = "%AD", time_format = "%AT", ...) {
  tdir <- file.path(tempdir(), gsub(".dat.gz", "", basename(data_path)), "arrow-tmp")
  dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
  f <- function(x, pos) {
    d <- readr::read_fwf(
      paste0(x, "\n"), ## required for readr to recognize as a string of data
      col_positions =
        readr::fwf_positions(
          start = data_dict$start,
          end = data_dict$stop,
          col_names = data_dict$name
        ),
      progress = TRUE,
      lazy = FALSE,
      locale = dipr_locale(tz = tz, date_format = date_format, time_format = time_format), ...
    )

    num_files <- length(list.files(tdir, pattern = ".parquet"))
    arrow::write_parquet(d, file.path(tdir, paste0("data-", num_files + 1,".parquet")))

    #print(readr::problems(d))
  }
  readr::read_lines_chunked(file = data_path, readr::SideEffectChunkCallback$new(f), chunk_size = chunk_size)

  arrow::open_dataset(tdir) %>%
    arrow::write_dataset(path = path, partitioning = partitioning)

  ## clean up
  unlink(list.files(tdir, pattern = "*.parquet", full.names = TRUE))
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



