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

## a function to handle the initial READ of data
vroom_reader <- function(data_path, data_dict, col_types, col_select, ...) {

  ## a bit of hack because vroom doesn't seem to like NULLs
  ## in the col_select
  if(is.null(col_select)) {
    d <- vroom::vroom_fwf(
      data_path,
      col_positions =
        vroom::fwf_positions(
          start = data_dict$start,
          end = data_dict$stop,
          col_names = data_dict$name
          ),
      col_types = col_types,
      ...
      )
  } else {
    d <- vroom::vroom_fwf(
      data_path,
      col_positions =
        vroom::fwf_positions(
          start = data_dict$start,
          end = data_dict$stop,
          col_names = data_dict$name
        ),
      col_types = col_types,
      col_select = !!col_select,
      ...
    )
  }

  d
}



col_selector <- function(dict, col_select) {
  df <- data.frame(matrix(ncol = length(dict$name), nrow = 0))
  names(df) <- dict$name
  col_select <- names(dplyr::select(df, col_select))
}


## Accept either NULL or file
dip.file.exists <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  } else {
    file.exists(x)
  }
}


