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

#' Use the template export documentation
#'
#' This will open and save a markdown file with pre-populated
#' headings and sections to aid in generating output documentation.
#' This is not official documentation but rather a useful guide to
#' meet the standard for the output checkers. This function will
#' automatically output to "out/export" and number sequentially
#' the export folders.
#'
#' @inheritParams usethis::use_template
#' @param ... further arguments passed to usethis::use_template
#'
#' @export
#' @examples
#' \dontrun{
#' dipr_use_export_doc()
#' }


dipr_use_export_doc <- function(template = "export-doc.md", ...) {

  export_dir <- file.path("out/export")
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  export_suffix <- length(list.dirs(export_dir, recursive = FALSE)) + 1

  output_path <- file.path(export_dir, glue::glue("export",export_suffix), template)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  usethis::use_template(
    template = template,
    save_as = output_path,
    open = TRUE,
    package = "dipr",
    ...
  )
}
