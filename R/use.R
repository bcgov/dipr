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

#' Convenience function to generate templates for documenting export datasets
#'
#' The usage of this function is to print templated documentation to the console
#' and then copy and paste into the export document create with `dipr_use_export_doc`.
#' This function works only on csv exports.
#'
#' @inheritParams base::list.files
#' @inheritDotParams base::list.files
#'
#' @export
#'
dipr_document_output_groups <- function(path, ...) {
  files <- list.files(path, pattern = ".csv", full.names = TRUE, ...)

  invisible(lapply(files, function(x) {
    d <- utils::read.csv(x)
    cat(paste0(
      "### ", basename(x), "\n",
      nrow(d), " rows by ", ncol(d), " columns\n"
    ))

    cat(paste0("- ", names(d), ":"), sep = "\n")

    invisible(TRUE)
  }))
}

#' An opinionated template for package-like targets project
#'
#' This function will create a DESCRIPTION file, a `_targets.R` template and an `R`
#' directory to store function files.
#'
#' @inheritDotParams usethis::use_description
#'
#' @export
dipr_create_targets_project <- function(...) {
  dipr_use_description(...)
  usethis::use_template("_targets.R", open = TRUE, package = "dipr")
  dir.create("./R", showWarnings = FALSE)

  if (file.exists(".gitignore")) usethis::use_git_ignore("_targets", directory = ".")
  if (Sys.getenv("USERDOMAIN") == "POPDATA") usethis::use_template("Rprofile.R", ".Rprofile", package = "dipr")

}

dipr_use_description <- function(...) {
  bcgovr_desc <- list("License" = "Apache License (== 2.0) | file LICENSE",
                      "Authors@R" = paste0('c(person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
                                          person("Province of British Columbia", role = "cph"))')
  )

  usethis::use_description(fields = bcgovr_desc, check_name = FALSE, ...)
  usethis::use_package("desc", type = "Depends")
  usethis::use_package("dipr", type = "Depends")
}
