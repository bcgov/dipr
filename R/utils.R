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
dipr_reader <- function(data_path, data_dict, col_types, col_select, ...) {

    readr::read_fwf(
      data_path,
      col_positions =
        readr::fwf_positions(
          start = data_dict$start,
          end = data_dict$stop,
          col_names = data_dict$name
        ),
      col_types = col_types,
      col_select = !!col_select,
      ...
    )

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

#' Comment out html in the file that contains images and
#' urls that can't be accessed inside the SRE
#'
#' @param readme path to README file. Default `"README.md"`
#'
#' @return logical value if README was changed.
prepare_readme_for_ocwa <- function(readme = "README.md") {
  readme_txt <- readme_txt_orig <- readLines(readme)

  badges_lines <- which(grepl("badges: (start|end)", readme_txt))

  badges_to_comment <- seq(badges_lines[1] + 1, max(badges_lines) - 1)

  # Only do if not already commented
  badges_to_comment <- badges_to_comment[!grepl("<\\!--", readme_txt[badges_to_comment])]
  if (length(badges_to_comment)) {
    readme_txt[badges_to_comment] <- gsub("(.*)", "<!-- \\1 -->", readme_txt[badges_to_comment])
  }

  img_lines <- !grepl("(?=<!--).*<img", readme_txt, perl = TRUE)

  if (length(img_lines)) {
    readme_txt[img_lines] <- gsub("(.*)(<img.+?/>)(.*)", "\\1 <!-- \\2 --> \\3",
                                  readme_txt[img_lines])
  }

  changed <- !isTRUE(all.equal(readme_txt, readme_txt_orig))
  if (changed) writeLines(readme_txt, readme)
  changed
}

#' Remove files from repo that are listed in `_ocwaignore` file.
#'
#' @return files that were removed
process_ocwaignore <- function() {
  file_globs <- readLines("_ocwaignore")
  files <- Sys.glob(file_globs)
  go <- askYesNo(paste0("The following files will be deleted. Is that ok?\n",
                  paste(files, collapse = "\n"), "\n"))
  if (isTRUE(go)) {
    unlink(files)
    files
  }
}

#' Prepare repo for import into the SRE through OCWA
#'
#' Removes files listed in `_ocwaignore` and processes `README.md`
#' so that it is not using images or URLs that can't be accessed in the SRE.
#'
#' Then checks out a branch in `{dipr}` and commits those changes to that branch.
#' Then use that branch to import into OCWA.
#'
#' @param branch branch name to prepare for OCWA. Default `"ocwa-import"`
#'
#' @return hash of commit
#' @export
ocwa_branch_export <- function(branch = "ocwa-import") {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop(
      "The gert package is required for this function"
    )
  }
  if (gert::git_branch_exists(branch)) {
    gert::git_branch_checkout(branch)
  } else {
    gert::git_branch_create(branch)
  }

  ocwa_ignored_files <- process_ocwaignore()
  readme_changed <- prepare_readme_for_ocwa()

  if (length(ocwa_ignored_files)) {
    gert::git_add(ocwa_ignored_files)
  }
  if (readme_changed) {
    gert::git_add("README.md")
  }

  if (nrow(gert::git_status())) {
    gert::git_commit("remove files for ocwa import")
  }
}
