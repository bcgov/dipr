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
dipr_reader <- function(data_path, data_dict, col_types, col_select, data_format = "fwf", ...) {

  delim = switch(data_format,
                 "fwf" = NULL,
                 "csv" = ",",
                 "tsv" = "\t",
                 "csv2" = ";")

  if (data_format == "fwf") {
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
  } else {
    readr::read_delim(
      data_path,
      delim = delim,
      col_types = col_types,
      col_select = !!col_select,
      ...
    )
  }


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
#' @noRd
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
#' @noRd
process_ocwaignore <- function() {
  file_globs <- readLines("_ocwaignore")
  files <- Sys.glob(file_globs)
  go <- utils::askYesNo(paste0("The following files will be deleted. Is that ok?\n",
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
#' This function:
#' 1. Creates a new branch
#' 2. Removes files that can't be imported - these are listed in the `_ocwaignore` file in the root of the repo
#' 3. Cleans `README.md` to comment out references to images and links that won't be available in the SRE.
#' 4. Commits the changes from 2 and 3 to the new branch and pushing that to GitHub. This branch can then be used as the basis for an import into OCWA.
#'
#' @param branch branch name to prepare for OCWA. Default `"ocwa-import"`
#' @param ask should the function stop and ask for verification to proceed? Default `TRUE`
#'
#' @return hash of commit
#' @export
ocwa_branch_export <- function(branch = "ocwa-import", ask = TRUE) {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop(
      "The gert package is required for this function"
    )
  }

  curr_branch <- gert::git_branch()

  proceed <- TRUE
  if (ask) {
    proceed <- utils::askYesNo(paste0("This will prepare a branch named ", branch,
                      "\nwith the current changes in ", curr_branch,
                      ".\nWould you like to proceed?\n"))
  }

  if(!isTRUE(proceed)) stop("export branch not created", call. = FALSE)

  if (gert::git_branch_exists(branch)) {
    # Checkout the branch and merge the changes from the branch you want to export.
    gert::git_branch_checkout(branch)
    gert::git_merge(curr_branch)
  } else {
    gert::git_branch_create(branch)
    cat(cli::col_green(cli::symbol$tick), " Created branch ", branch, "\n")
  }

  ocwa_ignored_files <- process_ocwaignore()

  readme_md_changed <- prepare_readme_for_ocwa("README.md")
  readme_rmd_changed <- prepare_readme_for_ocwa("README.Rmd")

  if (length(ocwa_ignored_files)) {
    gert::git_add(ocwa_ignored_files)
    cat("The following files were removed for export:\n ",
        paste(cli::col_green(cli::symbol$tick), ocwa_ignored_files, collapse = "\n  "),
        "\n")
  }
  if (readme_md_changed) {
    cat(cli::col_green(cli::symbol$tick), " Cleaned README.md\n")
    gert::git_add("README.md")
  } else {
    cat(cli::col_green(cli::symbol$tick), " No changes made to README.md\n")
  }

  if (readme_rmd_changed) {
    cat(cli::col_green(cli::symbol$tick), " Cleaned README.Rmd\n")
    gert::git_add("README.Rmd")
  } else {
    cat(cli::col_green(cli::symbol$tick), " No changes made to README.Rmd\n")
  }

  if (!nrow(gert::git_status())) {
    message("No changes made")
  } else {
    gert::git_commit("prepare files for ocwa import")
  }

  message("pushing to branch: ", branch, ". Use this branch to prepare your OCWA import.")
  gert::git_push()

  message("Switching your local branch back to ", curr_branch)
  gert::git_branch_checkout(curr_branch)
}
