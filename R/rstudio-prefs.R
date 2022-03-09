#' Copy saved RStudio preferences from PopData Gitlab repo to you local SRE
#' machine
#'
#' DSP R Users are storing their Rstudio preferences in the repo at:
#' <https://projectsc.popdata.bc.ca/shares/using-sre-gitlab>, in a named user
#' folder under `rstudio-prefs` (See the
#' [generic](https://projectsc.popdata.bc.ca/shares/using-sre-gitlab/-/tree/develop/rstudio-prefs%generic)
#' folder for a minimal recommended starting point, or the
#' [andy](https://projectsc.popdata.bc.ca/shares/using-sre-gitlab/-/tree/develop/rstudio-prefs%2Fandy)
#' folder for a more comprehensive example). Your local RStudio preferences are
#' located in C:/Users/\[your-user-name\]/AppData/Roaming/Rstudio. When you have
#' an instance of Rstudio set up to your liking, copy these to your named folder
#' in the repo of your choice, then use this function to restore those
#' preferences to a new SRE machine.
#'
#' @param folder The folder in which your preferences are stored in the repo.
#'   For a minimal recommended setup, use `"rstudio-prefs/generic"` as a starting point.
#' @param repo The name of the GitLab repo in which you have stored your
#'   preferences. Default `"using-sre-gitlab"`
#'
#' @return a character vector of file paths of the files that were copied to
#'   your machine (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#'  restore_rstudio_prefs(folder = "rstudio-prefs/andy")
#' }
restore_rstudio_prefs <- function(folder, repo = "using-sre-gitlab") {
  token <- set_gitlab_credentials()$password

  id <- get_repo_id(repo, token)

  resp <- httr::GET(glue::glue("https://projectsc.popdata.bc.ca/api/v4/projects/{id}/repository/tree"),
                    query = list(path = folder, recursive = TRUE),
                    config = httr::add_headers(`Private-token` = token))

  httr::stop_for_status(resp)

  content <- httr::content(resp)

  if (!length(content)) {
    stop("No content found at folder '", folder,"' in gitlab repo '", repo, "'",
         call. = FALSE)
  }

  pref_file_blobs <- Filter(function(x) x$type == "blob", content)

  rs_prefs_dir <- file.path(Sys.getenv("APPDATA"), "RStudio")

  files_written <- lapply(pref_file_blobs, function(x) {
    sha <- x$id

    resp <- httr::GET(glue::glue("https://projectsc.popdata.bc.ca/api/v4/projects/{id}/repository/blobs/{sha}/raw"),
                      config = httr::add_headers(`Private-token` = token))

    httr::stop_for_status(resp)

    content <- httr::content(resp, as = "text")

    outfile <- normalizePath(file.path(rs_prefs_dir,
                                       remove_root_from_path(x$path, folder)),
                             winslash = "/",
                             mustWork = FALSE)

    dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(outfile)) {
      pass <- utils::askYesNo(paste("File exists:", outfile, "\nWould you like to replace it?"),
                       prompts = "Y/N/cancel")
      if (is.na(pass)) stop("Operation cancelled", call. = FALSE)
      if (!pass) return(invisible(NULL))
    }

    cat(content, file = outfile, append = FALSE)

    outfile
  })

  message("Copied files:\n  ", paste(unlist(files_written), collapse = "\n  "))
  message("You will need to restart RStudio for the changes to take effect.")

  invisible(unlist(files_written))
}

remove_root_from_path <- function(path, root) {
  if (length(root) != 1) stop("root must be length 1")

  root_length <- length(strsplit(root, "/|\\\\")[[1]])

  path_split <- strsplit(path, "/|\\\\")

  vapply(path_split, function(x) {
    x <- x[(root_length + 1):length(x)]
    paste0(x, collapse = .Platform$file.sep)
  }, FUN.VALUE = character(1))
}
