#' Update the dipr package from gitlab
#'
#' This function is a thin wrapper around `install_sre_gitlab` and is specific to
#' updating dipr. The default behaviour of this function is to only install if your
#' current version needs updating.
#'
#' @inheritParams install_sre_gitlab
#' @param ... passed to install_sre_gitlab
#' @export

dipr_update <- function(ref = NULL, ...) {
  install_sre_gitlab("dipr", ref = ref, ...)
}

#' Install a package from the SRE gitlab.
#'
#' This function will install packages other than dipr from the SRE gitlab. The default behaviour
#' is to install releases.
#'
#' @param repo The name of the repo.
#' @param ref A tag of specific commit hash. Default to NULL which is the most recent release. "dev" will
#' install at the most recent commit.
#' @inheritParams remotes::install_local
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_sre_gitlab("hawkeye")
#' }
install_sre_gitlab <- function(repo, ref = NULL, quiet = TRUE) {
  cat(glue::glue("You are about to install/update the {repo} package. Are you sure you want to do this?"))
  ans <- utils::menu(c("Yes", "No"))
  if(ans != 1) stop("Have a nice day then!", call. = TRUE)

  if(Sys.getenv("USERDOMAIN") != "POPDATA") {
    stop("This function only works inside POPDATA's secure research environment
       If you are using this outside that environment devtools::install_github will work fine",
         call. = FALSE)
  }

  token <- set_gitlab_credentials()$password

  sha <- update_needed(repo, ref, token)

  ## get id
  id <- get_repo_id(repo, token)


  tmp_file <- tempfile(fileext = ".tar.gz")
  resp <- httr::GET(glue::glue("https://projectsc.popdata.bc.ca/api/v4/projects/{id}/repository/archive.tar.gz"),
                    query = list(sha = sha),
                    httr::write_disk(tmp_file),
                    config = httr::add_headers(`Private-token` = token))

  on.exit(unlink(tmp_file), add = TRUE)
  httr::stop_for_status(resp)
  remotes::install_local(tmp_file)
}


update_needed <- function(repo, ref = NULL, token) {

  if (isTRUE(ref == "dev")) return(NULL)

  if (!is.null(ref)) return(ref)

  ## get id
  id <- get_repo_id(repo, token)

  # get a release - note the use of the project ID (90) vs the name (dipr)
  releases <- httr::GET(glue::glue("https://projectsc.popdata.bc.ca/api/v4/projects/{id}/releases/"),
                        config = httr::add_headers(`Private-token` = token))

  httr::stop_for_status(releases)
  releases_content <- httr::content(releases)

  ## Get release dates
  release_dates <- vapply(seq_along(releases_content), function(x) {
    release_at = releases_content[[x]]$released_at
  }, FUN.VALUE = character(1))

  if (rlang::is_empty(release_dates)) stop("this package has no releases. try modifying the ref argument to 'dev'")

  if (is.null(ref)) {
    idx_most_recent_release <- which.max(as.POSIXct(release_dates))
    ref <- releases_content[[idx_most_recent_release]]$tag_name

    # get a file - note the 'raw' endpoint to just get the file
    resp <- httr::GET(glue::glue("https://projectsc.popdata.bc.ca/api/v4/projects/{id}/repository/files/DESCRIPTION/raw"),
                      query = list(ref = ref),
                      config = httr::add_headers(`Private-token` = token))
    httr::stop_for_status(resp)

    gitlab_version <- read.dcf(textConnection(httr::content(resp, as = "parsed")), fields = "Version")

    installed_version <- get_installed_version(pkg = repo)

    should_install <- package_version(gitlab_version) > installed_version

    if (!should_install) stop(glue::glue("{repo} update not needed. You are using the most recently released version {installed_version}."), call. = FALSE)

    releases_content[[idx_most_recent_release]]$commit$id
  }

}

get_installed_version <- function(pkg) {
  # find lib pkg is in:
  libs <- .libPaths()
  pkg_paths <- vapply(libs, function(x) file.path(x, pkg), FUN.VALUE = "")
  lib <- libs[dir.exists(pkg_paths)]

  # return version 0.0 if not installed
  if (!length(lib)) return(package_version("0.0"))

  # in case > 1 path, pick the first one
  local_desc <- file.path(lib[1], pkg, "DESCRIPTION")

  package_version(read.dcf(local_desc, fields = "Version"))
}

#' Return a data.frame of all repos available to the project you are currently in
#'
#' Access information on the repo in the current project in a data.frame.
#'
#' @param token Credentials from gitlab, accessed via: `set_gitlab_credentials()$password`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_gitlab_sre_repos(set_gitlab_credentials()$password)
#' }
get_gitlab_sre_repos <- function(token = set_gitlab_credentials()$password) {
  res <- httr::GET("https://projectsc.popdata.bc.ca/api/v4/projects/",
                   query = list(per_page = 100, simple = TRUE),
                   config = httr::add_headers(`Private-token` = token))
  httr::stop_for_status(res)

  cont <- httr::content(res)

  cont_list <- lapply(seq_along(cont), function(x) {
    data.frame(
      id = cont[[x]]$id,
      name = cont[[x]]$name,
      created_at = as.POSIXlt(cont[[x]]$created_at),
      web_url = cont[[x]]$web_url,
      last_activity_at = cont[[x]]$last_activity_at
    )}
  )

  dplyr::bind_rows(cont_list)

}

get_repo_id <- function(repo_name, token) {

  repo_df <- get_gitlab_sre_repos(token)

  id <- repo_df[repo_df$name == repo_name,]$id

  if (rlang::is_empty(id)) {
    stop("That repository is not available. Perhaps you need to create or join it first by visiting this url: https://projectsc.popdata.bc.ca/v1/selfserve/main", call. = FALSE)
  }

  id
}
