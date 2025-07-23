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

#' Set gitlab credentials from R
#'
#' Running this function raises the Windows Git Credential Manager to enter
#' the GitLat PAT. Accessing the GitLab PAT requires following these steps:
#' - Go to this website: [https://projectsc.popdata.bc.ca/](https://projectsc.popdata.bc.ca/).
#' - Scroll down and click on "Sign in with Pop Data SSO".
#' - Next go to this site: [https://projectsc.popdata.bc.ca/profile/personal_access_tokens](https://projectsc.popdata.bc.ca/profile/personal_access_tokens)
#' - Create a new Personal Access Token enabling each scope and typing in your username. This should create a new personal access token. Those are your "git credentials". Save those on your "U" drive along with your user name.
#' @inheritDotParams credentials::git_credential_ask save verbose
#' @export
set_gitlab_credentials <- function(...) {

  if (Sys.getenv("USERDOMAIN") != "POPDATA") {
    stop("This function only works inside POPDATA's secure research environment.",
       call. = FALSE)
  }

  credentials::git_credential_ask(url = "https://projectsc.popdata.bc.ca", ...)
}

#' Save your Rstudio preferences, you local `.Rprofile`, and your local
#' `.Renviron` to your SRE network drive for restoring later when you set up a
#' new machine.
#'
#' Once you have saved your preferences, you can restore them on a new machine
#' with [setup_rstudio()]
#'
#' @seealso [setup_rstudio()]
#'
#' @return `TRUE` if successful, `FALSE` if not
#' @export
save_rstudio_prefs <- function() {
  homedrive <- file.path("R:", Sys.getenv("USERNAME"))
  saved_rstudio_prefs <- file.path(homedrive, "Rstudio")
  dir.create(saved_rstudio_prefs, showWarnings = FALSE)

  appdata <- Sys.getenv("APPDATA")

  rstudio_prefs_path <- file.path(
    appdata,
    "RStudio",
    c("dictionaries", "keybindings", "rstudio-prefs.json", "Preferences")
  )

  file.copy(
    rstudio_prefs_path,
    saved_rstudio_prefs,
    recursive = TRUE
  )
  save_rprofile()
  save_renviron()
}

#' Save your .Rprofile to your SRE network drive for restoring later when you
#' set up a new machine
#'
#' @return `TRUE` if successful, `FALSE` if not
#' @export
save_rprofile <- function() {
  homedrive <- file.path("R:", Sys.getenv("USERNAME"))
  rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
  file.copy(rprofile, file.path(homedrive, "saved.Rprofile"), overwrite = TRUE)
}

#' Save your .Renviron to your SRE network drive for restoring later when you
#' set up a new machine
#'
#' @return `TRUE` if successful, `FALSE` if not
#' @export
save_renviron <- function() {
  homedrive <- file.path("R:", Sys.getenv("USERNAME"))
  renviron <- file.path(Sys.getenv("HOME"), ".Renviron")
  file.copy(renviron, file.path(homedrive, "saved.Renviron"), overwrite = TRUE)
}

#' Restore your Rstudio preferences from those saved in your U:/ drive
#'
#' Restores a previously saved Rstudio preferences, a previously saved
#' .Rprofile, and helps configure gitlab credentials when you set up a new
#' SRE machine
#'
#' Save your rstudio prefs to your U:/ user drive: [save_rstudio_prefs()]
#' Save a .Rprofile file to your U:/ drive: [save_rprofile()]
#'
#' @export
setup_rstudio <- function() {
  homedrive <- file.path("R:", Sys.getenv("USERNAME"))

  set_gitlab_creds_from_pat_file(homedrive)
  get_rstudio_prefs(homedrive)
  restore_rprofile(homedrive)
  restore_renviron(homedrive)
  invisible(NULL)
}

get_rstudio_prefs <- function(homedrive) {
  appdata <- Sys.getenv("APPDATA")
  saved_rstudio_prefs <- file.path(homedrive, "Rstudio")
  if (!dir.exists(saved_rstudio_prefs)) {
    cli::cli_alert_warning("No saved RStudio prefs at {.path {saved_rstudio_prefs}}")
    return(FALSE)
  }
  cli::cli_alert_info("Restoring Rstudio preferences")
  file.copy(saved_rstudio_prefs, appdata, recursive = TRUE, overwrite = TRUE)
}

#' Restore previously saved .Rprofile
#'
#' @param homedrive path to your home directory
#'
#' @return `TRUE` or `FALSE`
#' @export
restore_rprofile <- function(homedrive = file.path("R:", Sys.getenv("USERNAME"))) {
  saved_rprofile <- file.path(homedrive, "saved.Rprofile")
  if (!file.exists(saved_rprofile)) {
    cli::cli_alert_warning("No saved .Rprofile at {.path {saved_rprofile}}")
    return(FALSE)
  }
  cli::cli_alert_info("Restoring .Rprofile")
  file.copy(saved_rprofile, file.path(Sys.getenv("HOME"), ".Rprofile"), overwrite = TRUE)
}

#' Restore previously saved .Renviron
#'
#' @param homedrive path to your home directory
#'
#' @return `TRUE` or `FALSE`
#' @export
restore_renviron <- function(homedrive = file.path("R:", Sys.getenv("USERNAME"))) {
  saved_renviron <- file.path(homedrive, "saved.Renviron")
  if (!file.exists(saved_renviron)) {
    cli::cli_alert_warning("No saved .Renviron at {.path {saved_renviron}}")
    return(FALSE)
  }
  cli::cli_alert_info("Restoring .Renviron")
  file.copy(saved_renviron, file.path(Sys.getenv("HOME"), ".Renviron"), overwrite = TRUE)
}

set_gitlab_creds_from_pat_file <- function(homedrive) {
  cli::cli_alert_info("Setting up GitLab credentials")
  pat_path <- file.path(homedrive, "PAT.txt")
  use_clip <- FALSE
  if (!file.exists(pat_path)) {
    cli::cli_alert_warning(
      "Unable to find file containing your PAT at {.file {pat_path}}. Please have it ready if you have it saved elsewhere, otherwise generate one in GitLab and save it in {.file {pat_path}} for future use.")
  } else {
    pat <- trimws(readLines(pat_path, n = 1, warn = FALSE))
    use_clip <- TRUE
    old_clip <- suppressWarnings(clipr::read_clip(allow_non_interactive = TRUE))
    clipr::write_clip(pat)
    cli::cli_alert_success("Your PAT has been copied to your clipboard from {.file {pat_path}}. Paste it as your password if prompted. If not prompted, it means your GitLab credentials are already configured.")
  }
  out <- credentials::git_credential_ask("https://projectsc.popdata.bc.ca")
  if (use_clip && !is.null(old_clip)) clipr::write_clip(old_clip)
  invisible(out)
}

#' Write a default user-level .Rprofile and .Renviron
#'
#' These files will allow you to install packages from the internal popdata
#' CRAN mirror at https://cran.popdata.bc.ca by enabling certain settings. These
#' settings will establish your personal library on your C: drive.
#'
#' @param open open files for editing after they are created
#' @param overwrite should it overwrite existing files? Default `TRUE`. If
#'     `FALSE` it will append.
#'
#' @return TRUE, invisibly
#' @export
use_sre_startup_files <- function(open = TRUE, overwrite = TRUE) {
  if (!is_sre()) {
    stop("Not in the SRE. Using these files outside the SRE doesn't make sense.")
  }

  rprofile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  new_rprofile <- readLines(
    fs::path_package("dipr", "templates", "home.Rprofile")
  )

  new_renviron <- whisker::whisker.render(
    readLines(fs::path_package("dipr", "templates", "home.Renviron")),
    data = list(
      local_appdata_path = normalizePath(Sys.getenv("LOCALAPPDATA"), winslash = "/")
    )
  )

  cat(new_rprofile, file = rprofile_path, sep = "\n", append = !overwrite)
  cat(new_renviron, file = renviron_path, sep = "\n", append = !overwrite)

  if (open) {
    usethis::edit_r_profile()
    usethis::edit_r_environ()
  }

  invisible(TRUE)
}
