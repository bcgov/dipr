#' Update the dipr package from gitlab
#'
#' This function clones the dipr gitlab repo and install the package. This
#' is a useful function update the package and saves the user from having to
#' manually accomplish each step.
#'
#' @param ref A tag of specific commit hash
#' @inheritParams devtools::install
#'
#' @export

dipr_update <- function(quiet = TRUE, ref = NULL) {

  cat("You are about to update the dipr package. Are you sure you want to do this?")
  ans <- utils::menu(c("Yes", "No"))
  if(ans != 1) stop("Have a nice day then!", call. = TRUE)

  if(Sys.getenv("USERDOMAIN") != "POPDATA") {
    stop("This function only works inside POPDATA's secure research environment
       If you are using this outside that environment devtools::install_github will work fine",
         call. = FALSE)
  }


  temp_dir <- tempdir()

  if (unlink(temp_dir, recursive = TRUE) !=0L) {
    stop(glue::glue("There was problem deleting previous files.
                   Please go to {temp_dir} and manually delete the `git_clone` directory", call. = FALSE))
  }

  on.exit(unlink(temp_dir, recursive = TRUE))

  git_clone_dir <- file.path(temp_dir, "git_clone")

  if(is.null(ref)) {
    git_clone_cmd <- glue::glue("git clone https://projectsc.popdata.bc.ca/shares/dipr {git_clone_dir}")
  } else {
    git_clone_cmd <- glue::glue("git clone --branch {ref} https://projectsc.popdata.bc.ca/shares/dipr {git_clone_dir}")
  }
  clone_status <- system(git_clone_cmd, show.output.on.console = quiet)

  if (clone_status == 0L) {
    message(glue::glue("Install to {Sys.getenv('R_LIBS_USER')}"))
    devtools::install(pkg = git_clone_dir, dependencies = FALSE, quiet = quiet)

  } else{
    stop(glue::glue("'{git_clone_cmd}' was successful with a '{clone_status}' status"), call. = FALSE)
  }

  invisible(TRUE)
}
