## Start code added by dipr ----------------

if (Sys.getenv("USERDOMAIN") == "POPDATA") {
  ## Choose cran mirror
  options(repos = "https://cran.popdata.bc.ca")

  if (interactive()) {
    .__has_devtools__ <- require(devtools, quietly = TRUE)
    require(testthat, quietly = TRUE)

    if (!.__has_devtools__) {
      warning("{devtools} is not installed.
    If this is the first time setting up a new machine consider running:
            1. .install_core_packages()
            2. .install_dipr_manual()")
    }
  }

  .install_core_packages <- function(...) {

    pkgs <- c(
      "devtools",
      "tidyverse",
      "duckdb",
      "duckplyr",
      "janitor",
      "glue",
      "fs",
      "arrow",
      "pscl",
      "gamm4",
      "marginaleffects",
      "nlme",
      "lme4",
      "mgcv",
      "greta",
      "bench",
      "tictoc",
      "digest",
      "R.utils",
      "styler"
    )

    install <- askYesNo(
      paste0(
        "This will install the following packages:\n",
        paste(pkgs, collapse = ", "),
        "\nWould you like to continue?"
      ),
      prompts = "Y/N/Cancel"
    )

    if (isTRUE(install)) {
      installed <- vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))

      install.packages(pkgs[!installed], type = "binary", ...)
    }

    message("You can now install the dipr package with .install_dipr_manual()")
  }

  .install_dipr_manual <- function() {
    ## First you need to store your credentials created in the step above. This is a one time only thing
    token <- credentials::git_credential_ask("https://projectsc.popdata.bc.ca")$password

    # Get most recent release
    releases <- httr::GET(glue::glue("https://projectsc.popdata.bc.ca/api/v4/projects/90/releases/"),
                      config = httr::add_headers(`Private-token` = token))
    rel <- httr::content(releases)[[1]]$tag_name

    ## These next two lines download the package to a temporary folder and install it.
    resp <- httr::GET("https://projectsc.popdata.bc.ca/api/v4/projects/90/repository/archive.tar.gz",
                      query = list(sha = rel),
                      httr::write_disk(tempfile(fileext = ".tar.gz")),
                      config = httr::add_headers(`Private-token` = token))
    remotes::install_local(resp[["content"]])
  }

  invisible(NULL)
}

## End code added by dipr ----------------
